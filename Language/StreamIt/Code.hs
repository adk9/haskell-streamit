module Language.StreamIt.Code (code) where

import Data.List
import qualified Control.Monad.State as S

import Language.StreamIt.Core
import Language.StreamIt.Filter
import Language.StreamIt.Graph

indent :: String -> String
indent = unlines . map ("\t" ++) . lines

-- | Generate StreamIt program.
code :: TypeSig -> Name -> StatementS -> IO (FilePath)
code ty name node = do
  writeFile (name ++ ".str") $
    (intercalate "\n\n" $ map codeFilter fs)
    ++ "\n" ++ (intercalate "\n\n" $ map codeGraph gs)
    ++ "\n" ++ codeGraph (ty, name, node) ++ "\n"
  return (name ++ ".str")
  where
    (fs, gs) = S.execState (findDefs node) ([],[])

-- | Generate StreamIt code for the aggregate filters.
codeGraph :: (TypeSig, Name, StatementS) -> String
codeGraph (ty, name, sn) = case sn of
  DeclS (V inp n v) -> if inp
                       then ""
                       else showConstType (const' v) ++ " " ++ n ++ " = "
                            ++ showConst (const' v) ++ ";\n"
  AssignS a b       -> show a ++ " = " ++ codeExpr b ++ ";"
  BranchS a b Empty -> "if (" ++ codeExpr a ++ ") {\n"
                       ++ indent (codeGraph (ty, name, b)) ++ "}\n"
  BranchS a b c     -> "if (" ++ codeExpr a ++ ") {\n"
                       ++ indent (codeGraph (ty, name, b))
                       ++ "} else {\n" ++ indent (codeGraph (ty, name, c)) ++ "}\n"
  AddS _ n _ args   -> "add " ++ n ++ "(" ++ (intercalate ", " $ map codeExpr args)
                       ++ ");\n"
  Pipeline False a  -> ty ++ " pipeline " ++ name ++ "("
                       ++ (intercalate ", " $ codeInputS a) ++ ")\n{\n"
                       ++ (indent $ codeGraph (ty, name, a)) ++ "}\n"
  Pipeline True a   -> "add pipeline {\n"
                       ++ (indent $ codeGraph (ty, name, a)) ++ "}\n"
  SplitJoin False a -> ty ++ " splitjoin " ++ name ++ " {\n"
                       ++ (indent $ codeGraph (ty, name, a)) ++ "}\n"
  SplitJoin True a  -> "add splitjoin {\n"
                       ++ (indent $ codeGraph (ty, name, a)) ++ "}\n"
  Split a           -> "split " ++ show a ++ ";\n"
  Join a            -> "join " ++ show a ++ ";\n"
  Chain a b         -> codeGraph (ty, name, a) ++ codeGraph (ty, name, b)
  Empty             -> ""

-- | Walk down the Stream AST and find the declared inputs to print.
codeInputS :: StatementS -> [String]
codeInputS a = case a of
  DeclS (V inp n v) -> if inp then [showConstType (const' v) ++ " " ++ n] else []
  AssignS _ _       -> []
  BranchS _ b c     -> codeInputS b ++ codeInputS c
  AddS _ _ _ _      -> []
  Pipeline _ a      -> codeInputS a
  SplitJoin _ a     -> codeInputS a
  Split _           -> []
  Join _            -> []
  Chain a b         -> codeInputS a ++ codeInputS b
  Empty             -> []

-- | Generate StreamIt code inside a filter.
codeFilter :: (TypeSig, Name, Statement) -> String
codeFilter (ty, name, stmt) = ty ++ " filter " ++ name ++ "("
                              ++ (intercalate ", " $ codeInput stmt) ++ ")\n{\n"
                              ++ indent (codeStmt name stmt)
                              ++ "}"

-- | Walk down the Filter AST and find the declared inputs to print.
codeInput :: Statement -> [String]
codeInput a = case a of
  Decl (V inp n v) -> if inp then [showConstType (const' v) ++ " " ++ n] else []
  Assign _ _       -> []
  Branch _ b c     -> codeInput b ++ codeInput c
  Loop _ _ _ a     -> codeInput a
  Sequence a b     -> codeInput a ++ codeInput b
  Init a           -> codeInput a
  Work _ a         -> codeInput a
  Push _           -> []
  Pop              -> []
  Println _        -> []
  Null             -> []

instance Show Statement where show = codeStmt "none"

-- | Generate code corresponding to a StreamIt statement.
codeStmt :: Name -> Statement -> String
codeStmt name a = case a of
  Decl (V inp n v) -> if inp then ""
                      else showConstType (const' v) ++ " " ++ n ++ " = "
                           ++ showConst (const' v) ++ ";\n"
  Assign _ _       -> codeStmtExpr a ++ ";\n"
  Branch a b Null  -> "if (" ++ codeExpr a ++ ") {\n" ++ indent (codeStmt name b) ++ "}\n"
  Branch a b c     -> "if (" ++ codeExpr a ++ ") {\n" ++ indent (codeStmt name b)
                      ++ "} else {\n" ++ indent (codeStmt name c) ++ "}\n"
  Loop Null a Null b -> "while (" ++ codeExpr a ++ ") {\n"
                        ++ indent (codeStmt name b) ++ "}\n"
  Loop a b c d     -> "for (" ++ codeStmtExpr a ++ "; " ++ codeExpr b ++ "; "
                      ++ codeStmtExpr c ++ ") {\n" ++ indent (codeStmt name d)
                      ++ "}\n"
  Sequence a b     -> codeStmt name a ++ codeStmt name b
  Init a           -> "init {\n" ++ indent (codeStmt name a) ++ "}\n"
  Work (a, b, c) d -> "work" ++ showFlowRate " push " a
                      ++ showFlowRate " pop " b
                      ++ showFlowRate " peek " c
                      ++ " {\n" ++ indent (codeStmt name d) ++ "}\n"
  Push a           -> "push(" ++ codeExpr a ++ ");\n"
  Pop              -> codeStmtExpr a ++ ";\n"
  Println a        -> "println(" ++ codeStmtExpr a ++ ");\n"
  Null             -> ""
  where
    codeStmtExpr :: Statement -> String
    codeStmtExpr a = case a of
      Assign a b     -> show a ++ " = " ++ codeExpr b
      Sequence a b   -> codeStmtExpr a ++ codeStmtExpr b
      Push _         -> ""
      Pop	     -> "pop()"
      Decl _	     -> ""
      Branch _ _ _   -> ""
      Loop _ _ _ _   -> ""
      Init _         -> ""
      Work _ _       -> ""
      Println _      -> ""
      Null	     -> ""

    showFlowRate :: String -> E Int -> String
    showFlowRate token a = case a of
      Const 0 -> ""
      _       -> token ++ codeExpr a

codeExpr :: E a -> String
codeExpr a = case a of
  Ref a     -> show a
  Peek a    -> "peek(" ++ codeExpr a ++ ")"
  Const a   -> showConst $ const' a
  Add a b   -> group [codeExpr a, "+", codeExpr b]
  Sub a b   -> group [codeExpr a, "-", codeExpr b]
  Mul a b   -> group [codeExpr a, "*", codeExpr b]
  Div a b   -> group [codeExpr a, "/", codeExpr b]
  Mod a b   -> group [codeExpr a, "%", showConst (const' b)]
  Not a     -> group ["!", codeExpr a]
  And a b   -> group [codeExpr a, "&&",  codeExpr b]
  Or  a b   -> group [codeExpr a, "||",  codeExpr b]
  Eq  a b   -> group [codeExpr a, "==",  codeExpr b]
  Lt  a b   -> group [codeExpr a, "<",   codeExpr b]
  Gt  a b   -> group [codeExpr a, ">",   codeExpr b]
  Le  a b   -> group [codeExpr a, "<=",  codeExpr b]
  Ge  a b   -> group [codeExpr a, ">=",  codeExpr b]
  Mux a b c -> group [codeExpr a, "?", codeExpr b, ":", codeExpr c] 
  where
    group :: [String] -> String
    group a = "(" ++ intercalate " " a ++ ")"

showConst :: Const -> String
showConst a = case a of
  Bool  True  -> "true"
  Bool  False -> "false"
  Int   a     -> show a
  Float a     -> show a

showConstType :: Const -> String
showConstType a = case a of
  Bool  _ -> "boolean"
  Int   _ -> "int"
  Float _ -> "float"
