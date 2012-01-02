module Language.StreamIt.Code (code) where

import Data.List

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
    (fs, gs) = findDefs node

-- | Generate StreamIt code for the aggregate filters.
codeGraph :: (TypeSig, Name, StatementS) -> String
codeGraph (ty, name, sn) = case sn of
  DeclS (V inp n v) -> if inp then ""
                       else showConstType (const' v) ++ " " ++ n ++ " = "
                            ++ showConst (const' v) ++ ";\n"
  AssignS a b     -> show a ++ " = " ++ codeExpr b ++ ";"
  BranchS a b Empty -> "if (" ++ codeExpr a ++ ") {\n"
                     ++ indent (codeGraph (ty, name, b)) ++ "}\n"
  BranchS a b c   -> "if (" ++ codeExpr a ++ ") {\n"
                     ++ indent (codeGraph (ty, name, b))
                     ++ "}\nelse {\n" ++ indent (codeGraph (ty, name, c)) ++ "}\n"
  AddS _ n _ args -> "add " ++ n ++ "(" ++ (intercalate ", " $ map codeExpr args)
                     ++ ");\n"
  Pipeline a      -> ty ++ " pipeline " ++ name ++ " {\n"
                     ++ (indent $ codeGraph (ty, name, a)) ++ "}\n"
  SplitJoin a     -> ty ++ " splitjoin " ++ name ++ " {\n"
                     ++ (indent $ codeGraph (ty, name, a)) ++ "}\n"
  Split a         -> "split " ++ show a ++ ";\n"
  Join a          -> "join " ++ show a ++ ";\n"
  Chain a b       -> codeGraph (ty, name, a) ++ codeGraph (ty, name, b)
  Empty           -> ""

-- | Generate StreamIt code inside a filter.
codeFilter :: (TypeSig, Name, Statement) -> String
codeFilter (ty, name, stmt) = ty ++ " filter " ++ name ++ "("
                              ++ (intercalate ", " $ codeInput stmt) ++ ")\n{\n"
                              ++ indent (codeStmt name stmt)
                              ++ "}"

-- | Walk down the AST and find out the declared inputs to print.
codeInput :: Statement -> [String]
codeInput a = case a of
  Decl (V inp n v) -> if inp then [showConstType (const' v) ++ " " ++ n] else []
  Assign _ _       -> []
  Branch _ b Null  -> codeInput b
  Branch _ b c     -> codeInput b ++ codeInput c
  Loop _ _ _ a     -> codeInput a
  Sequence a b     -> codeInput a ++ codeInput b
  Init a           -> codeInput a
  Work _ a         -> codeInput a
  Push _           -> []
  Pop              -> []
  Peek _           -> []
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
                      ++ "}\nelse {\n" ++ indent (codeStmt name c) ++ "}\n"
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
  Peek a           -> "peek(" ++ show a ++ ");\n"
  Println a        -> "println(" ++ codeStmtExpr a ++ ");\n"
  Null             -> ""
  where
    codeStmtExpr :: Statement -> String
    codeStmtExpr a = case a of
      Assign a b     -> show a ++ " = " ++ codeExpr b
      Sequence a b   -> codeStmtExpr a ++ codeStmtExpr b
      Push _         -> ""
      Pop	     -> "pop()"
      Peek _         -> ""
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
