module Language.StreamIt.Backend.TBB (codeTBB) where

import Data.List
import qualified Control.Monad.State as S

import Language.StreamIt.Core
import Language.StreamIt.Filter
import Language.StreamIt.Graph

indent :: String -> String
indent = unlines . map ("\t" ++) . lines

-- | Generate StreamIt program.
codeTBB :: String -> Name -> StatementS -> IO (FilePath)
codeTBB ty name node = do
  (fs, gs) <- S.execStateT (findDefs node) ([],[])
  filters <- mapM codeFilter fs
  graphs <- mapM codeGraph gs
  mains <- codeGraph (ty, name, node)
  writeFile (name ++ ".cpp") $
    "#include \"tbb/pipeline.h\"\n"
    ++ "#include \"tbb/tick_count.h\"\n"
    ++ "#include \"tbb/task_scheduler_init.h\"\n"
    ++ "#include \"tbb/tbb_allocator.h\"\n"
    ++ "#include <cstring>\n"
    ++ "#include <cstdlib>\n"
    ++ "#include <cstdio>\n"
    ++ "#include <cctype>\n\n"
    ++ (intercalate "\n\n" filters)
    ++ "\n" ++ (intercalate "\n\n" graphs)
    ++ "\n" ++ mains ++ "\n"
  return (name ++ ".cpp")

-- | Generate StreamIt code for the aggregate filters.
codeGraph :: GraphInfo -> IO String
codeGraph (ty, name, sn) = case sn of
  DeclS (V inp n v) -> if inp
                       then return ""
                       else return (showConstType (const' v) ++ " " ++ n
                                    ++ " = " ++ showConst (const' v) ++ ";\n")
  AssignS a b       -> return (show a ++ " = " ++ codeExpr b ++ ";")
  BranchS a b Empty -> do
    bs <- codeGraph (ty, name, b)
    return ("if (" ++ codeExpr a ++ ") {\n" ++ indent bs ++ "}\n")
  BranchS a b c     -> do
    bs <- codeGraph (ty, name, b)
    cs <- codeGraph (ty, name, c)
    return ("if (" ++ codeExpr a ++ ") {\n" ++ indent bs ++ "} else {\n"
            ++ indent cs ++ "}\n")
  AddS n _ args     -> return ("add " ++ n ++ "("
                               ++ (intercalate ", " $ map codeExpr args) ++ ");\n")
  Pipeline False a  -> do
    as <- codeGraph (ty, name, a)
    return (ty ++ " pipeline " ++ name ++ "("
            ++ (intercalate ", " $ codeInputS a) ++ ")\n{\n"
            ++ indent as ++ "}\n")
  Pipeline True a   -> do
    as <- codeGraph (ty, name, a)
    return ("add pipeline {\n" ++ indent as ++ "}\n")
  SplitJoin False a -> do
    as <- codeGraph (ty, name, a)
    return (ty ++ " splitjoin " ++ name ++ " {\n" ++ indent as ++ "}\n")
  SplitJoin True a  -> do
    as <- codeGraph (ty, name, a)
    return ("add splitjoin {\n" ++ indent as ++ "}\n")
  Split a           -> return ("split " ++ show a ++ ";\n")
  Join a            -> return ("join " ++ show a ++ ";\n")
  Chain a b         -> do
    as <- codeGraph (ty, name, a) 
    bs <- codeGraph (ty, name, b)
    return (as ++ bs)
  Empty             -> return ""

-- | Walk down the Stream AST and find the declared inputs to print.
codeInputS :: StatementS -> [String]
codeInputS a = case a of
  DeclS (V inp n v) -> if inp then [showConstType (const' v) ++ " " ++ n] else []
  AssignS _ _       -> []
  BranchS _ b c     -> codeInputS b ++ codeInputS c
  AddS _ _ _        -> []
  Pipeline _ a      -> codeInputS a
  SplitJoin _ a     -> codeInputS a
  Split _           -> []
  Join _            -> []
  Chain a b         -> codeInputS a ++ codeInputS b
  Empty             -> []

-- | Generate StreamIt code inside a filter.
codeFilter :: FilterInfo -> IO String
codeFilter (ty, name, stmt) = return (ty ++ " filter " ++ name ++ "("
                                      ++ (intercalate ", " $ codeInput stmt)
                                      ++ ")\n{\n" ++ indent (codeStmt name stmt) ++ "}")

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
  Void _      -> ""


showConstType :: Const -> String
showConstType a = case a of
  Bool  _ -> "boolean"
  Int   _ -> "int"
  Float _ -> "float"
  Void _  -> "void"
