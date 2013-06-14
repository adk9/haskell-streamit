module Language.StreamIt.Backend.StreamIt (codeStreamIt) where

import Data.List
import qualified Control.Monad.State as S

import Language.StreamIt.Core
import Language.StreamIt.Filter
import Language.StreamIt.Graph

indent :: String -> String
indent = unlines . map ("\t" ++) . lines

-- | Generate StreamIt program.
codeStreamIt :: String -> Name -> StatementS -> IO (FilePath)
codeStreamIt ty name node = do
  (fs, gs) <- S.execStateT (findDefs node) ([],[])
  filters <- mapM codeFilter fs
  graphs <- mapM codeGraph gs
  mains <- codeGraph (ty, name, node)
  writeFile (name ++ ".str") $
    (intercalate "\n\n" filters)
    ++ "\n" ++ (intercalate "\n\n" graphs)
    ++ "\n" ++ mains ++ "\n"
  return (name ++ ".str")

-- | Generate StreamIt code for the aggregate filters.
codeGraph :: GraphInfo -> IO String
codeGraph (ty, name, sn) = case sn of
  DeclS v -> if inp v
             then return ""
             else let c = (const' $ val v) in
             if (isScalar c)
             then return (showConstType c ++ " " ++ (vname v)
                          ++ " = " ++ show (const' $ val v) ++ ";\n")
             else return (showConstType c ++ " " ++ (vname v) ++ ";\n")
  AssignS a b       -> return (show a ++ " = " ++ show b ++ ";\n")
  BranchS a b Empty -> do
    bs <- codeGraph (ty, name, b)
    return ("if (" ++ show a ++ ") {\n" ++ indent bs ++ "}\n")
  BranchS a b c     -> do
    bs <- codeGraph (ty, name, b)
    cs <- codeGraph (ty, name, c)
    return ("if (" ++ show a ++ ") {\n" ++ indent bs ++ "} else {\n"
            ++ indent cs ++ "}\n")
  LoopS Empty a Empty b -> do
    bs <- codeGraph (ty, name, b)
    return ("while (" ++ show a ++ ") {\n" ++ indent bs ++ "}\n")
  LoopS a b c d     -> do
    ds <- codeGraph (ty, name, d)
    return ("for (" ++ codeGraphExpr a ++ "; " ++ show b ++ "; "
      ++ codeGraphExpr c ++ ") {\n" ++ indent ds ++ "}\n")
  AddS n _ (Just a) Nothing Nothing -> return ("add " ++ n ++ "(" ++ show a ++ ");\n")
  AddS n _ (Just a) (Just b) Nothing -> return ("add " ++ n ++ "(" ++ show a
                                                ++ ", " ++ show b ++ ");\n")
  AddS n _ (Just a) (Just b) (Just c) -> return ("add " ++ n ++ "(" ++ show a
                                                 ++ ", " ++ show b ++ ", "
                                                 ++ show c ++ ");\n")
  AddS n _ _ _ _ -> return ("add " ++ n ++ "();\n")
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
  File rw ty name   -> case rw of
    False -> return ("add FileReader<" ++ showConstType ty ++ ">(\"" ++ name ++ "\");\n")
    True ->  return ("add FileWriter<" ++ showConstType ty ++ ">(\"" ++ name ++ "\");\n")
  Empty             -> return ""
  where
    codeGraphExpr :: StatementS -> String
    codeGraphExpr a = case a of
      AssignS a b -> show a ++ " = " ++ show b
      Chain a b   -> codeGraphExpr a ++ codeGraphExpr b
      _  	  -> ""

-- | Walk down the Stream AST and find the declared inputs to print.
codeInputS :: StatementS -> [String]
codeInputS a = case a of
  DeclS v       -> if (inp v) then [showConstType (const' $ val v) ++ " " ++ vname v] else []
  BranchS _ b c -> codeInputS b ++ codeInputS c
  LoopS _ _ _ a -> codeInputS a
  Pipeline _ a  -> codeInputS a
  SplitJoin _ a -> codeInputS a
  Chain a b     -> codeInputS a ++ codeInputS b
  _             -> []

-- | Generate StreamIt code inside a filter.
codeFilter :: FilterInfo -> IO String
codeFilter (ty, name, stmt) = return (ty ++ " filter " ++ name ++ "("
                                      ++ (intercalate ", " $ codeInput stmt)
                                      ++ ")\n{\n" ++ indent (codeStmt name stmt) ++ "}")

-- | Walk down the Filter AST and find the declared inputs to print.
codeInput :: Statement -> [String]
codeInput a = case a of
  Decl v       -> if (inp v) then [showConstType (const' $ val v) ++ " " ++ vname v] else []
  Branch _ b c -> codeInput b ++ codeInput c
  Loop _ _ _ a -> codeInput a
  Sequence a b -> codeInput a ++ codeInput b
  Init a       -> codeInput a
  Work _ a     -> codeInput a
  _            -> []

instance Show Statement where show = codeStmt "none"

-- | Generate code corresponding to a StreamIt statement.
codeStmt :: Name -> Statement -> String
codeStmt name a = case a of
  Decl v -> if inp v then ""
            else let c = (const' $ val v) in
            if (isScalar c)
            then (showConstType c ++ " " ++ (vname v)
                  ++ " = " ++ show (const' $ val v) ++ ";\n")
            else (showConstType c ++ " " ++ (vname v) ++ ";\n")
  Assign _ _       -> codeStmtExpr a ++ ";\n"
  Branch a b Null  -> "if (" ++ show a ++ ") {\n" ++ indent (codeStmt name b) ++ "}\n"
  Branch a b c     -> "if (" ++ show a ++ ") {\n" ++ indent (codeStmt name b)
                      ++ "} else {\n" ++ indent (codeStmt name c) ++ "}\n"
  Loop Null a Null b -> "while (" ++ show a ++ ") {\n"
                        ++ indent (codeStmt name b) ++ "}\n"
  Loop a b c d     -> "for (" ++ codeStmtExpr a ++ "; " ++ show b ++ "; "
                      ++ codeStmtExpr c ++ ") {\n" ++ indent (codeStmt name d)
                      ++ "}\n"
  Sequence a b     -> codeStmt name a ++ codeStmt name b
  Init a           -> "init {\n" ++ indent (codeStmt name a) ++ "}\n"
  Work rate d      -> "work " ++ show rate
                      ++ " {\n" ++ indent (codeStmt name d) ++ "}\n"
  Push a           -> "push(" ++ show a ++ ");\n"
  Pop              -> codeStmtExpr a ++ ";\n"
  Println a        -> "println(" ++ codeStmtExpr a ++ ");\n"
  Null             -> ""
  where
    codeStmtExpr :: Statement -> String
    codeStmtExpr a = case a of
      Assign a b     -> show a ++ " = " ++ show b
      Sequence a b   -> codeStmtExpr a ++ codeStmtExpr b
      Pop	     -> "pop()"
      _  	     -> ""

