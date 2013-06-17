module Language.StreamIt.Backend.StreamIt (codeStreamIt) where

import Data.List
import qualified Control.Monad.State as S
import System.IO
import System.IO.Temp

import Language.StreamIt.Core
import Language.StreamIt.Filter
import Language.StreamIt.Graph

indent :: String -> String
indent = unlines . map ("\t" ++) . lines

-- | Generate StreamIt program.
codeStreamIt :: StatementS -> IO (FilePath)
codeStreamIt st = do
  (fs, gs) <- S.execStateT (findDefs node) ([],[])
  filters <- mapM codeFilter fs
  graphs <- mapM codeGraph gs
  mains <- codeGraph st
  (filename, hdl) <- openTempFile "." "streamhs.str"
  hPutStr hdl $
    (intercalate "\n\n" filters)
    ++ "\n" ++ (intercalate "\n\n" graphs)
    ++ "\n" ++ mains ++ "\n"
  hClose hdl
  return filename

-- | Generate StreamIt code for the aggregate filters.
codeGraph :: StatementS -> IO String
codeGraph sn = case sn of
  DeclS v -> do
    let c = (const' $ val v) in
      if (isScalar c)
      then return (showConstType c ++ " " ++ (vname v)
                   ++ " = " ++ show (const' $ val v) ++ ";\n")
      else return (showConstType c ++ " " ++ (vname v) ++ ";\n")
  AssignS a b       -> return (show a ++ " = " ++ show b ++ ";\n")
  BranchS a b Empty -> do
    bs <- codeGraph b
    return ("if (" ++ show a ++ ") {\n" ++ indent bs ++ "}\n")
  BranchS a b c     -> do
    bs <- codeGraph b
    cs <- codeGraph c
    return ("if (" ++ show a ++ ") {\n" ++ indent bs ++ "} else {\n"
            ++ indent cs ++ "}\n")
  LoopS Empty a Empty b -> do
    bs <- codeGraph b
    return ("while (" ++ show a ++ ") {\n" ++ indent bs ++ "}\n")
  LoopS a b c d     -> do
    ds <- codeGraph d
    return ("for (" ++ loopAssignment a ++ "; " ++ show b ++ "; "
      ++ loopAssignment c ++ ") {\n" ++ indent ds ++ "}\n")
  AddS n _ (Just a) Nothing Nothing -> return ("add " ++ n ++ "(" ++ show a ++ ");\n")
  AddS n _ (Just a) (Just b) Nothing -> return ("add " ++ n ++ "(" ++ show a
                                                ++ ", " ++ show b ++ ");\n")
  AddS n _ (Just a) (Just b) (Just c) -> return ("add " ++ n ++ "(" ++ show a
                                                 ++ ", " ++ show b ++ ", "
                                                 ++ show c ++ ");\n")
  AddS n _ _ _ _ -> return ("add " ++ n ++ "();\n")
  Pipeline name a -> do
    as <- codeGraph a
    case name of
      Nothing -> return ("add pipeline {\n" ++ indent as ++ "}\n")
      Just n  -> return (show sn ++ " pipeline " ++ n ++ "("
                         ++ (intercalate ", " $ codeInputS a) ++ ")\n{\n"
                         ++ indent as ++ "}\n")
  SplitJoin name a -> do
    as <- codeGraph a
    case name of
      Nothing -> return ("add splitjoin {\n" ++ indent as ++ "}\n")
      Just n  -> return (show sn ++ " splitjoin " ++ n ++ " {\n"
                         ++ indent as ++ "}\n")
  Split a           -> return ("split " ++ show a ++ ";\n")
  Join a            -> return ("join " ++ show a ++ ";\n")
  Chain a b         -> do
    as <- codeGraph a 
    bs <- codeGraph b
    return (as ++ bs)
  File rw ty name   -> case rw of
    FileReader -> return ("add FileReader<" ++ showConstType ty ++ ">(\"" ++ name ++ "\");\n")
    FileWriter -> return ("add FileWriter<" ++ showConstType ty ++ ">(\"" ++ name ++ "\");\n")
  Empty             -> return ""
  where
    loopAssignment :: StatementS -> String
    loopAssignment a = case a of
      AssignS a b -> show a ++ " = " ++ show b
      Chain a b   -> loopAssignment a ++ loopAssignment b
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
codeFilter :: Statement -> IO String
codeFilter st = return (ty ++ " filter " ++ name ++ "("
                        ++ (intercalate ", " $ codeInput st)
                        ++ ")\n{\n" ++ indent (codeStmt st) ++ "}")

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
codeStmt :: String -> Statement -> String
codeStmt name a = case a of
  Decl v -> do
    let c = (const' $ val v) in
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

