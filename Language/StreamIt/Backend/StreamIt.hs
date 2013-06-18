module Language.StreamIt.Backend.StreamIt (codeStreamIt) where

import Data.List
import Data.Typeable
import Control.Monad.Trans
import qualified Control.Monad.State as S

import Language.StreamIt.Core
import Language.StreamIt.Filter
import Language.StreamIt.Graph

indent :: String -> String
indent = unlines . map ("\t" ++) . lines

-- | Generate StreamIt program.
codeStreamIt :: (Elt a, Elt b, Typeable a, Typeable b) => StreamIt a b () -> IO (FilePath)
codeStreamIt st = do
  s <- liftIO $ execStream st
  (fs, gs) <- S.execStateT (findDefs s) ([],[])
  filters <- mapM codeFilter fs
  graphs <- mapM codeGraph gs
  mains <- codeGraph (show st, "streamhs", "", s)
  let filename = "streamhs.str"
  writeFile filename $
    (intercalate "\n\n" filters)
    ++ "\n" ++ (intercalate "\n\n" graphs)
    ++ "\n" ++ mains ++ "\n"
  return filename

-- | Generate StreamIt aggregate filter declarations.
codeGraph :: GraphDecl -> IO String
codeGraph (ty, name, args, st) = do
  case st of
    Pipeline True a -> do
      as <- codeStmtS a
      return (ty ++ " pipeline " ++ name ++ "(" ++ args 
              ++ ")\n{\n" ++ indent as ++ "}\n")
    SplitJoin True a -> do
      as <- codeStmtS a
      return (ty ++ " splitjoin " ++ name ++ "(" ++ args
              ++ ")\n{\n" ++ indent as ++ "}\n")
    Chain a b -> do
      as <- codeGraph (ty, name, args, a)
      bs <- codeGraph (ty, name, args, b)
      return (as ++ bs)
    _ -> return ""

-- | Generate StreamIt body code for the aggregate filters.
codeStmtS :: StatementS -> IO String
codeStmtS sn = case sn of
  DeclS v -> do
    let c = (const' $ val v) in
      if (isScalar c)
      then return (showConstType c ++ " " ++ (vname v)
                   ++ " = " ++ show (const' $ val v) ++ ";\n")
      else return (showConstType c ++ " " ++ (vname v) ++ ";\n")
  AssignS a b       -> return (show a ++ " = " ++ show b ++ ";\n")
  BranchS a b Empty -> do
    bs <- codeStmtS b
    return ("if (" ++ show a ++ ") {\n" ++ indent bs ++ "}\n")
  BranchS a b c     -> do
    bs <- codeStmtS b
    cs <- codeStmtS c
    return ("if (" ++ show a ++ ") {\n" ++ indent bs ++ "} else {\n"
            ++ indent cs ++ "}\n")
  LoopS Empty a Empty b -> do
    bs <- codeStmtS b
    return ("while (" ++ show a ++ ") {\n" ++ indent bs ++ "}\n")
  LoopS a b c d     -> do
    ds <- codeStmtS d
    return ("for (" ++ loopAssignment a ++ "; " ++ show b ++ "; "
      ++ loopAssignment c ++ ") {\n" ++ indent ds ++ "}\n")
  AddS _ name _ args -> do
    return ("add " ++ name ++ "("
            ++ intercalate ", " (printArgs args) ++ ");\n")
  Pipeline named a -> do
    as <- codeStmtS a
    case named of
      False -> return ("add pipeline {\n" ++ indent as ++ "}\n")
      True  -> return ""
  SplitJoin named a -> do
    as <- codeStmtS a
    case named of
      False -> return ("add splitjoin {\n" ++ indent as ++ "}\n")
      True  -> return ""
  Split a           -> return ("split " ++ show a ++ ";\n")
  Join a            -> return ("join " ++ show a ++ ";\n")
  Chain a b         -> do
    as <- codeStmtS a
    bs <- codeStmtS b
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
    printArgs (Nothing, Nothing, Nothing) = []
    printArgs (Just a1, Nothing, Nothing) = [show a1]
    printArgs (Just a1, Just a2, Nothing) = [show a1, show a2]
    printArgs (Just a1, Just a2, Just a3) = [show a1, show a2, show a3]
    printArgs (_, _, _) = []

-- | Generate StreamIt filter declarations.
codeFilter :: FilterDecl -> IO String
codeFilter (ty, name, args, stmt) =
  return (ty ++ " filter " ++ name ++ "(" ++ args ++ ")\n{\n"
          ++ indent (codeStmt stmt) ++ "}")

-- | Generate StreamIt filter body code.
codeStmt :: Statement -> String
codeStmt a = case a of
  Decl v -> do
    let c = (const' $ val v) in
      if (isScalar c)
      then (showConstType c ++ " " ++ (vname v)
            ++ " = " ++ show (const' $ val v) ++ ";\n")
      else (showConstType c ++ " " ++ (vname v) ++ ";\n")
  Assign _ _       -> codeStmtExpr a ++ ";\n"
  Branch a b Null  -> "if (" ++ show a ++ ") {\n" ++ indent (codeStmt b) ++ "}\n"
  Branch a b c     -> "if (" ++ show a ++ ") {\n" ++ indent (codeStmt b)
                      ++ "} else {\n" ++ indent (codeStmt c) ++ "}\n"
  Loop Null a Null b -> "while (" ++ show a ++ ") {\n"
                        ++ indent (codeStmt b) ++ "}\n"
  Loop a b c d     -> "for (" ++ codeStmtExpr a ++ "; " ++ show b ++ "; "
                      ++ codeStmtExpr c ++ ") {\n" ++ indent (codeStmt d)
                      ++ "}\n"
  Sequence a b     -> codeStmt a ++ codeStmt b
  Init a           -> "init {\n" ++ indent (codeStmt a) ++ "}\n"
  Work rate d      -> "work " ++ show rate
                      ++ " {\n" ++ indent (codeStmt d) ++ "}\n"
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

