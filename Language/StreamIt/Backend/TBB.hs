module Language.StreamIt.Backend.TBB (codeTBB) where

import Data.List
import Data.Typeable
import Control.Monad.Trans
import qualified Control.Monad.State as S
import System.IO

import Language.StreamIt.Core
import Language.StreamIt.Filter
import Language.StreamIt.Graph

indent :: String -> String
indent = unlines . map ("\t" ++) . lines

-- | Generate StreamIt program.
codeTBB :: (Elt a, Elt b, Typeable a, Typeable b) => StreamIt a b () -> IO (FilePath)
codeTBB st = do
  s <- liftIO $ execStream st
  name <- newStableName s "filt"
  (fs, gs) <- S.execStateT (findDefs s) ([],[])
  filters <- mapM codeFilter fs
  graphs <- mapM codeGraph gs
  mains <- codeGraph (show st, name, "", s)
  (filename, hdl) <- openTempFile "." "streamhs.cpp"
  hPutStr hdl $
    "#include \"tbb/pipeline.h\"\n#include \"tbb/task_scheduler_init.h\"\n"
    ++ "#include \"tbb/tbb_allocator.h\"\n#include <iostream>\n\n"
    ++ "using namespace std;\nstatic int iter;\n"
    ++ "#define tbb_return(val){if (iter++ >= 10000)return NULL;return val;}\n\n"
    ++ (intercalate "\n\n" filters)
    ++ "\n" ++ (intercalate "\n\n" graphs)
    ++ "\n" ++ mains ++ "\n"
  hClose hdl
  return filename

-- | Generate StreamIt code for the aggregate filters.
codeGraph :: GraphDecl -> IO String
codeGraph (ty, name, args, sn) = case sn of
  DeclS (Var n v) -> return (showConstType (const' v) ++ " " ++ n
                             ++ " = " ++ show (const' v) ++ ";\n")
  AssignS a b       -> return (show a ++ " = " ++ show b ++ ";")
  BranchS a b Empty -> do
    bs <- codeGraph (ty, name, args, b)
    return ("if (" ++ show a ++ ") {\n" ++ indent bs ++ "}\n")
  BranchS a b c     -> do
    bs <- codeGraph (ty, name, args, b)
    cs <- codeGraph (ty, name, args, c)
    return ("if (" ++ show a ++ ") {\n" ++ indent bs ++ "} else {\n"
            ++ indent cs ++ "}\n")
  LoopS Empty a Empty b -> do
    bs <- codeGraph (ty, name, args, b)
    return ("while (" ++ show a ++ ") {\n" ++ indent bs ++ "}\n")
  LoopS a b c d     -> do
    ds <- codeGraph (ty, name, args, d)
    return ("for (" ++ codeGraphExpr a ++ "; " ++ show b ++ "; "
      ++ codeGraphExpr c ++ ") {\n" ++ indent ds ++ "}\n")
  AddS _ name _ args -> do
    return ("add " ++ name ++ "(" ++ intercalate ", " (printArgs args)
            ++ ");\n" ++ "pipeline.add_filter(" ++ name ++ "_);\n")
  Pipeline False a  -> do
    as <- codeGraph (ty, name, args, a)
    return ("int main(int argc, char* argv[]) {\n\t"
            ++ "tbb::task_scheduler_init init_serial(1);\n"
            -- ++ "void pipeline" ++ name ++ "("
            -- ++ (intercalate ", " $ codeInputS a) ++ ")\n{\n"
            ++ "\ttbb::pipeline pipeline;\n" ++ indent as
            ++ "\tpipeline.run(1);\n}\n")
  Pipeline True a   -> do
    as <- codeGraph (ty, name, args, a)
    return ("add pipeline {\n" ++ indent as ++ "}\n")
  SplitJoin False a -> do
    as <- codeGraph (ty, name, args, a)
    return (ty ++ " splitjoin " ++ name ++ " {\n" ++ indent as ++ "}\n")
  SplitJoin True a  -> do
    as <- codeGraph (ty, name, args, a)
    return ("add splitjoin {\n" ++ indent as ++ "}\n")
  Split a           -> return ("split " ++ show a ++ ";\n")
  Join a            -> return ("join " ++ show a ++ ";\n")
  Chain a b         -> do
    as <- codeGraph (ty, name, args, a) 
    bs <- codeGraph (ty, name, args, b)
    return (as ++ bs)
  File rw ty name   -> case rw of
    FileReader -> return ("add FileReader<" ++ showConstType ty ++ ">(\"" ++ name ++ "\");\n")
    FileWriter ->  return ("add FileWriter<" ++ showConstType ty ++ ">(\"" ++ name ++ "\");\n")
  Empty             -> return ""
  where
    codeGraphExpr :: StatementS -> String
    codeGraphExpr a = case a of
      AssignS a b -> show a ++ " = " ++ show b
      Chain a b   -> codeGraphExpr a ++ codeGraphExpr b
      _  	  -> ""
    printArgs (Nothing, Nothing, Nothing) = []
    printArgs (Just a1, Nothing, Nothing) = [show a1]
    printArgs (Just a1, Just a2, Nothing) = [show a1, show a2]
    printArgs (Just a1, Just a2, Just a3) = [show a1, show a2, show a3]
    printArgs (_, _, _) = []

-- | Generate StreamIt code inside a filter.
codeFilter :: FilterDecl -> IO String
codeFilter (_, name, _, stmt) = return ("class " ++ name ++ ": public tbb::filter {\n"
                                     ++ "public:\n\t" ++ name ++ "();\nprivate:\n"
                                     ++ indent (intercalate ";\n" $ findDecls stmt)
                                     ++ ";\n\tvoid* operator()(void* item);\n};\n\n"
                                     ++ (if (noInit stmt)
                                         then (name ++ "::" ++ name
                                               ++ "() : tbb::filter(serial_in_order) {}\n\n")
                                         else "")
                                     ++ (codeStmt stmt))

-- | Walk down the Filter AST and find the declared inputs to print.
findDecls :: Statement -> [String]
findDecls a = case a of
  Decl (Var n v) -> [showConstType (const' v) ++ " " ++ n]
  Branch _ b c   -> findDecls b ++ findDecls c
  Loop _ _ _ a   -> findDecls a
  Sequence a b   -> findDecls a ++ findDecls b
  Init a         -> findDecls a
  Work _ a       -> findDecls a
  _              -> []

-- | Generate code corresponding to a StreamIt statement.
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
  Init a           -> let name = "foo" in
    (name ++ "::" ++ name ++ "() : tbb::filter(serial_in_order) {\n"
     ++ indent (codeStmt a) ++ "}\n\n")
  Work rate d      -> let name = "foo" in
    "void* " ++ name ++ "::operator()(void* item) {\n"
    ++ "/*" ++ show rate ++ "*/\n"
                      ++ indent (codeStmt d) ++ "}\n"
  Push a           -> "tbb_return(&" ++ show a ++ ");\n"
  Pop              -> codeStmtExpr a ++ ";\n"
  Println a        -> "std::cout << " ++ codeStmtExpr a ++ " << std::endl;\n"
  Null             -> ""
  where
    codeStmtExpr :: Statement -> String
    codeStmtExpr a = case a of
      Assign a b     -> show a ++ " = " ++ show b
      Sequence a b   -> codeStmtExpr a ++ codeStmtExpr b
      Pop	     -> "*static_cast<int*>(item)" -- FIXME FIXME FIXME!!
      _ 	     -> ""

noInit :: Statement -> Bool
noInit a = case a of
  Init _           -> False
  Branch _ b c     -> noInit b && noInit c
  Loop _ _ _ d     -> noInit d
  Sequence a b     -> noInit a && noInit b
  _ -> True
