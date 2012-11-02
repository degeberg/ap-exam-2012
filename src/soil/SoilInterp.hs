--
-- Skeleton for Soil interpreter
-- To be used at the exam for Advanced Programming, B1-2012
--

module SoilInterp where

import SoilAst

import qualified Data.Map as M
import qualified Data.Foldable as F
import Data.Maybe
import Data.List (intercalate)
import Control.Monad.State
import Control.Arrow ((&&&))

--
-- Part 1: Define a name environment
--

type NameEnv = M.Map Name [Ident]

-- Functions for insert and lookup

lookupName :: Name -> NameEnv -> [Ident]
lookupName n e = fromMaybe (error $ "Invalid name '" ++ n ++ "'") (M.lookup n e)

insertName :: Name -> [Ident] -> NameEnv -> NameEnv
insertName n = M.insertWith (\_ -> error $ "Duplicate name '" ++ n ++ "'") n

insertNames :: [Name] -> [Ident] -> NameEnv -> NameEnv
insertNames ns is e
  = foldr (\(n, i) env -> insertName n i env) e (zipWith (flip (,) . return) ns is)

--
-- Part 2: Define a function environment
--

type FuncEnv = M.Map Ident Func

-- Functions for insert and lookup

lookupFunc :: Ident -> FuncEnv -> Maybe Func
lookupFunc = M.lookup

insertFunc :: Ident -> Func -> FuncEnv -> FuncEnv
insertFunc n = M.insertWith (\_ -> error $ "Duplicate name '" ++ n ++ "'") n

--
-- Part 3: Define a process and process queue
--
type Message = [Ident]

data Process = Process { function  :: Ident
                       , arguments :: [Ident]
                       , mailbox   :: [Message]}
                       deriving (Show)

type ProcessQueue = [(Ident, Process)]

data PState = PState { nEnv   :: NameEnv
                     , fEnv   :: FuncEnv
                     , pq     :: ProcessQueue
                     , getPid :: Ident
                     , stdout :: [String]
                     , stderr :: [String]
                     } deriving (Show)
type ProcessState = State PState

-- Function for process modification

pqOp :: (Process -> Process) -> Ident -> ProcessQueue -> ProcessQueue
pqOp f pid = map (\(pid', p) -> if pid == pid' then (pid, f p) else (pid', p))

procOp :: (Process -> Process) -> Ident -> ProcessState ()
procOp f pid = do s <- get
                  case lookup pid (pq s) of
                    Nothing -> errorlog ["invalidFunction", pid]
                    Just _  -> put s { pq = pqOp f pid (pq s) }

addMsg :: Message -> Ident -> ProcessState ()
addMsg msg = procOp (\p -> p { mailbox = mailbox p ++ [msg] })

setFunction :: Ident -> Ident -> ProcessState ()
setFunction funid = procOp (\p -> p { function = funid })

setArguments :: [Ident] -> Ident -> ProcessState ()
setArguments args = procOp (\p -> p { arguments = args })

getArguments :: Ident -> ProcessQueue -> [Ident]
getArguments pid procq = fromJust $ lookup pid procq >>= Just . arguments

getMsg :: Ident -> ProcessQueue -> Maybe Message
getMsg pid procq = lookup pid procq >>= listToMaybe . mailbox

removeMsg :: Ident -> ProcessState ()
removeMsg = procOp (\p -> p { mailbox = tail $ mailbox p } )

getFunction :: Ident -> ProcessQueue -> Maybe Ident
getFunction pid procq = lookup pid procq >>= Just . function

addProcess :: Ident -> Process -> ProcessQueue -> ProcessQueue
addProcess pid p q = q ++ [(pid, p)]

--
-- Part 4: Define and implement a process step
--

outputString :: Message -> String
outputString = intercalate ":"

println :: Message -> ProcessState ()
println msgs = do s <- get
                  put s { stdout = outputString msgs : stdout s }

errorlog :: Message -> ProcessState ()
errorlog msgs = do s <- get
                   put s { stderr = outputString msgs : stderr s }

evalPrim :: Prim -> ProcessState [Ident]
evalPrim (Id n)       = return [n]
evalPrim (Par n)      = do s <- get
                           return $ lookupName n (nEnv s)
evalPrim Self         = gets $ return . getPid
evalPrim (Concat a b) = do aPrim <- evalPrim a
                           bPrim <- evalPrim b
                           return [concat (aPrim ++ bPrim)]

interpExpr :: Expr -> ProcessState ()
interpExpr (CaseOf _ [] d)
  = interpExpr d
interpExpr (CaseOf switch ((m, e):cs) d)
  = do switch' <- evalPrim switch
       if length switch' == length m
       then do s <- get
               put s { nEnv = insertNames m switch' (nEnv s) }
               interpExpr e
       else interpExpr (CaseOf switch cs d)
interpExpr (IfEq p1 p2 e1 e2)
  = do p1e <- evalPrim p1
       p2e <- evalPrim p2
       interpExpr $ if p1e == p2e then e1 else e2
interpExpr (Acts as)
  = mapM_ interpAct as

interpAct :: ActOp -> ProcessState ()
interpAct (SendTo msgs receiver)
  = do receiver' <- evalPrim receiver
       msgs'     <- mapM evalPrim msgs
       case receiver' of
           ["println"]  -> println (concat msgs')
           ["errorlog"] -> errorlog (concat msgs')
           [r]          -> addMsg (concat msgs') r
           x            -> error $ "invalid receiver '" ++ show x ++ "'"
interpAct (Create pid funid vals)
  = do [funid'] <- evalPrim funid
       [pid']   <- evalPrim pid
       vals'    <- mapM evalPrim vals
       let proc = Process { function = funid', arguments = concat vals', mailbox = [] }
       s <- get
       put s { pq = addProcess pid' proc (pq s) }
interpAct (Become funid vals)
  = do s <- get
       [funid'] <- evalPrim funid
       vals'    <- mapM evalPrim vals
       setArguments (concat vals') (getPid s)
       setFunction funid' (getPid s)

processStep' :: Ident -> Message -> ProcessState ()
processStep' pid msg
  = do s <- get
       case getFunction pid (pq s) >>= flip lookupFunc (fEnv s) of
         Nothing  -> errorlog ["undefFunc"]
         Just fun -> do let nEnv'  = insertName (receive fun) msg (nEnv s)
                        let nEnv'' = insertNames (params fun) (getArguments pid (pq s)) nEnv'
                        put s { nEnv=nEnv'', getPid = pid }
                        removeMsg pid -- will not fail
                        interpExpr (body fun)
                        s' <- get
                        put s' { nEnv = nEnv s } -- restore nEnv

processStep :: Ident -> ProcessState ()
processStep pid
  = do s <- get
       F.forM_ (getMsg pid (pq s)) (processStep' pid)

--
-- Part 5: Define and implement the round-robin algorithm
--

nextProcessRR :: ProcessState Ident
nextProcessRR
  = do s <- get
       case pq s of
         []            -> error "no processes"
         ((pid, p):xs) -> do put s { pq = addProcess pid p xs }
                             return pid

--
-- Part 6: Implement the round-robin evaluator
--

progInit :: [Func] -> PState
progInit fs = PState ne fe [] "main" [] []
    where ne = M.empty
          fe = foldr (\f e -> insertFunc (funcname f) f e) M.empty fs

runProgRR' :: Int -> ProcessState ()
runProgRR' 0 = return ()
runProgRR' n = nextProcessRR >>= processStep >> runProgRR' (n - 1)

runProgRRfinal :: Int -> Program -> PState
runProgRRfinal n (fs, as) = execState (interpExpr (Acts as) >> runProgRR' n) (progInit fs)

runProgRR :: Int -> Program -> ([String], [String])
runProgRR n = (stdout &&& stderr) . runProgRRfinal n

--
-- Part 7: Implement a find all possible executions evaluator
--

type ProcessStates a = StateT PState [] a

nextProcAll' :: ProcessQueue -> [Ident]
nextProcAll' = map fst . filter (not . null . mailbox . snd)

nextProcAll :: ProcessStates [Ident]
nextProcAll = liftM nextProcAll' (gets pq)

promoteState :: ProcessState a -> ProcessStates a
promoteState act = do s <- get
                      let (r,s') = runState act s
                      put s'
                      return r

runProgAll' :: Int -> ProcessStates ()
runProgAll' 0 = return ()
runProgAll' n = do pids <- nextProcAll
                   mapM_ (promoteState . processStep) pids
                   runProgAll' (n - 1)

runProgAll :: Int -> Program -> [([String], [String])]
runProgAll n (fs, as) = map (stdout &&& stderr) states
    where states = execStateT (inits >> runProgAll' n) (progInit fs)
          inits  = (promoteState . interpExpr) (Acts as)

--runProgAllFinal n (fs, as) = states
--    where states = execStateT (inits >> runProgAll' n) (progInit fs)
--          inits  = (promoteState . interpExpr) (Acts as)
