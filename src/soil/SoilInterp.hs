--
-- Skeleton for Soil interpreter
-- To be used at the exam for Advanced Programming, B1-2012
--

module SoilInterp where

import SoilAst

import qualified Data.Map as M
import Data.Maybe
import Data.List (intercalate)
import Control.Monad.State

--
-- Part 1: Define a name environment
--

type NameEnv = M.Map Name [Ident]

-- Functions for insert and lookup

lookupName :: Name -> NameEnv -> [Ident]
lookupName n e = fromMaybe (error $ "Invalid name '" ++ n ++ "'") (M.lookup n e)

insertName :: Name -> [Ident] -> NameEnv -> NameEnv
insertName n = M.insertWith (\_ -> error $ "Duplicate name '" ++ n ++ "'") n

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

-- Function for process modification

pqOp :: (Process -> Process) -> Ident -> ProcessQueue -> ProcessQueue
pqOp f pid = map (\(pid', p) -> if pid == pid' then (pid, f p) else (pid', p))

addMsg' :: Message -> Process -> Process
addMsg' m p = let box = mailbox p
              in p { mailbox = box ++ [m] }

addMsg :: Message -> Ident -> ProcessQueue -> ProcessQueue
addMsg msg = pqOp (addMsg' msg)

setFunction :: Ident -> Ident -> ProcessQueue -> ProcessQueue
setFunction funid = pqOp (\p -> p{function=funid})

setArguments :: [Ident] -> Ident -> ProcessQueue -> ProcessQueue
setArguments args = pqOp (\p -> p{arguments=args})

getMsg' :: Process -> Maybe Message
getMsg' p = case mailbox p of
             []    -> Nothing
             (x:_) -> Just x

getMsg :: Ident -> ProcessQueue -> Maybe Message
getMsg pid procq = lookup pid procq >>= getMsg'

getFunction :: Ident -> ProcessQueue -> Maybe Ident
getFunction pid procq = lookup pid procq >>= Just . function

addProcess :: Ident -> Process -> ProcessQueue -> ProcessQueue
addProcess pid p q = q ++ [(pid, p)]

--
-- Part 4: Define and implement a process step
--

data PState = PState { nEnv   :: NameEnv
                     , fEnv   :: FuncEnv
                     , pq     :: ProcessQueue
                     , getPid :: Ident
                     , stdout :: [String]
                     , stderr :: [String]
                     } deriving (Show)
type ProcessState = State PState

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
evalPrim Self         = do s <- get
                           return [getPid s]
evalPrim (Concat a b) = do aPrim <- evalPrim a
                           bPrim <- evalPrim b
                           return (aPrim ++ bPrim)

interpExpr :: Expr -> ProcessState ()
interpExpr (CaseOf _ [] d)
  = interpExpr d
interpExpr (CaseOf switch ((m, e):cs) d)
 = do switch' <- evalPrim switch
      if length switch' == length m
      then do s <- get
              let nEnv' = foldr (\(n, i) env -> insertName n i env) (nEnv s)
                            (zipWith (\n i -> (n, [i])) m switch')
              put s{nEnv=nEnv'}
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
  = do s         <- get
       receiver' <- evalPrim receiver
       msgs'     <- mapM evalPrim msgs
       case receiver' of
           ["println"]  -> println (concat msgs')
           ["errorlog"] -> errorlog (concat msgs')
           [r]          -> do let pq' = addMsg (concat msgs') r (pq s)
                              put s { pq = pq' }
           _            -> error "invalid receiver"
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
       let pq'  = setArguments (concat vals') (getPid s) (pq s)
       let pq'' = setFunction funid' (getPid s) pq'
       put s { pq = pq'' }


processStep :: Ident -> ProcessState ()
processStep pid
  = do s <- get
       case getMsg pid (pq s) of
         Nothing -> return ()
         Just x -> case getFunction pid (pq s) >>= flip lookupFunc (fEnv s) of
                     Nothing -> errorlog ["undefFunc"]
                     Just fun -> do let nEnv' = insertName (receive fun) x (nEnv s)
                                    put s{nEnv=nEnv', getPid = pid}
                                    interpExpr (body fun)
                                    s' <- get
                                    put s' { nEnv = nEnv s }
                                    return ()

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

runProgRR :: Int -> Program -> ([String], [String])
runProgRR n (fs, as) = (stdout s, stderr s)
    where s = execState (interpExpr (Acts as) >> runProgRR' n) (progInit fs)

runProgRRfinal :: Int -> Program -> PState
runProgRRfinal n (fs, as) = execState (interpExpr (Acts as) >> runProgRR' n) (progInit fs)

--
-- Part 7: Implement a find all possible executions evaluator
--
--nextProcAll :: ...

--nextProcAll :: ProcessState [Ident]
--nextProcAll = liftM (map fst . filter (not . null . mailbox . snd)) getPQ

nextProcAll :: ProcessQueue -> [Ident]
nextProcAll = map fst . filter (not . null . mailbox . snd)

runProgAll :: Int -> Program -> [([String], [String])]
runProgAll = undefined
