--
-- Skeleton for Soil interpreter
-- To be used at the exam for Advanced Programming, B1-2012
--

module SoilInterp where

import SoilAst

--
-- Part 1: Define a name environment
--
data NameEnv = ...

-- Functions for insert and lookup

--
-- Part 2: Define a function environment
--
data FuncEnv = ...

-- Functions for insert and lookup

--
-- Part 3: Degine a process and process queue
--
type Message = [Ident]

data Process = Process { function  :: Ident
                       , arguments :: [Ident]
                       , mailbox   :: [Message]}

data ProcessQueue = ... 

-- Function for process modification

--
-- Part 4: Define and implement a process step
--
processStep :: ...

--
-- Part 5: Define and implement the roind-robin algorithm
--
nextProcessRR :: ...

--
-- Part 6: Implement the round-robin evaluator
--
runProgRR :: Int -> Program -> ([String], [String])

--
-- Part 7: Implement a find all possible executions evaluator
--
nextProcAll :: ...

runProgAll :: Int -> Program -> [([String], [String])]