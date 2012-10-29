--
-- Skeleton for Soil parser
-- To be used at the exam for Advanced Programming, B1-2012
--

module SoilParser where

import SoilAst

parseString :: String -> Either Error Program

parseFile :: FilePath -> Either Error Program