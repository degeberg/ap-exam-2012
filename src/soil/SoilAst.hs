--
-- Abstract syntax tree for Soil
-- To be used at the exam for Advanced Programming, B1-2012
--

module SoilAst where

type Ident    = String  -- w/o leading #
type Name     = String

type Program  = ([Func],[ActOp]) 

data Func     = Func { funcname  :: Ident
                     , params    :: [Name]   -- Zero or more
                     , receive   :: Name
                     , body      :: Expr}
              deriving (Eq, Show)

data Expr     = CaseOf Prim [([Name], Expr)] Expr
              | IfEq Prim Prim Expr Expr
              | Acts [ActOp]
              deriving (Eq, Show)

data ActOp    = SendTo [Prim] Prim
              | Create Prim Prim [Prim]
              | Become Prim [Prim]
              deriving (Eq, Show)

data Prim     = Id Ident
              | Par Name
              | Self
              | Concat Prim Prim
              deriving (Eq, Show)