module Lang where

type Id = String

type Program = [Statement]

data Statement
  = Display Expr (Maybe Id)
  | Define Id Expr
  | While Expr Program
  | If Expr Program (Maybe Program)
  deriving (Show, Eq)

data Expr
  = Binary String Expr Expr
  | Var Id
  | Num Int
  deriving (Show, Eq)

-- data BinOp = Add | Sub | Mul | Div | RelOp String deriving (Show, Eq)
