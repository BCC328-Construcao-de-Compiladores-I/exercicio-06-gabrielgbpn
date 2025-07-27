module L.L2.Frontend.Syntax where

import Utils.Value
import Utils.Var

data L2
  = L2 [S2]
    deriving (Eq, Ord, Show)

data S2
  = Def Var E2 [S2]
  | LRead E2 Var 
  | LPrint E2
  | LAssign Var E2
  deriving (Eq, Ord, Show)

data E2
  = LVal Value
  | LVar Var
  | LAdd E2 E2
  | LMinus E2 E2
  | LMul E2 E2
  | LDiv E2 E2
  deriving (Eq, Ord, Show)