--- Given Code
--- ==========

module Lib where

import Data.Functor.Identity (Identity)
import Data.List (intercalate)
import System.IO (hFlush, hPutStr, hPutStrLn, stdout)
import Text.Parsec.Prim (ParsecT)
import Text.ParserCombinators.Parsec hiding (Parser)

--- Metadata for autograder
--- -----------------------
tag1 = 36392

tag2 = 13977

tag3 = 68529

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
  deriving (Eq)

instance Show Stmt where
  show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp
  = IntExp Integer
  | VarExp String
  | LamExp String Exp
  | IfExp Exp Exp Exp
  | OpExp String Exp Exp
  | AppExp Exp Exp
  deriving (Eq)

instance Show Exp where
  show (VarExp s) = s
  show (IntExp i) = show i
  show (LamExp x e) = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
  show (IfExp e1 e2 e3) =
    "(if "
      ++ show e1
      ++ " then "
      ++ show e2
      ++ " else "
      ++ show e3
      ++ ")"
  show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
  show (AppExp f e) = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s) = "VarExp " ++ show s
ctorShow (IntExp i) = "IntExp " ++ show i
ctorShow (LamExp x e) = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) =
  "IfExp ("
    ++ ctorShow e1
    ++ ") ("
    ++ ctorShow e2
    ++ ") ("
    ++ ctorShow e3
    ++ ")"
ctorShow (OpExp op e1 e2) =
  "OpExp "
    ++ show op
    ++ " ("
    ++ ctorShow e1
    ++ ") ("
    ++ ctorShow e2
    ++ ")"
ctorShow (AppExp f e) = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk n k = factk (n - 1) (\v -> k (n * v))

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk [x] ek ok
  | even x = ek x
  | otherwise = ok x
evenoddk (x : xs) ek ok
  | even x = evenoddk xs (\v -> ek (x + v)) ok
  | otherwise = evenoddk xs ek (\v -> ok (x + v))

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple = undefined

--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)
cpsExp = undefined

--- #### Define `cpsExp` for Integer and Variable Expressions

--- #### Define `cpsExp` for Application Expressions

--- #### Define `cpsExp` for Operator Expressions

--- #### Define `cpsExp` for If Expressions

--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl = undefined
