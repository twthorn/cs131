{-# LANGUAGE FlexibleInstances #-}

-- Implementation of the Syntax and Operational Semantics of the Pi Calculus

module Pi where

-- For documentation, see the following pages:
-- http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Concurrent.html
-- http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Concurrent-Chan.html

import Concurrent

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.List (concatMap)

-- Syntax of the Pi Calculus

type Name = String

instance Show (Chan Value) where
  show chan = "<channel>"

-- When reading through these data types, it is worth noting that *all* values
-- in this pi calculus are like locations in the STLC with references: they only
-- show up during evaluation, but *not* in programs a user might write.
--
-- In other words, the "abstract channel" object defined in your handout (as
-- "c" in the syntax) will actually be a Haskell channel (VChan below).  But
-- your translation will generate Pi terms, which only include expressions
-- (Exp), not values.

data Value
  = VChan (Chan Value)  -- channel value
  | VTup [Value]        -- tuple of values
  deriving Show

data Exp
  = EVar Name           -- variable expression
  | ETup [Exp]          -- tuple of expressions
  deriving Show

data Pattern
  = PVar Name           -- variable pattern
  | PTup [Pattern]      -- tuple pattern
  | Wild                -- wildcard pattern
  deriving Show

data Typ
  = TChan Typ           -- channel type
  | TTup [Typ]          -- tuple type
  deriving Eq


instance Show Typ where
  show (TChan t) = "Chan " ++ (show t)
  show (TTup []) = "()"
  show (TTup (h:ts)) = "(" ++ (show h) ++
    (concatMap (\x -> ", " ++ (show x)) ts) ++ ")"

instance Show (Env -> IO ()) where
  show f = "<function>"

data Pi
  = Nil
  | Pi :|: Pi
  | New Name Typ Pi
  | Out Name Exp
  | Inp Name Pattern Pi
  | RepInp Name Pattern Pi   -- repeated input
  | Embed (Env -> IO ()) Pi

instance Show Pi where
  show Nil = "0"
  show (p1 :|: p2) =
    "(" ++ (show p1) ++ ") | (" ++ (show p2) ++ ")"
  show (New x t p) =
    "new " ++ x ++ " : " ++ (show t) ++ ". " ++ (show p)
  show (Out x e) =
    "send " ++ x ++ "(" ++ (show e) ++ ")"
  show (Inp x pat p) =
    "rec " ++ x ++ "(" ++ (show pat) ++ "). " ++ (show p)
  show (RepInp x pat p) =
    "rec! " ++ x ++ "(" ++ (show pat) ++ "). " ++ (show p)
  show (Embed _ p) = "<function> " ++ (show p)

-- Useful Abbreviations

unitT :: Typ
unitT = TTup []

unitE :: Exp
unitE = ETup []

unitP :: Pattern
unitP = PTup []

printer :: String -> Pi
printer s = Embed (\_ -> putStrLn s) Nil

-- Static type checking

-- TASK!
-- Implement your pi calculus type checker here!

type Gamma = Map Name Typ

typeExp :: Gamma -> Exp -> Either String Typ
typeExp g (EVar name) = case Map.lookup name g of
                        (Just t) -> Right t
                        (Nothing) -> Left $ "unbound variable " ++ name

typeExp g (ETup [])     = Right $ TTup []
typeExp g (ETup (x:xs)) = case typeExp g x of
                          (Right t) -> case typeExp g (ETup xs) of
                                       (Right (TTup ts)) -> Right $ TTup (t:ts)
                                       (Left s)   -> Left s
                          (Left s)   -> Left s

typePat :: Gamma -> Pattern -> Typ -> Either String Gamma
typePat g Wild _ = Right g
typePat g (PVar name) t = Right $ Map.insert name t g
typePat g (PTup []) (TTup []) = Right g
typePat g (PTup []) _ = Left "tuple length mismatch: ptup ended"
typePat g _ (TTup []) = Left "tuple length mismatch: ttup ended"
typePat g (PTup (p:ps)) (TTup (t:ts)) = case typePat g p t of
    (Right g') -> typePat g' (PTup ps) (TTup ts)
    (Left e) -> Left e

checkPi :: Gamma -> Pi -> Either String ()
checkPi g Nil = Right ()
checkPi g (p1 :|: p2) = case checkPi g p1 of 
    Right _ -> checkPi g p2
    Left s  -> Left s
checkPi g (New name t p) = checkPi (Map.insert name t g) p
checkPi g (Out name e) = case typeExp g e of
    (Right t) -> case (Map.lookup name g) of 
                    (Just t') -> if t == t' then Right () 
                                 else Left "channel output type mismatch"
                    Nothing -> Left "unbound variable"
    (Left e)  -> Left e
checkPi g (Inp name pat p) = case (Map.lookup name g) of 
    (Just t) -> case typePat g pat t of 
                    (Right g') -> Right ()
                    (Left e) -> Left e
    Nothing -> Left "unbound variable"
checkPi g (RepInp name pat p) = case (Map.lookup name g) of 
    (Just t) -> case typePat g pat t of 
                    (Right g') -> Right ()
                    (Left e) -> Left e
    Nothing -> Left "unbound variable"
checkPi g (Embed func p) = checkPi g p

check :: Pi -> Either String ()
check p = checkPi Map.empty p




-- Signals a dynamic error

type_error :: String -> a
type_error s = error $ "Run-time Type Error: " ++ s

-- Environments for interpreters

-- TASK!
-- Implement your interpreter here!

type Env = Map Name Value




isDisjoint :: [Name] -> Pattern -> [Name]
isDisjoint s (PVar name)   = if elem name s then s 
                             else type_error "non-disjoint substitution domains"
isDisjoint s (PTup [])     = s
isDisjoint s (PTup ((PVar v):ps) ) = 
        if elem v s then type_error "non-disjoint substitution domains"
        else isDisjoint ([v]++ s) (PTup ps) 
isDisjoint s (PTup ((PTup p):ps) )  = isDisjoint (isDisjoint s (PTup p) ) (PTup ps)





-- evalPat env p v
-- match a value v against a pattern p and extend environment env
evalPat :: Env -> Pattern -> Value -> Env
evalPat env Wild           _               = env
evalPat env (PVar vname)   val             = Map.insert vname val env
evalPat env pt@(PTup (p:ps) ) (VTup (v:vs) )  = 
      case isDisjoint [] pt of
            _ -> evalPat (evalPat env p v) (PTup ps) (VTup vs)
evalPat env (PTup [] )      (VTup [] )     = env
evalPat _ _       _                        = type_error $ "mismatched value; needed list of channels"


-- evalExp env e
-- evaluates e to a value in environment env
evalExp :: Env -> Exp -> Value
evalExp env (EVar x) = env ! x
evalExp env (ETup es) = VTup (evalExps env es)
  where
    evalExps env [] = []
    evalExps env (e:es) = evalExp env e : evalExps env es

run :: Env -> Pi -> IO ()
run env Nil = return ()
run env (p1 :|: p2) = Concurrent.parallel [(run env p1),(run env p2)]
run env (New name typ pi) = 
    do
    chan <- newChan
    let env' = Map.insert name (VChan chan) env
    run env' pi
run env (Out name exp) =
    case Map.lookup name env of
        (Just (VChan chan)) -> writeChan chan $ evalExp env exp
        (Just _)            -> type_error "sending to nonchannel"
        Nothing             -> error "output to nonexistent channel"
run env (Inp name pat pi) =
    do
    let chan = case Map.lookup name env of
               (Just (VChan chan)) -> chan
               _   -> error "input from nonexistent channel"
    val <- readChan chan 
    let env' = evalPat env pat val
    run env' pi
run env (RepInp name pat pi) =
    do
    let chan = case Map.lookup name env of
               (Just (VChan chan)) -> chan
               (Just _) -> type_error "sending to nonchannel"
               _   -> error "input from nonexistent channel"
    val <- readChan chan 
    let env' = evalPat env pat val
    run env' ((RepInp name pat pi) :|: pi)
run env (Embed func pi) = 
  do 
    func env
    run env pi
    


start :: Pi -> IO ()
start p = run Map.empty p
