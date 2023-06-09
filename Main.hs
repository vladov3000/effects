{-# LANGUAGE BlockArguments #-}

module Main where

import Prelude hiding (error)

data Domain
  = IntegerDomain Integer
  | BoolDomain Bool
  | FunctionDomain (Domain -> Computation)

data StateRequest = Get | Put Integer

data LocalRequest = Variable String | Closure String Computation

data Request
  = ErrorRequest
  | StateRequest StateRequest
  | LocalRequest LocalRequest

data Computation
  = Done Domain
  | Request Request (Domain -> Computation)

error :: Computation
error = Request ErrorRequest \_ -> error

bind :: Computation -> (Domain -> Computation) -> Computation
bind (Done x) f = f x
bind (Request r f) f' = Request r \x -> bind (f x) f'

handleState :: Integer -> Computation -> Computation
handleState _ x@(Done _) = x
handleState s (Request (StateRequest r) k) =
  case r of
    Get   -> handleState s $ k (IntegerDomain s)
    Put s -> handleState s $ k (IntegerDomain s)
handleState s (Request r k) = Request r (handleState s . k)

type Environment = [(String, Domain)]

handleVariable :: Environment -> Computation -> Computation
handleVariable env (Request (LocalRequest r) k) =
  case r of
    Variable v | Just x <- lookup v env -> handleVariable env $ k x
    Variable _                          -> error
    Closure v body                      ->
      handleVariable env $ k $ FunctionDomain \x -> handleVariable ((v, x) : env) body
handleVariable env (Request r k) = Request r (handleVariable env . k)

class Basic d where
  integer   :: Integer -> d
  bool      :: Bool -> d
  increment :: d
  apply     :: d -> d -> d

instance Basic Computation where
  integer   = Done . IntegerDomain
  bool      = Done . BoolDomain
  increment = Done . FunctionDomain $ \x ->
    case x of
      IntegerDomain n -> integer $ n + 1
      _               -> error
  apply (Done (FunctionDomain f)) (Done x)      = f x
  apply (Done _)                  (Done x)      = error
  apply (Request r k)             x             = Request r \f -> apply (k f) x
  apply f                         (Request r k) = Request r \x -> apply f (k x)

class Conditional d where
  equals :: d
  if'    :: d -> d -> d -> d

instance Conditional Computation where
  equals = Done . FunctionDomain $ \x -> Done . FunctionDomain $ \y ->
    case (x, y) of
      (IntegerDomain x, IntegerDomain y) -> bool $ x == y
      (BoolDomain    x, BoolDomain    y) -> bool $ x == y
      _                                  -> error
  if' b x y = bind b \b ->
    case b of
      BoolDomain True  -> x
      BoolDomain False -> y
      _                -> error

class GlobalIntegerState d where
  getGlobal :: d
  putGlobal :: d -> d
      
instance GlobalIntegerState Computation where
  getGlobal   = Request (StateRequest Get) Done
  putGlobal x = bind x \x ->
    case x of
      IntegerDomain x -> Request (StateRequest (Put x)) Done
      _               -> error

class Lambda d where
  variable :: String -> d
  lambda   :: String -> d -> d

instance Lambda Computation where
  variable v      = Request (LocalRequest (Variable v))     Done
  lambda   v body = Request (LocalRequest (Closure v body)) Done

main :: IO ()
main = undefined
