{-# LANGUAGE OverloadedStrings #-}

module MDRN.Module.Base
    ( apply
    , clientModule
    , defaultScope
    , module'
    ) where

import qualified Control.Monad.Except    as E
import           Control.Monad.IO.Class  (liftIO)
import           Data.Functor            ((<&>))
import qualified Data.Text               as T
import qualified Data.Text.IO            as TIO
import           MDRN.Data.Encode        (ToData (..))
import qualified MDRN.Language.Evaluator as Evaluator
import           MDRN.Language.Expr
import qualified MDRN.Language.Expr      as Expr
import qualified MDRN.Language.Map       as Map
import qualified MDRN.Language.Scope     as Scope
import qualified MDRN.Module.ByteString  as ByteStringModule
import qualified MDRN.Module.Either      as EitherModule
import qualified MDRN.Module.File        as FileModule
import qualified MDRN.Module.Import      as ImportModule
import qualified MDRN.Module.List        as ListModule
import qualified MDRN.Module.Map         as MapModule
import qualified MDRN.Module.Maybe       as MaybeModule
import qualified MDRN.Module.Mustache    as MustacheModule
import qualified MDRN.Module.Text        as TextModule
import qualified MDRN.Prim               as Prim

module' :: Map
module' =
  Map.fromList
    [ ("eval", F eval)
    , ("apply", F apply)
    , ("fn", F fn)
    , ("let", F let')
    , ("if", F if')
    , ("case", F case')
    , ("type-for", F typeFor)
    , ("eq", F equals)
    , ("lt", F $ lessThan False)
    , ("lte", F $ lessThan True)
    , ("gt", F $ greaterThan False)
    , ("gte", F $ greaterThan True)
    , ("and", F and')
    , ("or", F or')
    , ("not", F not')
    , ("add", F add)
    , ("sub", F subtract')
    , ("mul", F multiply)
    , ("div", F divide)
    , ("mod", F modulo)
    , ("throw", F throw')
    , ("catch", F catch')
    , ("identity", F id')
    , ("always", F const')
    , ("partial", F partial)
    , ("compose", F compose)
    , ("get", F MapModule.get)
    , ("map", M MapModule.module')
    , ("text", M TextModule.module')
    , ("bytestring", M ByteStringModule.module')
    , ("list", M ListModule.module')
    , ("either", M EitherModule.module')
    , ("maybe", M MaybeModule.module')
    , ("mustache", M MustacheModule.module')
    ]

defaultScope :: Scope
defaultScope = Scope.fromMap module'

-- | Helper for making a client module with extras that are useful for client applications but potentially unsafe for server applications.
clientModule :: Map
clientModule =
  moduleWithExtra
    [ ("print", F print')
    , ("import", M $ ImportModule.module' allowedClientImports)
    ]

-- | Define the imports the default client module is able to import.
allowedClientImports :: [(T.Text, Map)]
allowedClientImports = [("file", FileModule.module')]

moduleWithExtra :: [(T.Text, Expr)] -> Map
moduleWithExtra = foldr (\(k, v) m -> Map.insert k v m) module'

-- * Core
-- | Evaluate a 'Expr' in the calling scope if the 'Expr' is 'Data', otherwise simply return it.
--
-- @
-- (let '((x 1) (y 2)) '(eval '(+ x y)))
-- ; ...produces the same result as...
-- (+ 1 2)
-- @
eval :: Function
eval scope evalExpr args =
  case args of
    [exprs] -> snd <$> evalExpr scope exprs
    _       -> throwEvaluationError EEInvalidArguments

-- | Apply a 'Function' with a supplied list of arguments.
--
-- @
-- (apply (fn '(x y) '(+ x y)) '(1 2))
-- ; ...produces the same result as...
-- (+ 1 2)
-- @
apply :: Function
apply scope evalExpr args =
  case args of
    [F f, L argsExprs] -> f scope evalExpr argsExprs
    _                  -> throwEvaluationError EEInvalidArguments

-- | Define a function (fn).
--
-- @
-- (fn '(x y) '(add x y))
-- (fn [x y] [add x y])
-- @
fn :: Function
fn scope _ args =
  case args of
    [L argsExpr, body] ->
      F <$>
      buildFunction body (buildArgsBinder argsExpr (Scope.push Map.empty scope))
    _ -> throwEvaluationError EEInvalidArguments
  where
    buildArgsBinder :: [Expr] -> Scope -> Args -> Result Scope
    -- ^ Build function that will bind the 'Function' 'Args' correctly when called.
    buildArgsBinder expr scope' args =
      case (expr, args) of
        ([], []) -> return scope' -- If empty argument list and no parameters, return scope unchanged.
        (P (Symbol name):exprs, a:as) -- Argument with more to go. Ensure at least one argument has been passed to this function.
         -> do
          newScope <- Evaluator.bind name a scope'
          buildArgsBinder exprs newScope as
        _ -- Fail in all other scenarios.
         -> throwEvaluationError EEInvalidArguments
    buildFunction :: Expr -> (Args -> Result Scope) -> Result Function
    -- ^ Build the 'Function' with correct 'Args' to be executed when called.
    buildFunction body bindArgs =
      return
        (\_ evalExpr args -> do
           fnScope <- bindArgs args
           Evaluator.evalNestedExpr fnScope evalExpr body)

-- | Bind variables to a temporary 'Scope' that is only used when
-- evaluating the symbolic expression that was passed to this function.
--
-- @
-- (let '(x 1) '(y 2) '(add x y))
-- (let [x 1] [y 2] [add x y])
-- @
let' :: Function
let' scope evalExpr args = loop args (Scope.push Map.empty scope)
  where
    loop xs scope' =
      case xs of
        [body] -> Evaluator.evalNestedExpr scope' evalExpr body
        (bindingExpr:rest) -> do
          newScope <- addBinding bindingExpr scope'
          loop rest newScope
        _ -> throwEvaluationError EEInvalidArguments
    addBinding :: Expr -> Scope -> Result Scope
    addBinding bindingExpr scope' =
      case bindingExpr of
        L [P (Symbol name), expr] -> do
          (_, v) <- evalExpr scope' expr
          Evaluator.bind name v scope'
        _ -> throwEvaluationError EEInvalidArguments

-- | Conditionally evaluate one of two 'Expr's based on whether a 'Bool' is 'True' or 'False'.
--
-- @
-- (if #t 123 456) == 123
-- (if #f 123 456) == 456
-- ((if #t '(fn '(a) '(add 5 (a)) '(fn '(a) '(add 10 (a))))) 2) == 7
-- (if #t [add 1 2] [add 3 4]) ;3
-- @
if' :: Function
if' scope evalExpr args =
  case args of
    [P (Bool condition), a, b] ->
      Evaluator.evalNestedExpr
        scope
        evalExpr
        (if condition
           then a
           else b)
    _ -> throwEvaluationError EEInvalidArguments

-- | Evaluate expressions based on whether a main expr meets one of several conditions. Each condition contains two expressions. The first expression is the expr to compare equivalence to the main expr. The second is the expression to evaluate if the condition is met. The first expression is only evaluated when that branch is being evaluated. So, evaluation of an invocation of @case@ terminates once a condition has been met, and all expressions in subsequent branches remain unevaluated.
--
-- @
-- ; match an integer based on equality
-- (case 123
--   '((#n:123 '#t)
--     (456 #f)))
-- ; match an s-expression based on equality
-- (case '(foo bar)
--   '(('(foo bar) '(+ 1 2))
--     (456 #f)))
-- @
case' :: Function
case' scope evalData args =
  case args of
    (F _):_            -> throwEvaluationError EEInvalidArguments
    [mainExpr, L expr] -> handleCase mainExpr expr
    _                  -> throwEvaluationError EEInvalidArguments
  where
    handleCase :: Expr -> [Expr] -> Result Expr
    handleCase mainExpr exprs =
      case exprs of
        L [comparisonExpr, resultExpr]:remainingCases -> do
          (_, comparison) <- evalData scope comparisonExpr
          if mainExpr == comparison
            then snd <$> evalData scope resultExpr
            else handleCase mainExpr remainingCases
        _ -> throwEvaluationError EEInvalidArguments

-- | Returns a symbol indicating the type of the argument.
typeFor :: Function
typeFor scope evalData args =
  case args of
    [a] -> (result . process) a
    _   -> throwEvaluationError EEInvalidArguments
  where
    process a =
      case a of
        P Unit           -> "unit"
        P (Bool _)       -> "boolean"
        P (Integer _)    -> "integer"
        P (Rational _)   -> "rational"
        P (Symbol _)     -> "symbol"
        P (Text _)       -> "text"
        P (ByteString _) -> "bytestring"
        F _              -> "function"
        L l              -> "(" <> T.unwords (map process l) <> ")"
        M _              -> "map"
    result = return . P . Symbol

-- * Comparitive Operations
-- | Checks equality on two given expressions. Only primitives and lists are checked for equality. Functions and Maps cannot be compared.
--
-- @
-- (equals "foo" "foo"), returns #t
-- (equals foo "bar"), returns #f
-- (equals (fn [a b] [add a b]) (fn [a b] [add a b]), returns #f
-- @
equals :: Function
equals _ _ args =
  case args of
    [a, b] -> (return . P . Bool) $ eq a b
    _      -> throwEvaluationError EEInvalidArguments
  where
    eq a b =
      case (a, b) of
        (P p1, P p2) -> p1 == p2
        (L l1, L l2) -> all (uncurry eq) $ zip l1 l2
        (_, _)       -> False

-- | Compares whether the first expression is less than (or less than or equal to) the other. Only primitives are compared.
--
-- @
-- (lessThan "foo" "foo"), returns #f
-- (lessThan foo "bar"), returns #f
-- (equals (fn [a b] [add a b]) (fn [a b] [add a b]), returns #f
-- @
lessThan :: Bool -> Function
lessThan allowEquals _ _ args =
  case args of
    [a, b] -> (return . P . Bool) $ lt a b
    _      -> throwEvaluationError EEInvalidArguments
  where
    lt a b =
      case (a, b) of
        (P p1, P p2) ->
          if allowEquals
            then p1 <= p2
            else p1 < p2
        (_, _) -> False

-- | Compares whether the first expression is greater than (or greater than or equal to ) the other. Only primitives are compared.
--
-- @
-- (greaterThan 5 1), return #t
-- (greaterThan "foo" "foo"), returns #f
-- (greaterThan foo "bar"), returns #f
-- (greaterThan (fn [a b] [add a b]) (fn [a b] [add a b]), returns #f
-- @
greaterThan :: Bool -> Function
greaterThan allowEquals _ _ args =
  case args of
    [a, b] -> (return . P . Bool) $ lt a b
    _      -> throwEvaluationError EEInvalidArguments
  where
    lt a b =
      case (a, b) of
        (P p1, P p2) ->
          if allowEquals
            then p1 >= p2
            else p1 > p2
        (_, _) -> False

-- * Boolean Operations
and' :: Function
and' scope evalExpr args =
  case args of
    [P (Bool b1), P (Bool b2)] -> (return . P . Bool) $ b1 && b2
    _                          -> throwEvaluationError EEInvalidArguments

or' :: Function
or' scope evalExpr args =
  case args of
    [P (Bool b1), P (Bool b2)] -> (return . P . Bool) $ b1 || b2
    _                          -> throwEvaluationError EEInvalidArguments

not' :: Function
not' scope evalExpr args =
  case args of
    [P (Bool b)] -> (return . P . Bool) $ not b
    _            -> throwEvaluationError EEInvalidArguments

-- * Mathematical Operations
-- | Helper function to run a math operation on two numbers. Ensures the resulting expression does not lose data by coercing the result to the most precise type. For example, running the operation on an 'Integer' and 'Rational' returns a 'Rational'.
--
-- Each function argument is wrapped in a 'Maybe'. If provided in a 'Just', operations on associated arguments are successfully called. However, if 'Nothing' is provided, then invocations of the resulting 'Function' with associated arguments result in an error.
--
-- @
-- add :: Function
-- add = math (Just (+)) Nothing (Just (+))
--
-- (+ 1 -2) ; Succeeds, returns an Integer
-- (+ #n:1 -2) ; Succeeds, returns an Integer
-- (+ #n:1 #n:2) ; Fails
-- (+ #r:3/2 -5) ; Succeeds, returns a Rational
-- @
math ::
     Maybe (Integer -> Integer -> Prim) -- ^ Run the operation on two 'Integer's.
  -> Maybe (Rational -> Rational -> Prim) -- ^ Run the operation on two 'Rational's.
  -> Function
math ii rr _ _ = f
  where
    runOp :: Maybe (a -> b -> Prim) -> a -> b -> Result Expr
    runOp maybeFn a b =
      maybe (throwEvaluationError EEInvalidArguments ) (\g -> return $ P $ g a b) maybeFn
    f :: Args -> Result Expr
    f args =
      case args of
        [P (Integer a), P (Integer b)] -> runOp ii a b
        [P (Rational a), P (Rational b)] -> runOp rr a b
        [a@(P (Rational _)), P (Integer b)] ->
          f [a, P (Rational $ toRational b)]
        [P (Integer a), b@(P (Rational _))] ->
          f [P (Rational $ toRational a), b]
        _ -> throwEvaluationError EEInvalidArguments

-- | Similar to 'math', but preserves the type of the input arguments to make actual mathematical 'Function's easier to implement.
math' ::
     Maybe (Integer -> Integer -> Integer)
  -> Maybe (Rational -> Rational -> Rational)
  -> Function
math' ii rr =
  math (ii <&> \f a b -> Integer (f a b)) (rr <&> \f a b -> Rational (f a b))

-- | Add two numbers together.
add :: Function
add = math' f f
  where
    f :: Num a => Maybe (a -> a -> a)
    f = Just (+)

-- | Subtract a number from another number.
subtract' :: Function
subtract' = math' f f
  where
    f :: Num a => Maybe (a -> a -> a)
    f = Just (-)

-- | Multiply two numbers together.
multiply :: Function
multiply = math' f f
  where
    f :: Num a => Maybe (a -> a -> a)
    f = Just (*)

-- | Divide a number by another number. Always results in a 'Rational'.
divide :: Function
divide = math f f
  where
    f :: Real a => Maybe (a -> a -> Prim)
    f = Just (\a b -> Rational (fromRational $ toRational a / toRational b))

-- | Calculate the integer modulus. Only works on 'Integer's'
modulo :: Function
modulo = math' (Just mod) Nothing

-- | Throw an error. (throw invalid-arguments)
throw' :: Function
throw' scope evalExpr args =
  case args of
    [P (Symbol "non-function-apply"), P (Text msg)] ->
      throwEvaluationError (EENonFunctionApply msg)
    [P (Symbol "expr-already-bound"), P (Text msg)] ->
      throwEvaluationError (EEExprAlreadyBound msg)
    [P (Symbol "not-found"), P (Text msg)] -> throwEvaluationError (EENotDefined msg)
    [P (Symbol "domain-error"), data'] -> do
      errorData <- either (const $ throwEvaluationError EEInvalidArguments) return (Expr.tryConvertToData data')
      throwEvaluationError (EEDomainError errorData)
    [P (Symbol "not-serializable"), P (Text msg)] ->
      throwEvaluationError (EENotSerializable msg)
    _ -> throwEvaluationError EEInvalidArguments

-- | Catch an error. (catch '(throw invalid-arguments) (fn '(e) '(case (e) '((invalid-arguments 1) (expression-already-bound 2)))))
catch' :: Function
catch' scope evalExpr args =
  case args of
    [thrower, F catcher] -> do
      result <- E.liftIO $ runResult $ Evaluator.evalNestedExpr scope evalExpr thrower
      case result of
        Left err -> do
          let errExpr = (Expr.optimize . toData) err
          catcher scope evalExpr [errExpr]
        Right value -> return value
    _ -> throwEvaluationError EEInvalidArguments

-- | Identity function. Returns argument unchanged. (id "foo")
id' :: Function
id' _ _ args =
  case args of
    [a] -> return a
    _   -> throwEvaluationError EEInvalidArguments

-- | Constant function. Results in a function that ignores its arguments and runs the specified function.
const' :: Function
const' scope evalExpr args =
  case args of
    (F f:fArgs) -> return $ F (\_ _ _ -> f scope evalExpr fArgs)
    _           -> throwEvaluationError EEInvalidArguments

-- | Creates a partially applied function when given a Function f, and a list of arguments.
partial :: Function
partial scope evalExpr args =
  case args of
    (F f:fArgs) ->
      return $
      F $ \fScope fEvalExpr fArgs' -> f fScope fEvalExpr $ fArgs ++ fArgs'
    _ -> throwEvaluationError EEInvalidArguments

-- | Creates a composed Function from the two given Functions.
compose :: Function
compose scope evalExpr args =
  case args of
    [F f, F g] -> return $ F $ buildComposed f g
    _          -> throwEvaluationError EEInvalidArguments
  where
    buildComposed :: Function -> Function -> Function
    buildComposed f g =
      \scope' evalExpr' args' -> do
        gResult <- g scope' evalExpr' args'
        f scope' evalExpr' [gResult]

-- | Prints the given expression.
print' :: Function
print' scope evalExpr args =
  case args of
    [a] -> do
      liftIO $ TIO.putStrLn $ toText a
      return $ P Unit
    _ -> throwEvaluationError EEInvalidArguments
  where
    toText expr =
      case expr of
        P p -> Prim.toText p
        L l -> "(" <> T.unwords (map toText l) <> ")"
        M m -> T.pack $ show m
        F _ -> "fn"
