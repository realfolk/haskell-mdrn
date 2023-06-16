{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module MDRN.Language.Expr
    ( Args
    , CallStack
    , Data (..)
    , ErrorMessage
    , EvalExpr
    , EvaluationError (..)
    , Expr (..)
    , Function
    , Map
    , Name
    , Prim (..)
    , Result
    , RuntimeError (..)
    , Scope
    , callStackExprToText
    , fromExpr
    , optimize
    , runResult
    , throwEvaluationError
    , tryConvertToData
    ) where

import qualified Control.Monad.Except  as E
import qualified Control.Monad.State   as S
import           Data.Bifunctor        (first)
import qualified Data.Text             as T
import           MDRN.Data             (Data (..), Prim (..))
import qualified MDRN.Data             as Data
import           MDRN.Data.Decode      (Decoder, FromData (..))
import qualified MDRN.Data.Decode      as Decode
import qualified MDRN.Data.Decode.List as DL
import           MDRN.Data.Encode      (ToData (..))
import qualified MDRN.Language.Map     as Map
import qualified MDRN.Language.Scope   as Scope
import           Pouch.Either          (toMaybe)

-- * Expression

data Expr
  = P !Prim
  | F !Function
  | L ![Expr]
  | M !Map
  deriving (Eq, Show)

type Name = T.Text

type Map = Map.Map Name Expr

type Scope = Scope.Scope Name Expr

type Function = Scope -> EvalExpr -> Args -> Result Expr

type EvalExpr = Scope -> Expr -> Result (Scope, Expr)

type Args = [Expr]

instance Show Function where
  show _ = "Function"

instance Eq Function where
  (==) _ _ = False

-- * Constructors

optimize :: Data -> Expr
optimize (Node p) = P p
optimize (List l) = L $ map optimize l

-- * Converters

fromExpr :: Decode.FromData a => Expr -> Maybe a
fromExpr expr = do
  either (const Nothing) (toMaybe . Decode.fromData) $ tryConvertToData expr

tryConvertToData :: Expr -> Either T.Text Data
tryConvertToData expr =
  case expr of
    P p -> Right $ Node p
    L l -> List <$> mapM tryConvertToData l
    F _ -> Left "Cannot convert Function to Data"
    M _ -> Left "Cannot convert Map to Data - you must apply 'toList' first"

-- * Result

newtype Result a
  = Result (E.ExceptT EvaluationError (S.StateT CallStack IO) a)
  deriving
    ( Applicative
    , E.MonadError EvaluationError
    , E.MonadIO
    , Functor
    , Monad
    , S.MonadState CallStack
    )

type CallStack = [Expr]

runResult :: Result a -> IO (Either RuntimeError a)
runResult (Result m) = do
  (r, cs) <- S.runStateT (E.runExceptT m) []
  return $ first (`RuntimeError` callStackToText cs) r

callStackToText :: CallStack -> [T.Text]
callStackToText = map callStackExprToText

callStackExprToText :: Expr -> T.Text
callStackExprToText = either id (Data.toText . replaceByteStringsWithPlaceholder) . tryConvertToData

-- Used primarily to prevent large bytestrings from taking up
-- too much screen real estate when the call stack is displayed.
replaceByteStringsWithPlaceholder :: Data -> Data
replaceByteStringsWithPlaceholder data' =
  case data' of
    Node (ByteString b) -> Node (Text "<ByteString>")
    List xs             -> List $ map replaceByteStringsWithPlaceholder xs
    _                   -> data'

-- * Error

data RuntimeError
  = RuntimeError EvaluationError [T.Text]
  deriving (Eq, Show)

data EvaluationError
  = EEInvalidArguments
  | EENonFunctionApply ErrorMessage
  | EEExprAlreadyBound ErrorMessage
  | EENotDefined ErrorMessage
  | EEDomainError Data
  | EENotSerializable ErrorMessage
  | EEIOError
  deriving (Eq, Show)


type ErrorMessage = T.Text

instance ToData RuntimeError where
  toData (RuntimeError evaluationError callStack) =
    Data.list
      [ Data.symbol "runtime-error"
      , toData evaluationError
      , Data.list (map Data.text callStack)
      ]

instance FromData RuntimeError where
  decoder = DL.list $ RuntimeError <$ DL.item (DL.list $ DL.symbolEq "runtime-error") <*> DL.item' <*> DL.item decodeCallStack
    where
      decodeCallStack = Decode.list Decode.text

instance ToData EvaluationError where
  toData = encodeEvaluationError

encodeEvaluationError :: EvaluationError -> Data
encodeEvaluationError e =
  case e of
    EEInvalidArguments -> Data.list [Data.symbol "invalid-arguments"]
    EENonFunctionApply msg -> Data.list [Data.symbol "non-function-apply", Data.text msg]
    EEExprAlreadyBound msg -> Data.list [Data.symbol "expr-already-bound", Data.text msg]
    EENotDefined msg -> Data.list [Data.symbol "not-defined", Data.text msg]
    EEDomainError data' -> Data.list [Data.symbol "domain-error", data']
    EENotSerializable msg -> Data.list [Data.symbol "not-serializable", Data.text msg]
    EEIOError -> Data.list [Data.symbol "io-error"]

instance FromData EvaluationError where
  decoder = decodeEvaluationError

decodeEvaluationError :: Decoder EvaluationError
decodeEvaluationError = Decode.oneOf $ map DL.list
  [ decodeInvalidArguments
  , decodeNonFunctionApply
  , decodeExprAlreadyBound
  , decodeNotDefined
  , decodeDomainError
  , decodeNotSerializable
  , decodeIOError
  ]
    where
      decodeInvalidArguments = EEInvalidArguments <$ DL.item (DL.list $ DL.symbolEq "invalid-arguments")
      decodeNonFunctionApply = EENonFunctionApply <$ DL.item (DL.list $ DL.symbolEq "non-function-apply") <*> DL.item'
      decodeExprAlreadyBound = EEExprAlreadyBound <$ DL.item (DL.list $ DL.symbolEq "expr-already-bound") <*> DL.item'
      decodeNotDefined = EENotDefined <$ DL.item (DL.list $ DL.symbolEq "not-defined") <*> DL.item'
      decodeDomainError = EEDomainError <$ DL.item (DL.list $ DL.symbolEq "domain-error") <*> DL.item'
      decodeNotSerializable = EENotSerializable <$ DL.item (DL.list $ DL.symbolEq "not-serializable") <*> DL.item'
      decodeIOError = EEIOError <$ DL.item (DL.list $ DL.symbolEq "io-error")


throwEvaluationError :: EvaluationError -> Result a
throwEvaluationError = E.throwError
