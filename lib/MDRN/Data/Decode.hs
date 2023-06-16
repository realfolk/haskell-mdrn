{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module MDRN.Data.Decode
    ( Decoder
    , Error (..)
    , FromData (..)
    , bool
    , byteString
    , byteStringEq
    , decodeByteString
    , decodeData
    , decodeText
    , decodeTextCustom
    , fromData
    , integer
    , list
    , oneOf
    , prim
    , rational
    , symbol
    , symbolEq
    , text
    , textEq
    , unit
    , when
    ) where

import           Data.Bifunctor            (first)
import qualified Data.Binary               as B
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Text                 as T
import           MDRN.Data                 (Data (..), Prim (..))
import           MDRN.Data.Decode.Internal
import qualified MDRN.Data.Decode.List     as DL
import qualified MDRN.Data.Parser          as Parser
import qualified Pouch.Crypto.Hash         as Hash
import qualified Pouch.Crypto.Hash.SHA1    as SHA1
import           Pouch.Text                (tshow)
import qualified Pouch.Time                as Time
import qualified Pouch.Time.Date           as Date
import qualified Pouch.URL.Component.Query as Query
import           Prelude                   hiding (drop, head, length, tail,
                                            take)

-- * FromData

instance FromData () where
  decoder = unit

instance (FromData a, FromData b) => FromData (a, b) where
  decoder = DL.list $ (,) <$> DL.item' <*> DL.item' <* DL.end

instance FromData a => FromData (Maybe a) where
  decoder = oneOf
    [ DL.list $ Just <$ DL.symbolEq "just" <*> DL.item'
    , Nothing <$ symbolEq "nothing"
    ]

instance FromData a => FromData [a] where
  decoder = list decoder

instance FromData Data where
  decoder = Decoder pure

instance FromData Prim where
  decoder = prim

instance FromData Bool where
  decoder = bool

instance FromData Integer where
  decoder = integer

instance FromData T.Text where
  decoder = text

instance FromData BS.ByteString where
  decoder = byteString

instance FromData (Hash.Digest SHA1.SHA1) where
  decoder = byteString >>= maybe (fail "Unable to decode SHA1 digest from ByteString") return . Hash.decodeStrict

instance FromData Error where
  decoder = oneOf $ map DL.list
    [ TypeMismatch <$ DL.symbolEq "type-mismatch"
    , ParseError <$ DL.symbolEq "parse-error" <*> DL.item'
    , MultipleErrors <$ DL.symbolEq "multiple-errors" <*> DL.item (list decoder)
    , Failure <$ DL.symbolEq "failure" <*> DL.item'
    ]

instance FromData Query.Query where
  decoder = Query.fromList <$> decoder

instance FromData Date.Date where
  decoder = DL.list $ Date.date <$> (DL.symbolEq "date" *> DL.item year) <*> month <*> DL.item day
    where
      year = fromIntegral <$> integer
      month = DL.item'
      day = fromIntegral <$> integer

instance FromData Time.Time where
  decoder = DL.list $ DL.symbolEq "time" *> DL.item valueDecoder
    where
      valueDecoder =
        oneOf
          [ Time.fromWeeks <$> (decoder :: Decoder Time.Weeks)
          , Time.fromDays <$> (decoder :: Decoder Time.Days)
          , Time.fromHours <$> (decoder :: Decoder Time.Hours)
          , Time.fromMinutes <$> (decoder :: Decoder Time.Minutes)
          , Time.fromSeconds <$> (decoder :: Decoder Time.Seconds)
          , Time.fromMilliseconds <$> (decoder :: Decoder Time.Milliseconds)
          , Time.fromMicroseconds <$> (decoder :: Decoder Time.Microseconds)
          , Time.fromNanoseconds <$> (decoder :: Decoder Time.Nanoseconds)
          , Time.fromPicoseconds <$> (decoder :: Decoder Time.Picoseconds)
          ]

instance FromData Time.Years where
  decoder = timeUnitDecoder "years"

instance FromData Time.Month where
  decoder =
    oneOf
      [ symbolEq "january" >> return Time.January
      , symbolEq "february" >> return Time.February
      , symbolEq "march" >> return Time.March
      , symbolEq "april" >> return Time.April
      , symbolEq "may" >> return Time.May
      , symbolEq "june" >> return Time.June
      , symbolEq "july" >> return Time.July
      , symbolEq "august" >> return Time.August
      , symbolEq "september" >> return Time.September
      , symbolEq "october" >> return Time.October
      , symbolEq "november" >> return Time.November
      , symbolEq "december" >> return Time.December
      ]

instance FromData Time.Weeks where
  decoder = timeUnitDecoder "weeks"

instance FromData Time.Weekday where
  decoder =
    oneOf
      [ symbolEq "monday" >> return Time.Monday
      , symbolEq "tuesday" >> return Time.Tuesday
      , symbolEq "wednesday" >> return Time.Wednesday
      , symbolEq "thursday" >> return Time.Thursday
      , symbolEq "friday" >> return Time.Friday
      , symbolEq "saturday" >> return Time.Saturday
      , symbolEq "sunday" >> return Time.Sunday
      ]

instance FromData Time.Days where
  decoder = timeUnitDecoder "days"

instance FromData Time.Hours where
  decoder = timeUnitDecoder "hours"

instance FromData Time.Minutes where
  decoder = timeUnitDecoder "minutes"

instance FromData Time.Seconds where
  decoder = timeUnitDecoder "seconds"

instance FromData Time.Milliseconds where
  decoder = timeUnitDecoder "milliseconds"

instance FromData Time.Microseconds where
  decoder = timeUnitDecoder "microseconds"

instance FromData Time.Nanoseconds where
  decoder = timeUnitDecoder "nanoseconds"

instance FromData Time.Picoseconds where
  decoder = timeUnitDecoder "picoseconds"

timeUnitDecoder :: Num a => T.Text -> Decoder a
timeUnitDecoder unit = DL.list $ fromInteger <$ DL.symbolEq unit <*> DL.item integer

-- * Runners

fromData :: FromData a => Data -> Either Error a
fromData = decodeData decoder

decodeText :: FromData a => T.Text -> Either Error a
decodeText = decodeTextCustom decoder

decodeTextCustom :: Decoder a -> T.Text -> Either Error a
decodeTextCustom decoder s =
  first (ParseError . tshow) (Parser.parseData s) >>= decodeData decoder

decodeByteString :: FromData a => LBS.ByteString -> Either Error a
decodeByteString b = do
  (remaining, _, data') <- first (const decodeError) $ B.decodeOrFail b
  if remaining == LBS.empty
     then maybe (Left $ Failure "Invalid data") Right (Parser.validateData data') >>= fromData
     else Left decodeError
  where
    decodeError = Failure "Unable to decode binary"
