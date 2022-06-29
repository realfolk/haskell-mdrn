{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module MDRN.Data.Encode
    ( ToData (..)
    , timeAsDays
    , timeAsHours
    , timeAsMicroseconds
    , timeAsMilliseconds
    , timeAsMinutes
    , timeAsNanoseconds
    , timeAsPicoseconds
    , timeAsSeconds
    , timeAsWeeks
    ) where

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Text               as T
import qualified Lib.Crypto.Hash         as Hash
import qualified Lib.Crypto.Hash.SHA1    as SHA1
import qualified Lib.Time                as Time
import qualified Lib.Time.Date           as Date
import qualified Lib.URL.Component.Query as Query
import           MDRN.Data               (Data (..), Prim (..), byteString,
                                          integer, list, symbol, text, unit)
import qualified MDRN.Data.Decode        as Decode

class ToData a where
  toData :: a -> Data

instance ToData () where
  toData = const unit

instance (ToData a, ToData b) => ToData (a, b) where
  toData (a, b) = list [toData a, toData b]

instance ToData a => ToData (Maybe a) where
  toData = maybe (symbol "nothing") (\a -> list [symbol "just", toData a])

instance ToData a => ToData [a] where
  toData = list . map toData

instance ToData Prim where
  toData = Node

instance ToData Data where
  toData = id

instance ToData T.Text where
  toData = text

instance ToData BS.ByteString where
  toData = byteString

instance ToData (Hash.Digest SHA1.SHA1) where
  toData = byteString . Hash.encodeStrict

instance ToData Query.Query where
  toData = toData . Query.toList

instance ToData Decode.Error where
  toData Decode.TypeMismatch = symbol "type-mismatch"
  toData (Decode.ParseError t) = list [symbol "parse-error", text t]
  toData (Decode.MultipleErrors es) = list [symbol "multiple-errors", list (map toData es)]
  toData (Decode.Failure t) = list [symbol "failure", text t]

instance ToData Date.Date where
  toData date =
    list
      [ symbol "date"
      , integer $ toInteger $ Date.getYear date
      , toData $ Date.getMonth date
      , integer $ toInteger $ Date.getDayOfMonth date
      ]

-- * TIME

instance ToData Time.Years where
  toData a = encodeTimeUnit "years" a

instance ToData Time.Month where
  toData a =
    symbol $ case a of
      Time.January   -> "january"
      Time.February  -> "february"
      Time.March     -> "march"
      Time.April     -> "april"
      Time.May       -> "may"
      Time.June      -> "june"
      Time.July      -> "july"
      Time.August    -> "august"
      Time.September -> "september"
      Time.October   -> "october"
      Time.November  -> "november"
      Time.December  -> "december"

instance ToData Time.Weeks where
  toData a = encodeTimeUnit "weeks" a

instance ToData Time.Weekday where
  toData a =
    symbol $ case a of
      Time.Monday    -> "monday"
      Time.Tuesday   -> "tuesday"
      Time.Wednesday -> "wednesday"
      Time.Thursday  -> "thursday"
      Time.Friday    -> "friday"
      Time.Saturday  -> "saturday"
      Time.Sunday    -> "sunday"

instance ToData Time.Days where
  toData a = encodeTimeUnit "days" a

instance ToData Time.Hours where
  toData a = encodeTimeUnit "hours" a

instance ToData Time.Minutes where
  toData a = encodeTimeUnit "minutes" a

instance ToData Time.Seconds where
  toData a = encodeTimeUnit "seconds" a

instance ToData Time.Milliseconds where
  toData a = encodeTimeUnit "milliseconds" a

instance ToData Time.Microseconds where
  toData a = encodeTimeUnit "microseconds" a

instance ToData Time.Nanoseconds where
  toData a = encodeTimeUnit "nanoseconds" a

instance ToData Time.Picoseconds where
  toData a = encodeTimeUnit "picoseconds" a

encodeTimeUnit :: Integral a => T.Text -> a -> Data
encodeTimeUnit unit' a =
  list
    [ symbol unit'
    , integer $ toInteger a
    ]

timeAsWeeks :: Time.Time -> Data
timeAsWeeks = timeWith Time.toWeeks

timeAsDays :: Time.Time -> Data
timeAsDays = timeWith Time.toDays

timeAsHours :: Time.Time -> Data
timeAsHours = timeWith Time.toHours

timeAsMinutes :: Time.Time -> Data
timeAsMinutes = timeWith Time.toMinutes

timeAsSeconds :: Time.Time -> Data
timeAsSeconds = timeWith Time.toSeconds

timeAsMilliseconds :: Time.Time -> Data
timeAsMilliseconds = timeWith Time.toMilliseconds

timeAsMicroseconds :: Time.Time -> Data
timeAsMicroseconds = timeWith Time.toMicroseconds

timeAsNanoseconds :: Time.Time -> Data
timeAsNanoseconds = timeWith Time.toNanoseconds

timeAsPicoseconds :: Time.Time -> Data
timeAsPicoseconds = timeWith Time.toPicoseconds

timeWith :: ToData a => (Time.Time -> a) -> Time.Time -> Data
timeWith f time = list [ symbol "time", toData (f time) ]
