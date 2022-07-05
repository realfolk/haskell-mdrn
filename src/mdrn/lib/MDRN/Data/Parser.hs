{-# LANGUAGE OverloadedStrings #-}

module MDRN.Data.Parser
    ( parseData
    , validateData
    ) where

import qualified Data.ByteString   as BS
import qualified Data.Char         as Char
import           Data.Maybe        (fromJust)
import qualified Data.Text         as T
import qualified Lib
import           Lib.Math.Rational (Rational, (%))
import           MDRN.Data         (Data (..), Prim (..))
import qualified MDRN.Data         as Data
import           Numeric.Natural   (Natural)
import qualified Text.Parsec       as P
import           Text.Parsec       ((<|>))

type Parser a = P.Parsec T.Text () a

parseData :: T.Text -> Either P.ParseError Data
parseData = P.runParser parser () "Data"
  where
    parser = P.spaces *> data' <* P.spaces <* P.eof

data' :: Parser Data
data'
  = symbolOrMapReference
  <|> listOrMapReference
  <|> unitBooleanOrByteString
  <|> rational
  <|> text
  <|> quoted
  <|> quotedShorthand
  <|> createListShorthand

symbolOrMapReference :: Parser Data
symbolOrMapReference = do
  s <- Data.symbol <$> symbol'
  dot *> mapReference s <|> return s

symbol' :: Parser T.Text
symbol' = toText <$> firstChar <*> restChars
  where
    toText first rest = T.pack $ first : rest
    firstChar = P.letter
    restChars = P.many (P.alphaNum <|> P.char '-')

listOrMapReference :: Parser Data
listOrMapReference = do
  l <- list
  dot *> mapReference l <|> return l

list :: Parser Data
list = List <$> between openParen closeParen data'
  where
    openParen = P.char '('
    closeParen = P.char ')'

mapReference :: Data -> Parser Data
mapReference m = toExpr m <$> keys
  where
    toExpr m ks = foldl applyGet m ks
    applyGet m k = List [ Data.symbol "get", m, k ]
    keys = P.sepBy1 key dot
    key = symbolKey <|> list
    symbolKey = Data.quote . Data.symbol <$> symbol'

unitBooleanOrByteString :: Parser Data
unitBooleanOrByteString = P.char '#' *> primitive
  where
    primitive
      = Data.unit <$ P.char 'u'
      <|> Data.bool True <$ P.char 't'
      <|> Data.bool False <$ P.char 'f'
      <|> Data.byteString <$> (P.char 'x' *> byteString)

byteString :: Parser BS.ByteString
byteString = BS.pack <$> P.many colonHexPair
  where
    colonHexPair = toByte <$> (P.char ':' *> hexDigit) <*> hexDigit
    toByte a b = fromJust $ Lib.hexToByte [a, b]

rational :: Parser Data
rational = do
  (isNegative, n) <- signedNatural
  rationalFraction isNegative n <|> rationalDecimal isNegative n <|> integer isNegative n

signedNatural :: Parser (Bool, Natural)
signedNatural =
  (,) <$> optionalNegativeSign <*> natural
    where
      optionalNegativeSign = P.option False negativeSign
      negativeSign = True <$ P.char '-'

natural :: Parser Natural
natural = read <$> digits

rationalFraction :: Bool -> Natural -> Parser Data
rationalFraction isNegative n =
  Data.rational . makeRationalFromFraction isNegative n <$> (P.char '/' *> natural)

makeRationalFromFraction :: Bool -> Natural -> Natural -> Rational
makeRationalFromFraction isNegative numerator denominator =
  applySign isNegative $ fromIntegral numerator % fromIntegral denominator

rationalDecimal :: Bool -> Natural -> Parser Data
rationalDecimal isNegative n =
  Data.rational . makeRationalFromDecimal isNegative n <$> (P.char '.' *> natural)

makeRationalFromDecimal :: Bool -> Natural -> Natural -> Rational
makeRationalFromDecimal isNegative integralPart fractionalPart =
  applySign isNegative $ numerator % denominator
    where
      power = if fractionalPart == 0 then 1 else fromIntegral $ length $ show fractionalPart
      denominator = 10 ^ power
      numerator = fromIntegral integralPart * denominator + fromIntegral fractionalPart

integer :: Bool -> Natural -> Parser Data
integer isNegative n =
  return $ Data.integer $ makeInteger isNegative n

makeInteger :: Bool -> Natural -> Integer
makeInteger isNegative n =
  if isNegative then
    negate $ fromIntegral n
  else
    fromIntegral n

text :: Parser Data
text = Data.text . T.concat <$> P.between doubleQuote doubleQuote elements
  where
    doubleQuote = P.char '"'
    elements = P.many element
    element
      = unescapeSpecialChar <$> (P.char '\\' *> P.anyChar)
      <|> T.pack <$> P.many1 (P.noneOf ['\\', '"'])

quoted :: Parser Data
quoted = toQuoted <$> (singleQuote *> data')
  where
    toQuoted expr = List [Data.symbol "quote", expr]
    singleQuote = P.char '\''

quotedShorthand :: Parser Data
quotedShorthand = toQuotedShorthand <$> between openBracket closeBracket data'
  where
    toQuotedShorthand exprs = List [Data.symbol "quote", List exprs]
    openBracket = P.char '['
    closeBracket = P.char ']'

createListShorthand :: Parser Data
createListShorthand =
  toCreateListShorthand <$> between openCurlyBracket closeCurlyBracket data'
    where
      toCreateListShorthand exprs =
        List
          [ List
              [ Data.symbol "get"
              , Data.symbol "list"
              , List
                  [ Data.symbol "quote"
                  , Data.symbol "create"
                  ]
              ]
          , List
              [ Data.symbol "quote"
              , List exprs
              ]
          ]
      openCurlyBracket = P.char '{'
      closeCurlyBracket = P.char '}'

validateData :: Data -> Maybe Data
validateData raw =
  case raw of
    Node (Symbol s) -> if isSymbol s then Just raw else Nothing
    Node _          -> Just raw
    List xs         -> List <$> mapM validateData xs

isSymbol :: T.Text -> Bool
isSymbol s =
  nonEmpty s && startsWithAlpha s && allAlphaNumOrHyphen s
    where
      nonEmpty = not . T.null
      startsWithAlpha = Char.isAlpha . T.head
      allAlphaNumOrHyphen = T.all (\c -> Char.isAlphaNum c || ('-' == c))

-- HELPERS

applySign :: Num a => Bool -> (a -> a)
applySign isNegative =
  if isNegative then negate else id

digits :: Parser String
digits = P.many1 P.digit

dot :: Parser Char
dot = P.char '.'

hexDigit :: Parser Char
hexDigit = Char.toLower <$> P.hexDigit

between :: Parser open -> Parser close -> Parser a -> Parser [a]
between open close p = P.between (lexeme open) close (P.many (lexeme p))

lexeme :: Parser a -> Parser a
lexeme p = p <* P.spaces

unescapeSpecialChar :: Char -> T.Text
unescapeSpecialChar c =
  case c of
    'n' ->
      "\n"

    't' ->
      "\t"

    _ ->
      T.singleton c
