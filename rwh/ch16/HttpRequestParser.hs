module HttpRequestParser
    ( HttpRequest(..)
    , Method(..)
    , p_request
    , p_query) where

import           Numeric (readHex)
import           Control.Monad (liftM4)
import           Control.Applicative (Applicative(liftA2))
import           Text.ParserCombinators.Parsec

data Method = Get
            | Post
  deriving (Eq, Ord, Show)

data HttpRequest = HttpRequest { reqMethod :: Method
                               , reqURL :: String
                               , reqHeaders :: [(String, String)]
                               , reqBody :: Maybe String
                               }
  deriving (Eq, Show)

p_query :: CharParser () [(String, Maybe String)]
p_query = p_pair `sepBy` char '&'

p_pair :: CharParser () (String, Maybe String)
p_pair = do
  name <- many1 p_char
  value <- optionMaybe (char '=' >> many p_char)
  return (name, value)

p_char :: CharParser () Char
p_char = oneOf urlBaseChars <|> (char '+' >> return ' ') <|> p_hex

p_hex :: CharParser () Char
p_hex = do
  char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _):_) = readHex [a, b]
  return . toEnum $ d

urlBaseChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "$-_.!*'(),"

p_request :: CharParser () HttpRequest
p_request = q "GET" Get (pure Nothing)
  <|> q "POST" Post (Just <$> many anyChar)
  where
    q name ctor body = liftM4 HttpRequest req url p_headers body
      where
        req = ctor <$ string name <* char ' '

    url = optional (char '/')
      *> manyTill notEOL (try $ string " HTTP/1." <* oneOf "01")
      <* crlf

p_headers :: CharParser st [(String, String)]
p_headers = header `manyTill` crlf
  where
    header = liftA2 (,) fieldName (char ':' *> spaces *> contents)

    contents = liftA2 (++) (many1 notEOL <* crlf) (continuation <|> pure [])

    continuation = liftA2 (:) (' ' <$ many1 (oneOf " \t")) contents

    fieldName = (:) <$> letter <*> many fieldChar

    fieldChar = letter <|> digit <|> oneOf "-_"

crlf :: CharParser st ()
crlf = (() <$ string "\r\n") <|> (() <$ newline)

notEOL :: CharParser st Char
notEOL = noneOf "\r\n"

parser :: CharParser () String
parser = (++) <$> string "HT" <*> (string "TP" <|> string "ML")