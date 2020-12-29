import           Text.ParserCombinators.Parsec (char, hexDigit, oneOf, many1
                                              , optionMaybe, (<|>), many
                                              , GenParser, CharParser)
import           Numeric (readHex)
import           Control.Applicative (Applicative(liftA2))

hexify :: Char -> Char -> Char
hexify a b = toEnum . fst . head . readHex $ [a, b]

a_hex :: GenParser Char st Char
a_hex = char '%' *> hexDigit

b_hex = hexify <$> a_hex <*> hexDigit

a_char = oneOf urlBaseChars <|> (' ' <$ char '+') <|> a_hex

urlBaseChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "$-_.!*'(),"

a_pair :: CharParser () (String, Maybe String)
a_pair = liftA2 (,) (many1 a_char) (optionMaybe (char '=' *> many a_char))