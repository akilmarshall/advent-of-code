import Text.Megaparsec
import Text.Megaparsec.Char

singleLetterP :: Parser Char
singleLetterP = char 'h'

