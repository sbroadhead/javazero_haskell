module JavaZero.Language where

import Text.Parsec
import Text.Parsec.Token as T

languageDef :: LanguageDef st
languageDef =
    LanguageDef { commentStart = "/*"
                , commentEnd = "*/"
                , commentLine = "//"
                , nestedComments = False
                , identStart = letter
                , identLetter = alphaNum <|> char '_'
                , opStart = oneOf "!=<>+-*/"
                , opLetter = oneOf "!=<>+-*/"
                , reservedNames = ["import", "class", "extends", "static", "private", "public", "protected",
                                   "void", "int", "bool", "if", "while", "print", "println", "null", "new", "return", "true", "false"]
                , reservedOpNames = ["<", ">", "==", "!=", "+", "-", "*", "/", "="]
                , caseSensitive = True
                }

tokenParser :: TokenParser st
tokenParser = makeTokenParser languageDef

identifier = T.identifier tokenParser
reserved = T.reserved tokenParser
operator = T.operator tokenParser
reservedOp = T.reservedOp tokenParser
charLiteral = T.charLiteral tokenParser
stringLiteral = T.stringLiteral tokenParser
natural = T.natural tokenParser
integer = T.integer tokenParser
float = T.float tokenParser
naturalOrFloat = T.naturalOrFloat tokenParser
decimal = T.decimal tokenParser
hexadecimal = T.hexadecimal tokenParser
octal = T.octal tokenParser
symbol = T.symbol tokenParser
lexeme = T.lexeme tokenParser
whiteSpace = T.whiteSpace tokenParser
parens = T.parens tokenParser
braces = T.braces tokenParser
brackets = T.brackets tokenParser
semi = T.semi tokenParser
comma = T.comma tokenParser
colon = T.colon tokenParser
dot = T.dot tokenParser
semiSep = T.semiSep tokenParser
semiSep1 = T.semiSep1 tokenParser
commaSep = T.commaSep tokenParser
commaSep1 = T.commaSep1 tokenParser
