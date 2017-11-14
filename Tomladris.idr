module Tomladris

import public Data.String
import public Data.SortedMap as SM
import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

%access public export

mutual 
  Table : Type
  Table = SM.SortedMap String TomlValue

  data TomlValue = TComment String
                 | TString String
                 | TInteger Integer
                 | TDouble Double
                 | TBoolean Bool
                 | TArray (List TomlValue)
                 | TTable Table

parseComment : Parser TomlValue
parseComment = TComment . pack <$> (char '#' *> many (noneOf "\n"))

parseNumber : Parser TomlValue
parseNumber = (parseNum . pack) <$> many (oneOf "1234567890.eE+-")
  where
    parseNum : String -> TomlValue
    parseNum s = case parseInteger s of
                 (Just x) => TInteger x
                 Nothing => case parseDouble s of
                            (Just x) => TDouble x
                            Nothing => TDouble 0.0     


parseTBoolean : Parser TomlValue
parseTBoolean = toBool <$> (string "true" <|> string "false")
  where
    toBool s = case s of
                 "true" => TBoolean True
                 "false" => TBoolean False


parseTString : Parser TomlValue
parseTString = (TString) <$> (quoted '\'' <|> quoted '\"')


--lazying  '[' ']' match very very important
--else we get infinite loop
mutual
  parseTArray : Parser TomlValue
  parseTArray = TArray <$> (char '[' *> parsePrimitives <* char ']' <* spaces)
  
  parsePrimitives : Parser (List TomlValue)
  parsePrimitives = spaces *> (sepBy1 parseNumber (char ',')) <|>|
                    (sepBy1 parseTBoolean (char ',')) <|>|
                    (sepBy1 parseTString (char ',')) <|>|
                    (sepBy1 parseTArray (char ','))

  parseEntry : Parser (String, TomlValue)
  parseEntry = do
    k <- pack <$> (many (noneOf " [=") <* spaces)
    v <- (char '=' *> spaces *> parseTomlValue)
    pure (k, v)

  parseTable : Parser Table
  parseTable = do
    entries <- many parseEntry
    pure $ SM.fromList entries

  parseTableHeader : Parser String
  parseTableHeader = pack <$> (char '[' *>| many (noneOf "]")  <*| char ']')

  parseNamedTable : Parser (String, TomlValue)
  parseNamedTable = do
    header <- parseTableHeader
    spaces
    table <- parseTable
    spaces
    pure (header, TTable table)

  parseTomlValue : Parser TomlValue
  parseTomlValue = (parseTString <|> parseTArray <|> parseTBoolean <|> parseNumber)

  parseToml : Parser TomlValue
  parseToml = do
    spaces
    topTable <- parseTable
    namedTables <- many parseNamedTable
    eof
    pure $ TTable $ mergeLeft (SM.fromList namedTables) topTable

parseTomlString : String -> Either String TomlValue
parseTomlString s = parse parseToml s

joinWith : String -> List String -> String
joinWith sep = foldl (++) "" . intersperse sep

mutual
  showPair : (String, TomlValue) -> String
  showPair (key, (TTable t)) = "[" ++ key ++ "]\n" ++ showTable t
  showPair (key, val) = key ++ " = " ++ show val

  showTable : Table -> String
  showTable t = joinWith "\n" $ map showPair $ SM.toList t

  implementation Show TomlValue where
    show (TComment x) = "# " ++ show x
    show (TString x) = show x
    show (TInteger x) = show x
    show (TDouble x) = show x
    show (TBoolean x) = show x
    show (TArray xs) = "[" ++ (joinWith ", " $ map show xs) ++ "]"
    show (TTable x) = showTable x
