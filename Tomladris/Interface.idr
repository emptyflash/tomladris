module Tomladris.Interface

import public Lightyear
import public Tomladris
import public Data.SortedMap as SM

%access public export

data TomlType = TomlComment
               | TomlString
               | TomlInteger
               | TomlDouble
               | TomlBoolean
               | TomlArray
               | TomlTable

interpTomlType : TomlType -> Type
interpTomlType TomlComment = String
interpTomlType TomlString = String
interpTomlType TomlInteger = Integer
interpTomlType TomlDouble = Double
interpTomlType TomlBoolean = Bool
interpTomlType TomlArray = List TomlValue
interpTomlType TomlTable = Table

withToml : (a: TomlType) -> String -> (interpTomlType a -> Parser b) -> TomlValue -> Parser b
withToml TomlComment desc f (TComment str) = f str
withToml TomlString  desc f (TString str) = f str
withToml TomlInteger desc f (TInteger int) = f int
withToml TomlDouble desc f (TDouble d) = f d
withToml TomlBoolean desc f (TBoolean b) = f b
withToml TomlArray desc f (TArray l) = f l
withToml TomlTable desc f (TTable table) = f table
withToml ty desc f val = fail $ "expected " ++ desc

interface FromToml a where
  parseToml : TomlValue -> Parser a

FromToml String where
  parseToml = withToml TomlString "a string" pure

FromToml Integer where
  parseToml = withToml TomlInteger "a integer" pure

FromToml Bool where
  parseToml = withToml TomlBoolean "a boolean" pure

FromToml Double where
  parseToml = withToml TomlDouble "a double" pure

FromToml a => FromToml (List a) where
  parseToml = withToml TomlArray "an array" $ traverse parseToml



FromToml a => FromToml (SM.SortedMap String a) where
  parseToml = withToml TomlTable "a table" parseTomlTable
  where 
    parseTomlTuple : (String, TomlValue) -> Parser (String, a)
    parseTomlTuple (k, tv) = do
      v <- parseToml tv
      pure (k, v)

    parseTomlTable : Table -> Parser (SM.SortedMap String a)
    parseTomlTable table = map SM.fromList $ traverse parseTomlTuple $ SM.toList $ table


interface ToToml a where
  toToml : a -> TomlValue

Show TomlValue where
  show (TComment x) = ?lmao_1
  show (TString x) = ?lmao_2
  show (TInteger x) = ?lmao_3
  show (TDouble x) = ?lmao_4
  show (TBoolean x) = ?lmao_5
  show (TArray xs) = ?lmao_6
  show (TTable x) = ?lmao_7

