{-
Реализуйте функцию parsePerson, которая разбирает строки вида firstName = John\nlastName = Connor\nage = 30 и возвращает либо результат типа Person, 
либо ошибку типа Error.

* Строка, которая подается на вход, должна разбивать по символу '\n' на список строк, каждая из которых имеет вид X = Y. 
    Если входная строка не имеет указанный вид, то функция должна возвращать ParsingError.
* Если указаны не все поля, то возвращается IncompleteDataError.
* Если в поле age указано не число, то возвращается IncorrectDataError str, где str — содержимое поля age.
* Если в строке присутствуют лишние поля, то они игнорируются.
-}

module ParsePerson where

import qualified Data.List.Split as Split
import qualified Data.Map as Map

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show


readInt s = res
  where
    res =
      case read s :: Int of
        Just x -> return x
        Nothing -> throwM $ ReadException s (typeRep res)


buildPerson dict p = case Map.lookup "firstName" dict of
                        Just value -> p { firstName = value }


parsePerson :: String -> Either Error Person
parsePerson s = let makeDict = map (\[key, val] -> (key, val)) . map (Split.splitOn " = ") . lines
                    dict = Map.fromList $ makeDict s 


                    p = case Map.lookup "firstName" dict of
                        Nothing  -> Left IncompleteDataError
                        Just firstName -> p { firstName = firstName } Map.lookup "lastName" dict
                    ageStr = Map.lookup "age" dict
                    age = read ageStr :: Int
                in Right Person { firstName = firstName, lastName = lastName, age = age}