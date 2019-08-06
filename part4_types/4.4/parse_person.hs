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

import qualified Data.Map as Map
import Data.List.Split(splitOn)
import Data.Maybe(listToMaybe)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving (Eq, Show)

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving (Eq, Show)

splitKeyValue :: String -> Maybe [String]
splitKeyValue s = case length $ splited of
          0 -> Nothing
          1 -> Nothing
          _ -> Just splited
          where
            splited = splitOn " = " s

parsePairs :: String -> Maybe [[String]]
parsePairs = sequence . map splitKeyValue . lines

personFromDict :: Map.Map String String -> Maybe (Either Error Person)
personFromDict dict = do
                  firstName <- Map.lookup "firstName" dict
                  lastName <- Map.lookup "lastName" dict
                  ageStr <- Map.lookup "age" dict
                  case maybeRead ageStr of
                    Just age -> return $ Right (Person { firstName = firstName, lastName = lastName, age = age})
                    Nothing  -> return $ Left (IncorrectDataError ageStr) 
                  where
                    maybeRead = fmap fst . listToMaybe . reads

parsePerson :: String -> Either Error Person
parsePerson s = case parsePairs s of
                  Nothing -> Left ParsingError
                  Just pairs -> case personFromDict (Map.fromList $ map (\([x, y]) -> (x, y)) pairs) of
                                Just p  -> p
                                Nothing -> Left IncompleteDataError

-- parsePersonWithoutErrorsHandling :: String -> Either Error Person
-- parsePersonWithoutErrorsHandling s = let makeDict = map (\[key, val] -> (key, val)) . map (Split.splitOn " = ") . lines 
--                                          dict = Map.fromList $ makeDict s 
--                                          Just firstName = Map.lookup "firstName" dict
--                                          Just lastName = Map.lookup "lastName" dict
--                                          Just ageStr = Map.lookup "age" dict
--                                          age = read ageStr :: Int
--                                     in Right Person { firstName = firstName, lastName = lastName, age = age}


test1 = parsePerson "firstName = John\nlastName = Connor\nage = 30" == Right (Person { firstName = "John", lastName = "Connor", age = 30 })
test2 = parsePerson "firstName = John\nlastName = Connor\nage = 30\nbirsday = 01.01.1970" == Right (Person { firstName = "John", lastName = "Connor", age = 30 })
test3 = parsePerson "firstName = John\nlastName = Connor\naddress = 221B Baker Street, London UK\nage = 30" == Right (Person { firstName = "John", lastName = "Connor", age = 30 })

test4 = parsePerson "incorrectFormatString" == Left ParsingError
test5 = parsePerson "firstName : John\nlastName : Connor\nage : 30" == Left ParsingError
test6 = parsePerson "firstName = John\nlastName : Connor\nage = 30" == Left ParsingError

test7 = parsePerson "firstName = John\nlastName = Connor" == Left IncompleteDataError
test8 = parsePerson "firstName = John\nage = 30" == Left IncompleteDataError
test9 = parsePerson "lastName = Connor\nage = 30" == Left IncompleteDataError

test10 = parsePerson "firstName = John\nlastName = Connor\nage = foo" == Left (IncorrectDataError "foo")
test11 = parsePerson "firstName = John\nlastName = Connor\nage = 30.5" == Left (IncorrectDataError "30.5")
test12 = parsePerson "firstName = John\nlastName = Connor\nage = pi" == Left (IncorrectDataError "pi")

testAll = test1 && test2 && test3 && test4 && test5 && test6 && test7 && test8 && test9 && test10 && test11 && test12



makeDict :: String -> Either Error (Map.Map String String)
makeDict s = case parsePairs s of
              Just pairs -> Right $ Map.fromList $ map (\([x, y]) -> (x, y)) pairs
              Nothing    -> Left ParsingError

lookupValue :: String -> Map.Map String String -> Either Error String
lookupValue s dict = case Map.lookup s dict of
                      Just value -> Right value
                      Nothing    -> Left IncompleteDataError

readAge :: String -> Either Error Int                    
readAge ageStr = case maybeRead ageStr of
                  Just age -> Right age
                  Nothing  -> Left (IncorrectDataError ageStr)
                where
                  maybeRead = fmap fst . listToMaybe . reads

parsePerson' :: String -> Either Error Person
parsePerson' s = do
        dict      <- makeDict s
        firstName <- lookupValue "firstName" dict
        lastName  <- lookupValue "lastName" dict
        ageStr    <- lookupValue "age" dict
        age       <- readAge ageStr
        return $ Person { firstName = firstName, lastName = lastName, age = age}


test1' = parsePerson "firstName = John\nlastName = Connor\nage = 30" == Right (Person { firstName = "John", lastName = "Connor", age = 30 })
test2' = parsePerson "firstName = John\nlastName = Connor\nage = 30\nbirsday = 01.01.1970" == Right (Person { firstName = "John", lastName = "Connor", age = 30 })
test3' = parsePerson "firstName = John\nlastName = Connor\naddress = 221B Baker Street, London UK\nage = 30" == Right (Person { firstName = "John", lastName = "Connor", age = 30 })

test4' = parsePerson "incorrectFormatString" == Left ParsingError
test5' = parsePerson "firstName : John\nlastName : Connor\nage : 30" == Left ParsingError
test6' = parsePerson "firstName = John\nlastName : Connor\nage = 30" == Left ParsingError

test7' = parsePerson "firstName = John\nlastName = Connor" == Left IncompleteDataError
test8' = parsePerson "firstName = John\nage = 30" == Left IncompleteDataError
test9' = parsePerson "lastName = Connor\nage = 30" == Left IncompleteDataError

test10' = parsePerson "firstName = John\nlastName = Connor\nage = foo" == Left (IncorrectDataError "foo")
test11' = parsePerson "firstName = John\nlastName = Connor\nage = 30.5" == Left (IncorrectDataError "30.5")
test12' = parsePerson "firstName = John\nlastName = Connor\nage = pi" == Left (IncorrectDataError "pi")

testAll' = test1' && test2' && test3' && test4' && test5' && test6' && test7' && test8' && test9' && test10' && test11' && test12'