{-
Пусть имеется тип данных, который описывает конфигурацию шахматной доски:

data Board = ...
Кроме того, пусть задана функция
nextPositions :: Board -> [Board]
которая получает на вход некоторую конфигурацию доски и возвращает все возможные конфигурации, которые могут получиться, если какая-либо фигура сделает один ход. 
Напишите функцию:
nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
которая принимает конфигурацию доски, число ходов n, предикат и возвращает все возможные конфигурации досок, которые могут получиться, если фигуры сделают n 
ходов и которые удовлетворяют заданному предикату. При n < 0 функция возвращает пустой список.
-}

module Board where

data Board = undefined

nextPositions :: Board -> [Board]
nextPositions = undefined

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred | n < 0     = []
                        | n == 0    = do
                            --if pred b then return b else []
                            True <- return $ pred b
                            return b
                        | otherwise = do 
                            b' <- nextPositions b
                            nextPositionsN b' (n - 1) pred
                            --concat $ map (\b -> nextPositionsN b (n - 1) pred) bs
                            --bs >>= \b -> nextPositionsN b (n - 1) pred


                            
