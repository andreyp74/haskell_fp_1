{-
Реализуйте фукцию bindLog

bindLog :: Log a -> (a -> Log b) -> Log b
которая работает подобно оператору >>= для контекста Log.

GHCi> Log ["nothing done yet"] 0 `bindLog` add1Log
Log ["nothing done yet","added one"] 1

GHCi> Log ["nothing done yet"] 3 `bindLog` add1Log `bindLog` mult2Log
Log ["nothing done yet","added one","multiplied by 2"] 8
-}

module BindLog where

data Log a = Log [String] a deriving (Eq, Show)

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = \x -> Log [msg] (f x)

returnLog :: a -> Log a
returnLog = Log []

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log s1 x) f = let Log s2 y = f x
                       in Log (s1 ++ s2) y 


add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"
test0 = (Log ["nothing done yet"] 0 `bindLog` add1Log) == Log ["nothing done yet","added one"] 1
test1 = (Log ["nothing done yet"] 3 `bindLog` add1Log `bindLog` mult2Log) == Log ["nothing done yet","added one","multiplied by 2"] 8