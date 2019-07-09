{-
Тип LogLevel описывает различные уровни логирования.
data LogLevel = Error | Warning | Info

Определите функцию cmp, сравнивающую элементы типа LogLevel так, чтобы было верно, что Error > Warning > Info.
GHCi> cmp Error Warning
GT
GHCI> cmp Info Warning
LT
-}

data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error   Error   = EQ
cmp Warning Warning = EQ
cmp Info    Info    = EQ
cmp Error   _       = GT
cmp Info    _       = LT
cmp Warning Error   = LT
cmp Warning Info    = GT
