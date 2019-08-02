{-
Плоскость разбита на квадратные ячейки. Стороны ячеек параллельны осям координат. Координаты углов ячейки с координатой (0,0) имеют неотрицательные координаты. 
Один из углов этой ячейки имеет координату (0,0). С ростом координат ячеек увеличиваются координаты точек внутри этих ячеек.

Реализуйте функции getCenter, которая принимает координату ячейки и возвращает координату ее центра, и функцию getCell, которая принимает координату точки и 
возвращает номер ячейки в которой находится данная точка. В качестве первого аргумента обе эти функции принимают ширину ячейки.
-}

module GetCellCenter where

data Coord a = Coord a a deriving (Eq, Show)

getCenter :: Double -> Coord Int -> Coord Double
getCenter size (Coord x y) = Coord (fromIntegral x * size + size / 2) (fromIntegral y * size + size / 2)

getCell :: Double -> Coord Double -> Coord Int
getCell size (Coord x y) = Coord (floor (x / size)) (floor (y / size))


test1 = (getCenter 2.2 (Coord 2 1)) == Coord 5.5 3.3
test2 = (getCell 2.2 (Coord 3.2 1.6)) == Coord 1 0

testAll = test1 && test2