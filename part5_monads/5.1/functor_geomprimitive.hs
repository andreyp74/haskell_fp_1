{-
Определите представителя класса Functor для типа данных GeomPrimitive, который определён следующим образом:

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)
При определении, воспользуйтесь тем, что Point3D уже является представителем класса Functor.

GHCi> fmap (+ 1) $ Point (Point3D 0 0 0)
Point (Point3D 1 1 1)

GHCi> fmap (+ 1) $ LineSegment (Point3D 0 0 0) (Point3D 1 1 1)
LineSegment (Point3D 1 1 1) (Point3D 2 2 2)
-}

module FunctorGeomPrimitive where

data Point3D a = Point3D a a a deriving (Eq, Show)

instance Functor Point3D where
    fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a) deriving (Eq, Show)

instance Functor GeomPrimitive where
    fmap f (Point x) = Point (fmap f x)
    fmap f (LineSegment x y) = LineSegment (fmap f x) (fmap f y)

test0 = (fmap (+ 1) $ Point (Point3D 0 0 0)) == Point (Point3D 1 1 1)
test1 = (fmap (+ 1) $ LineSegment (Point3D 0 0 0) (Point3D 1 1 1)) == LineSegment (Point3D 1 1 1) (Point3D 2 2 2)