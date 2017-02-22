module Group
( Group(..)
, verAxAll
, isGroup
, identity
, inverse
) where

import Data.Maybe
import Data.Either

data Group a = Group { set  :: [a]
                     , func :: (a -> a -> a) }

instance (Show a) => Show (Group a) where
  show grp = show $ set grp

--instance Ord (Group a) where

verAx :: (Eq a) => Int -> (Group a -> Bool)
verAx 0 = verAx0
verAx 1 = verAx1
verAx 2 = verAx2
verAx 3 = verAx3

verAxAll :: (Eq a) => Group a -> [Bool]
verAxAll grp = map (\f -> f grp) (map verAx [0..3])

isGroup :: (Eq a) => Group a -> Bool
isGroup = and . verAxAll


--Returns true iff f(g,h) is in the group.
isCl :: (Eq a) => Group a -> (a, a) -> Bool
isCl grp (g,h) = elem (f g h) (set grp)
  where f = func grp
--Returns true iff the group is closed.
verAx0 :: (Eq a) => Group a -> Bool
verAx0 grp = and (map (isCl grp) (cSquare (set grp)))

--Returns true iff f(e, g) = f(g, e) = g
isId :: (Eq a) => Group a -> a -> a -> Bool
isId grp e g = f e g == g && f g e == g
  where f = (func grp)
--Returns true iff e is an identity element of the group.
isIdentity :: (Eq a) => Group a -> a -> Bool
isIdentity grp e = and (map (isId grp e) (set grp))
--Returns true iff the group has an identity element.
verAx1 :: (Eq a) => Group a -> Bool
verAx1 grp = or (map (isIdentity grp) (set grp))
--Returns the given group's identity element (wrapped in a Maybe type) if it has one; returns a Nothing otherwise.
identity :: (Eq a) => Group a -> Maybe a
identity grp = takeFirst (isIdentity grp) (set grp)

--Returns true iff g and h are inverses.
isInv :: (Eq a) => Group a -> a -> a -> Bool
isInv grp g h = (f g h == e) && (f h g == e)
  where f = func grp
        e = maybe (error "No identity") (id) (identity grp)
--Returns true iff g has in inverse.
isInverse :: (Eq a) => Group a -> a -> Bool
isInverse grp g = or (map (isInv grp g) (set grp))
--Returns true iff every element of grp has an inverse.
verAx2 :: (Eq a) => Group a -> Bool
verAx2 grp = and (map (isInverse grp) (set grp))
--Returns the inverse function of the group (wrapped in a Maybe type) if it has one; returns a Nothing otherwise.
inverse :: (Eq a) => Group a -> (a -> Maybe a)
inverse grp = (\g -> takeFirst (isInv grp g) (set grp))

--Returns true iff the group function is associative over the three inputs.
isAssoc :: (Eq a) => Group a -> (a,a,a) -> Bool
isAssoc grp (g,h,j) = f (f g h) j == f g (f h j)
  where f = func grp
--Returns true iff the group is associative.
verAx3 :: (Eq a) => Group a -> Bool
verAx3 grp = and (map (isAssoc grp) (cCube (set grp)))


--Non-foundational group stuff
--In group theory, "order" refers both to the cardinality of a group's underlying set and the least positive integer n for a given element g such that g^n = 1_G. In here, the function "order" refers to the former notion, while the function "ord" refers to the latter.
--Order of a group (in the sense of cardinality)
order :: (Num b) => Group a -> b
order = length . set

--Group "exponentation;" it's repeated application of the group function. I.e., g^n = g*g*...*g, with n occurences of g. Note that if the Group is the integers with addition, then g^n refers to multiplication. That is, just because I'm using the "^" symbol and the words power and exponentation doesn't mean it's actually the function pow :: C^2 -> C.
pow :: (Integral b) => Group a -> (a -> b -> a)
pow grp _ 0 = identity grp
pow grp g n = (func grp) g (pow grp g (n-1))

--Order of an element in a group (not cardinality; that's order :: (Num b) => Group a -> b)). That is, the order of g in G is the least integer n such that g^n = 1_G. Every element of every finite group has a finite order, so this will only get stuck in an infinite loop for infinite groups, which I can't even create yet.
ord :: (Integral b) => Group a -> (a -> b)
ord grp g = 1 + length (takeWhile (/= e) (map (pow grp g) [1..(order grp)]))
  where e = identity grp


--Some non-group theory functions used above:

--cartesian product
cProd :: [a] -> [b] -> [(a, b)]
cProd [] ys     = []
cProd [x] ys    = map (\y -> (x,y)) ys
cProd (x:xs) ys = (cProd [x] ys) ++ (cProd xs ys)
--cartesian square
cSquare :: [a] -> [(a,a)]
cSquare xs = cProd xs xs
--cartesian cube [note: this can't just use cProd because in Haskell, (x,y,z) =/= ((x,y),z).]
cCube :: [a] -> [(a,a,a)]
cCube xs = [(a,b,c) | a <- xs, b <- xs, c <- xs]

--Purely functional alternative to [if/then/else] syntax.
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

--Returns the first element of the list that satisfies the predicate (wrapped in a Maybe type), or Nothing if no such element exists.
takeFirst :: (a -> Bool) -> [a] -> Maybe a
takeFirst _ []     = Nothing
takeFirst p (x:xs) = if' (p x) (Just x) (takeFirst p xs)
