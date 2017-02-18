import Data.Maybe
import Data.Either

data Group a = Group { set  :: [a]
                     , func :: (a -> a -> a) }

showGroup :: (Show a) => Group a -> String
showGroup grp = show (set grp)


verAx :: (Eq a) => Int -> (Group a -> Bool)
verAx 0 = verAx0
verAx 1 = verAx1
verAx 2 = verAx2
verAx 3 = verAx3

verAxAll :: (Eq a) => Group a -> [Bool]
verAxAll grp = map (\f -> f grp) (map verAx [0..3])

isGroup :: (Eq a) => Group a -> Bool
isGroup grp = and (verAxAll grp)


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
--Returns the inverse function of the group if it has one; returns a Nothing otherwise.
inverse :: (Eq a) => Group a -> (a -> Maybe a)
inverse grp = (\g -> takeFirst (isInv grp g) (set grp))

--Returns true iff the group function is associative over the three inputs.
isAssoc :: (Eq a) => Group a -> (a,a,a) -> Bool
isAssoc grp (g,h,j) = f (f g h) j == f g (f h j)
  where f = func grp
--Returns true iff the group is associative.
verAx3 :: (Eq a) => Group a -> Bool
verAx3 grp = and (map (isAssoc grp) (cCube (set grp)))


--Returns the group of nonzero integers mod n with multiplication mod n.
zx :: (Integral a) => a -> Group a
zx n = Group { set  = set_Zx  n
             , func = func_Zx n }
--Returns the set of nonzero integers mod n
set_Zx :: (Integral a) => a -> [a]
set_Zx n = [1..(n-1)]
--Returns multiplication mod n
func_Zx :: (Integral a) => a -> a -> a -> a
func_Zx n = (\x y -> mod (x * y) n)

zn :: (Integral a) => Group a
zn = zx 13

main = do
  { putStrLn . showGroup $ zn
  ; print . verAxAll $ zn
  ; print . isGroup  $ zn
  ; print . identity $ zn
  ; print $ map (inverse zn) (set zn)
  }


cProd :: [a] -> [b] -> [(a, b)]
cProd [] ys     = []
cProd [x] ys    = map (\y -> (x,y)) ys
cProd (x:xs) ys = (cProd [x] ys) ++ (cProd xs ys)

cSquare :: [a] -> [(a,a)]
cSquare xs = cProd xs xs

cCube :: [a] -> [(a,a,a)]
cCube xs = [(a,b,c) | a <- xs, b <- xs, c <- xs]

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

--Returns the first element of the list that satisfies the predicate, or Nothing if no such element exists.
takeFirst :: (a -> Bool) -> [a] -> Maybe a
takeFirst _ []     = Nothing
takeFirst p (x:xs) = if' (p x) (Just x) (takeFirst p xs)
