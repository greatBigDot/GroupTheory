import Data.Maybe
import Data.Either

data Group a = Group {
  set      :: [a]
, func     :: (a -> a -> a) } 

showGroup :: (Show a) => Group a -> String
showGroup grp = show (set grp)
{-
isValidGroup :: (Eq a) => Group a -> Bool
isValidGroup grp = foldl1 (&&) (map (\f -> f grp) [isClosed, isIdentityIdentity, isInverseInverse, isAssociative])
-}

--Returns true iff f(g,h) is in the group.
iC :: (Eq a) => Group a -> (a, a) -> Bool
iC grp (g,h) = elem (f g h) (set grp)
  where f = func grp
--Returns true iff the group is closed.
isClosed :: (Eq a) => Group a -> Bool
isClosed grp = and (map (iC grp) (cSquare (set grp)))

--Returns true iff f(e, g) = f(g, e) = g
isId :: (Eq a) => Group a -> a -> a -> Bool
isId grp e g = f e g == g && f g e == g
  where f = (func grp)
--Returns true iff e is an identity element of the group.
isIdentity :: (Eq a) => Group a -> a -> Bool
isIdentity grp e = and (map (isId grp e) (set grp))
--Returns true iff the group has an identity element.
verAx2 :: (Eq a) => Group a -> Bool
verAx2 grp = or (map (isIdentity grp) (set grp))
--Returns the given group's identity element (wrapped in a Maybe type) iff it has one; returns a Nothing iff it doesn't.
identity :: (Eq a) => Group a -> Maybe a
identity grp
  = if' (verAx2 grp)
      (Just ((set grp) !! (length (takeWhile (not . (isIdentity grp)) (set grp)))))
      (Nothing)

--Returns true iff the given function acts as an inverse function on the given group element.
isInv :: (Eq a) => Group a -> (a -> a) -> a -> Bool
isInv grp inv g = f g (inv g) == e && f (inv g) g == e
  where f = func grp
        e = identity grp
--Returns true iff the given function is an inverse on the group.
isInverse :: (Eq a) => Group a -> (a -> a) -> Bool
isInverse grp inv = and (map (isInv grp inv) (set grp))

verAx3 :: (Eq a) => Group a -> Bool
verAx3 grp = or (map (isInverse grp) ({-set of all closed unary functions on grp.-}))

isInverseInverse :: (Eq a) => Group a -> Bool
isInverseInverse grp = foldl (&&) True (map (iInIn grp) (set grp))



iA :: (Eq a) => Group a -> (a,a,a) -> Bool
iA grp (g,h,j) = f (f g h) j == f g (f h j)
  where f = func grp

isAssociative :: (Eq a) => Group a -> Bool
isAssociative grp = foldl (&&) True (map (iA grp) (cCube (set grp)))


z :: (Integral a) => a -> Group a
z n = Group { set      = set_Z n
            , func     = func_Z n
            }
set_Z :: (Integral a) => a -> [a]
set_Z n = [0..(n-1)]

func_Z :: (Integral a) => a -> a -> a -> a
func_Z n x y = mod (x + y) n

{-
identity_Z :: (Integral a) => a -> a
identity_Z n = 0

inverse_Z :: (Integral a) => a -> a -> a
inverse_Z n x = mod (-x) n
-}

n :: Int
n = 20

main = do
  { putStrLn . showGroup . z $ n
  ; print . isClosed . z $ n
  ; print . verAx2 . z $ n
  ; print . fromJust . identity . z $ n
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
