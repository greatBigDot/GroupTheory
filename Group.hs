import Data.Maybe

data Group a = Group {
  set      :: [a]
, func     :: (a -> a -> a) } 

showGroup :: (Show a) => Group a -> String
showGroup grp = show (set grp)
{-
isValidGroup :: (Eq a) => Group a -> Bool
isValidGroup grp = foldl1 (&&) (map (\f -> f grp) [isClosed, isIdentityIdentity, isInverseInverse, isAssociative])
-}

iC :: (Eq a) => Group a -> (a, a) -> Bool
iC grp (g,h) = elem (f g h) (set grp)
  where f = func grp

isClosed :: (Eq a) => Group a -> Bool
isClosed grp = foldl (&&) True (map (iC grp) (cSquare (set grp)))



--checks whether the given prospective identity (e) acts as the identity for the given input.
iIdId :: (Eq a) => Group a -> a -> a -> Bool
iIdId grp e g = (f g e == g) && (f e g == g)

--checks whether the given input  is the identity element for the group.
isIdentity :: (Eq a) => Group a -> a -> Bool
isIdentity grp e = 
  | n == 0                = iIdId' ((set grp)!!0)
  | iIdId' ((set grp)!!n) = True
  | otherwise             = isIdentity grp (n-1)
  where iIdId' g = iIdId grp g ((length (set grp))-1)

--checks whether the given group has an identity element
verAx2 :: (Eq a) => Group a -> Bool
verAx2 grp = isIdentity grp ((length (set grp)) - 1)

--returns the given group's identity element (wrapped in a Maybe type) if it has one; returns a Nothing if it doesn't.
identity :: (Eq a) => Group a -> Maybe a
identity grp
  = if' (verAx2 grp)
      (Just ((set grp) !! (length (takeWhile (not . isIdentity grp) [0..(ord-1)]))))
      (Nothing)
  where ord = length (set grp)


--isInverseInverse :: (Eq a) => Group a -> Bool
--isInverseInverse grp = foldl (&&) True (map (iInIn grp) (set grp))

isAssociative :: (Eq a) => Group a -> Bool
isAssociative grp = foldl (&&) True (map (iA grp) (cCube (set grp)))

--iInIn :: (Eq a) => Group a -> a -> Bool
--iInIn grp g = (f g (inv g)) == e && (f (inv g) g) == e
--  where f = func grp
--        e = identity grp
--        inv = inverse grp

iA :: (Eq a) => Group a -> (a,a,a) -> Bool
iA grp (g,h,j) = f (f g h) j == f g (f h j)
  where f = func grp


z :: (Integral a) => a -> Group a
z n = Group { set      = set_Z n
            , func     = func_Z n
            }
set_Z :: (Integral a) => a -> [a]
set_Z n = [1,2,0,3,4]

func_Z :: (Integral a) => a -> a -> a -> a
func_Z n x y = mod (x + y) n

{-
identity_Z :: (Integral a) => a -> a
identity_Z n = 0

inverse_Z :: (Integral a) => a -> a -> a
inverse_Z n x = mod (-x) n
-}

main = do
  { putStrLn . showGroup . z $ 5
  ; print . verAx2 . z $ 5
  ; print . fromJust . identity . z $ 5
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
