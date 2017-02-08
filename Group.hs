data Group a = Group
{ set      :: [a]
, func     :: (a -> a -> a)
, identity :: a
, inverse  :: a -> a
}

showGroup :: (Show a) => Group a -> String 
showGroup grp = show (set grp)

isValidGroup :: (Eq a) => Group a -> Bool
isValidGroup = foldl1 (&&) (map (\f -> f grp) [isClosed, isIdentityIdentity, isInverseInverse, isAssociative])


isClosed :: (Eq a) => Group a -> Bool
isClosed grp = foldl (&&) True (map (iC grp) (cSquare (set grp)))

isIdentityIdentity :: (Eq a) => Group a -> Bool
isIdentityIdentity grp = foldl1 (&&) (map (iIdId grp) (set grp))

isInverseInverse :: (Eq a) => Group a -> Bool
isInverseInverse grp = foldl (&&) True (map (iInIn grp) (set grp))

isAssociative :: (Eq a) => Group a -> Bool
isAssociative = foldl (&&) True (map (iA grp) (cCube (set grp)))

iC :: (Eq a) => Group a -> (a, a) -> Bool
iC grp (g,h) = elem (f g h) (set grp)
  where f = func grp

iIdId :: (Eq a) => Group a -> a -> Bool
iIdId grp g = (f g e) == g && (f e g) == g
  where f = func grp
	e = identity grp

iInIn :: (Eq a) => Group a -> a -> Bool
iInIn grp g = (f g h) == e && (f h g) == e
  where f = func grp
	e = identity grp
	h = inverse grp

iA :: (Eq a) => Group a -> (a,a,a) -> Bool
iA grp (g,h,j) = f (f g h) j == f g (f h j)
  where f = func grp


main = print "Hello World!"


cProd :: [a] -> [b] -> [(a, b)]
cProd [] ys     = []
cProd [x] ys    = map (\y -> (x,y)) ys
cProd (x:xs) ys = (cProd [x] ys) ++ (cProd xs ys)

cSquare :: [a] -> [(a,a)]
cSquare xs = cProd xs xs

cCube :: [a] -> [(a,a,a)]
cCube xs = [(a,b,c) | a <- xs, b <- xs, c <- xs]