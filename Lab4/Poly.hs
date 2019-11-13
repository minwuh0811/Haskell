-- | Abstract type of polynomials (over Int)
module Poly ( Poly     -- The type of polymomials
                       -- Instances: Eq, Show, Arbitrary, Num
            , evalPoly -- :: Int -> Poly -> Int
                       -- Convert to and from lists of Int
            , toList   -- :: Poly -> [Int]
            , fromList -- :: [Int] -> Poly 
            )
 where

import Test.QuickCheck

data Poly = Poly [Int] -- least significant powers first
  deriving Eq

instance Show Poly where
  show (Poly ns) =
      showParts . reverse . filter (\(n,_) -> n /= 0) $ zip ns powers
    where showParts []           = show 0
          showParts ((n,p):rest) = showNum n p ++ concatMap showRest rest

          showNum n ""   = show n
          showNum (-1) p = "-" ++ p -- only used for showing first one
          showNum 1 p    = p
          showNum n p    = show n ++ p

          showRest (n,p) | n < 0     = " - " ++ showNum (negate n) p
                         | otherwise = " + " ++ showNum n          p

          powers =
            prettyOnes ++ map (\n -> "x^" ++ show n ) [length prettyOnes ..]
              where
                prettyOnes = ["", "x"]
                          ++ ["x\178", "x\179"]                       
                          ++ map (\c -> 'x':c:[]) ['\8308'..'\8313']
  

instance Arbitrary Poly where
  arbitrary = do
            l <- choose (1,9) -- stick the nice looking powers
            ns <- vectorOf l arbitrary
            return $ fromList ns 

-- |Convert a polynomial to a list representation
-- @x² + 2x + 3@ would be represented by @[1,2,3]@
toList :: Poly -> [Int]
toList (Poly ns) = reverse ns

-- |Convert a list to a polynomial
fromList :: [Int] -> Poly
fromList = poly . reverse

poly :: [Int] -> Poly -- remove zeros from little-endian list
poly = Poly . reverse . strip . reverse

strip = dropWhile (== 0)

prop_Poly p = fromList (toList p) == p

instance Num Poly where
  (+)    = addPoly
  (*)    = mulPoly
  negate = fromList . map negate . toList
  abs    = undefined
  signum = undefined
  fromInteger n = fromList [fromInteger n]


-- |Evaluate a polynomial at the given point
evalPoly :: Int -> Poly -> Int
evalPoly x (Poly cs) = sum $ zipWith (*) cs powersOfx 
         where powersOfx = map (x^) [0..]          

-- | Addition for polynomials
addPoly (Poly p1) (Poly p2) =
   poly $ zipWith (+) (pad l1 p1) (pad l2 p2)
     where pad lp p = p ++ replicate (maxlen - lp) 0
           (l1,l2, maxlen) = (length p1, length p2, max l1 l2)

mulPoly (Poly p1) (Poly p2) = poly $ mul p1 p2 
  where
      mul [] p         = []
      mul (n:ns) p     =  (n `times` p) `plus` timesX (ns `mul` p) 
      timesX  p = 0:p
      times n p = map (n*) p
      plus p1 p2 = reverse . toList $ Poly p1 + Poly p2

-- Tests -----------------------------------------------------------------------
-- check that eval is a homomorphism (and maintains the invariant)
prop_PolyOps p1 p2 x = evalHom (*) mulPoly && evalHom (+) addPoly
   where evalHom f g = let p' = p1 `g` p2  in
                           evalPoly x p1 `f` evalPoly x p2 == evalPoly x p'
                           && prop_Poly p'
                       
ex1    = fromList [1,2,3]
x      = fromList [1,0]
xPlus1 = fromList [1,1]
-----
