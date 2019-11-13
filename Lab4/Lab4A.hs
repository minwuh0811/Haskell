--Authors:Min Wu, Noor Alweli, Andrea Bertl
--Date:2019/10/21
--Lab group:8
 
import Poly
import Test.QuickCheck


-- Use the following simple data type for binary operators
data BinOp = AddOp | MulOp

--test     
ex5 = (Op MulOp (Op AddOp (Var 0) (Num 2)) (Var 3))

--------------------------------------------------------------------------------
-- * A1
-- Define a data type Expr which represents three kinds of expression:
-- binary operators (use BinOp as a helper type) applied to two expressions,
-- numbers (use Int), and exponentiation x^n.
-- Note that since we consider expressions containing just a single variable,
-- x, your data type should not use String or Char anywhere, since this is
-- not needed.

data Expr = Num Int | Op BinOp Expr Expr | Var Int



--------------------------------------------------------------------------------
-- * A2
-- Define the data type invariant that checks that exponents are never negative
prop_Expr :: Expr -> Bool
prop_Expr (Var n) = n > 0
prop_Expr _       = True 



--------------------------------------------------------------------------------
-- * A3
-- Make Expr an instance of Show (along the lines of the example in the lecture)
-- You can use Haskell notation for powers: x^2
-- You should show x^1 as just x. 


showExpr :: Expr -> String
showExpr (Num n)       = show n
showExpr (Var 0)       = show 1
showExpr (Var 1)       = "x"
showExpr (Var n)       = "x^" ++ show n
showExpr (Op AddOp e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Op MulOp e1 e2) = showFac e1 ++ " * " ++ showFac e2
         where showFac :: Expr -> String
               showFac e@(Op AddOp _ _) = "(" ++ showExpr e ++ ")"
               showFac e             = showExpr e


instance Show Expr where
  show = showExpr

--------------------------------------------------------------------------------
-- * A4
-- Make Expr and instance of Arbitrary.
-- Now you can check the data type invariant that you defined in A3 using
-- quickCheck

rNum = do n <-choose(1,9) 
          return (Num n)

rVar = do n <- choose (1,9)
          return (Var n)

rExpr :: Int -> Gen Expr
rExpr 0 = oneof [rNum,rVar]
rExpr n = do op <- elements [AddOp, MulOp]
             l <- choose (0,n-1)
             let r = n-1 - l
             e1 <- rExpr l
             e2 <- rExpr r
             return (Op op  e1 e2)
             

instance Arbitrary Expr where
    arbitrary = do n <- choose (1,3)
                   rExpr n


prop_rExpr :: Expr -> Bool
prop_rExpr (Var n) = n > 0 
prop_rExpr (Num n) = True
prop_rExpr (Op op a b) = (prop_Expr a && prop_Expr b)

--------------------------------------------------------------------------------
-- * A5
-- Define the eval function which takes a value for x and an expression and
-- evaluates it

eval :: Int -> Expr -> Int
eval _ (Num n) = n
eval value (Var m) = value^m
eval value (Op AddOp e1 e2) = eval value e1 + eval value e2
eval value (Op MulOp e1 e2) = eval value e1 * eval value e2



--------------------------------------------------------------------------------
-- * A6
-- Define
-- Which converts an expression into a polynomial.
-- Here it is important to think recursively to just solve the bigger problem
-- by solving the smaller problems and combining them in the right way. 

exprToPoly :: Expr -> Poly
exprToPoly (Num n)          = fromList [n]
exprToPoly (Var 0)          = fromList [1]
exprToPoly (Var n)          = fromList (1 : replicate (n) 0)
exprToPoly (Op AddOp e1 e2) = (exprToPoly e1) + (exprToPoly e2)
exprToPoly (Op MulOp e1 e2) = (exprToPoly e1) * (exprToPoly e2)

-- Define (and check) prop_exprToPoly, which checks that evaluating the
-- polynomial you get from exprToPoly gives the same answer as evaluating
-- the expression
prop_exprToPoly:: Expr -> Int -> Bool
prop_exprToPoly expr n = evalPoly n (exprToPoly expr) == eval n expr
--------------------------------------------------------------------------------
-- * A7
-- Now define the function going in the other direction, 
listToExpr :: [Int] -> Expr
listToExpr [] = Num 0
listToExpr [a] = Num a
listToExpr (r:list)= Op AddOp (Op MulOp (Num r) (Var (length(list)))) (listToExpr list)

polyToExpr :: Poly -> Expr
polyToExpr poly= listToExpr (toList poly) 
-- Write (and check) a quickCheck property for this function similar to
-- question 6. 
prop_polyToExpr:: Poly -> Int -> Bool
prop_polyToExpr poly n = evalPoly n poly == eval n (polyToExpr poly)
--------------------------------------------------------------------------------
-- * A8
-- Write a function
-- which simplifies an expression by converting it to a polynomial
-- and back again
simplify :: Expr -> Expr
simplify expr = polyToExpr (exprToPoly expr)


--------------------------------------------------------------------------------
-- * A9
-- Write a quickCheck property
--that checks that a simplified expression does not contain any "junk":
--where junk is defined to be multiplication by one or zero,
--addition of zero, addition or multiplication of numbers, or x to the
--power zero. (You may need to fix A7)
prop_noJunk :: Expr -> Bool
prop_noJunk e = ((prop_muls (simplify e)) && (prop_add (simplify e)) && (prop_Vars(simplify e)))
               where
                prop_muls :: Expr -> Bool
                prop_muls (Op MulOp _ (Num 0))       = False
                prop_muls (Op MulOp (Num 0)_)        = False
                prop_muls (Op MulOp _ (Num 1))       = False
                prop_muls (Op MulOp (Num 1)_)        = False
                prop_muls (Op MulOp (Num _) (Num _)) = False
                prop_muls (Op MulOp e1 e2 )          = True
                prop_muls _                          =True

                prop_add :: Expr -> Bool
                prop_add (Op AddOp (Num 0)_)         = False
                prop_add (Op AddOp _ (Num 0))        = False
                prop_add (Op AddOp (Num _)(Num _))   = False
                prop_add (Op AddOp e1 e2)            = True
                prop_add _                           = True

                prop_Vars :: Expr -> Bool
                prop_Vars (Var 0)                    = False
                prop_Vars (Var _)                    = True
                prop_Vars _                          = True

----------------------------------------------------------------------------
