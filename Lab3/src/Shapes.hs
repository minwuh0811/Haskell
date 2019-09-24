-- | Types and functions for shapes. The list of all tetris pieces.
module Shapes where
import Data.List(transpose)
import Data.Maybe(isNothing)
import Test.QuickCheck

-- * Shapes

type Square = Maybe Colour

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
              deriving (Eq,Bounded,Enum,Show)

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.

data Shape = S [Row] deriving (Eq)
type Row = [Square]

rows :: Shape -> [Row]
rows (S rs) = rs

-- * Showing shapes

showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
  where
    showRow :: Row -> String
    showRow r = [showSquare s | s <- r]
    
    showSquare Nothing = '.'
    showSquare (Just Black) = '#' -- can change to '█' on linux/mac
    showSquare (Just Grey)  = 'g' -- can change to '▓'
    showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss)++r


-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of connected 4 blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [S (makeSquares s) | s <- shapes] 
   where
      makeSquares = map (map colour)
      colour c    = lookup c [('I',Red),('J',Grey),('T',Blue),('O',Yellow),
                              ('Z',Cyan),('L',Green),('S',Purple)]
      shapes = 
              [["I",
               "I",
               "I",
               "I"],
              [" J",
               " J",
               "JJ"],
              [" T",
               "TT",
               " T"],
              ["OO",
               "OO"],
              [" Z",
               "ZZ",
               "Z "],
              ["LL",
               " L",
               " L"],
              ["S ",
               "SS",
               " S"]]

-- * Some simple functions

-- ** A01
emptyRow::Int -> Row
emptyRow n = replicate n Nothing
emptyShape :: (Int,Int) -> Shape
emptyShape (n,k)=S (replicate k (emptyRow n))
-- ** A02

-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int,Int)
shapeSize (S (r:rowlist)) =(length(r), length (r:rowlist))
--Test
--shapeSize (emptyShape (4,2)) = (4,2)

-- ** A03

-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount (S rowlist)= sum [1| r <-(concat rowlist), r /= Nothing]

--Test 
--blockCount (emptyShape (4,2)) = 0
    
-- * The Shape invariant

-- ** A04
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)

prop_Shape :: Shape -> Bool
prop_Shape (S (r:rowlist))  = (length(r)>0) && (lengthCount (r:rowlist))
        where lengthCount (r:[a]) = length(r)==length(a)
              lengthCount (r:rowlist) = length(r)==length(rowlist!!0)&&lengthCount rowlist
        
-- * Test data generators

-- ** A05
-- | A random generator for colours
rColour :: Gen Colour
rColour = elements [Black,Red,Green,Yellow,Blue,Purple,Cyan,Grey]

instance Arbitrary Colour where
  arbitrary = rColour

-- ** A06
-- | A random generator for shapes
rShape :: Gen Shape
rShape = elements allShapes


instance Arbitrary Shape where
  arbitrary = rShape

-- * Transforming shapes

-- ** A07
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape (S rowlist)= S (reverse (transpose rowlist))

-- ** A08
-- | shiftShape adds empty squares above and to the left of the shape
shiftHoriz::Int -> [Row] -> [Row]
shiftHoriz n (r:rowlist) = (rows (emptyShape ((length(r)),n))) ++ (r:rowlist) 
shiftVert::Int -> [Row] -> [Row]
shiftVert n rowlist = [(replicate n Nothing) ++ r|r<-rowlist]
shiftShape :: (Int,Int) -> Shape -> Shape
shiftShape (n,k) (S rowlist)=S (shiftVert n (shiftHoriz k rowlist))

-- ** A09
-- | padShape adds empty sqaure below and to the right of the shape
shiftHoriz'::Int -> [Row] -> [Row]
shiftHoriz' n (r:rowlist) =(r:rowlist) ++ (rows (emptyShape ((length(r)),n)))
shiftVert'::Int -> [Row] -> [Row]
shiftVert' n rowlist = [r++(replicate n Nothing)|r<-rowlist]
padShape :: (Int,Int) -> Shape -> Shape
padShape (n,k) (S rowlist) = S (shiftVert' n (shiftHoriz' k rowlist))

-- ** A10
-- | pad a shape to a given size
padShapeTo :: (Int,Int) -> Shape -> Shape
padShapeTo (n,k) shape = padShape ((n-a),(k-b)) shape 
  where (a,b)=shapeSize shape

-- * Comparing and combining shapes

-- ** B01

-- | Test if two shapes overlap
rowsOverlap :: Row -> Row -> Bool
rowsOverlap [] _ = False
rowsOverlap _ [] = False 
rowsOverlap (r1:squarelist1) (r2:squarelist2) = r1==r2 || rowsOverlap squarelist1 squarelist2
--Test 
--rowsOverlap ((rows (shiftShape (1,2) (allShapes!!1)))!!1) ((rows (padShape (1,2) (allShapes!!1)))!!1) -> true (passed)
overlaps :: Shape -> Shape -> Bool
overlaps (S []) _ = False
overlaps  _ (S []) = False
overlaps (S (r1:rowlist1)) (S (r2:rowlist2)) = rowsOverlap r1 r2 ||  overlaps (S (rowlist1)) (S (rowlist2)) 

-- ** B02
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square->Square->Square) -> Shape -> Shape -> Shape
zipShapeWith clash (S [x1]) (S (x2:s2)) = S [zipWith clash x1 x2]
zipShapeWith clash (S (x1:s1)) (S [x2]) = S [zipWith clash x1 x2]
zipShapeWith clash (S (x1:s1)) (S (x2:s2)) = S ( [zipWith clash x1 x2] ++ (rows (zipShapeWith clash (S s1) (S s2))))
blackClashes :: Shape -> Shape -> Shape
blackClashes s1 s2 = zipShapeWith clash s1 s2  
  where clash :: Square -> Square -> Square 
        clash Nothing Nothing = Nothing
        clash Nothing s       = s
        clash s       Nothing = s
        clash (Just c1) (Just c2) = Just Black

-- ** B03
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.                     
finalShape :: Shape -> Shape -> Shape
finalShape s1 s2 | h1>=h2 = padShapeTo (v1+v2,h1) s1
                 | otherwise = padShapeTo (v1+v2,h2) s1
                        where (v1,h1)=shapeSize s1
                              (v2,h2)=shapeSize s2
combine :: Shape -> Shape -> Shape
combine s1 s2  = zipShapeWith clash newShape1 newShape2
  where clash::Square -> Square -> Square
        clash Nothing Nothing = Nothing
        clash Nothing s = s
        clash s Nothing = s     
        (newShape1, newShape2) = finalShape s1 s2 
          where finalShape :: Shape -> Shape -> (Shape,Shape)
                finalShape s1 s2 | h1>=h2 = ((shiftShape (v2,0) s1), (padShapeTo (v1+v2,h1) s2))
                                 | otherwise = ((shiftShape (v2,(h2-h1)) s1), (padShapeTo (v1+v2,h2) s2))
                                      where (v1,h1)=shapeSize s1
                                            (v2,h2)=shapeSize s2
                     
                                         
        
        
