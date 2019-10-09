-- | The Tetris game (main module)
module Main where
import ConsoleGUI       -- cabal install ansi-terminal 
--import CodeWorldGUI     -- cabal install codeworld-api
import Shapes

--------------------------------------------------------------------------------
-- * The code that puts all the piece together

main = runGame tetrisGame

tetrisGame = Game { startGame     = startTetris,
                    stepGame      = stepTetris,
                    drawGame      = drawTetris,
                    gameInfo      = defaultGameInfo prop_Tetris,
                    tickDelay     = defaultDelay,
                    gameInvariant = prop_Tetris }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation


-- | The state of the game
data Tetris = Tetris (Vector,Shape) Shape [Shape]
-- The state consists of three parts:
--   * The position and shape of the falling piece
--   * The well (the playing field), where the falling pieces pile up
--   * An infinite supply of random shapes

-- ** Positions and sizes

type Vector = (Int,Int)

-- | The size of the well
wellSize :: (Int,Int)
wellSize = (wellWidth,wellHeight)
wellWidth = 10
wellHeight = 20

-- | Starting position for falling pieces
startPosition :: Vector
startPosition = (wellWidth `div` 2 - 1, 0)

-- | Vector addition
vAdd :: Vector -> Vector -> Vector
(x1,y1) `vAdd` (x2,y2) = (x1+x2,y1+y2)

-- | Move the falling piece into position
place :: (Vector,Shape) -> Shape
place (v,s) = shiftShape v s


-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris (_, shape) well _) = (prop_Shape shape) && (wellSize==shapeSize well)

-- test case for testing Tetris
testTetris = Tetris ((0, 0), (allShapes !! 5)) (emptyShape (10,20)) (allShapes)

-- | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls s = S ([replicate (shapeWidth+2) (Just Black)] ++ addBlack s ++ [replicate (shapeWidth+2) (Just Black)])
  where (shapeWidth,shapeHeight)=shapeSize s
        addBlack:: Shape -> [Row]
        addBlack (S []) = []
        addBlack (S (r:rowlist)) = [[(Just Black)]++r++[(Just Black)]] ++ addBlack (S rowlist)

-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (v,p) w _) = addWalls (combine  (shiftShape v p)  w )

--a function to move the falling piece
move :: Vector -> Tetris -> Tetris
move  v1 (Tetris (v,p) w s) = (Tetris (v2,p) w s)
   where v2=(vAdd v1 v)

--Define a function tick that can be called from stepTetris to handle the Tick action.
tick :: Tetris -> Maybe (Int,Tetris)
tick t 
  | collision (move (0,1) t) = dropNewPiece t
  | otherwise = Just (0, (move(0,1) t))


-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris (rs) = Tetris (startPosition,shape1) (emptyShape wellSize) supply
  where 
    shape1:supply = [allShapes!!(doubleToInt r)|r<-rs]
      where doubleToInt :: Double -> Int
            doubleToInt r = floor(r*(fromIntegral(length allShapes))) 


-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int,Tetris)
stepTetris Tick t = tick t 
stepTetris MoveDown t = tick t
stepTetris MoveLeft t = Just (0, (movePiece (-1) t))
stepTetris MoveRight t = Just (0, (movePiece (1) t))
stepTetris Rotate t = Just (0,rotatePiece t)

--a function to test if the falling piece has collided with the walls or something in the well
collision :: Tetris -> Bool
collision  (Tetris ((width,height),s) w shapes)
  |  width <0 =True
  | width+shapeWidth > wellWidth = True
  | height+shapeHeight > wellHeight  = True
  | overlaps (place ((width,height), s)) w = True
  | otherwise =False
      where (shapeWidth,shapeHeight) = shapeSize s
            (wellWidth,wellHeight) = shapeSize w
--a function that can be called from stepTetris to handle the MoveLeft and MoveRight actions
movePiece :: Int -> Tetris -> Tetris       
movePiece n t
  |  collision (move (n,0) t) = t
  | otherwise = move(n,0) t 

--a function that rotates the falling piece
rotate :: Tetris -> Tetris
rotate (Tetris (v,s) w slist) = (Tetris (v, rotateShape(s)) w slist)

-- a function that can be called from stepTetris to handle the Rotate action
rotatePiece :: Tetris -> Tetris
rotatePiece t
  | collision (rotate t) = t
  | otherwise = rotate t

--a function that handles the case when the falling piece can’t move any further down
dropNewPiece :: Tetris -> Maybe (Int,Tetris)  
dropNewPiece (Tetris (v,p) w (s:shapes))
  | collision (Tetris (startPosition,s) newWell shapes) = Just(0,  (Tetris (v,p) w (s:shapes)) )
  | otherwise =Just (n,(Tetris (startPosition,s) newWell shapes))
    where (n,newWell)=clearLines (combine (place (v,p)) w)

-- a function that removes completed lines from the well
clearLines :: Shape -> (Int,Shape)  
clearLines s = (n, s2)
    where 
      n = sum [1| r<-(rows s), isComplete r] 
      s2 =S ((rows (emptyShape (wellWidth, n))) ++ [r| r<-(rows s), not (isComplete r)] )
        
isComplete :: Row -> Bool
isComplete (r:row) = r/=Nothing && isComplete row