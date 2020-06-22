module Art where  

import ShapeGraphics
import Codec.Picture

art :: Picture
art = tree 8 10 (-1) 0.8 (Point 400 800) (Vector 0 (-100)) red 2 green

tree :: Int -> Float -> Float -> Float -> Point -> Vector 
        -> Colour -> Float -> Colour -> Picture
tree depth -- depth of the tree
     density  -- density of the tree, the larger the denser
     lean -- leaning style, negative for left, positive for right
     growth -- length relation between father and child, 0.8 means child is 0.8 length of father
     base direction colour
     leafRadius -- radius of leaves on the edges of the tree, 0 means no leaf
     leafColour -- colour of the leaves
  | depth == 0 = drawLine line ++ leaves
  | otherwise
    =  drawLine line
    ++ tree (depth - 1) density lean growth nextBase left 
       nextColour leafRadius leafColour -- left tree
    ++ tree (depth - 1) density lean growth nextBase right 
       nextColour leafRadius leafColour -- right tree
  where
    drawLine :: Line -> Picture
    drawLine (Line start end) =
      [ Path [start, end] colour Solid ]

    line = Line base nextBase
    nextBase = offset direction base

    left = rotate ((-pi + lean) / density) $ scale growth $ direction
    right = rotate ((pi + lean) / density) $ scale growth $ direction

    nextColour =
      colour { redC = (redC colour) - 24, blueC = (blueC colour) + 24 }

    leaves = [Circle nextBase leafRadius leafColour Solid SolidFill]

-- Offset a point by a vector
offset :: Vector -> Point -> Point
offset (Vector vx vy) (Point px py)
  = Point (px + vx) (py + vy)

-- Scale a vector
scale :: Float -> Vector -> Vector
scale factor (Vector x y) = Vector (factor * x) (factor * y)

-- Rotate a vector (in radians)
rotate :: Float -> Vector -> Vector
rotate alpha (Vector vx vy)
  = Vector (cos alpha * vx - sin alpha * vy)
           (sin alpha * vx + cos alpha * vy)

-- use 'writeToFile' to write a picture to file "ex01.png" to test your
-- program if you are not using Haskell for Mac
-- e.g., call
-- writeToFile [house, door]

writeToFile pic
  = writePng "art.png" (drawPicture 3 art)
