module Ex01 where

-- needed to display the picture in the playground
import Codec.Picture

-- our line graphics programming interface
import ShapeGraphics

-- Part 1
-- picture of a house
housePic :: Picture
housePic = [door, house]
  where
    house :: PictureObject
    house = Path (map cosToPoint houseCOs) green Solid
    door :: PictureObject
    door  = Path (map cosToPoint doorCOs) red Solid
-- these are the coordinates - convert them to a list of Point
houseCOs :: [(Float, Float)]
houseCOs = [(300, 750), (300, 450), (270, 450), (500, 200),
         (730, 450), (700, 450), (700, 750)]

doorCOs :: [(Float, Float)]
doorCOs = [(420, 750), (420, 550), (580, 550), (580, 750)]

grey :: Colour
grey = Colour 255 255 255 128

chimneyHouse :: Picture
chimneyHouse = [newHouse, door, smoke]
  where
    newHouse :: PictureObject
    newHouse = Path (map cosToPoint newHouseCOs) green Solid
    door :: PictureObject
    door  = Path (map cosToPoint doorCOs) red Solid
    smoke :: PictureObject
    smoke = Path (map cosToPoint smokeCOs) grey Solid

newHouseCOs :: [(Float, Float)]
newHouseCOs = firstHalf ++ chimneyCOs ++ secondHalf
  where 
    firstHalf = take 4 houseCOs
    secondHalf = drop 4 houseCOs

chimneyCOs :: [(Float, Float)]
chimneyCOs = [(615, 325), (615, 250), (650, 250), (650, 363)]

smokeCOs :: [(Float, Float)]
smokeCOs = [(635, 240), (625, 230), (635, 220), (625, 210)]

cosToPoint :: (Float, Float) -> Point
cosToPoint (x, y) = Point x y

-- Part 2
movePoint :: Point -> Vector -> Point
movePoint (Point x y) (Vector xv yv)
  = Point (x + xv) (y + yv)

movePoint' :: Vector -> Point -> Point
movePoint' = flip movePoint

movePictureObject :: Vector -> PictureObject -> PictureObject
movePictureObject vec (Path points colour lineStyle) = Path (map (movePoint' vec) points) colour lineStyle
movePictureObject vec (Circle centerPO radiusPO colourPO lineStylePO fillStylePO) = Circle (movePoint' vec centerPO) radiusPO colourPO lineStylePO fillStylePO
movePictureObject vec (Ellipse centerPO widthPO heightPO rotationPO colourPO lineStylePO fillStylePO) = Ellipse (movePoint' vec centerPO) widthPO heightPO rotationPO colourPO lineStylePO fillStylePO
movePictureObject vec (Polygon pointsPO colourPO lineStylePO fillStylePO) = Polygon (map (movePoint' vec) pointsPO) colourPO lineStylePO fillStylePO


-- Part 3


-- generate the picture consisting of circles:
-- [Circle (Point 400 400) (400/n) col Solid SolidFill,
--  Circle (Point 400 400) 2 * (400/n) col Solid SolidFill,
--  ....
--  Circle (Point 400 400) 400 col Solid SolidFill]
simpleCirclePic :: Colour -> Float -> Picture
simpleCirclePic col n =  map (\n -> Circle centerPO n col Solid SolidFill) rads
  where 
    centerPO = Point 400 400
    rads = enumFromThenTo 1 (400 / n) 400

myRed :: Colour
myRed = Colour 255 0 0 80

-- use 'writeToFile' to write a picture to file "ex01.png" to test your
-- program if you are not using Haskell for Mac
-- e.g., call
-- writeToFile [house, door]

writeToFile pic
  = writePng "ex01.png" (drawPicture 3 pic)
