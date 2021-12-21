module Engine
where

import qualified Data.Map as Map
import Graphics.Gloss
import System.Random (StdGen, getStdGen, randomR)
import Graphics.Gloss.Interface.Pure.Game

type X = Int
type Y = Int
type Coord = (X, Y)
type Direction = Coord

data Game = BlockGame { startPos :: Coord,
                        player :: [Coord],
                        direction :: Direction,
                        targets :: [Coord]}

getRandomNumberInRange :: StdGen -> Int -> Int -> (Int, StdGen)
getRandomNumberInRange stdGen l u = randomR (l, u) stdGen

screenGreen, screenGray, screenBlue :: Color
screenGreen = makeColorI 0x6F 0x77 0x5F 0xFF
screenGray  = makeColorI 0x64 0x6F 0x5D 0XFF
screenBlue  = makeColorI 0x37 0xA3 0xBB 0xFF

width :: X
width   = 10 -- de breedte van het bord
height :: Y
height  = 20 -- de hoogte van het bord
dblock :: Int
dblock  = 12 -- de zijde van 1 blokje (inclusief marge rondom)
dwidth :: Float
dwidth  = 10 -- de zijde van 1 blokje (exclusief marge, inclusief randje)
dinner :: Float
dinner  = 7 -- de zijde van 1 blokje (enkel het zwarte stuk middenin)
fscale :: Float
fscale  = 3 -- algemene schaal van de hele tekening
north :: Direction
north = (0, 1)
east :: Direction
east = (1, 0)
south :: Direction
south = (0, -1)
west :: Direction
west = (-1, 0)
northEast :: Direction
northEast = (1, 1)
southEast :: Direction
southEast = (1, -1)
southWest :: Direction
southWest = (-1, -1)
northWest :: Direction
northWest = (-1, 1)

bottom, top :: Y
left, right :: X
bottom = -(height `div` 2)
top    = height `div` 2
left   = -(width `div` 2)
right  = width `div` 2

filled :: Picture
filled = Scale fscale fscale $ Pictures[rectangleWire dwidth dwidth,rectangleSolid dinner dinner]

-- Een lege pixel, gecentreerd rond de oorsprong
empty :: Picture
empty = Color screenGray filled

coloredTile :: Picture
coloredTile = Color screenBlue filled

-- Maakt een rij van lege pixels
makeRow :: Y -> Picture
makeRow y = pictures[drawEmpty (x, y) | x <- [left..right]]

-- Een bord met enkel lege pixels, gecentreerd rond de oorsprong
emptyBoard :: Picture
emptyBoard = Pictures[makeRow y | y <- [bottom..top]]

-- Past het coordinaat van een pixel aan om het juist te kunnen translaten
adjustSize :: Int -> Float
adjustSize v = fromIntegral (round fscale * dblock * v)

-- Een gevulde/actieve pixel op de locatie aangeduid door de coördinaat.
drawCoord :: Coord -> Picture
drawCoord c = drawPicture c filled

-- Een lege pixel op de locatie aangeduid door de coördinaat.
drawEmpty :: Coord -> Picture
drawEmpty c = drawPicture c empty

drawColored :: Coord -> Picture
drawColored c = drawPicture c coloredTile

-- Hulpfunctie die gebruikt wordt om zowel tegels als gevulde pixels mee te tekenen
drawPicture :: Coord -> Picture -> Picture
drawPicture c = translate (adjustSize $ fst c) (adjustSize $ snd c)

-- Geeft alle mogelijke coordinaten terug van het bord
getBoardCoordinates :: [Coord]
getBoardCoordinates = [(x, y) | x <- [left..right],
                                y <- [bottom..top]]

getters :: Map.Map String (Game -> Int)
getters = Map.fromList [("getWidth", getWidth), ("getHeight", getHeight), ("getPlayerX", getPlayerX), 
                        ("getPlayerY", getPlayerY)]

getWidth :: Game -> Int 
getWidth _ = width

getHeight :: Game -> Int 
getHeight _ = height

getPlayerX :: Game -> Int 
getPlayerX (BlockGame s p d t) = fst $ head p

getPlayerY :: Game -> Int 
getPlayerY (BlockGame s p d t) = snd $ head p

builtInFunctions :: Map.Map String ([Int] -> Game -> Game)
builtInFunctions = Map.fromList [("setPlayerStart", setPlayerStart), ("updatePlayerPos", updatePlayerPos), ("addTarget", addTarget), 
                                ("moveAllTargets", moveAllTargets)]

setPlayerStart :: [Int] -> Game -> Game
setPlayerStart xy (BlockGame s p d t) = let x = head xy
                                            y = xy !! 1
                                        in BlockGame (x, y) [(x,y)] d t

updatePlayerPos :: [Int] -> Game -> Game
updatePlayerPos xydiff (BlockGame s p d t) = let x = head xydiff
                                                 y = xydiff !! 1
                                             in BlockGame s [(fst co + x, snd co + y) | co <- p] d t

addTarget :: [Int] -> Game -> Game
addTarget xy (BlockGame s p d t) = let x = head xy
                                       y = xy !! 1
                                    in BlockGame s p d $ (x,y):t

moveAllTargets :: [Int] -> Game -> Game
moveAllTargets xydiff (BlockGame s p d t) = let x = head xydiff
                                                y = xydiff !! 1
                                             in BlockGame s p d [(fst co + x, snd co + y) | co <- t]