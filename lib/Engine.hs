{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Engine
where

import qualified Data.Map as Map
import Graphics.Gloss
import System.Random (StdGen, getStdGen, randomR)
import Graphics.Gloss.Interface.Pure.Game
import Data.List

type X = Int
type Y = Int
type Coord = (X, Y)
type Direction = Coord

data Game = BlockGame { startPos :: Coord,
                        player :: [Coord],
                        direction :: Direction,
                        targets :: [Coord],
                        bullets :: [Coord]}
            | Won
            | GameOver

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

displayMessage :: String -> Picture
displayMessage m = Scale 0.5 0.5 $ translate (adjustSize (-width)) 0 $ Text m

-- Geeft alle mogelijke coordinaten terug van het bord
getBoardCoordinates :: [Coord]
getBoardCoordinates = [(x, y) | x <- [left..right],
                                y <- [bottom..top]]

getters :: Map.Map String ([Int] -> Game -> Int)
getters = Map.fromList [("getWidth", getWidth), ("getHeight", getHeight), ("getPlayerX", getPlayerX),
                        ("getPlayerY", getPlayerY), ("getTargetsAmount", getTargetsAmount), ("targetAt", targetAt)]

getWidth :: [Int] -> Game -> Int
getWidth _ _ = width

getHeight :: [Int] -> Game -> Int
getHeight _ _ = height

getPlayerX :: [Int] -> Game -> Int
getPlayerX _ (BlockGame s p d t b) = fst $ head p

getPlayerY :: [Int] -> Game -> Int
getPlayerY _ (BlockGame s p d t b) = snd $ head p

getTargetsAmount :: [Int] -> Game -> Int
getTargetsAmount _ (BlockGame s p d t b) = length t

targetAt :: [Int] -> Game -> Int
targetAt xy (BlockGame s p d t b) | (x,y) `elem` t = 1
                                  | (x,y) `notElem` t = 0
                                    where x = head xy
                                          y = xy !! 1


builtInFunctions :: Map.Map String ([Int] -> Game -> Game)
builtInFunctions = Map.fromList [("setPlayerStart", setPlayerStart), ("updatePlayerPos", updatePlayerPos), ("addTarget", addTarget),
                                ("moveAllTargets", moveAllTargets), ("shoot", shoot), ("moveAllBullets", moveAllBullets),
                                ("removeOutOfBoundBullets", removeOutOfBoundBullets), ("removeOutOfBoundsTargets", removeOutOfBoundsTargets),
                                ("removeCollidingTargetsAndBullets", removeCollidingTargetsAndBullets), ("setWon", setWon),
                                ("setGameOver", setGameOver), ("addTargets", addTargets)]

setPlayerStart :: [Int] -> Game -> Game
setPlayerStart xy (BlockGame s p d t b) = let x = head xy
                                              y = xy !! 1
                                            in BlockGame (x, y) [(x,y)] d t b

updatePlayerPos :: [Int] -> Game -> Game
updatePlayerPos xydiff (BlockGame s p d t b) = let x = head xydiff
                                                   y = xydiff !! 1
                                                in BlockGame s [(fst co + x, snd co + y) | co <- p] d t b

addTarget :: [Int] -> Game -> Game
addTarget xy (BlockGame s p d t b) = let x = head xy
                                         y = xy !! 1
                                        in BlockGame s p d ((x,y):t) b

addTargets :: [Int] -> Game -> Game
addTargets xys (BlockGame s p d t b) = BlockGame s p d (intListToCoords xys ++ t) b

intListToCoords :: [Int] -> [Coord]
intListToCoords i = let half = length i `div` 2 in [(i !! x, i !! (x + half)) | x <- [0..half-1]]

moveAll :: [Int] -> [Coord] -> [Coord]
moveAll xydiff cos = let x = head xydiff
                         y = xydiff !! 1
                        in [(fst co + x, snd co + y) | co <- cos]

moveAllTargets :: [Int] -> Game -> Game
moveAllTargets xydiff (BlockGame s p d t b) = BlockGame s p d (moveAll xydiff t) b

moveAllBullets :: [Int] -> Game -> Game
moveAllBullets xydiff (BlockGame s p d t b) = BlockGame s p d t $ moveAll xydiff b

shoot :: [Int] -> Game -> Game
shoot xy (BlockGame s p d t b) = let x = head xy
                                     y = xy !! 1
                                    in BlockGame s p d t $ b ++ [(x,y)]

removeOutOfBounds :: [Int] -> [Coord] -> [Coord]
removeOutOfBounds xybound cos = let xbound = head xybound
                                    ybound = xybound !! 1
                                   in filter (isInBound xbound ybound) cos

removeOutOfBoundBullets :: [Int] -> Game -> Game
removeOutOfBoundBullets xybound (BlockGame s p d t b) = BlockGame s p d t $ removeOutOfBounds xybound b

removeOutOfBoundsTargets :: [Int] -> Game -> Game
removeOutOfBoundsTargets xybound (BlockGame s p d t b) = BlockGame s p d (removeOutOfBounds xybound t) b

isInBound :: Int -> Int -> Coord -> Bool
isInBound xbound ybound co = let cox = fst co
                                 coy = snd co
                                in cox >= -xbound && cox <= xbound && coy >= -ybound && coy <= ybound

collide :: [Coord] -> [Coord] -> ([Coord], [Coord])
collide c1 c2 =  let common = c1 `intersect` c2 in (c1 \\ common, c2 \\ common)

removeCollidingTargetsAndBullets :: [Int] -> Game -> Game
removeCollidingTargetsAndBullets _ (BlockGame s p d t b) = let (nt, nb) = collide t b
                                            in BlockGame s p d nt nb

setWon :: [Int] -> Game -> Game
setWon _ _ = Won

setGameOver :: [Int] -> Game -> Game
setGameOver _ _ = GameOver


                                