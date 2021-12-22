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

data Game = BlockGame { player :: [Coord],
                        direction :: Direction,
                        targets :: [Coord],
                        bullets :: [Coord],
                        random :: (Int, StdGen)}
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

-- Geeft een lijst terug met de mogelijke coordinaten waar een appel terecht kan komen
getPossibleAppleLocations :: [Coord] -> [Coord]
getPossibleAppleLocations p = getBoardCoordinates \\ p

-- Geeft de som van de elementen van 2 tuples terug
tuplesSum :: Coord -> Direction -> Coord
tuplesSum (x, y) (x', y') = (x + x', y + y')

-- Beweegt een object in de gegeven richting
moveObject :: [Coord] -> Direction -> [Coord]
moveObject o d = tuplesSum (head o) d : init o

getOppositeDirection :: Direction -> Direction
getOppositeDirection d
  | d == north         = south
  | d == south         = north
  | d == west          = east
  | d == east          = west
getOppositeDirection _ = (0,0)

getters :: Map.Map String ([Int] -> Game -> Int)
getters = Map.fromList [("getWidth", getWidth), ("getHeight", getHeight), ("getPlayerX", getPlayerX),
                        ("getPlayerY", getPlayerY), ("getTargetsAmount", getTargetsAmount), ("targetAt", targetAt),
                        ("getDirectionX", getDirectionX), ("getDirectionY", getDirectionY), ("inBound", inBound),
                        ("getUniquePlayerTiles", getUniquePlayerTiles)]

getWidth :: [Int] -> Game -> Int
getWidth _ _ = width

getHeight :: [Int] -> Game -> Int
getHeight _ _ = height

getPlayerX :: [Int] -> Game -> Int
getPlayerX _ (BlockGame p d t b r) = fst $ head p

getPlayerY :: [Int] -> Game -> Int
getPlayerY _ (BlockGame p d t b r) = snd $ head p

getTargetsAmount :: [Int] -> Game -> Int
getTargetsAmount _ (BlockGame p d t b r) = length t

getDirectionX :: [Int] -> Game -> Int
getDirectionX _ (BlockGame p d t b r) = fst d

getDirectionY :: [Int] -> Game -> Int
getDirectionY _ (BlockGame p d t b r) = snd d

inBound :: [Int] -> Game -> Int
inBound xy (BlockGame p d t b r) | isInBound xb yb (xp, yp) = 1
                                 | otherwise = 0
                                    where xp = head xy
                                          yp = xy !! 1
                                          xb = xy !! 2
                                          yb = xy !! 3
                                      
-- Geeft 1 terug wanneer er op het gegeven coordinaat een target is, anders geeft het 0 terug
targetAt :: [Int] -> Game -> Int
targetAt xy (BlockGame p d t b r) | (x,y) `elem` t = 1
                                  | otherwise = 0
                                    where x = head xy
                                          y = xy !! 1

getUniquePlayerTiles :: [Int] -> Game -> Int
getUniquePlayerTiles _ (BlockGame p d t b r) = length $ nub p


builtInFunctions :: Map.Map String ([Int] -> Game -> Game)
builtInFunctions = Map.fromList [("setPlayerStart", setPlayerStart), ("updatePlayerPos", updatePlayerPos), ("addTarget", addTarget),
                                ("moveAllTargets", moveAllTargets), ("shoot", shoot), ("moveAllBullets", moveAllBullets),
                                ("removeOutOfBoundBullets", removeOutOfBoundBullets), ("removeOutOfBoundsTargets", removeOutOfBoundsTargets),
                                ("removeCollidingTargetsAndBullets", removeCollidingTargetsAndBullets), ("setWon", setWon),
                                ("setGameOver", setGameOver), ("addTargets", addTargets), ("setDirection", setDirection),
                                ("movePlayer", movePlayer), ("addRandomTarget", addRandomTarget), ("lengthenPlayer", lengthenPlayer),
                                ("removeAllTargets", removeAllTargets)]

setPlayerStart :: [Int] -> Game -> Game
setPlayerStart xy (BlockGame p d t b r) = let x = head xy
                                              y = xy !! 1
                                          in BlockGame [(x,y)] d t b r

updatePlayerPos :: [Int] -> Game -> Game
updatePlayerPos xydiff (BlockGame p d t b r) = let x = head xydiff
                                                   y = xydiff !! 1
                                                in BlockGame [(fst co + x, snd co + y) | co <- p] d t b r

addTarget :: [Int] -> Game -> Game
addTarget xy (BlockGame p d t b r) = let x = head xy
                                         y = xy !! 1
                                        in BlockGame p d ((x,y):t) b r

addTargets :: [Int] -> Game -> Game
addTargets xys (BlockGame p d t b r) = BlockGame p d (intListToCoords xys ++ t) b r

intListToCoords :: [Int] -> [Coord]
intListToCoords i = let half = length i `div` 2 in [(i !! x, i !! (x + half)) | x <- [0..half-1]]

moveAll :: [Int] -> [Coord] -> [Coord]
moveAll xydiff cos = let x = head xydiff
                         y = xydiff !! 1
                        in [(fst co + x, snd co + y) | co <- cos]

moveAllTargets :: [Int] -> Game -> Game
moveAllTargets xydiff (BlockGame p d t b r) = BlockGame p d (moveAll xydiff t) b r

moveAllBullets :: [Int] -> Game -> Game
moveAllBullets xydiff (BlockGame p d t b r) = BlockGame p d t (moveAll xydiff b) r

shoot :: [Int] -> Game -> Game
shoot xy (BlockGame p d t b r) = let x = head xy
                                     y = xy !! 1
                                    in BlockGame p d t (b ++ [(x,y)]) r

removeOutOfBounds :: [Int] -> [Coord] -> [Coord]
removeOutOfBounds xybound cos = let xbound = head xybound
                                    ybound = xybound !! 1
                                   in filter (isInBound xbound ybound) cos

removeOutOfBoundBullets :: [Int] -> Game -> Game
removeOutOfBoundBullets xybound (BlockGame p d t b r) = BlockGame p d t (removeOutOfBounds xybound b) r

removeOutOfBoundsTargets :: [Int] -> Game -> Game
removeOutOfBoundsTargets xybound (BlockGame p d t b r) = BlockGame p d (removeOutOfBounds xybound t) b r

isInBound :: Int -> Int -> Coord -> Bool
isInBound xbound ybound co = let cox = fst co
                                 coy = snd co
                                in cox >= -xbound && cox <= xbound && coy >= -ybound && coy <= ybound

collide :: [Coord] -> [Coord] -> ([Coord], [Coord])
collide c1 c2 =  let common = c1 `intersect` c2 in (c1 \\ common, c2 \\ common)

removeCollidingTargetsAndBullets :: [Int] -> Game -> Game
removeCollidingTargetsAndBullets _ (BlockGame p d t b r) = let (nt, nb) = collide t b
                                                            in BlockGame p d nt nb r

setWon :: [Int] -> Game -> Game
setWon _ _ = Won

setGameOver :: [Int] -> Game -> Game
setGameOver _ _ = GameOver

setDirection :: [Int] -> Game -> Game
setDirection xy (BlockGame p d t b r) = let x = head xy
                                            y = xy !! 1
                                      in BlockGame p (x,y) t b r

movePlayer :: [Int] -> Game -> Game
movePlayer _ (BlockGame p d t b r) = BlockGame (moveObject p d) d t b r

addRandomTarget :: [Int] -> Game -> Game
addRandomTarget _ (BlockGame p d t b r) = BlockGame p d (t':t) b r'
                                            where 
                                              possible = getPossibleAppleLocations p
                                              r'       = getRandomNumberInRange (snd r) 0 $ length possible
                                              t'       = possible !! fst r'

removeAllTargets :: [Int] -> Game -> Game
removeAllTargets _ (BlockGame p d t b r) = BlockGame p d [] b r

lengthenPlayer :: [Int] -> Game -> Game
lengthenPlayer _ (BlockGame p d t b r) = BlockGame (lengthenPlayer' d p) d t b r

lengthenPlayer' :: Direction -> [Coord] -> [Coord]
lengthenPlayer' d p
  | tuplesSum l south `elem` p = p ++ [tuplesSum l north]
  | tuplesSum l north `elem` p = p ++ [tuplesSum l south]
  | tuplesSum l west `elem` p  = p ++ [tuplesSum l east]
  | tuplesSum l east `elem` p  = p ++ [tuplesSum l west]
  | length p == 1              = p ++ [tuplesSum l $ getOppositeDirection d]
    where l = last p
lengthenPlayer' d p            = p