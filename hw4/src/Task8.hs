module Task8
  ( Config (..) 
  , newSimulationGrid
  , evolve
  , printGrid
  , simulateCOVID
  ) where

import Control.Comonad     ( Comonad (..))
import System.Random       ( StdGen
                           , mkStdGen
                           , split
                           , random)
import Data.List           ( intercalate)
import System.Console.ANSI ( clearScreen)
import Control.Concurrent  ( threadDelay)
import Control.Monad       ( foldM)

data ListZipper a = LZ [a] a [a]

listLeft :: ListZipper a -> ListZipper a
listLeft (LZ (a:as) x bs) = LZ as a (x:bs)
listLeft _                = error "listLeft"

listRight :: ListZipper a -> ListZipper a
listRight (LZ as x (b:bs)) = LZ (x:as) b bs
listRight _                = error "listRight"

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

genericMove :: (a -> a) -> (a -> a) -> a -> ListZipper (a)
genericMove f g e = LZ (iterateTail f e) e (iterateTail g e)

instance Functor ListZipper where
  fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

instance Comonad ListZipper where
  extract (LZ _ x _) = x
  duplicate = genericMove listLeft listRight

newtype Grid a
  = Grid
  { unGrid :: ListZipper (ListZipper a)
  }

instance Functor Grid where
  fmap f (Grid g) = Grid $ fmap f <$> g

up :: Grid a -> Grid a
up (Grid g) = Grid (listLeft g)

down :: Grid a -> Grid a
down (Grid g) = Grid (listRight g)

left :: Grid a -> Grid a
left (Grid g) = Grid (fmap listLeft g)

right :: Grid a -> Grid a
right (Grid g) = Grid (fmap listRight g)

gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite (listWrite x $ extract g) g

horizontal :: Grid a -> ListZipper (Grid a)
horizontal = genericMove left right

vertical :: Grid a -> ListZipper (Grid a)
vertical = genericMove up down

instance Comonad Grid where
  extract = gridRead
  duplicate = Grid . fmap horizontal . vertical

data Status
  = Receptive
  | Incubation
  | Infected
  | Recovered
  deriving (Eq, Enum)

nextStatus :: Status -> Status
nextStatus s 
  | s == Recovered = Receptive 
  | otherwise      = succ s

data Cell
  = Cell
  { status   :: Status
  , gen      :: StdGen
  , leftDays :: Int
  }

instance Show Cell where
  show (Cell Receptive _ _)  = "." 
  show (Cell Incubation _ _) = "*"
  show (Cell Infected _ _)   = "#"
  show (Cell Recovered _ _)  = "@"

data Config
  = Config
  { probabilty     :: Double
  , incubationDays :: Int
  , infectedDays   :: Int
  , recoveredDays  :: Int
  }

getStatusDays :: Config -> Status -> Int
getStatusDays c s 
  | s == Incubation = incubationDays c
  | s == Infected   = infectedDays   c
  | s == Recovered  = recoveredDays  c
  | otherwise       = 0

generateTwoCells :: Cell -> (Cell, Cell)
generateTwoCells c = 
  let (g1, g2) = split $ gen c
      st       = status c
  in ( Cell st g1 0
     , Cell st g2 0)

newSimulationList :: StdGen -> ListZipper Cell
newSimulationList g = 
  genericMove 
    (fst . generateTwoCells) 
    (snd . generateTwoCells) 
    $ Cell Receptive g 0

generateTwoLists :: ListZipper Cell -> (ListZipper Cell, ListZipper Cell)
generateTwoLists lz = 
  ( fmap (fst . generateTwoCells) lz
  , fmap (snd . generateTwoCells) lz)

newSimulationGrid :: Config -> StdGen -> Grid Cell
newSimulationGrid config g =
  let (g1, g2) = split g
   in gridWrite
        (Cell Incubation g2 $ incubationDays config)
        ( Grid $
            genericMove
              (fst . generateTwoLists)
              (snd . generateTwoLists)
              $ newSimulationList g1
        )

infectedCount :: [Cell] -> Int
infectedCount = length . filter (\c -> status c == Incubation || status c == Infected)

neighbours :: [Grid a -> Grid a]
neighbours = [left, right, up, down]

infectedNeighbours :: Grid Cell -> Int
infectedNeighbours g = infectedCount $ 
  map (\direction -> extract $ direction g) neighbours

updateReceptive :: Config -> Grid Cell -> Cell
updateReceptive config grid = 
  let g = gen $ extract grid
      (willInfected, newG) = 
        foldl 
          ( \(will, curG) _ ->
            let rnd = random curG :: (Double, StdGen) 
            in if will == False && (probabilty config) > fst rnd
              then (True, snd rnd) 
              else (will, snd rnd)
          ) 
          (False, g) 
          [1 .. infectedNeighbours grid]
  in if willInfected
    then Cell Incubation newG (incubationDays config)
    else Cell Receptive  newG 0

simpleUpdate :: Config -> Cell -> Cell
simpleUpdate config cell = 
  let s = status cell
      g = gen cell
      d = leftDays cell
  in if d > 1
    then Cell s g (d - 1)
    else Cell (nextStatus s) g $ getStatusDays config (nextStatus s) 

rule :: Config -> Grid Cell -> Cell
rule config grid = 
  let cell = extract grid
  in case cell of
    (Cell Receptive _ _) -> updateReceptive config grid
    _                    -> simpleUpdate config cell

evolve :: Config -> Grid Cell -> Grid Cell
evolve config = extend $ rule config

toMatrix :: Grid a -> Int -> [[a]]
toMatrix (Grid (LZ ls x rs)) n =
  reverse (take n $ map (\lz -> toList lz n) ls)
    ++ [toList x n]
    ++ (take n $ map (\lz -> toList lz n) rs)

printGrid :: Show a => Grid a -> Int -> IO ()
printGrid grid n = do
  putStrLn ""
  putStrLn $ intercalate "\n" $ map (unwords . map show) $ toMatrix grid n
  putStrLn ""

gridFrame :: Show a => Grid a -> Int -> IO ()
gridFrame grid n = do
  threadDelay 750000
  clearScreen
  printGrid grid n

simulateCOVID :: Config -> IO ()
simulateCOVID config = do
  let size = 26
  let rng  = mkStdGen 713
  let grid = newSimulationGrid config rng
  result <- 
    foldM 
      ( \curGrid _ -> do 
        gridFrame curGrid size
        return $ evolve config curGrid
      ) 
      grid 
      (repeat True)
  gridFrame result size
