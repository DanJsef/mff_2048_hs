import           Control.Monad
import           Data.List
import           System.IO
import           System.Info
import           System.Process
import           System.Random
import           Text.Printf

type Row = [Int]
type Board = [Row]
type Coord = (Int,Int)
type Coords = [Coord]

initBoard :: [Int] -> Board
initBoard [s1,s2,s3,s4] = addTile [s1,s2] $ addTile [s3,s4] $ replicate 4 [0,0,0,0]

getSeeds :: Int -> IO [Int]
getSeeds n = replicateM n randomIO

replace :: Int -> a -> [a] -> [a]
replace i val list = take i list ++ val : drop (i+1) list

setTile :: Coord -> Int -> Board ->  Board
setTile (rowIdx,colIdx) val board = replace rowIdx newRow board
  where newRow = replace colIdx val (board!!rowIdx)

freeTiles :: Board -> Coords
freeTiles board = filter (\(row,col) -> (board!!row)!!col == 0) coords
  where coords = [(x, y)| x<- [0..3], y<-[0..3]]


combine :: Row -> Row
combine (x:y:xs)
  | x == y = x * 2 : combine xs
  | otherwise = x  : combine (y:xs)
combine x = x

merge :: Row -> Row
merge row = merged ++ padding
  where padding = replicate (length row - length merged) 0
        merged  = combine $ filter (/= 0) row


move :: Char -> Board -> Board
move 'a' = map merge
move 'd' = map (reverse . merge . reverse)
move 'w' = transpose . move 'a'  . transpose
move 's' = transpose . move 'd' . transpose

isMovable :: Board -> Bool
isMovable board = sum freeCount > 0
  where freeCount = map (\d -> length $ freeTiles $ move d board) directions
        directions = ['a', 'd', 'w', 's']

printColor :: Int -> String
printColor i = "\x1b[" ++ color ++ "\x1b[0m|"
  where color =
          case i of
              0    -> printf "0m%4d" i
              2    -> printf "32m%4d" i
              4    -> printf "31m%4d" i
              8    -> printf "34m%4d" i
              16   -> printf "35m%4d" i
              32   -> printf "36m%4d" i
              64   -> printf "31m%4d" i
              128  -> printf "34m%4d" i
              256  -> printf "35m%4d" i
              512  -> printf "36m%4d" i
              1024 -> printf "33m%4d" i
              2048 -> printf "33m%4d" i

showRow :: [Int] -> String
showRow a = concatMap printColor a ++ printf "\n--------------------"

printBoard :: Board -> IO ()
printBoard board = do
  if System.Info.os == "mingw32" then system "cls" else system "clear"
  mapM_ (putStrLn . showRow) board

addTile :: [Int] -> Board -> Board
addTile [s1,s2] board = new_grid
  where freeList = freeTiles board
        free = choose s1 freeList
        val = choose s2 [2,2,2,2,2,2,2,4,4,4]
        new_grid = setTile free val board

choose :: Int -> [a] -> a
choose seed xs = xs !! i
  where (i, _) = randomR (0, length xs-1) $ mkStdGen seed

checkWin :: Int -> Board -> Bool
checkWin diff board = [] /= filter (== diff) (concat board)

captureMove :: IO Char
captureMove = do
  inp <- getChar
  case find (==inp) ['w','s','a','d'] of
      Just x  -> pure x
      Nothing -> do putStrLn "\nUse WASD to select move"
                    captureMove

captureDiff :: IO Int
captureDiff = do
  putStrLn "Choose difficulty:"
  putStrLn "1 = 128, 2 = 256, 3 = 512, 4 = 1024, 5 = 2048"
  inp <- getChar
  case inp of
      '1'  -> pure 128
      '2'  -> pure 256
      '3'  -> pure 512
      '4'  -> pure 1024
      '5'  -> pure 2048
      _ -> do putStrLn "\nNot supported"
              captureDiff

gameLoop :: Int -> Board -> IO ()
gameLoop diff board
  | isMovable board = do
      printBoard board
      if checkWin diff board
      then putStrLn "You won!"
      else do m <- captureMove
              seed <- getSeeds 2
              let mergedBoard = move m board
              if board /= mergedBoard
              then do let newBoard = addTile seed mergedBoard
                      gameLoop diff newBoard
              else gameLoop diff board
  | otherwise = do
      printBoard board
      putStrLn "Game over"


main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  seed <- getSeeds 4
  let board = initBoard seed
  diff <- captureDiff
  gameLoop diff board
