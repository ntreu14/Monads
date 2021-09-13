module State where

import           Control.Monad.State
import qualified Data.Array as A
import qualified Data.Ix as I
import           System.Random (StdGen, randomR, newStdGen)
import Data.Set (empty)

{- State Monad reference

get :: State s s
put :: s -> State s ()
runState :: State s a -> s -> (a, s)

-}

data Player = XPlayer | OPlayer

data TileState = Empty | HasX | HasO
  deriving (Show, Eq)

type TileIndex = (Int, Int)

boardIndices :: [TileIndex]
boardIndices = I.range ((0, 0), (2,2))

data GameState = GameState
  { board :: A.Array TileIndex TileState
  , currentPlayer :: Player
  , generator :: StdGen
  }

initialGameState :: StdGen -> GameState
initialGameState = GameState
  (A.array (head boardIndices, last boardIndices) [(i, Empty) | i <- boardIndices])
  XPlayer

nextPlayer :: Player -> Player
nextPlayer XPlayer = OPlayer
nextPlayer OPlayer = XPlayer

tileForPlayer :: Player -> TileState
tileForPlayer XPlayer = HasX
tileForPlayer OPlayer = HasO

-- TODO: Fill in these stateful functions!

-- Select a random move from among the Empty tiles
-- You can't use the IO monad, so you have to make use of the
-- stateful generator in the GameState!
chooseRandomMove :: State GameState TileIndex
chooseRandomMove = do 
  game <- get
  let spots = getSpots game
  let gen = generator game
  let (i, gen') = randomR (0, length spots - 1) gen
  put $ game { generator = gen' }
  pure $ spots !! i

  where
    getSpots game = [ fst pair | pair <- A.assocs (board game), snd pair == Empty]

-- Given a selected tile, mark it for the "current" player!
applyMove :: TileIndex -> State GameState ()
applyMove i = do
  game <- get
  let player = currentPlayer game
  let newBoard = board game A.// [(i, tileForPlayer player)]
  put $ game { currentPlayer = nextPlayer player, board = newBoard }

-- The game is done when there are no more Empty tiles!
isGameDone :: State GameState Bool
isGameDone = do
  game <- get
  let spots = [ fst pair | pair <- A.assocs (board game), snd pair == Empty]
  pure $ null spots

-- Combine your functions together for a function to complete a single turn!
resolveTurn :: State GameState Bool
resolveTurn = do
  i <- chooseRandomMove
  applyMove i
  isGameDone

