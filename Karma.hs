module KarmaBrief where

import System.Random 
import Control.Monad.State
import Data.List 
import Data.Ord 


-- Cards
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK | RA
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Eq, Show, Read)

type Deck = [Card]
type Pile = [Card]  

-- Players
type PlayerId   = Int
type PlayerName = String

data Player = Player
  { pId       :: PlayerId
  , pName     :: PlayerName
  , hand      :: [Card]
  , faceUp    :: [Card]
  , faceDown  :: [Card]
  }

-- Game state 
data GameState = GameState
  { players       :: [Player]    -- clockwise order
  , currentIx     :: Int         -- index into players
  , drawPile      :: Deck
  , discardPile   :: Pile
  , burnedPiles   :: [Pile]
  , rng           :: StdGen      -- random number generator
  , finishedOrder :: [PlayerId]
  } deriving (Show)


-- Different extension rules we can toggle
data Extension = ExtReverse8 | ExtThree3s | ExtNineClubs
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Step 1 
--------------------------------------------------------------------------------
legalPlay :: Maybe Card -> Card -> Bool

validPlays :: Maybe Card -> Deck -> Deck

dealCards :: Int -> State GameState Deck

giveWastePileTo :: Player -> State GameState ()

replenishCards :: Player -> State GameState ()

shuffleDeck :: StdGen -> Deck -> Deck

--------------------------------------------------------------------------------
-- Step 2 
--------------------------------------------------------------------------------
basicStrategy :: State GameState Deck

applyStrategy :: State GameState ()

gameLoop :: State GameState String

chooseStartingPlayer :: State GameState ()

--------------------------------------------------------------------------------
-- Step 3 
--------------------------------------------------------------------------------
basicStrategySets:: State GameState Deck

gameLoopWithHistory :: State GameState String

runOneGameWithHistory :: IO ()

--------------------------------------------------------------------------------
-- Step 4 
--------------------------------------------------------------------------------
playOneGameStep4 :: [Extension] -> IO ()

--------------------------------------------------------------------------------
-- Step 5 — Smart Player and Tournaments
--------------------------------------------------------------------------------
smartStrategy :: State GameState Deck

playTournament :: Int -> IO [(String, Int)]

