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
-- Helper
--------------------------------------------------------------------------------
isSpecial :: Rank -> Bool
isSpecial r = r `elem` [R2, R8, R10]
 
--------------------------------------------------------------------------------
-- Step 1 
--------------------------------------------------------------------------------
legalPlay :: Maybe Card -> Card -> Bool
legalPlay Nothing _ = True
legalPlay (Just topCard) myCard
  | isSpecial(rank myCard) = True
  |rank topCard == R7 = rank myCard <= R7
  | otherwise = rank myCard >= rank topCard

validPlays :: Maybe Card -> Deck -> Deck
validPlays Nothing myDeck = myDeck
validPlays _ [] = []
validPlays topCard (x:xs)
  | legalPlay topCard x = x : validPlays topCard xs
  | otherwise           =     validPlays topCard xs

dealCards :: Int -> State GameState Deck
dealCards n = do 
  currentState <- get
  let oldPile = drawPile currentState
      dealt   = take n oldPile
      rest    = drop n oldPile
  put currentState { drawPile = rest }
  return dealt

giveWastePileTo :: Player -> State GameState ()
giveWastePileTo player = do
    currentState <- get
    let pileToTake = discardPile currentState
    let updatePlayer = player {hand = hand player ++ pileToTake}
    let ix             = currentIx currentState
        updatedPlayers = take ix (players currentState) ++ [updatedPlayer] ++ drop (ix + 1) (players currentState)
        
    put currentState {
      players = updatedPlayers,
      discardPile = []
      }

replenishCards :: Player -> State GameState ()
replenishCards player = do
    currentState <- get
    let dp = drawPile currentState
        ix = currentIx currentState
        ph = hand player
        cardsNeeded = 3 - length ph
        draw = take cardsNeeded dp
        rest = drop cardsNeeded dp
        updatedPlayer = player { hand = ph ++ draw }
        updatedPlayers = take ix (players currentState) ++ [updatedPlayer] ++ drop (ix + 1) (players currentState)

    when (not (null dp) && length ph < 3) $
        put currentState { players = updatedPlayers, drawPile = rest }




shuffleDeck :: StdGen -> Deck -> Deck
shuffleDeck _ [] = []
shuffleDeck gen deck = 
    let (idx, newGen) = randomR (0, length deck - 1) gen
       pickedCard = deck !! idx
       remainingDeck = take idx deck ++ drop (idx + 1) deck
    in pickedCard : suffleDeck newGen remainingDeck

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

