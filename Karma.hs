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
-- Helpers
--------------------------------------------------------------------------------
isSpecial :: Rank -> Bool
isSpecial r = r `elem` [R2, R8, R10]

fullDeck :: Deck
fullDeck =
    [ Card r s
    | r <- [minBound .. maxBound]
    , s <- [minBound .. maxBound]
    ]

playFromHand :: Card -> State GameState()
playFromHand card = do
  st <- get
  let ix = currentIx st
      player = (players st) !! ix
      hd = hand player
      updHand = filter (/= card) (hd player)
      updPlayer = player {hand = updHand}
      updPlayers = take ix players st ++ updPlayer ++ drop( ix + 1) players st
      updDeck = card : discardPile st
  put st = {players = newPlayers, discardPile = newDiscard}

playFromFaceUp :: Card -> State GameState()
playFromFaceUp card = do
  st <- get
  let ix = currentIx st
      player = (players st) !! ix
      fUp = faceUp player
      updFUp = filter (/= card) (fUp player)
      updPlayer = player {faceUp = updFUp}
      updPlayers = take ix players ++ updatedPlayer ++ drop( ix + 1) players
      updDeck = card : discardPile st
  put st = {players = newPlayers, discardPile = newDiscard}

playFromFaceDown :: Card -> State GameState()
playFromFaceDown card = do
  st <- get
  let ix = currentIx st
      player = (players st) !! ix
      fDown = faceDown player
      updFDown = filter (/= card) (fDown player)
      updPlayer = player {faceDown = updFDown}
      updPlayers = take ix players ++ updatedPlayer ++ drop( ix + 1) players
      updDeck = card : discardPile st
  put st = {players = newPlayers, discardPile = newDiscard}

advancePlayer:: State GameState()
advancePlayer = do
  st <- get
  let n  = length (players st)
      ix = (currentIx st + 1) `mod` n
  put st { currentIx = ix }

applyAfterPlayEffects :: State GameState ()
applyAfterPlayEffects = do
    st <- get
    case discardPile st of
        (c:rest)
          | rank c == R10 -> burn
          | length rest >= 3
          , all (\x -> rank x == rank c) (take 3 rest) -> burn
        _ -> return ()
  where
    burn = do
        st <- get
        let burned = discardPile st
        put st { discardPile = [], burnedPiles = burned : burnedPiles st }

dealInitialPlayers :: [Card] -> ([Player], [Card])
dealInitialPlayers deck =
    let (h1,h2,h3,rest) = deal3 deck
        players =
          [ Player "Daizy" h1 basicStrategy
          , Player "Sue"   h2 basicStrategy
          , Player "Tracey" h3 basicStrategy
          ]
    in (players, rest)
  where
    deal3 xs =
        let (a, r1) = splitAt 3 xs
            (b, r2) = splitAt 3 r1
            (c, r3) = splitAt 3 r2
        in (a,b,c,r3)

findStartingPlayer :: [Player] -> Int
findStartingPlayer _ = 0  
  
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
basicStrategy = do
    st <- get
    let ix = currentIx st
        player = players st !! ix
        hd = hand player
        fu = faceUp player
        fd = faceDown player
        top = case discardPile st of
                [] -> Nothing
                (x:_) -> Just x
    if not (null hd) then
        return $ take 1 (validPlays top hd)
    else if null hd && null (drawPile st) && not (null fu) then
        return $ take 1 (validPlays top fu)
    else if null hd && null (drawPile st) && null fu && not (null fd) then
        let gen = rng st
            (idx, newGen) = randomR (0, length fd - 1) gen
            chosenCard = fd !! idx
        in do
            put st { rng = newGen } 
            return [chosenCard]
    else
        return []

applyStrategy :: State GameState ()
applyStrategy = do
  st <- get
  let ix = currentIx st
      player = (players st) !! ix
  chosenCards <- basicStrategy
  if null chosenCards
    then do 
      giveWastePileTo player
      advancePlayer
  else do 
    let card = head chosenCards
    if card `elem` hand player then
        playFromHand card
    else if card `elem` faceUp player then
        playFromFaceUp card
    else
        playFromFaceDown card
  applyAfterPlayEffects
  stNew <- get
  let playerNew = players stNew !! ix
  replenishCards playerNew
  advancePlayer
    
isOut :: Player -> Bool
isOut p = null (hand p) && null (faceUp p) && null (faceDown p)

gameLoop :: State GameState String
gameLoop = do
    st <- get
    let alive = filter (not . isOut) (players st)
    case alive of
      [winner] -> return $ pName winner
      []       -> return "No players left"
      _        -> do
        applyStrategy
        gameLoop

playOneGame :: IO ()
playOneGame = do
  gen <- newStdGen
  let deck = fullDeck
      shuffled = shuffleDeck gen deck
      (ps, restDeck) = dealInitialPlayers shuffled
      startIx = findStartingPlayer ps
      initialState = GameState
        { players = ps
        , currentIx = startIx
        , drawPile = restDeck
        , discardPile = []
        , burnedPiles = []
        , rng = gen
        , finishedOrder = []
        }
      (winner, _finalState) = runState gameLoop initialState
  putStrLn $ "Winner: " ++ winner

chooseStartingPlayer :: State GameState ()
chooseStartingPlayer = do
  st <- get
  let playersList = players st
      gen = rng st

      playersHands = [ (i, c) | (i, p) <- zip [0..] playersList, c <- hand p ]
      minCard = minimumBy (comparing snd) playersHands

      candidateIndices = [ i | (i, c) <- playersHands, c == minRankVal ]
      (randX, newGen) = randomR (0, length candidateIndices - 1) gen
      chosenIx = candidateIndices !! randX
  put st { currentIx = chosenIx, rng = newGen }



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

