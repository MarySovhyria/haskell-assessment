module KarmaBrief where

import System.Random 
import Control.Monad.State
import Control.Monad (when)
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
  , strategy  :: State GameState Deck
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
instance Ord Card where
    compare (Card r1 _) (Card r2 _) = compare r1 r2

instance Show Player where
  show (Player pid name hand fu fd _) =
    "Player { pId = " ++ show pid
    ++ ", pName = " ++ show name
    ++ ", hand = " ++ show hand
    ++ ", faceUp = " ++ show fu
    ++ ", faceDown = " ++ show fd
    ++ ", strategy = <function> }"

playFromHand :: Card -> State GameState()
playFromHand card = do
  st <- get
  let ix = currentIx st
      player = (players st) !! ix
      hd = hand player
      updHand = filter (/= card) (hd)
      updPlayer = player {hand = updHand}
      updPlayers = take ix (players st) ++ [updPlayer] ++ drop (ix + 1) (players st)
      updDeck = card : discardPile st
  put st {players = updPlayers, discardPile = updDeck}


playFromFaceUp :: Card -> State GameState ()
playFromFaceUp card = do
  st <- get
  let ix = currentIx st
      player = players st !! ix
      fUp = faceUp player
      updFUp = filter (/= card) fUp
      updPlayer = player { faceUp = updFUp }
      updPlayers = take ix (players st) ++ [updPlayer] ++ drop (ix + 1) (players st)
      updDeck = card : discardPile st
  put st { players = updPlayers, discardPile = updDeck }


playFromFaceDown :: Card -> State GameState()
playFromFaceDown card = do
  st <- get
  let ix = currentIx st
      player = (players st) !! ix
      fDown = faceDown player
      updFDown = filter (/= card) fDown 
      updPlayer = player {faceDown = updFDown}
      updPlayers = take ix (players st) ++ [updPlayer] ++ drop( ix + 1)( players st)
      updDeck = card : discardPile st
  put st {players = updPlayers, discardPile = updDeck}

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
    let (h1, r1) = splitAt 3 deck
        (fup1, r2) = splitAt 3 r1
        (fdown1, r3) = splitAt 3 r2

        (h2, r4) = splitAt 3 r3
        (fup2, r5) = splitAt 3 r4
        (fdown2, r6) = splitAt 3 r5

        (h3, r7) = splitAt 3 r6
        (fup3, r8) = splitAt 3 r7
        (fdown3, rest) = splitAt 3 r8

        players =
          [ Player 0 "Basic Strategy" h1 fup1 fdown1 basicStrategy
          , Player 1 "Basec Strategy Sets" h2 fup2 fdown2 basicStrategySets
          , Player 2 "Smart Strategy" h3 fup3 fdown3 smartStrategy
          ]
    in (players, rest)


findStartingPlayer :: [Player] -> Int
findStartingPlayer ps =
  let 
      playerCards = [ (i, c) | (i, p) <- zip [0..] ps, c <- hand p ++ faceUp p ++ faceDown p ]
  in fst $ minimumBy (comparing snd) playerCards
  
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
giveWastePileTo target = do
    currentState <- get
    let pileToTake = discardPile currentState
        -- find the index of the target player (fallback to currentIx)
        idx = case findIndex (\p -> pId p == pId target) (players currentState) of
                Just i  -> i
                Nothing -> currentIx currentState
        oldPlayer = players currentState !! idx
        updatePlayer = oldPlayer { hand = hand oldPlayer ++ pileToTake }
        updatedPlayers = take idx (players currentState) ++ [updatePlayer] ++ drop (idx + 1) (players currentState)
    put currentState { players = updatedPlayers, discardPile = [] }

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
    in pickedCard : shuffleDeck newGen remainingDeck

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
    if not (null hd) then do
        let sortedHand = sortOn rank hd
        return (take 1 (validPlays top sortedHand))
    else if null hd && null (drawPile st) && not (null fu) then do
        let sortedFU = sortOn rank fu
        return (take 1 (validPlays top sortedFU))
    else if null hd && null (drawPile st) && null fu && not (null fd) then do
        let gen = rng st
            (idx, newGen) = randomR (0, length fd - 1) gen
            chosenCard = fd !! idx
        put st { rng = newGen } 
        return [chosenCard]
    else
        return []

applyStrategy ::State GameState Deck -> State GameState ()
applyStrategy strategy = do
  st <- get
  let ix = currentIx st
      player = (players st) !! ix
  chosenCards <- strategy
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
  
applyCurrentStrategy :: State GameState ()
applyCurrentStrategy = do
    st <- get
    let ix = currentIx st
        player = players st !! ix
    chosenCards <- strategy player
    if null chosenCards
      then do 
        -- fallback: if can't play, give pile to current player and advance
        giveWastePileTo player
        advancePlayer
      else do
        -- play all chosen cards (respect dynamic state after each play)
        let playCard :: Card -> State GameState ()
            playCard card = do
              stNow <- get
              let pNow = players stNow !! ix
              if card `elem` hand pNow then
                  playFromHand card
              else if card `elem` faceUp pNow then
                  playFromFaceUp card
              else
                  playFromFaceDown card
        mapM_ playCard chosenCards
    applyAfterPlayEffects
    stNew <- get
    let playerNew = players stNew !! ix
    replenishCards playerNew
    -- mark player finished if they emptied all piles
    markFinished playerNew
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
        applyCurrentStrategy
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
      (_res, finalState) = runState (gameLoopLimited maxTurns) initialState
      winnerName = decideGameWinner finalState
  putStrLn $ "Winner: " ++ winnerName

chooseStartingPlayer :: State GameState ()
chooseStartingPlayer = do
  st <- get
  let playersList = players st
      gen = rng st

      playersHands = [ (i, c) | (i, p) <- zip [0..] playersList, c <- hand p ]
      minCard = minimumBy (comparing snd) playersHands

      candidateIndices = [ i | (i, c) <- playersHands, c == snd minCard ]
      (randX, newGen) = randomR (0, length candidateIndices - 1) gen
      chosenIx = candidateIndices !! randX
  put st { currentIx = chosenIx, rng = newGen }



--------------------------------------------------------------------------------
-- Step 3 
--------------------------------------------------------------------------------
basicStrategySets:: State GameState Deck
basicStrategySets = do
    st <- get
    let ix = currentIx st
        player = players st !! ix
        hd = hand player
        fu = faceUp player
        fd = faceDown player
        top = case discardPile st of
                [] -> Nothing
                (x:_) -> Just x

        -- choose the largest legal group; tie-breaker prefers lower rank
        selectBest groups =
          maximumBy (comparing (\g -> (length g, Down (rank (head g))))) groups

    if not (null hd) then
        let rankGroups = groupBy (\a b -> rank a == rank b) $ sortOn rank hd
            legalSets = filter (\g -> legalPlay top (head g)) rankGroups
        in return $ if null legalSets then [] else selectBest legalSets
    else if null hd && null (drawPile st) && not (null fu) then
        let rankGroups = groupBy (\a b -> rank a == rank b) $ sortOn rank fu
            legalSets = filter (\g -> legalPlay top (head g)) rankGroups
        in return $ if null legalSets then [] else selectBest legalSets
    else if null hd && null (drawPile st) && null fu && not (null fd) then do
        let gen = rng st
            (idx, newGen) = randomR (0, length fd - 1) gen
            chosenCard = fd !! idx
        put st { rng = newGen }
        return [chosenCard]
    else
        return []

gameLoopWithHistory :: State GameState String
gameLoopWithHistory = do
    st <- get
    let alive = filter (not . isOut) (players st)
    case alive of
      [winner] -> return $ pName winner
      []       -> return "No players left"
      _        -> do
          applyCurrentStrategy
          gameLoopWithHistory


runOneGameWithHistory :: IO ()
runOneGameWithHistory = do
    gen <- newStdGen
    let deck = fullDeck
        shuffled = shuffleDeck gen deck
        -- two players use basicStrategy, one uses basicStrategySets
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
    let gameStates = iterateGame initialState
    mapM_ printTurn gameStates

iterateGame :: GameState -> [GameState]
iterateGame st
  | all isOut (players st) = [st]
  | otherwise = let (_, st') = runState applyCurrentStrategy st
                in st : iterateGame st'

-- Print a single state
printTurn :: GameState -> IO ()
printTurn st = do
    let ix = currentIx st
        player = players st !! ix
    putStrLn $ "\nCurrent player: " ++ pName player
    putStrLn $ "Hand: " ++ show (hand player)
    putStrLn $ "Face-up: " ++ show (faceUp player)
    putStrLn $ "Face-down cards: " ++ show (length $ faceDown player)
    putStrLn $ "Discard pile: " ++ show (discardPile st)
    when (isOut player) $ putStrLn $ pName player ++ " has finished!"

--------------------------------------------------------------------------------
-- Step 4 
--------------------------------------------------------------------------------
reverseOrder ::  State GameState()
reverseOrder = do
  st <- get
  case discardPile st of
    (x:_)
      | rank x == R8 -> do
          let ps = players st
              currPid = pId (ps !! currentIx st)
              rev = reverse ps
              newIx = case findIndex (\p -> pId p == currPid) rev of
                        Just i  -> i
                        Nothing -> currentIx st
          put st { players = rev, currentIx = newIx }
    _ -> return ()


liftState :: State GameState a -> StateT GameState IO a
liftState st = do
    s <- get
    let (a, s') = runState st s
    put s'
    return a


customThreeBurn :: State GameState ()
customThreeBurn = do
    st <- get
    case discardPile st of
        (c1:c2:c3:_) 
            | all (\x -> rank x == R3) [c1,c2,c3] -> do
                let nextIx = (currentIx st + 1) `mod` length (players st)
                    nextPlayer = players st !! nextIx
                giveWastePileTo nextPlayer
        _ -> return ()


stealCard :: State GameState ()
stealCard = do
    st <- get
    case discardPile st of
      (x:_) 
        | rank x == R9 && suit x == Clubs -> do
            let currIx = currentIx st
                nPlayers = length (players st)
                nextIx = (currIx + 1) `mod` nPlayers
                currPlayer = players st !! currIx
                nextPlayer = players st !! nextIx
                nextHand = hand nextPlayer

            when (not (null nextHand)) $ do
                let gen = rng st
                    (randIdx, newGen) = randomR (0, length nextHand - 1) gen
                    stolenCard = nextHand !! randIdx
                    updatedNext = nextPlayer { hand = take randIdx nextHand ++ drop (randIdx + 1) nextHand }
                    updatedCurr = currPlayer { hand = stolenCard : hand currPlayer }
                    updatedPlayers = take currIx (players st)
                                   ++ [updatedCurr]
                                   ++ take (nextIx - currIx - 1) (drop (currIx + 1) (players st))
                                   ++ [updatedNext]
                                   ++ drop (nextIx + 1) (players st)

                put st { players = updatedPlayers, rng = newGen }
      _ -> return ()

playOneGameStep4 :: [Extension] -> IO String
playOneGameStep4 exts = do
    gen <- newStdGen
    let deck = fullDeck
        (genForShuffle, genForState) = split gen
        shuffled = shuffleDeck genForShuffle deck
        -- Deal initial hands (3 hand, 3 face-up, 3 face-down per player)
        (playersList, restDeck) = dealInitialPlayers shuffled
        startIx = findStartingPlayer playersList
        initialState = GameState
            { players = playersList
            , currentIx = startIx
            , drawPile = restDeck
            , discardPile = []
            , burnedPiles = []
            , rng = genForState
            , finishedOrder = []
            }

    putStrLn $ "Starting player: " ++ pName (players initialState !! startIx)
    
    (_res, finalState) <- runStateT (gameLoopWithExtensionsLimited maxTurns exts) initialState
    return (decideGameWinner finalState)

-- Game loop that also applies extensions
gameLoopWithExtensions :: [Extension] -> StateT GameState IO String
gameLoopWithExtensions exts = do
    st <- get
    let alive = filter (not . isOut) (players st)
    case alive of
      [winner] -> do
          liftIO $ putStrLn $ "Winner: " ++ pName winner
          return $ pName winner
      [] -> do
          liftIO $ putStrLn "No players left"
          return "No players left"
      _ -> do
          let ix = currentIx st
              player = players st !! ix
          liftIO $ putStrLn $ "\nCurrent player: " ++ pName player
          liftIO $ putStrLn $ "Hand: " ++ show (hand player)
          liftIO $ putStrLn $ "Face-up: " ++ show (faceUp player)
          liftIO $ putStrLn $ "Face-down cards: " ++ show (length $ faceDown player)

          -- capture top-of-pile before the play
          let topBefore = case discardPile st of
                            []    -> Nothing
                            (x:_) -> Just x

          -- Apply strategy
          liftState applyCurrentStrategy

          -- capture top-of-pile after the play
          stAfterPlay <- get
          let topAfter = case discardPile stAfterPlay of
                           []    -> Nothing
                           (x:_) -> Just x
              topBeforeIs8 = case topBefore of Just c -> rank c == R8; _ -> False
              topAfterIs8  = case topAfter  of Just c -> rank c == R8; _ -> False

          -- Apply extensions after the play
          -- reverse order only when an 8 was newly played (top changed from non-8 to 8)
          when (ExtReverse8 `elem` exts && (not topBeforeIs8) && topAfterIs8) $
              liftState reverseOrder

          when (ExtThree3s `elem` exts) $ liftState customThreeBurn
          when (ExtNineClubs `elem` exts) $ liftState stealCard

          stAfter <- get
          liftIO $ putStrLn $ "Discard pile: " ++ show (discardPile stAfter)

          let playerAfter = players stAfter !! ix
          when (isOut playerAfter) $
              liftIO $ putStrLn $ pName playerAfter ++ " has finished!"

          gameLoopWithExtensions exts

-- safety: cap the number of turns to avoid infinite loops
maxTurns :: Int
maxTurns = 20000  -- tune as needed

gameLoopLimited :: Int -> State GameState String
gameLoopLimited 0 = return "Max turns reached"
gameLoopLimited steps = do
    st <- get
    let alive = filter (not . isOut) (players st)
    case alive of
      [winner] -> return $ pName winner
      []       -> return "No players left"
      _        -> do
        applyCurrentStrategy
        gameLoopLimited (steps - 1)

gameLoopWithExtensionsLimited :: Int -> [Extension] -> StateT GameState IO String
gameLoopWithExtensionsLimited 0 _ = do
    liftIO $ putStrLn "Max turns reached"
    return "Max turns reached"
gameLoopWithExtensionsLimited steps exts = do
    st <- get
    let alive = filter (not . isOut) (players st)
    case alive of
      [winner] -> do
          liftIO $ putStrLn $ "Winner: " ++ pName winner
          return $ pName winner
      [] -> do
          liftIO $ putStrLn "No players left"
          return "No players left"
      _ -> do
          let ix = currentIx st
              player = players st !! ix
          liftIO $ putStrLn $ "\nCurrent player: " ++ pName player
          liftIO $ putStrLn $ "Hand: " ++ show (hand player)
          liftIO $ putStrLn $ "Face-up: " ++ show (faceUp player)
          liftIO $ putStrLn $ "Face-down cards: " ++ show (length $ faceDown player)

          -- capture top-of-pile before the play
          let topBefore = case discardPile st of
                            []    -> Nothing
                            (x:_) -> Just x

          -- Apply strategy
          liftState applyCurrentStrategy

          -- capture top-of-pile after the play
          stAfterPlay <- get
          let topAfter = case discardPile stAfterPlay of
                           []    -> Nothing
                           (x:_) -> Just x
              topBeforeIs8 = case topBefore of Just c -> rank c == R8; _ -> False
              topAfterIs8  = case topAfter  of Just c -> rank c == R8; _ -> False

          -- Apply extensions after the play
          -- reverse order only when an 8 was newly played (top changed from non-8 to 8)
          when (ExtReverse8 `elem` exts && (not topBeforeIs8) && topAfterIs8) $
              liftState reverseOrder

          when (ExtThree3s `elem` exts) $ liftState customThreeBurn
          when (ExtNineClubs `elem` exts) $ liftState stealCard

          stAfter <- get
          liftIO $ putStrLn $ "Discard pile: " ++ show (discardPile stAfter)

          let playerAfter = players stAfter !! ix
          when (isOut playerAfter) $
              liftIO $ putStrLn $ pName playerAfter ++ " has finished!"

          gameLoopWithExtensionsLimited (steps - 1) exts
--------------------------------------------------------------------------------
-- Step 5 — Smart Player and Tournaments
--------------------------------------------------------------------------------
updateWins :: String -> [(String, Int)] -> [(String, Int)]
updateWins winner = map update
  where
    update (name, count)
        | name == winner = (name, count + 1)
        | otherwise      = (name, count)

smartStrategy :: State GameState Deck
smartStrategy = do
    st <- get
    let ix = currentIx st
        player = players st !! ix
        hd = hand player
        fu = faceUp player
        fd = faceDown player
        pile = discardPile st
        nPlayers = length (players st)

        -- effective top: skip transparent 8s
        firstNon8 xs = case dropWhile (\c -> rank c == R8) xs of
                         []    -> Nothing
                         (c:_) -> Just c

        topEff = firstNon8 pile

        -- count consecutive same-rank cards from top ignoring 8s
        consecCountFor r =
          let rest = dropWhile (\c -> rank c == R8) pile
              samePrefix = takeWhile (\c -> rank c == r) rest
          in length samePrefix

        -- group cards by rank
        groupsOf cs =
          let sorted = sortOn rank cs
          in groupBy (\a b -> rank a == rank b) sorted

        -- approximate resulting effective top after playing group of rank r and length lenG
        resultingTopAfter r lenG =
          let isTen = r == R10
              isEight = r == R8
          in if isTen then Nothing
             else if isEight then topEff
             else Just (Card r Clubs)  -- suit irrelevant for legality

        -- next player quick analysis: can they finish immediately after resTop?
        nextPlayer = players st !! ((ix + 1) `mod` nPlayers)
        nextTotalCards p = length (hand p) + length (faceUp p) + length (faceDown p)

        nextCanFinish resTop p =
          let pool = hand p ++ faceUp p
              legal = validPlays resTop (sortOn rank pool)
              groups = groupBy (\a b -> rank a == rank b) legal
              maxPlay = if null groups then 0 else maximum (map length groups)
          in (nextTotalCards p > 0) && maxPlay >= nextTotalCards p

        -- how many of rank r remain in my hand after playing k of them
        remainingOfRankAfter r k = length (filter (\c -> rank c == r) hd) - k

        myTotal = length hd + length fu + length fd

        -- score a candidate play-group based on heuristics + simple 1-ply lookahead
        scoreGroup g =
          let r = rank (head g)
              lenG = length g
              isTen = r == R10
              isTwo = r == R2
              isEight = r == R8

              -- immediate finish for me
              myRemainingAfter = myTotal - lenG
              finishBonus = if myRemainingAfter == 0 then 2000 else 0

              -- strong bonuses for burning
              tenBonus = if isTen then 400 else 0
              makesFour =
                case topEff of
                  Just topCard ->
                    let topR = rank topCard
                        consec = consecCountFor topR
                    in topR == r && (consec + lenG) >= 4
                  Nothing -> False
              fourBonus = if makesFour then 450 else 0

              -- prefer shedding larger sets
              shedScore = lenG * 80

              -- prefer removing last copy of a rank from my hand
              removeRankBonus = if remainingOfRankAfter r lenG <= 0 then 60 else 0

              -- prefer lower ranks slightly (helps face-down/face-up stages)
              rankPref = - fromEnum r

              -- simulate resulting top and whether next can finish
              resTop = resultingTopAfter r lenG
              nextFinishes = nextCanFinish resTop nextPlayer
              nextFinishPenalty = if nextFinishes then 1000 else 0

              -- if next has 1 card and can play after this, severe penalty unless we burn/reset
              nxtTotal = nextTotalCards nextPlayer
              nxtCanPlaySingle = case resTop of
                                  Nothing -> not (null $ hand nextPlayer ++ faceUp nextPlayer)
                                  Just t  -> not (null $ validPlays (Just t) (hand nextPlayer ++ faceUp nextPlayer))
              oneCardPenalty = if nxtTotal == 1 && nxtCanPlaySingle then 800 else 0

              -- avoid wasting 2/10 unless it's needed to prevent next finishing or to finish self
              conservePenalty = if (isTwo || isTen) && not isTen && not isTwo && nxtTotal <= 2 then 0 else 0
              -- (conservePenalty kept simple; tens are still rewarded above)

          in finishBonus + tenBonus + fourBonus + shedScore + removeRankBonus + rankPref
             - nextFinishPenalty - oneCardPenalty - conservePenalty

        chooseBestGroup legalCards =
          let groups = groupsOf legalCards
              scored = map (\g -> (scoreGroup g, g)) groups
              sortedByScore = sortOn (Down . fst) scored
          in snd (head sortedByScore)

    -- choose from a pile (hand/faceUp)
    let chooseFrom cards =
          let legal = validPlays topEff (sortOn rank cards)
          in if null legal then [] else chooseBestGroup legal

    -- Decision flow: must play from hand if available; otherwise faceUp when draw exhausted; otherwise faceDown blind
    if not (null hd) then
        return (chooseFrom hd)
    else if null hd && null (drawPile st) && not (null fu) then
        return (chooseFrom fu)
    else if null hd && null (drawPile st) && null fu && not (null fd) then do
        -- blind play from face-down: choose random index and play that card
        let gen = rng st
            (idx, newGen) = randomR (0, length fd - 1) gen
            chosen = fd !! idx
        put st { rng = newGen }
        return [chosen]
    else
        return []

playTournament :: Int -> IO [(String, Int)]
playTournament n = go n initialScores
  where
    initialScores =
        [ ("Basic Strategy", 0)
        , ("Basic Strategy Sets", 0)
        , ("Smart Strategy", 0)
        ]

    go 0 scores = return scores
    go k scores = do
        winner <- playOneGameStep4 []
        let newScores = updateWins winner scores
        go (k - 1) newScores

markFinished :: Player -> State GameState ()
markFinished p = do
  st <- get
  let fid = pId p
  when (isOut p && not (fid `elem` finishedOrder st)) $
    put st { finishedOrder = finishedOrder st ++ [fid] }

decideGameWinner :: GameState -> String
decideGameWinner st =
  case finishedOrder st of
    (firstFinisherId:_) ->
      case find (\p -> pId p == firstFinisherId) (players st) of
        Just p  -> pName p
        Nothing -> fallback
    [] -> -- no one finished; if one alive return them, else fallback
      let alive = filter (not . isOut) (players st)
      in case alive of
           [winner] -> pName winner
           _        -> fallback
  where
    fallback = "No winner"

