module TestKarmaBrief where

import KarmaBrief
import Test.HUnit
import System.Random
import qualified Control.Monad.State as S
import Control.Monad.Trans.State (runStateT)
import Data.List (all, elem)

-- helpers used in tests
mkPlayer :: Int -> String -> [Card] -> [Card] -> [Card] -> S.State GameState Deck -> Player
mkPlayer pid name h fu fd strat = Player pid name h fu fd strat

mkState :: [Player] -> Int -> Deck -> Pile -> StdGen -> GameState
mkState ps ix dp disc g = GameState
  { players = ps
  , currentIx = ix
  , drawPile = dp
  , discardPile = disc
  , burnedPiles = []
  , rng = g
  , finishedOrder = []
  }

-- basicStrategy tests

testBasicStrategyChoosesLowestLegal :: Test
testBasicStrategyChoosesLowestLegal = TestCase $
  let p = mkPlayer 0 "P" [Card R3 Hearts, Card R5 Clubs, Card R8 Spades] [] [] basicStrategy
      st = mkState [p] 0 [] [Card R4 Diamonds] (mkStdGen 1)
      (chosen, _) = S.runState basicStrategy st
  in assertEqual "basicStrategy should pick lowest legal single (R5)" [Card R5 Clubs] chosen

testBasicStrategyUsesFaceUpWhenHandEmpty :: Test
testBasicStrategyUsesFaceUpWhenHandEmpty = TestCase $
  let p = mkPlayer 0 "P" [] [Card R6 Clubs, Card R9 Hearts] [] basicStrategy
      st = mkState [p] 0 [] [Card R5 Diamonds] (mkStdGen 1)
      (chosen, _) = S.runState basicStrategy st
  in assertEqual "basicStrategy should pick lowest legal faceUp single (R6)" [Card R6 Clubs] chosen

-- basicStrategySets tests

testBasicStrategySetsChoosesLargestGroup :: Test
testBasicStrategySetsChoosesLargestGroup = TestCase $
  let h = [Card R7 Clubs, Card R7 Diamonds, Card R9 Hearts, Card R7 Spades]
      p = mkPlayer 0 "P" h [] [] basicStrategySets
      st = mkState [p] 0 [] [] (mkStdGen 1)
      (chosen, _) = S.runState basicStrategySets st
  in do
    assertEqual "should choose three cards" 3 (length chosen)
    assertBool "all chosen should be rank R7" (all (\c -> rank c == R7) chosen)

testBasicStrategySetsSingleLegal :: Test
testBasicStrategySetsSingleLegal = TestCase $
  let h = [Card R3 Clubs]
      p = mkPlayer 0 "P" h [] [] basicStrategySets
      st = mkState [p] 0 [] [] (mkStdGen 1)
      (chosen, _) = S.runState basicStrategySets st
  in assertEqual "single legal card returned" [Card R3 Clubs] chosen

-- smartStrategy tests

testSmartStrategyFinishesWhenPossible :: Test
testSmartStrategyFinishesWhenPossible = TestCase $
  let h = [Card R5 Clubs]  -- playing this will finish the player
      p = mkPlayer 0 "P" h [] [] smartStrategy
      st = mkState [p] 0 [] [] (mkStdGen 1)
      (chosen, _) = S.runState smartStrategy st
  in assertEqual "smartStrategy should choose finishing play" h chosen

testSmartStrategyPlaysTenWhenOnlyTenAvailable :: Test
testSmartStrategyPlaysTenWhenOnlyTenAvailable = TestCase $
  let h = [Card R10 Clubs]
      p = mkPlayer 0 "P" h [] [] smartStrategy
      st = mkState [p] 0 [] [] (mkStdGen 1)
      (chosen, _) = S.runState smartStrategy st
  in assertEqual "smartStrategy should play the ten" [Card R10 Clubs] chosen

-- deterministic game-loop test (State gameLoop)
testGameLoopDeterministic :: Test
testGameLoopDeterministic = TestCase $ do
  let gen = mkStdGen 42
      (g1,g2) = split gen
      shuffled = shuffleDeck g1 fullDeck
      (ps, restDeck) = dealInitialPlayers shuffled
      initialState = GameState
        { players = ps
        , currentIx = 0
        , drawPile = restDeck
        , discardPile = []
        , burnedPiles = []
        , rng = g2
        , finishedOrder = []
        }
      -- choose deterministic starting player and run the State-only loop
      (_, initSt) = S.runState chooseStartingPlayer initialState
      (resStr, finalSt) = S.runState gameLoop initSt
      winnerName = decideGameWinner finalSt
      expectedNames = map pName (players finalSt)
  -- final invariants: winnerName should be non-empty and either be a player name or a recognized fallback
  assertBool ("gameLoop returned empty result: " ++ show resStr) (not (null resStr))
  assertBool ("decideGameWinner not in expected names: " ++ winnerName)
             (winnerName `elem` expectedNames || winnerName `elem` ["No winner", "No players left", "Max turns reached"])

-- deterministic game-loop with extensions (StateT)
testGameLoopWithExtensionsDeterministic :: Test
testGameLoopWithExtensionsDeterministic = TestCase $ do
  let gen = mkStdGen 123
      (g1,g2) = split gen
      shuffled = shuffleDeck g1 fullDeck
      (ps, restDeck) = dealInitialPlayers shuffled
      initialState = GameState
        { players = ps
        , currentIx = 0
        , drawPile = restDeck
        , discardPile = []
        , burnedPiles = []
        , rng = g2
        , finishedOrder = []
        }
      (_, initSt) = S.runState chooseStartingPlayer initialState
  -- run the StateT loop with no extensions (deterministic)
  (winnerStr, finalSt) <- runStateT (gameLoopWithExtensions []) initSt
  let expectedNames = map pName (players finalSt)
  assertBool ("gameLoopWithExtensions returned empty winner") (not (null winnerStr))
  assertBool ("winner not recognized: " ++ winnerStr)
             (winnerStr `elem` expectedNames || winnerStr `elem` ["No winner", "No players left", "Max turns reached"])

-- playOneGame IO wrapper smoke test (non-deterministic)
testPlayOneGameIO :: Test
testPlayOneGameIO = TestCase $ do
  winner <- fmap show playOneGame
  assertBool "playOneGame returned empty winner" (not (null winner))

-- playOneGameStep4 IO wrapper smoke test (non-deterministic)
testPlayOneGameStep4IO :: Test
testPlayOneGameStep4IO = TestCase $ do
  winner <- fmap show (playOneGameStep4 [])
  assertBool "playOneGameStep4 returned empty winner" (not (null winner))

-- Step 4 (extensions) and Step 5 (smartStrategy) tests

-- helper predicates
allSameRank :: [Card] -> Bool
allSameRank [] = True
allSameRank [_] = True
allSameRank (c1:c2:cs) = rank c1 == rank c2 && allSameRank (c2:cs)

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf xs ys = all (`elem` ys) xs

-- ExtReverse8: reverseOrder should reverse player order in state
testReverseOrderExtension :: Test
testReverseOrderExtension = TestCase $
  let p0 = Player 0 "A" [] [] [] basicStrategy
      p1 = Player 1 "B" [] [] [] basicStrategy
      p2 = Player 2 "C" [] [] [] basicStrategy
      st = mkState [p0,p1,p2] 0 [] [] (mkStdGen 1)
      (_, st') = S.runState reverseOrder st
  in assertEqual "players list should be reversed"
       ["C","B","A"] (map pName (players st'))

-- ExtThree3s: customThreeBurn should burn a pile of three 3s
testThree3sBurnExtension :: Test
testThree3sBurnExtension = TestCase $
  let three3s = [Card R3 Clubs, Card R3 Diamonds, Card R3 Hearts]
      p = mkPlayer 0 "P" [] [] [] basicStrategy
      st = mkState [p] 0 [] three3s (mkStdGen 1)
      (_, st') = S.runState customThreeBurn st
  in do
    assertEqual "discard should be cleared after three-3s burn" [] (discardPile st')
    assertEqual "burnedPiles should contain the burned pile" [three3s] (burnedPiles st')

-- smartStrategy edge: chosen play is subset of visible cards and all same-rank
testSmartStrategyChoosesLegalSameRank :: Test
testSmartStrategyChoosesLegalSameRank = TestCase $
  let h = [Card R6 Clubs, Card R6 Diamonds, Card R4 Hearts]
      p = mkPlayer 0 "P" h [] [] smartStrategy
      st = mkState [p] 0 [] [] (mkStdGen 1)  -- empty discard => any play is legal
      (chosen, _) = S.runState smartStrategy st
  in do
    assertBool "chosen cards must come from visible hand" (isSubsetOf chosen h)
    assertBool "chosen cards must all be same rank" (allSameRank chosen)

-- smartStrategy edge: when only faceUp available choose legal same-rank from faceUp
testSmartStrategyUsesFaceUpWhenHandEmpty_Step5 :: Test
testSmartStrategyUsesFaceUpWhenHandEmpty_Step5 = TestCase $
  let fu = [Card R7 Clubs, Card R7 Diamonds, Card R9 Hearts]
      p = mkPlayer 0 "P" [] fu [] smartStrategy
      st = mkState [p] 0 [] [] (mkStdGen 2)
      (chosen, _) = S.runState smartStrategy st
  in do
    assertBool "chosen must come from faceUp" (isSubsetOf chosen fu)
    assertBool "chosen must be same-rank" (allSameRank chosen)

-- append new tests to existing list
tests :: Test
tests = TestList $
  [ testBasicStrategyChoosesLowestLegal
  , testBasicStrategyUsesFaceUpWhenHandEmpty
  , testBasicStrategySetsChoosesLargestGroup
  , testBasicStrategySetsSingleLegal
  , testSmartStrategyFinishesWhenPossible
  , testSmartStrategyPlaysTenWhenOnlyTenAvailable
  , testGameLoopDeterministic
  , testGameLoopWithExtensionsDeterministic
  , testPlayOneGameIO
  , testPlayOneGameStep4IO
  , testReverseOrderExtension
  , testThree3sBurnExtension
  , testSmartStrategyChoosesLegalSameRank
  , testSmartStrategyUsesFaceUpWhenHandEmpty_Step5
  ]

main :: IO Counts
main = runTestTT tests

