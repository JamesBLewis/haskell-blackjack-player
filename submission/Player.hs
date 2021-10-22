-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import           Parser.Parser      -- This is the source for the parser from the course notes
import           Cards              -- Finally, the generic card type(s)

import           TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import           TwentyOne.Rules    -- Rules of the game
import           Debug.Trace

-- | given a set of rules determine what to do when you see a given set of cards 
attemptPairSplitting :: Hand -> Card -> Points -> Maybe Action
attemptPairSplitting [Card _ Ace, Card _ Ace] _ points = Just $ Split points
attemptPairSplitting [Card _ Ten, Card _ Ten] _ _ = Just Stand -- not actually sure about this...
attemptPairSplitting [Card _ Nine, Card _ Nine] upCard points = case upCard of
    Card _ Seven -> Just Stand -- not actually sure about this...
    Card _ Ten -> Just Stand-- not actually sure about this...
    Card _ Ace -> Just Stand-- not actually sure about this...
    _ -> Just $ Split points
attemptPairSplitting [Card _ Eight, Card _ Eight] _ points = Just $ Split points
attemptPairSplitting [Card _ Seven, Card _ Seven] upCard points | upCard > 7 = Just $ Stand otherwise $ Split points
attemptPairSplitting [Card _ Six, Card _ Six] upCard points | upCard > 6 = Just $ Stand otherwise $ Split points -- this is slightly wrong
attemptPairSplitting [Card _ Five, Card _ Five] _ _ = Just Stand
attemptPairSplitting [Card _ Four, Card _ Four] _ _ = Just Stand
attemptPairSplitting [Card _ Three, Card _ Three] upCard points | upCard > 7 = Stand otherwise $ Split points -- this is slightly wrong
attemptPairSplitting [Card _ Two, Card _ Two] upCard points | upCard > 7 = Stand otherwise $ Split points -- this is slightly wrong
attemptPairSplitting _ _ _ = Nothing -- default case



-- take our memory string in which we encode as a Json object and return it in a usable form
-- parseJsonMemoryString :: Maybe String -> [(String, JsonValue)]
parseJsonMemoryString :: Maybe String -> [(String, Int)]
parseJsonMemoryString _ = [(show "consecutiveLosses", 0)]


-- | take a maybe Int value (generally for a bid) and return an Int
pointsToInt :: Maybe Points -> Int
pointsToInt Nothing = 0
pointsToInt (Just number) = number

-- | take a maybe Int value (generally for a bid) and return an Int
getIntOrZero :: Maybe Int -> Int
getIntOrZero Nothing = 0
getIntOrZero (Just number) = number

-- employ the Martingale System - https://upswingpoker.com/the-best-blackjack-betting-strategy-basic-explanation/
chooseBidValue :: Int -> Int -> Action
chooseBidValue _ 0 = Bid minBid
chooseBidValue points i = Bid $ foldr min points [(i * minBid), maxBid]

pointsToTuple :: [PlayerPoints] -> [(PlayerId, Points)]
pointsToTuple p = foldl (\a (PlayerPoints playerId points) -> (playerId, points):a) [] p

-- filter points by ID
-- get a given players remaining points
getPointsById :: [PlayerPoints] -> PlayerId -> Int
getPointsById points playerId = getIntOrZero $ lookup playerId $ pointsToTuple points

-- | place a bid given a number of losses
placeBid :: Int -> [(String, Int)] -> (Action, String)
placeBid points list = (chooseBidValue points $ getIntOrZero $ lookup "consecutiveLosses" list, "")

-- | cheat with a strategy table: https://www.blackjackapprenticeship.com/wp-content/uploads/2018/08/BJA_Basic_Strategy.jpg
playHand :: Card -> Hand -> Action
playHand upCard hand = Just $ head $ filter (not Nothing) $ [attemptPairSplitting, attemptPairSplitting, attemptPairSplitting, attemptPairSplitting] <*> [hand, upCard, minBid]
--playHand upCard hand = attemptPairSplitting upCard hand (||||) (attemptSoftTotals upCard hand  (||||) (attemptHardTotals upCard hand (||||) attemptLateSurrender upCard hand))

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
-- bidding turn at the start of a round.
playCard Nothing allPoints _ thisPlayerId memory _ = trace  "Bidding turn" $ placeBid ( getPointsById allPoints thisPlayerId) (parseJsonMemoryString memory)
-- else play normally
playCard (Just upCard) _ _ _ _ hand = trace ("play turn: cards " ++ show hand) (playHand upCard hand, "")




