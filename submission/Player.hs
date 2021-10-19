-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import           Parser.Parser      -- This is the source for the parser from the course notes
import           Cards              -- Finally, the generic card type(s)

import           TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import           TwentyOne.Rules    -- Rules of the game
import           Debug.Trace

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

-- lookup playerId
lookupPlayerId :: [PlayerPoints] -> PlayerId -> Maybe Points
lookupPlayerId [(a,b)] id = 

-- filter points by ID
-- get a given players remaining points
getPointsById :: [PlayerPoints] -> PlayerId -> Int
getPointsById player playerId = getIntOrZero $ lookup playerId (player)


-- | place a bid given a number of losses
placeBid :: Int -> [(String, Int)] -> (Action, String)
placeBid points list = (chooseBidValue points $ getIntOrZero $ lookup "consecutiveLosses" list, "")

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
-- bidding turn at the start of a round.
playCard Nothing allPoints _ thisPlayerId memory _ = placeBid ( getPointsById allPoints thisPlayerId) (parseJsonMemoryString memory)
-- else play normally
playCard (Just _) _ _ _ _ hand = trace ("play turn: cards " ++ show hand) (Bid 1, "")