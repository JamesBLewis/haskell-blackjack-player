-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import           Parser.Instances      -- This is the source for the parser from the course notes
import           Cards              -- Finally, the generic card type(s)

import           TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import           TwentyOne.Rules    -- Rules of the game
import           Debug.Trace
import Data.Maybe

import Data.Char

import JSON

import Data.Foldable

-- | given a set of rules apply basic strategy to be universally correct decisions
attemptPairSplitting :: Hand -> Rank -> Points -> Maybe Action
attemptPairSplitting [Card _ Ace, Card _ Ace] _ points = Just $ Split points
attemptPairSplitting [Card _ Ten, Card _ Ten] _ _ = Nothing
attemptPairSplitting [Card _ Nine, Card _ Nine] upCard points = case upCard of
    Seven -> Nothing
    Ten -> Nothing
    Ace -> Nothing
    _ -> Just $ Split points
attemptPairSplitting [Card _ Eight, Card _ Eight] _ points = Just $ Split points
attemptPairSplitting [Card _ Seven, Card _ Seven] upCard points | upCard > Seven = Nothing | otherwise = Just $ Split points
attemptPairSplitting [Card _ Six, Card _ Six] upCard points | upCard > Six = Nothing | otherwise = Just $ Split points -- this is slightly wrong
attemptPairSplitting [Card _ Five, Card _ Five] _ _ = Nothing
attemptPairSplitting [Card _ Four, Card _ Four] _ _ = Nothing
attemptPairSplitting [Card _ Three, Card _ Three] upCard points | upCard > Seven = Nothing | otherwise = Just $ Split points -- this is slightly wrong
attemptPairSplitting [Card _ Two, Card _ Two] upCard points | upCard > Seven = Nothing | otherwise = Just $ Split points -- this is slightly wrong
attemptPairSplitting _ _ _ = Nothing -- default case

-- | next set of rules
attemptSoftTotals :: Hand -> Rank -> Points -> Maybe Action
attemptSoftTotals [Card _ Ace, Card _ Nine] _ _ = Just Stand
attemptSoftTotals [Card _ Ace, Card _ Eight] Six points = Just $ DoubleDown points
attemptSoftTotals [Card _ Ace, Card _ Eight] _ _ = Just Stand
attemptSoftTotals [Card _ Ace, Card _ Seven] upCard points 
    |  upCard < Seven  = Just $ DoubleDown points 
    |  upCard == Seven || upCard == Eight = Just Stand 
    | otherwise = Just Hit
attemptSoftTotals [Card _ Ace, Card _ Six] upCard points 
    | upCard > Two && upCard < Seven = Just $ DoubleDown points
    | otherwise = Just Hit
attemptSoftTotals [Card _ Ace, Card _ Five] upCard _ 
    | upCard == Four || upCard == Five || upCard == Six = Just Hit
attemptSoftTotals [Card _ Ace, Card _ Four] upCard _ 
    | upCard == Four || upCard == Five || upCard == Six = Just Hit
attemptSoftTotals [Card _ Ace, Card _ Three] upCard _ 
    | upCard == Five || upCard == Six = Just Hit
attemptSoftTotals [Card _ Ace, Card _ Two] upCard _ 
    | upCard == Five || upCard == Six = Just Hit
attemptSoftTotals _ _ _ = Nothing 

canDoubleDown :: Hand -> Bool
canDoubleDown cards = length cards == startingNumCards

-- | apply rules for hard totals
attemptHardTotals :: Hand -> Rank -> Points -> Maybe Action
attemptHardTotals hand upCard points = case handCalc hand of
    17 -> Just Stand 
    16 -> Just $ if  upCard < Seven then  Stand else  Hit 
    15 -> Just $ if  upCard < Seven then  Stand else  Hit  
    14 -> Just $ if  upCard < Seven then  Stand else  Hit  
    13 -> Just $ if  upCard < Seven then  Stand else  Hit  
    12 -> Just $ if  upCard < Seven && upCard > Three then  Stand else  Hit  
    11 -> Just $ if canDoubleDown hand then DoubleDown (points) else Hit
    10 -> Just $ if canDoubleDown hand && upCard < Ten then DoubleDown (points) else  Hit  
    9  -> Just $ if canDoubleDown hand && upCard < Seven &&  upCard > Two then DoubleDown (points) else  Hit  
    8  -> Just Hit 
    _  -> Nothing


-- | take a maybe Int value (generally for a bid) and return an Int
getIntOrZero :: Maybe Points -> Int
getIntOrZero Nothing = 0
getIntOrZero (Just number) = number

-- employ the Martingale System - https://upswingpoker.com/the-best-blackjack-betting-strategy-basic-explanation/
chooseBidValue :: Int -> Int -- fromIntegral $ 
chooseBidValue points = if startingPoints-points <= points && startingPoints-points <= maxBid then max (startingPoints-points) minBid else minBid

pointsToTuple :: [PlayerPoints] -> [(PlayerId, Points)]
pointsToTuple = foldl (\a (PlayerPoints playerId points) -> (playerId, points):a) []

assocToTuple :: Assoc -> [(String, JsonValue)]
assocToTuple = foldl (\a (key, value) -> (key, value):a) []

-- filter points by ID
-- get a given players remaining points
getPointsById :: [PlayerPoints] -> PlayerId -> Int
getPointsById points playerId = getIntOrZero $ lookup playerId $ pointsToTuple points

getInteger :: Maybe JsonValue -> Int
getInteger Nothing = 0
getInteger (Just (JsonRational a)) = round a

getArray :: Maybe JsonValue -> [JsonValue]
getArray Nothing = error "invalid string parsing operation"
getArray (Just (JsonArray a)) = a

getArrayFromMemory :: [Char] -> Assoc -> [JsonValue]  
getArrayFromMemory c a = getArray $ lookup c (assocToTuple a)

getRationalFromMemory :: [Char] -> Assoc -> Int  
getRationalFromMemory c a = getInteger $ lookup c (assocToTuple a)

-- TODO: use show for JsonValue
updateMemory :: Int -> Int -> [String] -> String
updateMemory lastBid points actionsThisTurn = "{\"lastBid\":"++show lastBid++",\"oldPoints\":"++show points++",\"actionsThisTurn\":"++ show actionsThisTurn ++"}"

-- | place a bid given a number of losses
placeBid :: Int -> Assoc -> (Action, String)
placeBid points value = do
    let bid = chooseBidValue points
    (Bid bid, updateMemory bid points [])

makeForcedMove' :: [String] -> Maybe Action
makeForcedMove' ["DoubleDown", "Hit"] = Just Stand
makeForcedMove' _ = Nothing

makeForcedMove'' :: [String] -> Maybe Action
makeForcedMove'' ["DoubleDown"] = Just Hit
makeForcedMove'' _ = Nothing

-- credit to https://stackoverflow.com/questions/17252851/how-do-i-take-the-last-n-elements-of-a-list
lastN' :: Int -> [a] -> [a]
lastN' n xs = foldl' (const . drop 1) xs (drop n xs)

makeForcedMove :: [String] -> Maybe Action
makeForcedMove ["DoubleDown"] = Just Hit
makeForcedMove [_] = Nothing
-- otherwise there is more then 1 value
makeForcedMove a = (makeForcedMove' $ lastN' 2 a) |||| (makeForcedMove'' $ lastN' 1 a)

toStringArray :: [JsonValue] -> [String]
toStringArray = foldl (\a (JsonString s) -> a++[s]) []


-- | make a new operator to 'or' possible actions together
(||||) :: Maybe Action -> Maybe Action -> Maybe Action
(||||) Nothing action2 = action2
(||||) action1 _ = action1

-- | cheat with a strategy table: https://www.blackjackapprenticeship.com/blackjack-strategy-charts/
applyFixedStrategy :: Hand -> Card -> Int -> [String] -> Maybe Action
applyFixedStrategy hand upCard points actionsThisTurn = 
    trace ("actions this turn"++show actionsThisTurn) (makeForcedMove actionsThisTurn)
    |||| attemptPairSplitting hand (getRank upCard) points 
    |||| attemptSoftTotals hand (getRank upCard) points 
    |||| attemptSoftTotals (reverse hand) (getRank upCard) points
    |||| trace "attemptHardTotals" (attemptHardTotals hand (getRank upCard) points)
    |||| Just Stand -- for testing only

justValueOrError :: Maybe Action -> Action 
justValueOrError (Just value) = value
justValueOrError Nothing = error "no valid action taken by player"

wasDoubleDown :: Action -> Bool
wasDoubleDown (DoubleDown _) = True
wasDoubleDown _ = False

trimAmount :: String -> String
trimAmount xs = [ x | x <- xs, not (x `elem` " 1234567890") ]


playHand :: Hand -> Card -> Assoc -> Int -> (Action, String)
playHand h c j p = do
    let lastBid = fromIntegral (getRationalFromMemory "lastBid" j)
    let actionsThisTurn = toStringArray $ getArrayFromMemory "actionsThisTurn" j
    let action = justValueOrError $ applyFixedStrategy h c lastBid actionsThisTurn
    trace ("took action: "++show action) (action, updateMemory lastBid p (actionsThisTurn++[trimAmount $ show action]))

reportParserErrors :: ParseResult Assoc -> Assoc
reportParserErrors (Result input r) = r
reportParserErrors (Error message) = error ("could not parse memory: " ++ show message)

parseJsonMemoryString :: Maybe String -> Assoc
parseJsonMemoryString (Just string) = reportParserErrors $ parse jsonObject (trace ("parsing memory string: " ++ string) string)
parseJsonMemoryString _ = trace "memory string was empty so making a new one." (parseJsonMemoryString $ (Just $ updateMemory 0 startingPoints []))

emptyOrString :: Maybe String -> String
emptyOrString Nothing = ""
emptyOrString (Just s) = s

-- | This function is called once it's your t urn, and keeps getting called until your turn ends.
playCard :: PlayFunc
-- bidding turn at the start of a round.
playCard Nothing allPoints _ thisPlayerId memory _ = trace  "Bidding turn" $ placeBid ( getPointsById allPoints thisPlayerId) (parseJsonMemoryString memory)
-- else play normally
playCard (Just upCard) allPoints _ thisPlayerId memory hand = trace ("play turn: cards " ++ show hand) $ playHand hand upCard (parseJsonMemoryString memory) (getPointsById allPoints thisPlayerId)