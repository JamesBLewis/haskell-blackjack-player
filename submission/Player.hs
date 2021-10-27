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

import Data.List

import Data.Function

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

-- | enforce a hand length rule to determine if this player is allowed to double down or not
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
-- in conjunction with card counting
chooseBidValue :: Int -> Int -> Int -- fromIntegral $ 
chooseBidValue points advantage = if startingPoints-points <= points && startingPoints-points <= maxBid then max (startingPoints-points) minBid else max (minBid + advantage) minBid

pointsToTuple :: [PlayerPoints] -> [(PlayerId, Points)]
pointsToTuple = Data.List.foldl (\a (PlayerPoints playerId points) -> (playerId, points):a) []

assocToTuple :: Assoc -> [(String, JsonValue)]
assocToTuple = Data.List.foldl (\a (key, value) -> (key, value):a) []

-- filter points by ID
-- get a given players remaining points
getPointsById :: [PlayerPoints] -> PlayerId -> Int
getPointsById points playerId = getIntOrZero $ lookup playerId $ pointsToTuple points

-- | convert a jsonValue into an int
getInteger :: Maybe JsonValue -> Int
getInteger Nothing = 0
getInteger (Just (JsonRational a)) = round a
getInteger _ = error "invalid int"

-- | cast a JsonValue as a list of JsonValues so that it is easier to interoperate
getArray :: Maybe JsonValue -> [JsonValue]
getArray (Just (JsonArray a)) = a
getArray _ = error "invalid string parsing operation"

-- | given an associative and a string. return an array
getArrayFromMemory :: [Char] -> Assoc -> [JsonValue]  
getArrayFromMemory c a = getArray $ lookup c (assocToTuple a)

-- | get a rational number from the associative
getRationalFromMemory :: [Char] -> Assoc -> Int  
getRationalFromMemory c a = getInteger $ lookup c (assocToTuple a)

-- TODO: implement show for JsonValue
updateMemory :: Int -> Int -> [String] -> Int -> Int -> Int -> Int -> String
updateMemory lastBid points actionsThisTurn cardCount remainingCards startingCardCount startingRemainingCards = "{\"lastBid\":"++show lastBid++",\"oldPoints\":"++show points++",\"actionsThisTurn\":"++ show actionsThisTurn ++",\"cardCount\":"++ show cardCount ++",\"remainingCards\":"++ show remainingCards ++",\"startingCardCount\":"++ show startingCardCount ++",\"startingRemainingCards\":"++ show startingRemainingCards ++"}"
 
-- | determine the high-low value to modify our bid with
calculateAdvantage :: Int -> Int -> Int
calculateAdvantage cardCount remainingCards = cardCount `div` (max (remainingCards `div` 52) 1)

-- | place a bid given a number of losses
placeBid :: Int -> Assoc -> (Action, String)
placeBid points value = do
    let cardCount = fromIntegral (getRationalFromMemory "cardCount" value)
    let remainingCards = fromIntegral (getRationalFromMemory "remainingCards" value)
    let safeRemainingCards = if remainingCards < 1 then  52*3 else remainingCards
    let safeCardCount = if remainingCards < 0 then  0 else cardCount
    let bid = chooseBidValue points $ calculateAdvantage safeCardCount safeRemainingCards
    (Bid bid, updateMemory bid points [] safeCardCount safeRemainingCards safeCardCount safeRemainingCards)

-- | pattern match specific turn history 
makeForcedMove' :: [String] -> Maybe Action
makeForcedMove' ["DoubleDown", "Hit"] = Just Stand
makeForcedMove' _ = Nothing

-- | pattern match specific turn history 
makeForcedMove'' :: [String] -> Maybe Action
makeForcedMove'' ["DoubleDown"] = Just Hit
makeForcedMove'' _ = Nothing

-- credit to https://stackoverflow.com/questions/17252851/how-do-i-take-the-last-n-elements-of-a-list
lastN' :: Int -> [a] -> [a]
lastN' n xs = Data.List.foldl' (const . Data.List.drop 1) xs (Data.List.drop n xs)

-- | attempt to find a valid action based on the history for this turn
makeForcedMove :: [String] -> Maybe Action
makeForcedMove ["DoubleDown"] = Just Hit
makeForcedMove [_] = Nothing
-- otherwise there is more then 1 value
makeForcedMove a = (makeForcedMove' $ lastN' 2 a) |||| (makeForcedMove'' $ lastN' 1 a)

-- | take an array of JsonValues and fold it into an array of strings to use later 
toStringArray :: [JsonValue] -> [String]
toStringArray = Data.List.foldl (\a (JsonString s) -> a++[s]) []


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
    |||| Just Stand

-- | errors if a given action doesn't actually exist
justValueOrError :: Maybe Action -> Action 
justValueOrError (Just value) = value
justValueOrError Nothing = error "no valid action taken by player"

-- | remove any numbers from a string. This is useful when we store actions as a string
trimAmount :: String -> String
trimAmount xs = [ x | x <- xs, not (x `elem` " 1234567890") ]

-- | determine a low-high value for a given card rank
assignCardCountingScore :: Rank -> Int 
assignCardCountingScore rank 
    | rank < Seven = 1
    | rank < Ten = 0
    | otherwise = -1

-- | determine low-high values for a list of cards (and the upCard)
calculateNewCardCount :: Rank -> [Rank] -> Int
calculateNewCardCount upCard newCards = assignCardCountingScore upCard + sum (assignCardCountingScore <$> newCards)

-- | convert stored card ranks into rank types
stringToRank :: String -> Rank
stringToRank "A" = Ace   
stringToRank "2" = Two    
stringToRank "3" = Three  
stringToRank "4" = Four  
stringToRank "5" = Five   
stringToRank "6" = Six    
stringToRank "7" = Seven  
stringToRank "8" = Eight  
stringToRank "9" = Nine   
stringToRank "T" = Ten    
stringToRank "J" = Jack   
stringToRank "Q" = Queen  
stringToRank "K" = King   
stringToRank _ = error "card rank invalid"

-- | handle all logic around choosing an action and updating memory
playHand :: Hand -> Card -> Assoc -> Int -> [Rank] -> (Action, String)
playHand h c j p currentHands = do
    let lastBid = fromIntegral (getRationalFromMemory "lastBid" j)
    let actionsThisTurn = toStringArray $ getArrayFromMemory "actionsThisTurn" j
    let action = justValueOrError $ applyFixedStrategy h c lastBid actionsThisTurn
    let startingRemainingCards = (getRationalFromMemory "startingRemainingCards" j)  - 1
    let remainingCards = startingRemainingCards - length currentHands
    let statingCardCount = fromIntegral (getRationalFromMemory "StartingCardCount" j)
    let cardCount = statingCardCount+(calculateNewCardCount (getRank c) currentHands)
    (action, updateMemory lastBid p (actionsThisTurn++[trimAmount $ show action]) cardCount remainingCards statingCardCount startingRemainingCards)

-- | throw an error if parsing didn't work correctly
reportParserErrors :: ParseResult Assoc -> Assoc
reportParserErrors (Result _ r) = r
reportParserErrors (Error message) = error ("could not parse memory: " ++ show message)

-- | parse memory
parseJsonMemoryString :: Maybe String -> Assoc
parseJsonMemoryString (Just string) = reportParserErrors $ parse jsonObject (trace ("parsing memory string: " ++ string) string)
parseJsonMemoryString _ = trace "memory string was empty so making a new one." (parseJsonMemoryString $ (Just $ updateMemory 0 startingPoints [] 0 (52*3) 0 (52*3)))

-- | fold a list of player info in to a list of card ranks to make it simpler to deal with later
getRankOfAllHands :: [PlayerInfo] -> [Rank]
getRankOfAllHands = Data.List.foldl (\a (PlayerInfo _ hand) -> (getRank <$> hand)++a) []

-- | This function is called once it's your t urn, and keeps getting called until your turn ends.
playCard :: PlayFunc
-- bidding turn at the start of a round.
playCard Nothing allPoints _ thisPlayerId memory _ = trace  "Bidding turn" $ placeBid ( getPointsById allPoints thisPlayerId) (parseJsonMemoryString memory)
-- else play normally
playCard (Just upCard) allPoints playerInfo thisPlayerId memory hand = trace ("play turn: upCard " ++ show upCard) $ playHand hand upCard (parseJsonMemoryString memory) (getPointsById allPoints thisPlayerId) (getRankOfAllHands playerInfo)