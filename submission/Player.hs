-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import           Parser.Parser      -- This is the source for the parser from the course notes
import           Cards              -- Finally, the generic card type(s)

import           TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import           TwentyOne.Rules    -- Rules of the game
import Debug.Trace

ParseLastNumberOfLosses :: Maybe String -> Int
ParseLastNumberOfLosses = 0

-- determine a good value for our bid
-- takes the min bid, max bid and 
placeBid :: Maybe String -> (Action, String)
placeBid _ = trace ("betting turn: " ++ show minBid) (Bid minBid, "")

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
-- bidding turn at the start of a round.
playCard Nothing _ _ _ memory _ = placeBid memory
-- else play normally
playCard (Just _) _ _ _ _ hand = trace ("play turn: cards " ++ show hand) (Bid 1, "")