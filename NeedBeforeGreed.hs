module NeedBeforeGreed where

  import Roller
  import Data.List

  -- Types of roll for Need before Greed.
  -- Choosing Pass skips the game
  -- Greed and Need are sorted on highest result of a roll1d100
  -- Need rolls take priority
  data RollType = Pass | Greed | Need  deriving (Eq, Show)

  -- A NB4G has 3 fields, the name of the roller, the type of roll and the result
  data NB4G = NB4G { name     :: String
                    ,rolltype :: RollType
                    ,result   :: Int
                    } deriving (Show)

  -- takes a list of NB4G and sorts it as follows:
  -- Filters out any Pass
  -- maps roll1d100 to the Greed rolls and sorts on the result
  -- maps roll1d100 to the Need rolls and sorts on the result
  -- appends Greed ++ Need so that the higest Need roll is at the end of the list
  rollNB4G :: [NB4G]->[NB4G]
  rollNB4G listofrollers =
    sortOn (result) (map (roll1d100) ((filter (takeGreed) listofrollers)))
    ++
    sortOn (result) (map (roll1d100) ((filter (takeNeed) listofrollers)))

    -- rollNB4G [(NB4G "Pass1" Pass 0), (NB4G "Greed1" Greed 0), (NB4G "Greed2" Greed 0), (NB4G "Need1" Need 0), (NB4G "Need2" Need 0), (NB4G "Greed3" Greed 0)]

  -- Takes a list of NB4G greeds and returns the last element as the winner
  -- assumes the list has already gone through rollNB4G
  winnerNB4G :: [NB4G]->NB4G
  winnerNB4G listofrolls =
    last listofrolls
    -- winnerNB4G (rollNB4G [(NB4G "Pass1" Pass 0), (NB4G "Greed1" Greed 0), (NB4G "Greed2" Greed 0), (NB4G "Need1" Need 0), (NB4G "Need2" Need 0), (NB4G "Greed3" Greed 0)])

  -- rolls a 1d100 and puts it as the result of a NB4G that has the same name and type as the given NB4G
  roll1d100 :: NB4G->NB4G
  roll1d100 roller =
    NB4G (name roller) (rolltype roller) (total (rollxdy 1 100))

  -- returns false if the rolltype of roller isnt Pass
  filterPass :: NB4G -> Bool
  filterPass roller =
    (rolltype roller) /= Pass

  -- returns true if rolltype is Greed
  takeGreed :: NB4G->Bool
  takeGreed roller =
    (rolltype roller) == Greed

  -- returns true if rolltype is Need
  takeNeed :: NB4G->Bool
  takeNeed roller =
    (rolltype roller) == Need
