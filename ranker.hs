import System.Environment -- Some I/O stuff
import Data.List (nub) -- remove duplicates from list

initialScore = 800.0
maxAdjustment = 30.0

data Match = Match
  { white :: String, 
    black :: String, 
    result :: Float
  }

-- Now that our different fields have names, each name actually acts as a function allows us to extract that value from the larger object. We no longer have to deconstruct the entire object via pattern matching to get each value out.

-- expected score is a logistic function of the difference in ratings
-- can be interpreted as a predicted probability of winning
expectedScore :: Float -> Float -> Float
expectedScore ratingA ratingB = 1/(1 + 10**(diff/400))
  where diff = ratingB - ratingA

-- Adjustment is symmetric:
-- r'a = ra + k(sa - ea)
-- r'b = rb + k (sb - eb) 
--     = k((1-sa) - (1-ea)) 
--     = rb + k(1 -sa -1 +ea) 
--     = rb - k(sa - ea)
--
-- we see that it is a linear update of the rating based on the difference
-- in expected score and actual score
adjustment :: Float -> Float -> Float
adjustment expected actual = maxAdjustment*(actual - expected)

-- outcome is 1 if A won
updateScore :: Float -> Float -> Float -> (Float, Float)
updateScore ratingA ratingB outcome = (ratingA + adj, ratingB - adj)
  where adj = adjustment (expectedScore ratingA ratingB) outcome


-- trim whitespaces on ends of strings
-- not in use atm.
-- trim :: String -> String
-- trim = reverse . stripLeft . reverse . stripLeft
--   where
--     stripLeft s
--       | s == " " || s == "" = ""
--       | head s == ' '       = stripLeft . tail $ s
--       | otherwise           = s

-- splits a record on its commas
splitComma :: String -> [String]
splitComma "" = []
splitComma str = term : (splitComma rest)
  where 
    term = takeWhile (/= ',') str
    rest = drop ((length term) + 1) str

-- takes a line from the records "Player A, Player B, 1-0" and spits out
-- (("Player A", "Player B"), 1.0)
recordToMatch :: String -> Match
recordToMatch rec = let [a, b, c] = splitComma $ rec in Match a b (score c)
  where
    score "1-0" = 1
    score "0-1" = 0
    score "1/2-1/2" = 0.5

-- want to use foldr somehow and take like ranking
-- updateRating :: [(String, Float)] -> ((String, String), Float) -> [(String, Float)]
-- updateRating = do
  

-- pull out unique player names from the set of matches
uniquePlayers :: [Match] -> [String]
uniquePlayers ls = nub $ foldr (++) [] $ map extractNames ls
  where 
    extractNames match = [white match, black match]

main = do
  contents <- readFile "record.csv"

  let matches = map recordToMatch (lines contents)

  putStrLn $ show (uniquePlayers matches)
