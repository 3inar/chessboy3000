import System.Environment -- Some I/O stuff

initialScore = 800.0
maxAdjustment = 30.0

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
-- not in use atm....
trim :: String -> String
trim = reverse . stripLeft . reverse . stripLeft
  where
    stripLeft s
      | s == " " || s == "" = ""
      | head s == ' '       = stripLeft . tail $ s
      | otherwise           = s

-- splits a record on its commas
splitComma :: String -> [String]
splitComma "" = []
splitComma str = taken : (splitComma rest)
  where 
    term = takeWhile (/= ',')
    taken = term str
    rest = drop ((length taken) + 1) str

-- takes a line from the records "Player A, Player B, 1-0" and spits out
-- (("Player A", "Player B"), 1.0)
recordToTuple :: String -> ((String, String), Float)
recordToTuple rec = tuplefy . splitComma $ rec
  where
    tuplefy [a, b, c] = ((a, b), score c)
    score "1/2-1/2" = 0.5
    score "1-0" = 1
    score "0-1" = 0


main = do
  contents <- readFile "record.csv"

  let matches = lines contents

  putStrLn $ show (map recordToTuple matches)
