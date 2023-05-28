import System.Environment -- Some I/O stuff
import Data.List (nub, sortOn) 

import System.IO

initialScore = 800.0
maxAdjustment = 30.0

data Match = Match
  { whitePlayer :: String 
  , blackPlayer :: String 
  , result      :: Float
  }

type Rating = (String, Float)

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
updateRating :: Float -> Float -> Float -> (Float, Float)
updateRating ratingA ratingB outcome = (ratingA + adj, ratingB - adj)
  where adj = adjustment (expectedScore ratingA ratingB) outcome

-- trim whitespaces on ends of strings
-- not in use atm but might be nice to have
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
-- a Match data structure
recordToMatch :: String -> Match
recordToMatch rec = let [a, b, c] = splitComma $ rec 
  in 
    Match a b (score c)
  where
    score "1-0" = 1
    score "0-1" = 0
    score "1/2-1/2" = 0.5

-- replaces a single rating in the ranks
replaceRating :: [Rating] -> Rating -> [Rating]
replaceRating ranks newRating = map replacer ranks
  where 
    replacer x
      | fst x == fst newRating = newRating
      | otherwise              = x

-- Need this because there is a theoretical chance that lookup returns Nothing?
getRating :: [Rating] -> String -> Float
getRating ratings player = case r of
  Just r -> r
  Nothing -> -1      -- shouldn't happen 
  where r = lookup player ratings

-- takes a full list of ratings + a match and returns new ratings for 
-- the two players who played the match
newRatings :: [Rating] -> Match -> [Rating]
newRatings ratings match = [newWhite, newBlack]
  where 
    newWhite = (white, fst newRatings)
    newBlack = (black, snd newRatings)

    white = whitePlayer match
    black = blackPlayer match

    newRatings  = updateRating prevWhite prevBlack (result match)

    prevWhite = getRating ratings white
    prevBlack = getRating ratings black

-- updates a ranking (list of ratings) from a single match
updateRanking :: [Rating] -> Match -> [Rating]
updateRanking ratings match = updateBlack . updateWhite $ ratings
  where 
    updateWhite ratings = replaceRating ratings ratingWhite
    updateBlack ratings = replaceRating ratings ratingBlack
    [ratingWhite, ratingBlack] = newRatings ratings match

-- does the actual updating of the rankings from a list of matches
processMatches :: ([Rating], [Match]) -> ([Rating], [Match])
processMatches (ratings, matches) 
  | length matches == 0 = (ratings, matches)
  | otherwise           = processMatches (ratings', (tail matches))
  where ratings' = updateRanking ratings (head matches)

-- pull out unique player names from the set of matches
uniquePlayers :: [Match] -> [String]
uniquePlayers ls = nub $ foldr (++) [] $ map extractNames ls
  where 
    extractNames match = [whitePlayer match, blackPlayer match]

-- converts a rather contrived mess into a formatted string
messToString :: (Int, Rating) -> String
messToString rating = place ++ ".\t" ++ name ++ "\t\t" ++ points
  where
    place = show (fst rating)
    name = fst.snd $ rating
    points = (show.round) (snd.snd $ rating)

main = do
  inputHandle <- openFile "record.csv" ReadMode 
  hSetEncoding inputHandle utf8
  hSetEncoding stdout utf8
  contents <- hGetContents inputHandle

  let matches = map recordToMatch (lines contents)
      ratings = zip (uniquePlayers matches) (cycle [initialScore])
      ratings' = processMatches (ratings, matches)
      ordered = zip [1..] (reverse $ sortOn snd $ fst $ ratings')

  putStrLn "A218B/HDL chess federation official ranking"
  putStrLn "-------------------------------------------"
  mapM_ (hPutStrLn stdout) (map messToString ordered)
