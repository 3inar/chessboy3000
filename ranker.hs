initialScore = 800.0
maxAdjustment = 30.0

expectedScore :: Float -> Float -> Float
expectedScore ratingA ratingB = 1/(1 + 10**(diff/400))
  where diff = ratingB - ratingA

-- Adjustment is symmetric:
-- r'a = ra + k(sa - ea)
-- r'b = rb + k (sb - eb) 
--     = k((1-sa) - (1-ea)) 
--     = rb + k(1 -sa -1 +ea) 
--     = rb - k(sa - ea)
adjustment :: Float -> Float -> Float
adjustment expected actual = maxAdjustment*(actual - expected)

-- outcome is 1 if A won
updateScore :: Float -> Float -> Float -> (Float, Float)
updateScore ratingA ratingB outcome = (ratingA + adj, ratingB - adj)
  where adj = adjustment (expectedScore ratingA ratingB) outcome

main = putStrLn $ show (updateScore 800 800 1)
