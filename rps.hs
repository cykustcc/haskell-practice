data Move = Rock | Paper | Scissors
	deriving (Eq, Read, Show, Enum, Bounded)

data Outcome = Lose | Tie | Win deriving (Show, Eq, Ord)

outcome :: Move -> Move -> Outcome
outcome Rock Scissors        = Win
outcome Paper Rock           = Win
outcome Scissors Paper       = Win
outcome us them | us == them = Tie
                | otherwise  = Lose

data Point = Point Double Double deriving (Show, Read)


parseMove1 :: String -> Maybe Move
parseMove1 str = case reads str of [(m, "")] -> Just m
                                   _         -> Nothing

parseMove :: String -> Maybe Move
parseMove str = case reads str of
  [(m, rest)] | ok rest -> Just m
  _       -> Nothing
  where ok = all (`elem` " \r\n")