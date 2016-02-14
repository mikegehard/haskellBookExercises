module Dates where

  data DayOfWeek =
    Mon
    | Tues
    | Weds
    | Thurs
    | Fri
    | Sat
    | Sun
    deriving (Ord, Show)

  data Date =
    Date DayOfWeek Int


  instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tues Tues = True
    (==) Weds Weds = True
    (==) Thurs Thurs = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False

  instance Eq Date where
    (==)
      (Date dayOfWeek monthNumber)
      (Date dayOfWeek' monthNumber') =
        dayOfWeek == dayOfWeek' && monthNumber == monthNumber'
