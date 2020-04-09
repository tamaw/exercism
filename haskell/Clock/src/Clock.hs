module Clock (addDelta, fromHourMin, toString) where
import Text.Printf

data Clock = Clock {
      minute :: Int
    , hour :: Int
} deriving (Show, Eq)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock calcMin calcHour
  where calcHour = (hour + min `div` 60) `mod` 24 
        calcMin = min `mod` 60

toString :: Clock -> String
toString (Clock {minute = m, hour = h}) = printf "%02d:%02d" h m

addDelta :: Int -> Int -> Clock -> Clock
addDelta addHour addMin (Clock {minute = m, hour = h}) = Clock calcMin calcHour
  where calcHour = (h + addHour + ((m + addMin) `div` 60)) `mod` 24
        calcMin  = (m + addMin) `mod` 60

