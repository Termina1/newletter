module Schedule (
  Schedule (..),
  SchedulePeriod (..)
) where

import Data.DateTime
import Network.Mail.Mime

data SchedulePeriod = Weekly
  deriving (Show)

data Schedule = Schedule {
  period :: SchedulePeriod,
  start  :: DateTime,
  source :: String,
  email  :: Address
} deriving (Show)
