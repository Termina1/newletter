module Task (
  Task(..)
) where

import Data.DateTime
import Network.Mail.Mime

data Task = Task {
  startDate   :: DateTime,
  endDate     :: DateTime,
  sourceRss   :: String,
  targetEmail :: Address
} deriving (Show)
