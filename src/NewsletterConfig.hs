module NewsletterConfig (
  NewsletterConfig(..)
) where

import Data.Text.Lazy
import Network.Mail.Mime

data NewsletterConfig = NewsletterConfig {
  from     :: Address,
  host     :: Text,
  username :: Text,
  password :: Text
} deriving (Show)
