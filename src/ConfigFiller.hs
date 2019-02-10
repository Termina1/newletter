{-# LANGUAGE ApplicativeDo, OverloadedStrings #-}

module ConfigFiller (
  fillConfig
) where

import Config
import Config.Schema.Load
import Config.Schema.Spec
import Control.Monad.Catch
import Data.String
import Data.Text.Lazy
import Data.Time.Clock
import Data.Time.Format
import Network.Mail.Mime
import NewsletterConfig
import Schedule

addressFromSpec :: ValueSpecs Address
addressFromSpec = sectionsSpec "address from" $ do
  name <- reqSection "name" "Name for email to send letters from"
  address <- reqSection "address" "Address for email to send letters from"
  return (Address (Just name) address)

newsletterConfigSpec :: ValueSpecs NewsletterConfig
newsletterConfigSpec = sectionsSpec "config" $ do
  username <- reqSection  "username" "Username for SMTP server"
  password <- reqSection "password" "Password for SMTP server"
  host <- reqSection "host" "Host for SMTP server"
  address <- reqSection' "addressFrom" addressFromSpec "Address to send email from"
  return $ NewsletterConfig address (fromStrict host) (fromStrict username) (fromStrict password)

timeSpec :: ValueSpecs UTCTime
timeSpec = customSpec "date" stringSpec (\date -> parseTimeM True defaultTimeLocale "%Y-%m-%d" date)

scheduleSpec :: ValueSpecs Schedule
scheduleSpec = sectionsSpec "schedule config" $ do
  sourceRss <- reqSection' "sourceRss" stringSpec "Rss feed to convert to newsletter"
  targetEmail <- reqSection "targetEmail" "Email to send newsletter to"
  startDate <- reqSection' "startDate" timeSpec "Date newsletter starts"
  return $ Schedule Weekly startDate sourceRss (Address Nothing targetEmail)


newsLetterSpec :: ValueSpecs (NewsletterConfig, [Schedule])
newsLetterSpec = sectionsSpec "whole config" $ do
  config <- reqSection' "config" newsletterConfigSpec "Config for SMTP sending emails"
  schedules <- reqSection' "schedules" (oneOrList scheduleSpec) "Schedules for newsletters"
  return (config, schedules)


fillConfig :: String -> IO (Either String (NewsletterConfig, [Schedule]))
fillConfig fileName = catchAll (
  fmap Right $ loadValueFromFile newsLetterSpec (fromString fileName)
  ) (\e -> return $ Left (show e))
