{-# LANGUAGE OverloadedStrings #-}

module Main where
import ConfigFiller
import NewsletterConfig
import Performer
import Scheduler
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  result <- fillConfig $ head args
  case result of
    Left err -> putStrLn err
    Right (config, schedules) -> do
      tasks <- schedule schedules
      perform tasks config
      return ()
