module Scheduler (
  schedule
) where

import Data.DateTime
import Data.Time.Calendar
import Data.Time.Clock    (utctDay)
import Schedule
import Task

convertScheduleToTask :: Schedule -> DateTime -> Task
convertScheduleToTask (Schedule Weekly start source email) ctime =
  Task (addMinutes (-60 * 24 * 7) ctime) ctime source email

checkSchedule :: DateTime -> [Task] -> Schedule -> [Task]
checkSchedule ctime tasks schedule@(Schedule period start source email)
  | checkDay period start ctime = (convertScheduleToTask schedule ctime) : tasks
  | otherwise = tasks
  where
    checkDay Weekly start currentTime = (diffDays (utctDay start) (utctDay currentTime)) `mod` 7 == 0


schedule :: [Schedule] -> IO [Task]
schedule schedules = do
  time <- getCurrentTime
  return $ foldl (checkSchedule time) [] schedules
