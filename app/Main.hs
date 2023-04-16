{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson               (FromJSON, ToJSON, decode, object)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson.Types         (ToJSON)
import qualified Data.ByteString.Lazy     as BSL
import           Data.Foldable            (foldl', toList)
import qualified Data.Heap                as Heap
import           Data.Maybe               (fromJust)
import           Data.Time                (LocalTime, localTimeToUTC)
import           Data.Time.Clock          (addUTCTime, getCurrentTime)
import           Data.Time.LocalTime      (TimeZone, ZonedTime,
                                           getCurrentTimeZone, getZonedTime,
                                           hoursToTimeZone, utcToLocalTime)
import           GHC.Generics
import           System.Directory         (doesFileExist)
import           System.FilePath          (FilePath)
import           System.IO                (IOMode (WriteMode), hFlush, stdout,
                                           withFile)

data Calendar = Calendar
    { timeBlocks :: [TimeBlock]
    } deriving (Show, Generic, FromJSON, ToJSON)

data TimeBlock = TimeBlock
    { startTime :: LocalTime
    , endTime   :: LocalTime
    , task      :: Task
    } deriving (Show, Generic, FromJSON, ToJSON)

data Task = Task
  { description   :: String
  , impact        :: Int
  , urgency       :: Int
  , hoursEstimate :: Int -- should be many <= 1, one remainder > 1
  , roi           :: Double
  } deriving (Show, Generic, ToJSON, FromJSON)

instance Eq Task where
    t1 == t2 =
        (description t1, impact t1, urgency t1, hoursEstimate t1, roi t1)
        == (description t2, impact t2, urgency t2, hoursEstimate t2, roi t2)

-- this ensures that a higher roi gets precedence
-- if we flipped the order, it would turn our max into a min heap
instance Ord Task where
  t1 `compare` t2 = (roi t2) `compare` (roi t1)

-- Prompt the user for a string input
promptString :: String -> IO String
promptString prompt = do
  putStr prompt
  hFlush stdout
  getLine

-- Prompt the user for an integer input
promptInt :: String -> IO Int
promptInt prompt = do
  input <- promptString prompt
  return (read input :: Int)

-- Prompt the user for a Task input
promptTask :: IO Task
promptTask = do
  description <- promptString "Enter a task description: "
  impact <- promptInt "Enter the impact of the task (1-10): "
  urgency <- promptInt "Enter the urgency of the task (1-10): "
  hoursEstimate <- promptInt "Enter the effort estimate of the task (in hours): "

  let roi = fromIntegral (impact * urgency) / fromIntegral hoursEstimate :: Double

  return (Task description impact urgency hoursEstimate roi)


writeQueueToFile :: FilePath -> Heap.Heap Task -> IO ()
writeQueueToFile dbPath queue = do
  let serialized = encodePretty (toList queue)
  BSL.writeFile dbPath serialized

getQueue :: FilePath -> IO (Heap.Heap Task)
getQueue dbPath = do
  fileExists <- doesFileExist dbPath
  if fileExists
    then decodeQueue dbPath
    else do
        return (Heap.empty)

decodeQueue :: FilePath -> IO (Heap.Heap Task)
decodeQueue dbPath = do
  jsonData <- BSL.readFile dbPath

  case decode jsonData :: Maybe [Task] of
    Nothing     -> do
        putStrLn ("Error: Failed to parse " ++ dbPath)
        return (Heap.empty)
    Just tasks -> return (Heap.fromList tasks)

writeCalendarToFile :: FilePath -> Calendar -> IO ()
writeCalendarToFile calendarPath calendar = do
  BSL.writeFile calendarPath $ encodePretty calendar

decodeCalendar :: FilePath -> IO Calendar
decodeCalendar calendarPath = do
  jsonData <- BSL.readFile calendarPath

  case decode jsonData :: Maybe Calendar of
    Nothing     -> do
        putStrLn ("Error: Failed to parse " ++ calendarPath)
        return $ Calendar {timeBlocks = []}
    Just calendar -> return calendar

deserializeCalendar :: FilePath -> IO Calendar
deserializeCalendar calendarPath = do
  fileExists <- doesFileExist calendarPath
  if fileExists
    then decodeCalendar calendarPath
    else return $ Calendar {timeBlocks = []}

popMaxPriorityTask :: Heap.Heap Task -> (Task, Heap.Heap Task)
popMaxPriorityTask heap = fromJust $ Heap.uncons heap

breakdownTask :: [Task] -> Task -> IO [Task]
breakdownTask acc fatTask =
    -- check whether fatTask has a time estimate less than 1 hour
    -- check that daily hours so far is below the daily limit
    let dailyHours = foldl' (+) 0 $ map (\t -> (hoursEstimate t)) acc
        dailyLimit = 3
    in if hoursEstimate fatTask < 1 || dailyHours >= dailyLimit
        then do
            return $ acc ++ [fatTask]
        else do
            putStrLn $ "This task is too fat:\n" ++ show fatTask
            putStrLn "We're going to break down a large task into smaller chunks. What's the next step you can complete in under an hour?"
            fatTask2 <- promptTask
            breakdownTask (acc ++ [fatTask2]) (fatTask {hoursEstimate = (hoursEstimate fatTask) - (hoursEstimate fatTask2)})

-- I use a guard here to skip when we encounter a task with > 1 hour time
-- estimate. I only want to time block chunked tasks.
buildTimeBlocks :: [TimeBlock] -> [Task] -> LocalTime -> [TimeBlock]
buildTimeBlocks acc tasks time = case tasks of
    [] -> acc
    [t]
        | (hoursEstimate t <= 1) ->
            buildTimeBlocks
                (acc ++ [TimeBlock time (addHoursToLocalTime time (hoursEstimate t)) t])
                []
                (addHoursToLocalTime time (hoursEstimate t))
        | otherwise ->
            buildTimeBlocks
                acc
                []
                (addHoursToLocalTime time (hoursEstimate t))

    t:ts
        | (hoursEstimate t <= 1) ->
            buildTimeBlocks
                (acc ++ [TimeBlock time (addHoursToLocalTime time (hoursEstimate t)) t])
                ts
                (addHoursToLocalTime time (hoursEstimate t))
        | otherwise ->
            buildTimeBlocks
                acc
                ts
                (addHoursToLocalTime time (hoursEstimate t))

    where
        addHoursToLocalTime lt hours = utcToLocalTime utc $ addUTCTime (fromIntegral (hours * 3600)) $ localTimeToUTC utc lt
        utc = hoursToTimeZone 0

getLocalTime :: IO LocalTime
getLocalTime = do
  utcTime <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ utcToLocalTime tz utcTime

buildCalendar :: Calendar -> Heap.Heap Task -> IO Calendar
buildCalendar cal heap = do
    localTime <- getLocalTime
    let (task, _) = popMaxPriorityTask heap
    chunks <- breakdownTask [] task
    return $ Calendar {timeBlocks = buildTimeBlocks [] chunks localTime}

getCalendar :: Calendar -> Heap.Heap Task -> IO Calendar
getCalendar loadedCalendar queue =
  -- null [] -> True
  let isEmptyCalendar = null (timeBlocks loadedCalendar)
  in case isEmptyCalendar of
      True  -> buildCalendar loadedCalendar queue
      False -> return loadedCalendar

main :: IO ()
main = do
 {-
 - [x] should help you prioritize what you should work on right now
 - [] should time block chunks throughout your day
 - [] should help you break large goals down into 1 hour chunks
 - [] should allow you to prioritize a new task
 - -}
  let dbPath = "stack_ranked_task_list.json"
  let calendarPath = "daily_calendar.json"
  loadedQueue <- getQueue dbPath
  queue <- if Heap.null loadedQueue
      then do
        task <- promptTask
        return $ Heap.insert task Heap.empty
      else return loadedQueue
  loadedCalendar <- deserializeCalendar calendarPath
  calendar <- getCalendar loadedCalendar queue
  putStrLn "Current queue:"
  mapM_ print (toList queue)
  putStrLn "Current calendar:"
  mapM_ print (timeBlocks calendar)
  writeQueueToFile dbPath queue
  writeCalendarToFile calendarPath calendar


