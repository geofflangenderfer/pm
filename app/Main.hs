{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}

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

data Project = Project
    { notStarted :: [Task]
    , inProgress :: [Task]
    , done :: [Task]
    } deriving (Show, Generic, ToJSON, FromJSON)

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
    else return (Heap.empty)

decodeQueue :: FilePath -> IO (Heap.Heap Task)
decodeQueue dbPath = do
  jsonData <- BSL.readFile dbPath

  case decode jsonData :: Maybe [Task] of
    Nothing     -> do
        putStrLn ("Error: Failed to parse " ++ dbPath)
        return (Heap.empty)
    Just tasks -> return (Heap.fromList tasks)

main :: IO ()
main = do
{-
 - load queue from json or create empty one
 - prompt user for task
 - add task to the queue
 - let user know if successful or not
 - save queue to json
 - -}
  let dbPath = "stack_ranked_task_list.json"
  queue <- getQueue dbPath
  task <- promptTask
  putStrLn ("New task created:\n" ++ show task)
  let queue' = Heap.insert task queue
  putStrLn "Current queue:"
  mapM_ print (toList queue')
  writeQueueToFile dbPath queue'


