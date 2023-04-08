{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Data.Aeson (ToJSON, FromJSON, object, decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (ToJSON)
import Data.Foldable (toList)
import GHC.Generics
import System.Directory (doesFileExist)
import System.FilePath (FilePath)
import System.IO (hFlush, stdout)
import System.IO (withFile, IOMode(WriteMode))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Heap as Heap

data Task = Task
  { description :: String
  , impact :: Int
  , urgency :: Int
  , hoursEstimate :: Int
  , roi :: Double
  } deriving (Show, Generic, ToJSON, FromJSON)


instance Eq Task where
    t1 == t2 =
        (description t1, impact t1, urgency t1, hoursEstimate t1, roi t1)
        == (description t2, impact t2, urgency t2, hoursEstimate t2, roi t2)

instance Ord Task where
  t1 `compare` t2 =
    let priority1 = roi t1
        priority2 = roi t2
    in priority2 `compare` priority1

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


writeQueueToFile :: Heap.Heap Task -> IO ()
writeQueueToFile queue = do
  let serialized = encodePretty (toList queue)
  BSL.writeFile "queue.json" serialized

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
  let dbPath = "queue.json"
  queue <- getQueue dbPath
  task <- promptTask
  putStrLn ("New task created: " ++ show task)
  let queue' = Heap.insert task queue
  putStrLn "Current queue:"
  mapM_ print (toList queue')
  writeQueueToFile queue'


