{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Data.Aeson (ToJSON, FromJSON, object, (.=))
import GHC.Generics
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (ToJSON)
import Data.Foldable (toList)
import System.IO (hFlush, stdout)
import System.IO (withFile, IOMode(WriteMode))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Heap as Heap

data Task = Task
  { description :: String
  , impact :: Int
  , urgency :: Int
  , effortEstimate :: Int
  , roi :: Double
  } deriving (Show, Generic, ToJSON, FromJSON)


instance Eq Task where
    t1 == t2 =
        (description t1, impact t1, urgency t1, effortEstimate t1, roi t1)
        == (description t2, impact t2, urgency t2, effortEstimate t2, roi t2)

instance Ord Task where
  t1 `compare` t2 =
    let priority1 = (impact t1 * urgency t1) `div` effortEstimate t1
        priority2 = (impact t2 * urgency t2) `div` effortEstimate t2
    in priority2 `compare` priority1

type TaskQueue = Heap.Heap Task

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

-- Prompt the user for a double input
promptDouble :: String -> IO Double
promptDouble prompt = do
  input <- promptString prompt
  return (read input :: Double)

-- Prompt the user for a Task input
promptTask :: IO Task
promptTask = do
  description <- promptString "Enter a task description: "
  impact <- promptInt "Enter the impact of the task (1-10): "
  urgency <- promptInt "Enter the urgency of the task (1-10): "
  effortEstimate <- promptInt "Enter the effort estimate of the task (in hours): "
  roi <- promptDouble "Enter the expected ROI of the task (as a decimal): "
  return (Task description impact urgency effortEstimate roi)


writeQueueToFile :: TaskQueue -> IO ()
writeQueueToFile queue = do
  let serialized = encodePretty (toList queue)
  BSL.writeFile "queue.json" serialized

main :: IO ()
main = do
{-
 - create a queue
 - prompt user for task
 - add task to the queue
 - let user know if successful or not
 - -}
  let queue = Heap.empty
  task <- promptTask
  putStrLn ("New task created: " ++ show task)
  let queue' = Heap.insert task queue
  putStrLn "Current queue:"
  mapM_ print (toList queue')
  writeQueueToFile queue'


