import System.IO (hFlush, stdout)

data Task = Task
  { description :: String
  , impact :: Int
  , urgency :: Int
  , effortEstimate :: Int
  , roi :: Double
  } deriving (Show)


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

main :: IO ()
main = do
  task <- promptTask
  putStrLn ("New task created: " ++ show task)

