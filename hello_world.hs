import qualified Control.Monad
import           Data.Char

sayHello (Just name) = "Hello " <> show name
sayHello Nothing     = "Hello world"

hw :: IO ()
hw = putStrLn "Hello World"

greet :: IO ()
greet = do
  putStrLn "What is your name?"
  name <- getLine
  let uname = map toUpper name
  putStrLn ("Hello " ++ uname ++ ".")

main :: IO ()
main = do
  i <- getLine
  Control.Monad.when (i /= "quit") $ do
    putStrLn ("Input: " ++ i)
    main

  --if i /= "quit" then do
  --  putStrLn ("Input: " ++ i)
  --  main
  --else
  --  return ()

count :: Int -> Int -> IO ()
count n m = do
  print n
  Control.Monad.when (n < m) $ count (n+1) m

--  putStrLn (show n)
--  if n < m then
--    count (n+1) m
--  else
--    return ()

