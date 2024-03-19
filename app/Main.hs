module Main (main) where

import MainMenu

main :: IO ()
main = do
  putStrLn "Hello from Hotel Haskell!"
  loop []
