module Main where

import DateUtils

main :: IO ()
main = putStrLn $ show (datetimeBallistaToLocalTime "19701009123400")
