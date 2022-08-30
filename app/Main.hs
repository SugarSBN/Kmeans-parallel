module Main where

import System.Environment
import qualified GenSample as GS
import qualified Kmeans as K

main :: IO ()
main = do
            putStrLn "Hello!"
            args <- getArgs
            print args
            -- GS.genMain args
            K.kmeansMain args