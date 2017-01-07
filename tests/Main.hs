module Main where

import qualified Spec
import Test.Hspec

main :: IO ()
main = hspec Spec.spec
