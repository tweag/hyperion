module Hyperion.MainSpec where

import Control.Lens ((^..))
import Data.List (group, sort)
import Hyperion.Analysis
import Hyperion.BenchmarkSpec ()
import Hyperion.Main
import Test.Hspec
import Test.QuickCheck ((==>), expectFailure, property)
import Test.QuickCheck.Monadic (monadicIO, run)

spec :: Spec
spec = do
    describe "defaultMain" $ do
      it "checks for duplicate names" $ property $ \b ->
        length (b^..namesOf) /= length (group (sort (b^..namesOf))) ==>
        expectFailure $ monadicIO $ run $
          defaultMainWith defaultConfig{configMonoidMode = return Run} "spec" [b]
      it "Analyzes uniquely named benchmarks" $ property $ \b ->
        length (b^..namesOf) == length (group (sort (b^..namesOf))) ==>
        monadicIO $ run $
          defaultMainWith defaultConfig{configMonoidOutputPath = return nullOutputPath} "specs" [b]
