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
      it "checks for duplicate identifiers" $ property $ \b ->
        length (b^..identifiers) /= length (group (sort (b^..identifiers))) ==>
        expectFailure $ monadicIO $ run $
          defaultMainWith defaultConfig{configMonoidMode = return Run} "spec" [b]
      it "Analyzes uniquely identified benchmarks" $ property $ \b ->
        length (b^..identifiers) == length (group (sort (b^..identifiers))) ==>
        monadicIO $ run $
          defaultMainWith defaultConfig{configMonoidReportOutputs = [ReportJson nullOutputPath]} "specs" [b]
