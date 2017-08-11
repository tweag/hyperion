{- This module is used for pretty printing the hyperion report.
 - This is mostly put in a separate module to avoid conflicts in
 - the imports of (<>) from Monoid an ansi-wl-pprint
 -}

module Hyperion.PrintReport (printReports) where

import Numeric
import Control.Lens (view)
import Data.Text (unpack)
import Data.HashMap.Strict (HashMap)
import Hyperion.Internal
import Hyperion.Report
import Text.PrettyPrint.ANSI.Leijen

formatReport :: Report -> Doc
formatReport report =
           green (bold (text (unpack (view reportBenchName report)))) <> line
           <> (indent 2 $ text ("Bench time: " ++ prettyNanos (view reportTimeInNanos report)))
           <> line
  where
    show2decs x= showFFloat (Just 2) x ""
    prettyNanos :: Double -> String
    prettyNanos x
      -- seconds
      | x > 1e9 = show2decs (x/1e9) ++ "s"
      -- milliseconds
      | x > 1e6 = show2decs (x/1e6) ++ "ms"
      -- microseconds
      | x > 1e3 = show2decs (x/1e3) ++ "us"
      | otherwise = show2decs x ++ "ns"

printReports :: HashMap BenchmarkId Report -> IO ()
printReports report = do
  putDoc $ foldMap formatReport report
