{- This module is used for pretty printing the hyperion report.
 - This is mostly put in a separate module to avoid conflicts in
 - the imports of (<>) from Monoid an ansi-wl-pprint
 -}

module Hyperion.PrintReport (printReports) where

import Control.Lens (view)
import Data.Text (Text, unpack)
import Data.HashMap.Strict (elems, HashMap)
import Hyperion.Report
import Text.PrettyPrint.ANSI.Leijen

formatReport :: Report -> Doc
formatReport report =
           green (bold (text (unpack (view reportBenchName report)))) <> line
           <> (indent 2 $ text ("Bench time: " ++ (show (view reportTimeInNanos report)) ++ "ns"))
           <> line

printReports :: HashMap Text Report -> IO ()
printReports report = do
  putDoc $ mconcat $ map formatReport (elems report)
