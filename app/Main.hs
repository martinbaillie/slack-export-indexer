module Main (main) where

import Options.Applicative (execParser)
import Slack.Export.Indexer (runIndexer)
import Slack.Export.Opts (opts)

main :: IO ()
main = runIndexer =<< execParser opts
