module Main where

import Control.Monad (when)

import Lambda.Parser (wholeParse)
import Lambda.Pretty (prettyExp)
import Lambda.SimplyTyped.Infer (infer)
import Lambda.SimplyTyped.Pretty (prettyTExp)

main :: IO ()
main = do
    inp <- getLine
    when (not (null inp)) $ do
      putStrLn (respond inp)
      main
  where
    respond inp =
      case wholeParse inp of
        Nothing -> "Couldn't parse"
        Just untyped ->
          unlines
            [ prettyExp untyped
            , case infer untyped of
                Nothing -> "Couldn't infer"
                Just typed -> prettyTExp show id typed
            ]
