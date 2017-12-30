{-| This module provides 'xmlFormatter' that can be used with 'Test.Hspec.Runner.hspecWith'.

  Example usage:

  > import Test.Hspec.Formatters.Jenkins (xmlFormatter)
  > import Test.Hspec.Runner
  >
  > main :: IO ()
  > main = do
  >   summary <- withFile "results.xml" WriteMode $ \h -> do
  >     let c = defaultConfig
  >           { configFormatter = xmlFormatter
  >           , configHandle = h
  >           }
  >     hspecWith c spec
  >   unless (summaryFailures summary == 0) $
  >     exitFailure

  An example project is located in @example@ directory.
-}

{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.Formatters.Jenkins (xmlFormatter) where
import Data.List (intercalate)
import Test.Hspec.Formatters
import Test.Hspec.Runner (Path)
import Text.Blaze.Renderer.String (renderMarkup)
import Text.Blaze.Internal

failure, skipped :: Markup -> Markup
failure = customParent "failure"
skipped = customParent "skipped"

name, className, message :: String -> Attribute
name = customAttribute "name" . stringValue
className = customAttribute "classname" . stringValue
message = customAttribute "message" . stringValue

testcase :: Path -> Markup -> Markup
testcase (xs,x) = customParent "testcase" ! name x ! className (intercalate "." xs)

reasonAsString :: FailureReason -> String
reasonAsString reason =
  case reason of
    NoReason -> "no reason given"
    Reason x -> x
    ExpectedButGot Nothing expected got ->
      "Expected " ++ expected ++ " but got " ++ got
    ExpectedButGot (Just src) expected got ->
      src ++ " expected " ++ expected ++ " but got " ++ got

-- | Format Hspec result to Jenkins-friendly XML.
xmlFormatter :: Formatter
xmlFormatter = silent {
    headerFormatter = do
      writeLine "<?xml version='1.0' encoding='UTF-8'?>"
      writeLine "<testsuite>"
  , exampleSucceeded = \path -> do
      writeLine $ renderMarkup $
        testcase path ""
  , exampleFailed = \path err -> do
      writeLine $ renderMarkup $
        testcase path $
          failure ! message (either formatException reasonAsString err) $ ""
  , examplePending = \path mdesc -> do
      writeLine $ renderMarkup $
        testcase path $
          case mdesc of
            Just desc -> skipped ! message desc  $ ""
            Nothing -> skipped ""
  , footerFormatter = do
      writeLine "</testsuite>"
  }
