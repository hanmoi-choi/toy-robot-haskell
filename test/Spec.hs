{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import CommandParser as CP
import Text.Trifecta

prop_parseDirectionShouldBeFailed :: Property
prop_parseDirectionShouldBeFailed =
  property $ do
    let randomString = Gen.string (Range.linear 0 100) Gen.alpha
    directionString <- forAll $ Gen.filter (\str -> not $ elem str ["north", "south", "east", "west"] ) $ randomString
    let r' = parseString CP.parseDirection mempty directionString
    case r' of
      Success _ -> failure
      _ -> success

prop_parseDirectionHappyPath :: Property
prop_parseDirectionHappyPath =
  property $ do
    directionString <- forAll $ Gen.element ["north", "south", "east", "west"]
    let r' = parseString CP.parseDirection mempty directionString
    case r' of
      Success _ -> success
      _ -> failure

propertyTests :: IO Bool
propertyTests =
  checkParallel $ Group "CommandParser.Check" [
      ("check happy path: parseDirection", prop_parseDirectionHappyPath)
    , ("check path to be failed: parseDirection", prop_parseDirectionShouldBeFailed)
    ]

main :: IO ()
main = do
  propertyTests
  return ()
