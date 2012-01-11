{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

{-|
  Template Haskell to generate defaultMain with a list of "Test" from
  \"doc_test\", \"case_*\", and \"prop_\".

  An example of source code (Data.MySet.hs):

  > { -| Creating a set from a list. O(N log N)
  >
  > >>> empty == fromList []
  > True
  > >>> singleton 'a' == fromList ['a']
  > True
  > >>> fromList [5,3,5] == fromList [5,3]
  > True
  > - }
  >
  > fromList :: Ord a => [a] -> RBTree a
  > fromList = foldl' (flip insert) empty

  An example of test code in the src directory (src/Test.hs):

  > module Main where
  >
  > import Test.Framework.TH.Prime
  > import Test.Framework.Providers.DocTest
  > import Test.Framework.Providers.HUnit
  > import Test.Framework.Providers.QuickCheck2
  > import Test.QuickCheck2
  > import Test.HUnit
  >
  > import Data.MySet
  >
  > main :: IO ()
  > main = $(defaultMainGenerator)
  >
  > doc_test :: DocTests
  > doc_test = docTest ["../Data/MySet.hs"] ["-i.."]
  >
  > prop_toList :: [Int] -> Bool
  > prop_toList xs = ordered ys
  >   where
  >     ys = toList . fromList $ xs
  >     ordered (x:y:xys) = x <= y && ordered (y:xys)
  >     ordered _         = True
  >
  > case_ticket4242 :: Assertion
  > case_ticket4242 = (valid $ deleteMin $ deleteMin $ fromList [0,2,5,1,6,4,8,9,7,11,10,3]) @?= True

  And run:

  > % runghc Test.hs -i..

  This code is based on Test.Framework.TH by Oscar Finnsson and Emil Nordling
  and the author integrated doctest.

-}

module Test.Framework.TH.Prime (
  defaultMainGenerator,
  DocTests
) where

import Language.Haskell.Extract
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Test.Framework (defaultMain)
import Test.Framework.Providers.API

----------------------------------------------------------------

-- | Type for \"doc_test\".
type DocTests = IO Test

----------------------------------------------------------------

{-|
  Generating defaultMain with a list of "Test" from \"doc_test\",
  \"case_*\", and \"prop_\".
-}
defaultMainGenerator :: ExpQ
defaultMainGenerator = do
    defined <- isDefined docTestKeyword
    if defined
       then [| do TestGroup _ doctests <- $(docListGenerator)
                  defaultMain [ testGroup $(locationModule) $ doctests ++ $(caseListGenerator) ++ $(propListGenerator) ] |]
       else [| defaultMain [ testGroup $(locationModule) $ $(caseListGenerator) ++ $(propListGenerator) ] |]

----------------------------------------------------------------
-- code from Test.Framework.TH of test-framework-th
-- by Oscar Finnsson & Emil Nordling

listGenerator :: String -> String -> ExpQ
listGenerator beginning funcName =
  functionExtractorMap beginning (applyNameFix funcName)

propListGenerator :: ExpQ
propListGenerator = listGenerator "^prop_" "testProperty"

caseListGenerator :: ExpQ
caseListGenerator = listGenerator "^case_" "testCase"

----------------------------------------------------------------

-- | The same as
--   e.g. \n f -> testProperty (fixName n) f
applyNameFix :: String -> ExpQ
applyNameFix n =
  do fn <- [|fixName|]
     return $ LamE [VarP (mkName "n")] (AppE (VarE (mkName n)) (AppE (fn) (VarE (mkName "n"))))

fixName :: String -> String
fixName name = replace '_' ' ' $ drop 5 name

replace :: Eq a => a -> a -> [a] -> [a]
replace b v = map (\i -> if b == i then v else i)

----------------------------------------------------------------
-- code from Hiromi Ishii

isDefined :: String -> Q Bool
isDefined n = do
  return False  `recover` do
    VarI (Name _ flavour) _ _ _ <- reify (mkName n)
    loc <- location
    case flavour of
      NameG ns _ mdl -> return (ns == VarName && modString mdl == loc_module loc)
      _              -> return False

----------------------------------------------------------------

docTestKeyword :: String
docTestKeyword = "doc_test"

docListGenerator :: ExpQ
docListGenerator = varE $ mkName docTestKeyword
