{-# LANGUAGE CPP, QuasiQuotes, TemplateHaskell #-}

-- |
-- Template Haskell to generate defaultMain with a list of "Test" from
-- \"doc_test\", \"case_\<somthing\>\", and \"prop_\<somthing\>\".
--
-- An example of source code (Data/MySet.hs):
--
-- > {-| Creating a set from a list. O(N log N)
-- >
-- > >>> empty == fromList []
-- > True
-- > >>> singleton 'a' == fromList ['a']
-- > True
-- > >>> fromList [5,3,5] == fromList [5,3]
-- > True
-- > -}
-- >
-- > fromList :: Ord a => [a] -> RBTree a
-- > fromList = foldl' (flip insert) empty
--
-- An example of test code in the src directory (test/Test.hs):
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > module Main where
-- >
-- > import Test.Framework.TH.Prime
-- > import Test.Framework.Providers.DocTest
-- > import Test.Framework.Providers.HUnit
-- > import Test.Framework.Providers.QuickCheck2
-- > import Test.QuickCheck2
-- > import Test.HUnit
-- >
-- > import Data.MySet
-- >
-- > main :: IO ()
-- > main = $(defaultMainGenerator)
-- >
-- > doc_test :: DocTests
-- > doc_test = docTest ["../Data/MySet.hs"] ["-i.."]
-- >
-- > prop_toList :: [Int] -> Bool
-- > prop_toList xs = ordered ys
-- >   where
-- >     ys = toList . fromList $ xs
-- >     ordered (x:y:xys) = x <= y && ordered (y:xys)
-- >     ordered _         = True
-- >
-- > case_ticket4242 :: Assertion
-- > case_ticket4242 = (valid $ deleteMin $ deleteMin $ fromList [0,2,5,1,6,4,8,9,7,11,10,3]) @?= True
--
--  And run:
--
-- > test% runghc -i.. Test.hs
--
-- "defaultMainGenerator" generates the following:
--
-- > main = do
-- >     TestGroup _ doctests <- docTest ["../Data/MySet.hs"] ["-i.."]
-- >     defaultMain [
-- >         testGroup "Doc tests" doctests
-- >       , testGroup "Unit tests" [
-- >              testCase "case_ticket4242" case_ticket4242
-- >            ]
-- >       , testGroup "Property tests" [
-- >              testProperty "prop_toList" prop_toList
-- >            ]
-- >       ]
--
-- Note: examples in haddock document is only used as unit tests at this
-- moment. I hope that properties of QuickCheck2 can also be specified in
-- haddock document in the future. I guess it's Haskell way of Behavior
-- Driven Development.

module Test.Framework.TH.Prime (
    defaultMainGenerator
  , DocTests
  ) where

import Control.Applicative
import Language.Haskell.TH hiding (Match)
import Language.Haskell.TH.Syntax hiding (Match)
import Test.Framework (defaultMain)
import Test.Framework.Providers.API
import Test.Framework.TH.Prime.Parser

----------------------------------------------------------------

-- | Type for \"doc_test\".
type DocTests = IO Test

----------------------------------------------------------------

{-|
  Generating defaultMain with a list of "Test" from \"doc_test\",
  \"case_\<somthing\>\", and \"prop_\<somthing\>\".
-}
defaultMainGenerator :: ExpQ
defaultMainGenerator = do
    defined <- isDefined docTestKeyword
    if defined then [|
        do TestGroup _ doctests <- $(docTests)
           let (unittests, proptests) = $(unitPropTests)
           defaultMain [ testGroup "Doc tests" doctests
                       , testGroup "Unit tests" unittests
                       , testGroup "Property tests" proptests
                       ]
      |] else [|
        do let (unittests, proptests) = $(unitPropTests)
           defaultMain [ testGroup "Unit tests" unittests
                       , testGroup "Property tests" proptests
                       ]
      |]

----------------------------------------------------------------
-- code from Hiromi Ishii

isDefined :: String -> Q Bool
isDefined n = return False `recover` do
#if MIN_VERSION_template_haskell(2, 11, 0)
    VarI (Name _ flavour) _ _ <- reify (mkName n)
#else
    VarI (Name _ flavour) _ _ _ <- reify (mkName n)
#endif
    modul <- loc_module <$> location
    case flavour of
      NameG ns _ mdl -> return (ns == VarName && modString mdl == modul)
      _              -> return False

----------------------------------------------------------------

docTestKeyword :: String
docTestKeyword = "doc_test"

docTests :: ExpQ
docTests = return $ symbol docTestKeyword
