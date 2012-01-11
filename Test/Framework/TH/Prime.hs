{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

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

type DocTests = IO Test

----------------------------------------------------------------

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
