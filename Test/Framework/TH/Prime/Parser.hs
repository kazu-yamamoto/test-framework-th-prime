{-# LANGUAGE CPP #-}
module Test.Framework.TH.Prime.Parser (
    unitPropTests
  , symbol, string
  ) where

import Control.Applicative
import Data.List
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Parser
#if MIN_VERSION_haskell_src_exts(1, 18, 0)
import Language.Haskell.Exts.SrcLoc
#endif
import Language.Haskell.Exts.Syntax hiding (VarName, Exp)
import Language.Haskell.TH hiding (Match, Extension (..))
import Language.Preprocessor.Cpphs hiding (Ident)

#if MIN_VERSION_haskell_src_exts(1, 18, 0)
-- location field for haskell-src-exts-1.18
#define L SrcSpanInfo
#define loc _
#else
#define L
#define loc
#endif

----------------------------------------------------------------

symbol :: String -> Exp
symbol = VarE . mkName

string :: String -> Exp
string = LitE . StringL

----------------------------------------------------------------

unitPropTests :: ExpQ
unitPropTests = do
    file <- loc_filename <$> location
    (cases, props) <- runIO $ getTests file
    return $ TupE [ListE (map toCase cases), ListE (map toProp props)]

----------------------------------------------------------------

toCase :: String -> Exp
toCase = toTest "testCase"

toProp :: String -> Exp
toProp = toTest "testProperty"

toTest :: String -> String -> Exp
toTest tag nm = AppE (AppE (symbol tag ) (string nm)) (symbol nm)

----------------------------------------------------------------

getTests :: FilePath -> IO ([String], [String])
getTests file = do
#if MIN_VERSION_haskell_src_exts(1, 18, 0)
    ParseOk (Module _ _ _ _ decls) <- parseTest file
#else
    ParseOk (Module _ _ _ _ _ _ decls) <- parseTest file
#endif
    let funs = map fromFunBind $ filter isFunBind decls
        pats = map fromPatBind $ filter isPatBind decls
        names = funs ++ pats
    return (filter isCase names, filter isProp names)
  where
    isProp = ("prop_" `isPrefixOf`)
    isCase = ("case_" `isPrefixOf`)

parseTest :: FilePath -> IO (ParseResult (Module L))
parseTest file = do
    raw <- readFile file
    parseModuleWithMode (opt raw) . pack <$> go raw
  where
    pack = unlines . tail . map snd
    go = cppIfdef "dummy" [] [] defaultBoolOptions
    exts raw =
      case getTopPragmas raw of
        ParseOk pragmas ->
          [ toExtention name
          | LanguagePragma _ names <- pragmas, name <- names]
        ParseFailed _ _ ->
          []
      where
#if MIN_VERSION_haskell_src_exts(1, 14, 0)
        toExtention = parseExtension . toStr
#else
        toExtention = read . toStr
#endif
        toStr (Ident loc str) = str
        toStr (Symbol loc str) = str
    opt raw = defaultParseMode {
#if MIN_VERSION_haskell_src_exts(1, 14, 0)
        extensions = nub $ EnableExtension TemplateHaskell : exts raw
#else
        extensions = nub $ TemplateHaskell : exts raw
#endif
      -- to prevent "Ambiguous infix expression"
      , fixities = Nothing
      }

----------------------------------------------------------------

isFunBind :: Decl L -> Bool
isFunBind (FunBind loc _) = True
isFunBind _               = False

isPatBind :: Decl L -> Bool
isPatBind PatBind{} = True
isPatBind _                   = False

fromPatBind :: Decl L -> String
#if MIN_VERSION_haskell_src_exts(1, 16, 0)
fromPatBind (PatBind _ (PVar loc (Ident  loc name)) _ _) = name
fromPatBind (PatBind _ (PVar loc (Symbol loc name)) _ _) = name
#else
fromPatBind (PatBind _ (PVar (Ident  name)) _ _ _) = name
fromPatBind (PatBind _ (PVar (Symbol name)) _ _ _) = name
#endif
fromPatBind _ = error "fromPatBind"

fromFunBind :: Decl L -> String
#if MIN_VERSION_haskell_src_exts(1, 18, 0)
fromFunBind (FunBind _floc (Match _ (Ident  _iloc name) _ _ _:_)) = name
fromFunBind (FunBind _floc (Match _ (Symbol _sloc name) _ _ _:_)) = name
#else
fromFunBind (FunBind (Match _ (Ident  name) _ _ _ _:_)) = name
fromFunBind (FunBind (Match _ (Symbol name) _ _ _ _:_)) = name
#endif
fromFunBind _ = error "fromFunBind"
