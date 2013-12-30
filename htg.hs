{-# LANGUAGE TupleSections,NoMonomorphismRestriction #-}
import qualified HTGen.HTML as HTG
import Text.Parsec hiding (many)
import Text.Parsec.Indent
import Text.Groom
import Data.Monoid
import Data.Either.Utils
import Control.Applicative 
import qualified Data.Map as Map
import System.Process
import Data.List
import Data.Maybe
import Debug.Trace
import System.Environment

stripSuffix sfx x = reverse <$> (stripPrefix (reverse sfx) (reverse x))
main = do
	let f x = ((x,).Left . forceEither.runIndent "".runParserT HTG.translationUnit () "" ) <$> readFile (x++".htg")
	fs<-readProcess "ls" [] "">>= mapM f.catMaybes.map (stripSuffix ".htg").lines
	as<-getArgs
	--trace (show fs) $ return ()
	let expand = HTG.expandMacros (Map.fromList fs)
	let f = case as of
		["2"] -> groom.expand
		["1"] -> groom
		_->HTG.generateHTML.expand
	interact $ concat.map f.forceEither.runIndent "".runParserT (many HTG.node) () ""
