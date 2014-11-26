{-# LANGUAGE TupleSections,NoMonomorphismRestriction #-}
module HTGen.HTML where
import Text.Parsec hiding (many,(<|>),optional)
import Text.Parsec.Indent
import Text.Groom
import Data.Monoid
import Data.Either.Utils
import Control.Applicative 
import Data.List
import Text.Parsec.Number
import qualified Data.Set as Set
import Text.RegexPR
import qualified Data.Map as Map

stake::String->String
stake = groom.runIndent "".runParserT word () "" --don't use

translationUnit = f <$> many node
	where
	f [x] = x
	f xs = Concat xs

parseH p = runIndent "".runParserT p () ""

ss =  many (oneOf " \t")

_x = (ss*>)
x_ = (<*ss)

x ★> y = x *> (ss *> y)
x <★ y = (x <* ss) <* y

data Node = 
	Apply Node Node |
	Tag String [(String,String)] |
	Slot String |
	Text String |
	Coefficient Int |
	Concat [Node] |
	ArgList [[String]]
	deriving (Show,Read)

slot = x_ $ Slot <$> ((string "¶">>return "") <|> (string "$"*>word))

tag = Tag <$> x_ word <*> option [] ((char ':'★>attributes)<*optional (char '.'))

attributes = concat <$> sequence [_class,_style,_more]
	where
	_class = option [] $ try ((\s->[("class",intercalate " " s)]) <$> endBy1 (word<*notFollowedBy (char '=')) ss)
	_style = option [] $ try ((\s->[("style",intercalate ";" s)]) <$> endBy1 literal ss)
	_more = option [] $ try (endBy ((,) <$> (word <* char '=') <*> literal) ss)

word = ((:) <$> letter <*> many (alphaNum<|>oneOf "-_"))
	
node = spaces *> withBlock f (chain<*spaces) node
	where
	f x [] = x
	f (Apply x y) z = Apply x $ f y z
	f x xs = Apply x $ case xs of
		[y]->y
		ys->Concat ys
	

chain = _x $ foldr1 (\a b->Apply a b) <$> endBy1 element ss

element = (slot<|>tag<|>text<|>argList<|>coefficient<|>elementList)

text = Text <$> literal

enclosed [l,r] = between (char l<*spaces) (spaces*>char r)

literal = choice $ (char '!' *> many (noneOf "\n")):[between (char l) (char r) (many $ noneOf [l,r])|
	[l,r]<-["><","\"\"","''","██"]]

argList = ArgList <$> enclosed "[]" (sepBy (sepBy1 literal (try $ spaces*>char '-'<*spaces)) spaces) -- Don't try to eliminate the case ["1" ] again! It's not easy!

coefficient = Coefficient <$> nat

elementList = Concat <$> enclosed "{}" (sepBy node (spaces*>char ';'<*spaces))

generateHTML::Node->String
generateHTML = f where
	(∙) = (++)
	startTag n a = "<"∙n∙concat [" "∙x∙"="∙showStringLiteral y|(x,y)<-a]∙">"
	endTag n = "</"∙n∙">"
	f (Apply (Tag n a) b) = startTag n a∙f b∙endTag n
	f (Tag n a) = startTag n a ∙ if isVoidTag n then "" else endTag n
	f (Text s) = s
	f (Concat s) = concat $ map f s

type SymbolTable=Map.Map String (Either Node String)

expandMacros::SymbolTable->Node->Node
expandMacros stbl = f where
	f (Apply (Coefficient a) b) = Concat $ replicate a $ f b
	f (Apply (ArgList ass) b') = case f b' of
		Text s -> if '¶' `elem` s' then Text s' else Concat $ forceEither $ parseH (many node) $ s'
			where
			s' = unlines $ sub "¶" s
		s -> Concat $ map read $ sub "\\$\\$" $ show s
		where
		sub x s = map (\as->(s0++) $ concat $ zipWith (++) (as++repeat x) ss) ass
			where
			(s0:ss) = splitRegexPR x s
			g a b = a++b
	f (Apply (Concat as) b) = Concat $ map (snd.(`g` b)) as
		where
		g (Concat (a:as)) (Slot "") = (Concat as,a)
		g (Concat []) (Slot "") = (Concat [],Slot "")
		g a (Slot "") = (a,f a)
		g a (Slot s) = g a (f (Slot s))
		g a (Apply x y) = (a'',Apply x' y')
			where
			(a',x') = g a x
			(a'',y') = g a' y
		g a (Concat xs) = (a',Concat $ reverse xs')
			where
			(a',xs') = foldl (\(a,bs) b->let (a',b') = g a b in (a',b':bs)) (a,[]) xs
		g a x = (a,x)
	f (Slot "") = Slot ""
	f (Slot s) = let Left n = stbl Map.! s in f n
	f (Apply a b) = Apply a $ f b
	f (Concat a) = Concat $ map f a
	f x = x

	--apply nodelist to nodes
	--param Tags
isVoidTag x = Set.member x $ Set.fromList $ [
	"area",
	"base",
	"br",
	"col",
	"command",
	"embed",
	"hr",
	"img",
	"input",
	"keygen",
	"link",
	"meta",
	"param",
	"source",
	"track",
	"wbr"]

--evaluateSubstitution::Node->Node

showStringLiteral s = "\""++s++"\""
