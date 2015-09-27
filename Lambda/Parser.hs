module Lambda.Parser (parse, wholeParse) where

import Control.Applicative (many, some)
import Control.Monad (guard, mzero)
import Control.Monad.Trans.State
import Data.Char (isAlpha, isPunctuation, isSpace, isSymbol)
import Data.Void (Void)

import Lambda.Untyped

dropSpace :: String -> String
dropSpace = dropWhile isSpace

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)

parseAtom :: StateT String Maybe (Exp Void String)
parseAtom = do
  s <- gets dropSpace
  case s of
    [] -> mzero
    ('(' : rest) -> do
      put rest
      e <- parseExp
      ')' <- StateT $ uncons . dropSpace
      return e
    (c : rest) | c == '\\' || c == 'λ' -> do
      put rest
      names <- some (parseBit isAlpha)
      sep <- parseBit $ \c -> isPunctuation c || isSymbol c
      guard (sep `elem` [".", "->", "→"])
      e <- parseExp
      return (foldr Lam e names)
    _ -> do
      xs@(_:_) <- parseBit isAlpha
      return (Var xs)

parseBit :: (Char -> Bool) -> StateT String Maybe String
parseBit p = StateT $ \s -> 
  case span p (dropSpace s) of
    ("", _) -> Nothing
    (got, left) -> Just (got, left)

parseExp :: StateT String Maybe (Exp Void String)
parseExp = foldl App <$> parseAtom <*> many parseAtom

parse :: String -> Maybe (Exp Void String, String)
parse = runStateT parseExp

wholeParse :: String -> Maybe (Exp Void String)
wholeParse s = do
  (e,r) <- parse s
  guard (all isSpace r)
  return e
