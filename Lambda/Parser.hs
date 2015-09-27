module Lambda.Parser (parse, wholeParse) where

import Control.Applicative (many)
import Control.Monad (guard, mzero)
import Control.Monad.Trans.State
import Data.Char (isAlpha, isPunctuation, isSpace, isSymbol)

import Lambda.Untyped

dropSpace :: String -> String
dropSpace = dropWhile isSpace

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)

parseAtom :: StateT String Maybe (Exp String)
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
      name@(_:_) <- parseBit isAlpha
      sep <- parseBit $ \c -> isPunctuation c || isSymbol c
      guard (sep `elem` [".", "->", "→"])
      Lam name <$> parseExp
    _ -> do
      xs@(_:_) <- parseBit isAlpha
      return (Var xs)

parseBit :: Monad m => (Char -> Bool) -> StateT String m String
parseBit p = StateT $ return . span p . dropSpace

parseExp :: StateT String Maybe (Exp String)
parseExp = foldl App <$> parseAtom <*> many parseAtom

parse :: String -> Maybe (Exp String,String)
parse = runStateT parseExp

wholeParse :: String -> Maybe (Exp String)
wholeParse s = do
  (e,r) <- parse s
  guard (all isSpace r)
  return e
