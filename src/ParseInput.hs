-- FLP: bkg-2-cnf
-- Aneta Dufkova (xdufko02)
-- 2022
{-# LANGUAGE RecordWildCards #-}

module ParseInput(
    parseBKG,
    nonTermP,
    arrow

) where

import Control.Applicative ((<$>), (<*>), (<*))
import Control.Arrow (left)
import Control.Monad ((<=<))
import Text.Parsec (parse, newline, string, char, sepBy1, endBy1, many1, oneOf)
import Text.Parsec.String (Parser)
import Data.List (nub)

import DataTypes (BKG(..), NonTerm, Term, Rule)

type ErrorM = Either String

comma :: Parser Char
comma = char ','

arrow :: Parser String
arrow = string "->"

-- validate BKG and if it's wrong then show error
parseBKG :: String -> ErrorM BKG 
parseBKG = validate <=< left show . parse parserBKG ""

-- parse the string containing bkg
parserBKG :: Parser BKG
parserBKG = BKG <$> nonTermsP <* newline
              <*> termsP  <* newline
              <*> startingNonTermP <* newline
              <*> rulesP

-- list of nonterms separated by comma
nonTermsP :: Parser [NonTerm]
nonTermsP = sepBy1 nonTermP comma

-- nonterm has to be upper case letter
nonTermP :: Parser NonTerm
nonTermP = oneOf ['A'..'Z']

-- list of terms separated by comma
termsP :: Parser [Term]
termsP = sepBy1 termP comma

-- term has to be lower case letter
termP :: Parser Term
termP = oneOf ['a'..'z']

-- starting nonterm has to be upper case letter
startingNonTermP :: Parser NonTerm
startingNonTermP = oneOf ['A'..'Z']

-- list of rules ended by newline check
rulesP :: Parser [Rule]
rulesP = endBy1 rulP newline

-- terms and nonterms check
termsNonTerms :: Parser String
termsNonTerms = many1 (oneOf (['a'..'z'] ++ ['A'..'Z']))

-- check if rule has nonterm, arrow and nonterms/terms
rulP :: Parser Rule
rulP = (,) <$> nonTermP <* arrow <*> termsNonTerms

-- check if there are duplicates in the list of nonterms & terms
-- https://stackoverflow.com/questions/26217871/haskell-function-that-tests-if-a-list-has-repeated-duplicate-elements
hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates xs = length (nub xs) /= length xs

-- check if the letters are in the list of terms/nonterms
isAllowed :: [(Char, String)] -> [Char] -> Bool
isAllowed [] _ = True
isAllowed ((l,_):pairs) s = l `elem` s && isAllowed pairs s

-- validate BKG
-- if starting nonterm is in the list of nonterms etc.
validate :: BKG -> ErrorM BKG
validate bkg@BKG{..} = if allOK then Right bkg else Left "invalid BKG"
  where
    allOK =
         startingNonTerm `elem` nonTerms
         && not (hasDuplicates nonTerms)
         && not (hasDuplicates terms)
         && isAllowed rules (nonTerms ++ terms)

