-- FLP: bkg-2-cnf
-- Aneta Dufkova (xdufko02)
-- 2022

{-# LANGUAGE RecordWildCards #-}

module DataTypes
where

import Data.List (intersperse)

-- in BKG nonTerm is just one char,
-- in CNF it can be more chars -> String
type NonTerm = Char
type NonTermCNF = String
type Term = Char
type Rule = (Char, String)
type RuleCNF = (String, String)

data BKG = BKG{
    nonTerms :: [NonTerm],
    terms :: [Term],
    startingNonTerm :: NonTerm,
    rules :: [Rule]
}

data CNF = CNF{
    nonTermsCNF :: [NonTermCNF],
    termsCNF :: [Term],
    startingNonTermCNF :: NonTermCNF,
    rulesCNF :: [RuleCNF]
}

-- join left and right side by arrow
showRule :: (Char,String) -> String
showRule (first, second) = [first] ++ "->" ++ second

-- join left and right side by arrow
-- CNF rule has different datatype (string instead of char),
showRuleCNF :: (String,String) -> String
showRuleCNF (first, second) = first ++ "->" ++ second

-- join nonterms and terms by ,
instance Show BKG where
    show BKG{..} = unlines $
        [concat (intersperse "," (map (:[]) nonTerms))] ++
        [concat (intersperse  "," (map (:[]) terms))] ++ [[startingNonTerm]] ++ 
        [concat (intersperse "\n" (map showRule rules))]

-- join nonterms and terms by ,
instance Show CNF where
    show CNF{..} = unlines $
        [concat (intersperse "," nonTermsCNF)] ++
        [concat (intersperse  "," (map (:[]) termsCNF))] ++ [startingNonTermCNF] ++ 
        [concat (intersperse "\n" (map showRuleCNF rulesCNF))]

