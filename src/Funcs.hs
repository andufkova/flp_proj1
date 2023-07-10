-- FLP: bkg-2-cnf
-- Aneta Dufkova (xdufko02)
-- 2022
{-# LANGUAGE RecordWildCards #-}
module Funcs(
    newBKGwithoutSimpleRules,
    newBKGinCNF
) where

import Data.List (nub)
import DataTypes (BKG(..), NonTerm, Rule, RuleCNF, NonTermCNF, CNF(..))

-- filter everything but simple rules
notSimpleRule :: Rule -> Bool
notSimpleRule (_, second) = ((length second) > 1) || ((head second) `elem` ['a'..'z'])

-- filter simple rules
simpleRule :: Rule -> Bool
simpleRule (_, second) = ((length second) == 1) && ((head second) `elem` ['A'..'Z'])

-- create a new rule for the one particular rule
createRulesFromOneList :: Rule -> [NonTerm] -> [Rule]
createRulesFromOneList (rule1, rule2) listNonTerms = if rule1 `elem` listNonTerms then [(head listNonTerms, rule2)] else []

-- create rules according to the step 2 of TIN algorithm 4.T
createOneRule :: Rule -> [[NonTerm]] -> [Rule]
createOneRule _ [] = []
createOneRule rule (list:listOfLists) = createRulesFromOneList rule list ++ createOneRule rule listOfLists

-- input: not simple rules, list of lists
createNewRules :: [Rule] -> [[NonTerm]] -> [Rule]
createNewRules [] _ = []
createNewRules (rule:rules) listOfLists = createOneRule rule listOfLists ++ createNewRules rules listOfLists

-- take the rules starting with particular nonterm
-- create the list of right sides of these rules
rulesStartingWith :: NonTerm -> [Rule] -> [NonTerm]
rulesStartingWith _ [] = []
rulesStartingWith st ((rule1,rule2):rules) = [x | x <- rule2, rule1 == st] ++ rulesStartingWith st rules

-- is there a rule with given left and right part
-- function for filtering
ruleExists :: NonTerm -> NonTerm -> [Rule] -> Bool
ruleExists st nt rules = nt `elem` (rulesStartingWith st rules)

-- input: each A, all nonterms, all the simple rules
nonTermSetCreate :: NonTerm -> [NonTerm] -> [Rule] -> [NonTerm]
nonTermSetCreate initNonTerm nonTerms rules = [x | x <- nonTerms, ruleExists initNonTerm x rules]

-- add an element to the list if it is not already present in the list
addIfNotContains :: (Eq a) => [a] -> [a] -> [a]
addIfNotContains _ [] = []
addIfNotContains a (b:bs) = [b | not (b `elem` a)] ++ addIfNotContains a bs

-- create set N_A for particular nonterm according to the step 1 of TIN algorithm 4.T
-- keep going through the list until it's not changed (its length is not changed)
-- the first step is special, therefore the check for 0 legth
-- input: [A] (aka old list), new list, all nonterms, rules
nonTermSet :: [NonTerm] -> [NonTerm] -> [NonTerm] -> [Rule] -> [NonTerm]
nonTermSet old new allNonTerms rules =
    if (length new) == 0
        then nonTermSet old (old ++ addIfNotContains old (nonTermSetCreate (last old) allNonTerms rules)) allNonTerms rules
        else
    (if (length old) /= (length new) -- lengths
        then (nonTermSet new (new ++ addIfNotContains new (nonTermSetCreate (last new) allNonTerms rules)) allNonTerms rules)
        else new)

-- input: nonTerms, nonTerms, simple rules
-- create set N_A (according to TIN algorithm 4.5) for every nonterm
nonTermsSets :: [NonTerm] -> [NonTerm] -> [Rule] -> [[NonTerm]]
nonTermsSets [] _ _ = []
nonTermsSets (nt:nts) allNts rules = (nonTermSet [nt] [] allNts rules):(nonTermsSets nts allNts rules)

getNewRules :: [Rule] -> [NonTerm] -> [Rule]
getNewRules rules nonTerms = newRules where
    newRules = createNewRules (filter notSimpleRule rules) (nonTermsSets nonTerms nonTerms (filter simpleRule rules))

-- create new BKG without simple rules
-- the only thing changed is the list of rules
newBKGwithoutSimpleRules :: BKG -> BKG
newBKGwithoutSimpleRules BKG{..} = BKG{
    nonTerms = nonTerms,
    terms = terms,
    startingNonTerm = startingNonTerm,
    rules = getNewRules rules nonTerms
    } 
--------------------------------
--          CNF PART          --
--------------------------------
-- rules in CNF has to be in String format
-- so rules from BKG are needed to be converted
charToStrRule :: [Rule] -> [RuleCNF]
charToStrRule [] = []
charToStrRule ((rule1, rule2):rules) = [([rule1], rule2)] ++ (charToStrRule rules)

-- function for filtering all the rules according to the step 1 of TIN algorithm 4.7 
simpleRuleCNF1 :: RuleCNF -> Bool
simpleRuleCNF1 (_, second) = ((length second) == 1) && ((head second) `elem` ['a'..'z'])

-- function for filtering all the rules according to the step 2 of TIN algorithm 4.7 
simpleRuleCNF2 :: RuleCNF -> Bool
simpleRuleCNF2 (_, second) = ((length second) == 2) && ((head second) `elem` ['A'..'Z']) && ((last second) `elem` ['A'..'Z'])

-- function for filtering all the rules according to the step 5 of TIN algorithm 4.7 
simpleRuleCNF5 :: RuleCNF -> Bool
simpleRuleCNF5 (_, second) = ((length second) == 2) && (((head second) `elem` ['a'..'z']) || ((last second) `elem` ['a'..'z']))

-- function for filtering all the rules according to the step 4 of TIN algorithm 4.7 
simpleRuleCNF4 :: RuleCNF -> Bool
simpleRuleCNF4 (_, second) = ((length second) > 2)

-- create the rules according to the step 4 of TIN algorithm 4.7
-- take the first char and wrap the rest with <>
-- if the last char is nonterm, than leave i without <>
-- if the last char is a term, add ' to make it a nonterm
-- if the first char is a term, add ' to make it a nonterm 
takeOneRuleCNF :: RuleCNF -> [RuleCNF]
takeOneRuleCNF (_, []) = []
takeOneRuleCNF (rule1, (first:rule2)) = 
    if (length (first:rule2)) == 1
        then []
        else 
        (if (first `elem` ['a'..'z'])  -- nonterminal, ' needed to be added
            then
                (if (length (rule2) == 1)
                    then 
                        (if ((head rule2) `elem` ['a'..'z'])
                            then [(rule1, [first] ++ "'" ++ rule2 ++ "'")] ++ singleLetter ++ singleLetter2 ++ callMethod
                            else [(rule1, [first] ++ "'" ++ rule2)] ++ singleLetter ++ callMethod)
                        else [(rule1, [first] ++ "'<" ++ rule2 ++ ">")] ++ singleLetter ++ callMethod)
        -- not adding ' after the first element
        else (if (length (rule2) == 1)
            then
                (if ((head rule2) `elem` ['a'..'z'])
                    then [(rule1, [first] ++ rule2 ++ "'")] ++ singleLetter2 ++ callMethod
                    else [(rule1, [first] ++ rule2)] ++ callMethod)
                else [(rule1, [first] ++ "<" ++ rule2 ++ ">")] ++ callMethod))
        where
            singleLetter = [([first] ++ "'", [first])]
            singleLetter2 = [(rule2 ++ "'", rule2)]
            callMethod = (takeOneRuleCNF ("<" ++ rule2 ++ ">", rule2))

-- create rules according to the step 4 from TIN algorithm
-- for every rule create all the new rules
createRulesCNF :: [RuleCNF] -> [RuleCNF]
createRulesCNF [] = []
createRulesCNF (rule:rules) = takeOneRuleCNF rule ++ createRulesCNF rules

-- for rules with right part of length 2
-- add ' to nonterms
-- check if the first one is nonterm, then if the second one is nonterm
createRulesCNF5 :: [RuleCNF] -> [RuleCNF]
createRulesCNF5 [] = []
createRulesCNF5 ((_, []) : _) = []
createRulesCNF5 ((rule1, rule21:rule22):rules) = 
    if (rule21 `elem` ['a'..'z'])
        then 
            (if (head rule22 `elem` ['a'..'z'])
                then [(rule1, [rule21] ++ "'" ++ rule22 ++ "'")] ++ (createRulesCNF5 rules)
                else [(rule1, [rule21] ++ "'" ++ rule22)] ++ (createRulesCNF5 rules))
        else
            (if (head rule22 `elem` ['a'..'z'])
                then [(rule1, [rule21] ++ rule22 ++ "'")] ++ (createRulesCNF5 rules)
                else [(rule1, [rule21] ++ rule22)] ++ (createRulesCNF5 rules))

-- join all the types of new rules
getNewRulesCNF :: [RuleCNF] -> [RuleCNF]
getNewRulesCNF rules = (filter simpleRuleCNF1 rules) ++ (filter simpleRuleCNF2 rules) 
 ++ (createRulesCNF (filter simpleRuleCNF4 rules)) ++ (createRulesCNF5 (filter simpleRuleCNF5 rules))

-- take all the rules and get nonterms from them
-- basically take the part before an arrow
getNonTermsFromRules :: [RuleCNF] -> [NonTermCNF]
getNonTermsFromRules [] = []
getNonTermsFromRules ((rule1,_):rules) = [rule1] ++ getNonTermsFromRules rules

-- create CNF from BKG
-- nub function for dealing with duplicates,
-- cause the duplicates are not treated in the functions creating rules
newBKGinCNF :: BKG -> CNF
newBKGinCNF BKG{..} = CNF{
    nonTermsCNF = (nub (getNonTermsFromRules newRules)),
    termsCNF = terms,
    startingNonTermCNF = [startingNonTerm],
    rulesCNF = nub (newRules)
} where
    newRules = getNewRulesCNF (charToStrRule rules)






