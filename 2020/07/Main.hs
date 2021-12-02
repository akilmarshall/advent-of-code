import Data.Set (empty)

data Rule = Rule String [(String, Int)] deriving (Show)

testRuleSet = 
    [ Rule "light red" [("bright white", 1), ("muted yellow", 2)]
    , Rule "dark orange" [("bright white", 3), ("muted yellow", 4)]
    , Rule "bright white" [("shiny gold", 1)]
    , Rule "muted yellow" [("shiny gold", 2), ("faded blue", 9)]
    , Rule "shiny gold" [("dark olive", 1), ("vibrant plum", 2)]
    , Rule "dark olive" [("faded blue", 3), ("dotted black", 4)]
    , Rule "vibrant plum" [("faded blue", 4), ("dotted black", 6)]
    , Rule "faded blue" []
    , Rule "dotted black" []
    ]

lookup' :: [Rule] -> String -> Maybe Rule
lookup' [] _ = Nothing
lookup' (Rule name rules:xs) rule
    | name == rule = Just (Rule name rules)
    | otherwise = lookup' xs rule


f s (Rule name lst)
    | s `elem` leaves = True
    | otherwise       = False
    where leaves = map fst lst 

oneStep = filter (f "shiny gold") testRuleSet
-- oneStep yields a list of rules that specify any number of shiny gold bags

