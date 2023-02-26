module ShuntingYard (getPosfix) where
    import Data.Char
    import Data.List
    import Stack

    merge :: String -> String -> String
    merge [] [] = []
    merge [] (t:ts) = (t:merge [] ts)
    merge (s:ss) t = (s:merge ss t)

    toString :: Stack Char -> String
    toString = read . head . (drop 1) . words . show

    -- returns true if first char is greater precedence
    precedenceCmp :: Char -> Char -> Int 
    precedenceCmp a b
        | a' > b' = 1 
        | a' == b' = 0
        | otherwise = -1
        where 
            a' = precedence a
            b' = precedence b

    precedence :: Char -> Int
    precedence '^' = 4
    precedence 'x' = 3
    precedence '/' = 3
    precedence '+' = 2
    precedence '-' = 2

    associativity :: Char -> String
    associativity '^' = "Right" 
    associativity 'x' = "Left" 
    associativity '/' = "Left" 
    associativity '+' = "Left" 
    associativity '-' = "Left" 

    isLeftAssoc :: Char -> Bool
    isLeftAssoc = (=="Left") . associativity

    shuntingYard :: String -> Stack Char -> String -> String
    shuntingYard [] stack output = merge (reverse output) $ toString stack
    shuntingYard (t:tokens) stack output
        | isDigit t = 
            shuntingYard tokens stack $ (t:output)
        | t == '(' = 
            pushT
        | t == ')' = 
            case topOperator of 
                Just '(' -> shuntingYard tokens (newStack $ pop stack) output
                Just a -> shuntingYard (t:tokens) (newStack $ pop stack) (a:output)
                Nothing -> "" -- mismatched parenthesis
        | otherwise =
            case topOperator of
                Just a -> caseWork a $ (precedenceCmp t a) == -1|| ( (precedenceCmp t a) == 0 && isLeftAssoc t )
                Nothing -> caseWork 'a' False
        where
            topOperator = top stack
            newStack (Just a) = a
            caseWork '(' _ = pushT
            caseWork a True = shuntingYard (t:tokens) (newStack $ pop stack) (a:output) 
            caseWork _ _ = pushT
            pushT = shuntingYard tokens (push t stack) output

    getPosfix :: String -> String
    getPosfix s = intersperse ' ' $ shuntingYard (filter (/=' ') s) (Stack []) []