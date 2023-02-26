import ShuntingYard

main :: IO ()
main = do
    input <- getLine
    let expression = head $ lines input
    putStrLn $ getPosfix expression