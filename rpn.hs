-- RPN solver a la learn you a haskell


solveRPN :: String -> Float
solveRPN expression = head . foldl f [] $ words expression
  where f (x:y:ys) "*" = (x * y):ys
        f (x:y:ys) "-" = (y - x):ys
        f (x:y:ys) "+" = (x + y):ys
        f (x:y:ys) "/" = (y / x):ys
        f (x:y:ys) "^" = (y ** x):ys
        f (x:xs)  "ln" = log x:xs
        f xs numberStr = read numberStr:xs
