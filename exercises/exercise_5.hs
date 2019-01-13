data Expr = Lit Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

eval :: Expr -> Int
eval (Lit n)     = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2

showExpr :: Expr -> String
showExpr (Lit n)     = show n
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ "-" ++ showExpr e2 ++")"
showExpr (Mul e1 e2) = "(" ++ showExpr e1 ++ "*" ++ showExpr e2 ++")"
showExpr (Div e1 e2) = "(" ++ showExpr e1 ++ "/" ++ showExpr e2 ++")"

--- B ---

size :: Expr -> Int
size (Lit n) = 1
size (Add e1 e2) = size e1 + size e2
size (Sub e1 e2) = size e1 + size e2
size (Div e1 e2) = size e1+size e2
size (Mul e1 e2) = size e1+size e2

--- C ---

