-- this is a comment
type Stack = [Int]

push :: Int -> State Stack ()
push x = do
    xs <- get
    put (x : xs)

pop :: State Stack Int
pop = do
    (x:xs) <- get
    put xs
    return x

-- this is another comment
exec :: Program -> Int
exec prog = evalState (exec' prog) []

exec' :: Program -> State Stack Int
exec' [] = pop
exec' (Push x:cs) = do
    push x
    exec' cs
exec' (Add:cs) = do
    x <- pop
    y <- pop
    push (x + y)
    exec' cs
exec' (Mul:cs) = do
    x <- pop
    y <- pop
    push (x * y)
    exec' cs