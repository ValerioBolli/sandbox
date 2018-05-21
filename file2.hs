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