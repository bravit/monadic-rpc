import ClientUtils
import RFunctions
import Control.Monad hiding (msum)
import Control.Concurrent

evalSmth n = do
    n2 <- if n /= 0 
            then double n 
            else unsupported n
    s <- sum' (n,n2)
    res <- lengths [1..n]
    len <- strlen (show n)
    b <- odd' n
    p <- fun (PairII 42 42)

    return (n, n2, s, res, len, b, p)

evalCorrect n = do
    n1 <- double n
    return n1

evalCorrect2 n = do
    n1 <- sum' (n, n)
    return n1

evalUnsupported = do
    r <- unsupported 42
    return 1

      
evalOrder = do
    n1 <- sum' (2,3)
    b <- odd' 5
    return b



main = runDistributed 
        (ServerAddr "localhost" 1502) 
        (evalOrder) >>= putStrLn.show



{-

main = runDistributed 
        (ServerAddr "localhost" 1502) 
        (liftM3 (\ t1 t2 t3 -> t1++", " ++ t2++", " ++ t3) time time time) >>= putStrLn.show


main = replicateM_ 100 $ do
        mapM_ 
            (\(port, n) -> runDistributed (ServerAddr "localhost" port) (evalSmth n) >>= putStrLn.show) 
            [(1501, 10), (1500, 20), (1502, 30), (1502, 0)] 
        putStrLn "------------"
        runDistributed 
            (ServerAddr "localhost" 1501) 
            (liftM2 (+) (evalCorrect 1) (evalCorrect 10) )  >>= putStrLn.show
        putStrLn "------------"
        runDistributed 
            (ServerAddr "localhost" 1502) 
            (liftM2 (+) evalUnsupported (evalCorrect 10)) >>= putStrLn.show
        putStrLn "------------"
        runDistributed 
            (ServerAddr "localhost" 1501) 
            (number) >>= putStrLn.show
        putStrLn "------------"
        runDistributed 
            (ServerAddr "localhost" 1501) 
            (liftM2 (,) time number) >>= putStrLn.show
-}
