import           Control.Exception

catchIt :: ArithException -> Maybe ()
catchIt DivideByZero = Just ()
catchIt _ = Nothing

-- catchIt :: Exception -> Maybe ()
-- catchIt (ArithException DivideByZero) = Just ()
-- catchIt _ = Nothing
handler :: () -> IO ()
handler _ = putStrLn "Caught error: divide by zero"

safePrint :: Integer -> IO ()
safePrint x = handleJust catchIt handler (print x)