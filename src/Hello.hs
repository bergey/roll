module Hello where

sayHello :: String -> IO ()
sayHello name = putStrLn ("hello " ++ name)
