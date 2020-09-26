module Hello.Lib where

sayHello :: String -> IO ()
sayHello name = putStrLn ("hello " ++ name)
