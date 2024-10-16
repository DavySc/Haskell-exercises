module RegisteredUser where

newtype Username = Username String

newtype AccountNumber = AccountNumber Int

data User = UnregisteredUser | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "Unregistered user"
printUser (RegisteredUser (Username name) (AccountNumber accNum)) = putStrLn $ name ++ " " ++ show accNum
