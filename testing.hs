module Testing where

import Control.Monad.Except (runExceptT)

program :: IO (Either Error Result)
program = runExceptT
  do
    user <- ExceptT $ fetchUser userId
    subscription <- liftIO $ findSubscription user
    pure $ Result {user, subscription}

fetchUser :: UserId -> IO (Either Error User)
fetchUser = undefined

findSubscription :: User -> IO Subscription
findSubscription = undefined
