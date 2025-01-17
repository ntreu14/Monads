module Transformers where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Data.Char (isUpper, isLower)
import GHC.RTS.Flags (TraceFlags(user))

main1 :: IO ()
main1 = do
  maybeUserName <- readUserName
  case maybeUserName of
    Nothing -> print "Invalid user name!"
    Just (uName) -> do
      maybeEmail <- readEmail
      case maybeEmail of
        Nothing -> print "Invalid email!"
        Just (email) -> do
          maybePassword <- readPassword
          case maybePassword of
            Nothing -> print "Invalid Password"
            Just password -> login uName email password

readUserName :: IO (Maybe String)
readUserName = do
  putStrLn "Please enter your username!"
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing

readEmail :: IO (Maybe String)
readEmail = do
  putStrLn "Please enter your email!"
  str <- getLine
  if '@' `elem` str && '.' `elem` str
    then return $ Just str
    else return Nothing

readPassword :: IO (Maybe String)
readPassword = do
  putStrLn "Please enter your Password!"
  str <- getLine
  if length str < 8 || null (filter isUpper str) || null (filter isLower str)
    then return Nothing
    else return $ Just str

login :: String -> String -> String -> IO ()
login username email password = putStrLn $ "Now logged in as: " ++ username

{- MaybeT Reference

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Monad m) => Monad (MaybeT m) where
  return = lift . return
  x >>= f = MaybeT $ do
    v <- runMaybeT x
    case v of
      Nothing -> return Nothing
      Just y  -> runMaybeT (f y)

-}

-- TODO: Fill in these functions to work with "MaybeT"!
readUserName' :: MaybeT IO String
readUserName' = MaybeT $ do
  str <- getLine
  if length str > 5 then
    pure $ Just str
  else
    pure Nothing

readEmail' :: MaybeT IO String
readEmail' = MaybeT $ do
  print "Please enter email:"
  str <- getLine
  if length str > 5 then
    pure $ Just str
  else
    pure Nothing


readPassword' :: MaybeT IO String
readPassword' = MaybeT $ do
  print "Please enter password:"
  str <- getLine
  if not (any isUpper str) || length str < 8 || not (any isLower str)
    then pure Nothing
    else pure $ Just str

main2 :: IO ()
main2 = do
  perhapsCreds <- runMaybeT $ do
    user <- readUserName'
    email <- readEmail'
    password <- readPassword'
    pure (user, email, password)
  
  case perhapsCreds of
    Just (u, e, p) -> login u e p
    Nothing -> print "can't login"

-- Lifting

type Env = (Maybe String, Maybe String, Maybe String)

-- TODO:
-- Try retrieving the username from the first element of the Env
-- If it is Nothing, then reading the input!
-- You'll have to use a couple "lift"s!
readUserName'' :: MaybeT (ReaderT Env IO) String
readUserName'' = MaybeT $ do
  (perhapsUserName, _, _) <- ask
  case perhapsUserName of
    Just str -> pure $ Just str
    Nothing -> do
      lift $ print "Enter username"
      input <- lift getLine
      if length input > 5 then
        pure $ Just input
      else 
        pure Nothing

-- These functions aren't necessary for the above example ^^
type TripleMonad a = MaybeT (ReaderT Env IO) a

performReader :: ReaderT Env IO a -> TripleMonad a
performReader = lift

performIO :: IO a -> TripleMonad a
performIO = lift . lift

{- MonadTrans and MonadIO Reference

class MonadTrans t where
  lift :: (Monad m) => m a -> t m a

class (Monad m) => MonadIO m where
  liftIO :: IO a -> m a

-}

debugFunc :: (MonadIO m) => String -> m ()
debugFunc input = liftIO $ putStrLn ("Successfully produced input: " ++ input)

-- TODO: Re-write main2, but run 'debugFunc' each time you get a portion of
--       the user's input!
main3 :: IO ()
main3 = do
  perhapsCreds <- runMaybeT $ do
    user <- readUserName'
    debugFunc user

    email <- readEmail'
    debugFunc email

    password <- readPassword'
    debugFunc password

    pure (user, email, password)

  case perhapsCreds of
    Just (u, e, p) -> login u e p
    Nothing -> print "can't login" 