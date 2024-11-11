module StateIO where

newtype StateIO st a = StateIO {runStateIO :: st -> IO (a, st)}

instance Functor (StateIO st) where
  fmap f (StateIO run) = StateIO $ \s -> do
    (a, newState) <- run s
    return (f a, newState)

instance Applicative (StateIO st) where
  pure x = StateIO $ \s -> return (x, s)
  (StateIO runF) <*> (StateIO runX) = StateIO $ \s -> do
    (f, s1) <- runF s
    (x, s2) <- runX s1
    return (f x, s2)

instance Monad (StateIO st) where
  return = pure
  (StateIO run) >>= f = StateIO $ \s -> do
    (a, newState) <- run s
    runStateIO (f a) newState

lift :: IO a -> StateIO st a
lift action = StateIO $ \s -> do
  x <- action
  return (x, s)

get :: StateIO st st
get = StateIO $ \s -> return (s, s)

put :: st -> StateIO st ()
put newState = StateIO $ const (return ((), newState))

gets :: (st -> a) -> StateIO st a
gets f = StateIO $ \s -> return (f s, s)

evalStateIO :: StateIO st a -> st -> IO a
evalStateIO (StateIO run) s = do
  (a, _) <- run s
  return a

modify :: (st -> st) -> StateIO st ()
modify f = do
  s <- get
  put (f s)
