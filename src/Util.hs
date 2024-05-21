module Util where

untilJustM :: Monad m => m (Maybe a) -> m a
untilJustM act = do
  res <- act
  case res of
    Just r -> pure r
    Nothing -> untilJustM act
