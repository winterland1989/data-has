{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Main where

-------------------------------------------------------------------------------

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class
import           Criterion.Main
import           Data.Has
import           GHC.TypeLits

main :: IO ()
main = defaultMain
    [ bgroup "has vs 2 layer reader"
        [ bench "has" $ whnf (runReader hasReader) (1 :: Int, "hello" :: String)
        , bench "2 layer" $ whnf (\ (i, s) -> runReader (runReaderT multiReader i) s)
                                   (1 :: Int, "hello" :: String)
        ]
    , bgroup "has vs 8 layer reader"
        [ bench "has" $ whnf (runReader hasReader8)
                             ( T 1 :: T 1 Int
                             , T 2 :: T 2 Int
                             , T 3 :: T 3 Int
                             , T 4 :: T 4 Int
                             , T 5 :: T 5 Int
                             , T 6 :: T 6 Int
                             , T 7 :: T 7 Int
                             , T 8 :: T 8 Int)
        , bench "8 layer" $ whnf (\ (i1, i2, i3, i4, i5, i6, i7, i8) ->
                                     runReader
                                     (runReaderT
                                     (runReaderT
                                     (runReaderT
                                     (runReaderT
                                     (runReaderT
                                     (runReaderT
                                     (runReaderT multiReader8 i1
                                     ) i2) i3) i4) i5) i6) i7) i8
                                 ) (1, 2, 3, 4, 5, 6, 7, 8)
        ]
    ]

hasReader :: (Has Int r, Has String r) => Reader r String
hasReader = do
    i :: Int <- asks getter
    s :: String <- asks getter
    return (s ++ show i)

multiReader :: ReaderT Int (Reader String) String
multiReader = do
    i <- ask
    s <- lift ask
    return (s ++ show i)

newtype T (a :: Nat) b = T { uT :: b }

hasReader8 :: ( Has (T 1 Int) r
              , Has (T 2 Int) r
              , Has (T 3 Int) r
              , Has (T 4 Int) r
              , Has (T 5 Int) r
              , Has (T 6 Int) r
              , Has (T 7 Int) r
              , Has (T 8 Int) r
              ) => Reader r Int
hasReader8 = do
    i1 :: T 1 Int <- asks getter
    i2 :: T 2 Int <- asks getter
    i3 :: T 3 Int <- asks getter
    i4 :: T 4 Int <- asks getter
    i5 :: T 5 Int <- asks getter
    i6 :: T 6 Int <- asks getter
    i7 :: T 7 Int <- asks getter
    i8 :: T 8 Int <- asks getter
    return (uT i1 + uT i2 + uT i3 + uT i4 + uT i5 + uT i6 + uT i7 + uT i8)

multiReader8 :: ReaderT Int
                (ReaderT Int
                (ReaderT Int
                (ReaderT Int
                (ReaderT Int
                (ReaderT Int
                (ReaderT Int
                (Reader Int))))))) Int
multiReader8 = do
    i1 <- ask
    i2 <- lift ask
    i3 <- lift (lift ask)
    i4 <- lift (lift (lift ask))
    i5 <- lift (lift (lift (lift ask)))
    i6 <- lift (lift (lift (lift (lift ask))))
    i7 <- lift (lift (lift (lift (lift (lift ask)))))
    i8 <- lift (lift (lift (lift (lift (lift (lift ask))))))
    return (i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8)
