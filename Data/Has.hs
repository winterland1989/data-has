{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Data.Has
Description : Simple extensible product
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

This module provide 'Has' class which provide simple extensible product.
The use case for this class is illustrated as following:

@
 \{\-\# LANGUAGE FlexibleContexts \#\-\}

 -- in some library code
 ...
 logInAnyReaderHasLogger :: (Has Logger r, MonadReader r m) => LogString -> m ()
 logInAnyReaderHasLogger s = asks getter >>= logWithLogger s

 queryInAnyReaderHasSQL :: (Has SqlBackEnd r, MonadReader r m) => Query -> m a
 queryInAnyReaderHasSQL q = asks getter >>= queryWithSQL q
 ...

 -- now you want to use these effects together
 ...
 logger <- initLogger  ...
 sql <- initSqlBackEnd ...

 (\`runReader\` (logger, sql)) $ do
       ...
       logInAnyReaderHasLogger ...
       ...
       x <- queryInAnyReaderHasSQL ...
       ...
@

If you need multiple elements with same type, you can use <http://hackage.haskell.org/package/tagged tagged>
like:

@
(Has (Tagged \"StdLogger\" Logger) r, Has (Tagged \"FileLogger\" Logger) r, ...) => ...

runYourMonad ... ( stdLogger :: Tagged \"StdLogger\" Logger
                 , fileLogger :: Tagged \"FileLogger\" Logger, ...)
@

Or you can define newtypes(which is less verbose and require no dependency):

@
newtype StdLogger = StdLogger Logger
newtype FileLogger = FileLogger Logger

runYourMonad ... (StdLogger stdLogger, FileLogger fileLogger)
@

Polymorphic values, such as numeric and string literals(with OverloadedString Enabled)
may lead to type inference failure, you simply need type annotations in these cases:

@ ... (3 :: Int, "hello" :: String, ...) @

-}

module Data.Has (Has(..)) where

import Data.Functor.Const
import Data.Functor.Identity

type Lens t a = forall f. Functor f => (a -> f a) -> t -> f t

-- | A type class for extensible product.
--
-- We provide instances for tuples up to 12 elements by default.
-- You can define your own instance of 'Has', but most of the time tuples will do fine.
--
class Has a t where
    {-# MINIMAL getter, modifier | hasL #-}
    getter :: t -> a
    getter = getConst . hasL Const

    modifier :: (a -> a) -> t -> t
    modifier f t = runIdentity (hasL (Identity . f) t)

    hasL :: Lens t a
    hasL afa t = (\a -> modifier (const a) t) <$> afa (getter t)

instance Has a a where
    getter = id
    {-# INLINABLE getter #-}
    modifier = id
    {-# INLINABLE modifier #-}

instance Has a (a, b) where
    getter (a, _) = a
    {-# INLINABLE getter #-}
    modifier f (a, b) = (f a, b)
    {-# INLINABLE modifier #-}
instance Has b (a, b) where
    getter (_, b) = b
    {-# INLINABLE getter #-}
    modifier f (a, b) = (a, f b)
    {-# INLINABLE modifier #-}

--------------------------------------------------------------------------------

instance Has a (a, b, c) where
    getter (a, _, _) = a
    {-# INLINABLE getter #-}
    modifier f (a, b, c) = (f a, b, c)
    {-# INLINABLE modifier #-}
instance Has b (a, b, c) where
    getter (_, b, _) = b
    {-# INLINABLE getter #-}
    modifier f (a, b, c) = (a, f b, c)
    {-# INLINABLE modifier #-}
instance Has c (a, b, c) where
    getter (_, _, c) = c
    {-# INLINABLE getter #-}
    modifier f (a, b, c) = (a, b, f c)
    {-# INLINABLE modifier #-}

--------------------------------------------------------------------------------

instance Has a (a, b, c, d) where
    getter (a, _, _, _) = a
    {-# INLINABLE getter #-}
    modifier f (a, b, c, d) = (f a, b, c, d)
    {-# INLINABLE modifier #-}
instance Has b (a, b, c, d) where
    getter (_, b, _, _) = b
    {-# INLINABLE getter #-}
    modifier f (a, b, c, d) = (a, f b, c, d)
    {-# INLINABLE modifier #-}
instance Has c (a, b, c, d) where
    getter (_, _, c, _) = c
    {-# INLINABLE getter #-}
    modifier f (a, b, c, d) = (a, b, f c, d)
    {-# INLINABLE modifier #-}
instance Has d (a, b, c, d) where
    getter (_, _, _, d) = d
    {-# INLINABLE getter #-}
    modifier f (a, b, c, d) = (a, b, c, f d)
    {-# INLINABLE modifier #-}

--------------------------------------------------------------------------------

instance Has a (a, b, c, d, e) where
    getter (a, _, _, _, _) = a
    {-# INLINABLE getter #-}
    modifier f (a, b, c, d, e) = (f a, b, c, d, e)
    {-# INLINABLE modifier #-}
instance Has b (a, b, c, d, e) where
    getter (_, b, _, _, _) = b
    {-# INLINABLE getter #-}
    modifier f (a, b, c, d, e) = (a, f b, c, d, e)
    {-# INLINABLE modifier #-}
instance Has c (a, b, c, d, e) where
    getter (_, _, c, _, _) = c
    {-# INLINABLE getter #-}
    modifier f (a, b, c, d, e) = (a, b, f c, d, e)
    {-# INLINABLE modifier #-}
instance Has d (a, b, c, d, e) where
    getter (_, _, _, d, _) = d
    {-# INLINABLE getter #-}
    modifier f (a, b, c, d, e) = (a, b, c, f d, e)
    {-# INLINABLE modifier #-}
instance Has e (a, b, c, d, e) where
    getter (_, _, _, _, e) = e
    {-# INLINABLE getter #-}
    modifier f (a, b, c, d, e) = (a, b, c, d, f e)
    {-# INLINABLE modifier #-}

--------------------------------------------------------------------------------

instance Has a (a, b, c, d, e, f) where
    getter (a, _, _, _, _, _) = a
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f) = (ff a, b, c, d, e, f)
    {-# INLINABLE modifier #-}
instance Has b (a, b, c, d, e, f) where
    getter (_, b, _, _, _, _) = b
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f) = (a, ff b, c, d, e, f)
    {-# INLINABLE modifier #-}
instance Has c (a, b, c, d, e, f) where
    getter (_, _, c, _, _, _) = c
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f) = (a, b, ff c, d, e, f)
    {-# INLINABLE modifier #-}
instance Has d (a, b, c, d, e, f) where
    getter (_, _, _, d, _, _) = d
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f) = (a, b, c, ff d, e, f)
    {-# INLINABLE modifier #-}
instance Has e (a, b, c, d, e, f) where
    getter (_, _, _, _, e, _) = e
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f) = (a, b, c, d, ff e, f)
    {-# INLINABLE modifier #-}
instance Has f (a, b, c, d, e, f) where
    getter (_, _, _, _, _, f) = f
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f) = (a, b, c, d, e, ff f)
    {-# INLINABLE modifier #-}

--------------------------------------------------------------------------------

instance Has a (a, b, c, d, e, f, g) where
    getter (a, _, _, _, _, _, _) = a
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g) = (ff a, b, c, d, e, f, g)
    {-# INLINABLE modifier #-}
instance Has b (a, b, c, d, e, f, g) where
    getter (_, b, _, _, _, _, _) = b
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g) = (a, ff b, c, d, e, f, g)
    {-# INLINABLE modifier #-}
instance Has c (a, b, c, d, e, f, g) where
    getter (_, _, c, _, _, _, _) = c
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g) = (a, b, ff c, d, e, f, g)
    {-# INLINABLE modifier #-}
instance Has d (a, b, c, d, e, f, g) where
    getter (_, _, _, d, _, _, _) = d
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g) = (a, b, c, ff d, e, f, g)
    {-# INLINABLE modifier #-}
instance Has e (a, b, c, d, e, f, g) where
    getter (_, _, _, _, e, _, _) = e
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g) = (a, b, c, d, ff e, f, g)
    {-# INLINABLE modifier #-}
instance Has f (a, b, c, d, e, f, g) where
    getter (_, _, _, _, _, f, _) = f
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g) = (a, b, c, d, e, ff f, g)
    {-# INLINABLE modifier #-}
instance Has g (a, b, c, d, e, f, g) where
    getter (_, _, _, _, _, _, g) = g
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g) = (a, b, c, d, e, f, ff g)
    {-# INLINABLE modifier #-}

--------------------------------------------------------------------------------

instance Has a (a, b, c, d, e, f, g, h) where
    getter (a, _, _, _, _, _, _, _) = a
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h) = (ff a, b, c, d, e, f, g, h)
    {-# INLINABLE modifier #-}
instance Has b (a, b, c, d, e, f, g, h) where
    getter (_, b, _, _, _, _, _, _) = b
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h) = (a, ff b, c, d, e, f, g, h)
    {-# INLINABLE modifier #-}
instance Has c (a, b, c, d, e, f, g, h) where
    getter (_, _, c, _, _, _, _, _) = c
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h) = (a, b, ff c, d, e, f, g, h)
    {-# INLINABLE modifier #-}
instance Has d (a, b, c, d, e, f, g, h) where
    getter (_, _, _, d, _, _, _, _) = d
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h) = (a, b, c, ff d, e, f, g, h)
    {-# INLINABLE modifier #-}
instance Has e (a, b, c, d, e, f, g, h) where
    getter (_, _, _, _, e, _, _, _) = e
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h) = (a, b, c, d, ff e, f, g, h)
    {-# INLINABLE modifier #-}
instance Has f (a, b, c, d, e, f, g, h) where
    getter (_, _, _, _, _, f, _, _) = f
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h) = (a, b, c, d, e, ff f, g, h)
    {-# INLINABLE modifier #-}
instance Has g (a, b, c, d, e, f, g, h) where
    getter (_, _, _, _, _, _, g, _) = g
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h) = (a, b, c, d, e, f, ff g, h)
    {-# INLINABLE modifier #-}
instance Has h (a, b, c, d, e, f, g, h) where
    getter (_, _, _, _, _, _, _, h) = h
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h) = (a, b, c, d, e, f, g, ff h)
    {-# INLINABLE modifier #-}

--------------------------------------------------------------------------------

instance Has a (a, b, c, d, e, f, g, h, i) where
    getter (a, _, _, _, _, _, _, _, _) = a
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i) = (ff a, b, c, d, e, f, g, h, i)
    {-# INLINABLE modifier #-}
instance Has b (a, b, c, d, e, f, g, h, i) where
    getter (_, b, _, _, _, _, _, _, _) = b
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i) = (a, ff b, c, d, e, f, g, h, i)
    {-# INLINABLE modifier #-}
instance Has c (a, b, c, d, e, f, g, h, i) where
    getter (_, _, c, _, _, _, _, _, _) = c
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i) = (a, b, ff c, d, e, f, g, h, i)
    {-# INLINABLE modifier #-}
instance Has d (a, b, c, d, e, f, g, h, i) where
    getter (_, _, _, d, _, _, _, _, _) = d
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i) = (a, b, c, ff d, e, f, g, h, i)
    {-# INLINABLE modifier #-}
instance Has e (a, b, c, d, e, f, g, h, i) where
    getter (_, _, _, _, e, _, _, _, _) = e
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i) = (a, b, c, d, ff e, f, g, h, i)
    {-# INLINABLE modifier #-}
instance Has f (a, b, c, d, e, f, g, h, i) where
    getter (_, _, _, _, _, f, _, _, _) = f
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i) = (a, b, c, d, e, ff f, g, h, i)
    {-# INLINABLE modifier #-}
instance Has g (a, b, c, d, e, f, g, h, i) where
    getter (_, _, _, _, _, _, g, _, _) = g
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i) = (a, b, c, d, e, f, ff g, h, i)
    {-# INLINABLE modifier #-}
instance Has h (a, b, c, d, e, f, g, h, i) where
    getter (_, _, _, _, _, _, _, h, _) = h
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i) = (a, b, c, d, e, f, g, ff h, i)
    {-# INLINABLE modifier #-}
instance Has i (a, b, c, d, e, f, g, h, i) where
    getter (_, _, _, _, _, _, _, _, i) = i
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i) = (a, b, c, d, e, f, g, h, ff i)
    {-# INLINABLE modifier #-}

--------------------------------------------------------------------------------

instance Has a (a, b, c, d, e, f, g, h, i, j) where
    getter (a, _, _, _, _, _, _, _, _, _) = a
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j) = (ff a, b, c, d, e, f, g, h, i, j)
    {-# INLINABLE modifier #-}
instance Has b (a, b, c, d, e, f, g, h, i, j) where
    getter (_, b, _, _, _, _, _, _, _, _) = b
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j) = (a, ff b, c, d, e, f, g, h, i, j)
    {-# INLINABLE modifier #-}
instance Has c (a, b, c, d, e, f, g, h, i, j) where
    getter (_, _, c, _, _, _, _, _, _, _) = c
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j) = (a, b, ff c, d, e, f, g, h, i, j)
    {-# INLINABLE modifier #-}
instance Has d (a, b, c, d, e, f, g, h, i, j) where
    getter (_, _, _, d, _, _, _, _, _, _) = d
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j) = (a, b, c, ff d, e, f, g, h, i, j)
    {-# INLINABLE modifier #-}
instance Has e (a, b, c, d, e, f, g, h, i, j) where
    getter (_, _, _, _, e, _, _, _, _, _) = e
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j) = (a, b, c, d, ff e, f, g, h, i, j)
    {-# INLINABLE modifier #-}
instance Has f (a, b, c, d, e, f, g, h, i, j) where
    getter (_, _, _, _, _, f, _, _, _, _) = f
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j) = (a, b, c, d, e, ff f, g, h, i, j)
    {-# INLINABLE modifier #-}
instance Has g (a, b, c, d, e, f, g, h, i, j) where
    getter (_, _, _, _, _, _, g, _, _, _) = g
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j) = (a, b, c, d, e, f, ff g, h, i, j)
    {-# INLINABLE modifier #-}
instance Has h (a, b, c, d, e, f, g, h, i, j) where
    getter (_, _, _, _, _, _, _, h, _, _) = h
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j) = (a, b, c, d, e, f, g, ff h, i, j)
    {-# INLINABLE modifier #-}
instance Has i (a, b, c, d, e, f, g, h, i, j) where
    getter (_, _, _, _, _, _, _, _, i, _) = i
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j) = (a, b, c, d, e, f, g, h, ff i, j)
    {-# INLINABLE modifier #-}
instance Has j (a, b, c, d, e, f, g, h, i, j) where
    getter (_, _, _, _, _, _, _, _, _, j) = j
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j) = (a, b, c, d, e, f, g, h, i, ff j)
    {-# INLINABLE modifier #-}

--------------------------------------------------------------------------------

instance Has a (a, b, c, d, e, f, g, h, i, j, k) where
    getter (a, _, _, _, _, _, _, _, _, _, _) = a
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k) = (ff a, b, c, d, e, f, g, h, i, j, k)
    {-# INLINABLE modifier #-}
instance Has b (a, b, c, d, e, f, g, h, i, j, k) where
    getter (_, b, _, _, _, _, _, _, _, _, _) = b
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k) = (a, ff b, c, d, e, f, g, h, i, j, k)
    {-# INLINABLE modifier #-}
instance Has c (a, b, c, d, e, f, g, h, i, j, k) where
    getter (_, _, c, _, _, _, _, _, _, _, _) = c
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k) = (a, b, ff c, d, e, f, g, h, i, j, k)
    {-# INLINABLE modifier #-}
instance Has d (a, b, c, d, e, f, g, h, i, j, k) where
    getter (_, _, _, d, _, _, _, _, _, _, _) = d
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k) = (a, b, c, ff d, e, f, g, h, i, j, k)
    {-# INLINABLE modifier #-}
instance Has e (a, b, c, d, e, f, g, h, i, j, k) where
    getter (_, _, _, _, e, _, _, _, _, _, _) = e
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k) = (a, b, c, d, ff e, f, g, h, i, j, k)
    {-# INLINABLE modifier #-}
instance Has f (a, b, c, d, e, f, g, h, i, j, k) where
    getter (_, _, _, _, _, f, _, _, _, _, _) = f
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k) = (a, b, c, d, e, ff f, g, h, i, j, k)
    {-# INLINABLE modifier #-}
instance Has g (a, b, c, d, e, f, g, h, i, j, k) where
    getter (_, _, _, _, _, _, g, _, _, _, _) = g
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k) = (a, b, c, d, e, f, ff g, h, i, j, k)
    {-# INLINABLE modifier #-}
instance Has h (a, b, c, d, e, f, g, h, i, j, k) where
    getter (_, _, _, _, _, _, _, h, _, _, _) = h
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k) = (a, b, c, d, e, f, g, ff h, i, j, k)
    {-# INLINABLE modifier #-}
instance Has i (a, b, c, d, e, f, g, h, i, j, k) where
    getter (_, _, _, _, _, _, _, _, i, _, _) = i
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k) = (a, b, c, d, e, f, g, h, ff i, j, k)
    {-# INLINABLE modifier #-}
instance Has j (a, b, c, d, e, f, g, h, i, j, k) where
    getter (_, _, _, _, _, _, _, _, _, j, _) = j
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k) = (a, b, c, d, e, f, g, h, i, ff j, k)
    {-# INLINABLE modifier #-}
instance Has k (a, b, c, d, e, f, g, h, i, j, k) where
    getter (_, _, _, _, _, _, _, _, _, _, k) = k
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k) = (a, b, c, d, e, f, g, h, i, j, ff k)
    {-# INLINABLE modifier #-}

--------------------------------------------------------------------------------

instance Has a (a, b, c, d, e, f, g, h, i, j, k, l) where
    getter (a, _, _, _, _, _, _, _, _, _, _, _) = a
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k, l) = (ff a, b, c, d, e, f, g, h, i, j, k, l)
    {-# INLINABLE modifier #-}
instance Has b (a, b, c, d, e, f, g, h, i, j, k, l) where
    getter (_, b, _, _, _, _, _, _, _, _, _, _) = b
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k, l) = (a, ff b, c, d, e, f, g, h, i, j, k, l)
    {-# INLINABLE modifier #-}
instance Has c (a, b, c, d, e, f, g, h, i, j, k, l) where
    getter (_, _, c, _, _, _, _, _, _, _, _, _) = c
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k, l) = (a, b, ff c, d, e, f, g, h, i, j, k, l)
    {-# INLINABLE modifier #-}
instance Has d (a, b, c, d, e, f, g, h, i, j, k, l) where
    getter (_, _, _, d, _, _, _, _, _, _, _, _) = d
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k, l) = (a, b, c, ff d, e, f, g, h, i, j, k, l)
    {-# INLINABLE modifier #-}
instance Has e (a, b, c, d, e, f, g, h, i, j, k, l) where
    getter (_, _, _, _, e, _, _, _, _, _, _, _) = e
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k, l) = (a, b, c, d, ff e, f, g, h, i, j, k, l)
    {-# INLINABLE modifier #-}
instance Has f (a, b, c, d, e, f, g, h, i, j, k, l) where
    getter (_, _, _, _, _, f, _, _, _, _, _, _) = f
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k, l) = (a, b, c, d, e, ff f, g, h, i, j, k, l)
    {-# INLINABLE modifier #-}
instance Has g (a, b, c, d, e, f, g, h, i, j, k, l) where
    getter (_, _, _, _, _, _, g, _, _, _, _, _) = g
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k, l) = (a, b, c, d, e, f, ff g, h, i, j, k, l)
    {-# INLINABLE modifier #-}
instance Has h (a, b, c, d, e, f, g, h, i, j, k, l) where
    getter (_, _, _, _, _, _, _, h, _, _, _, _) = h
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k, l) = (a, b, c, d, e, f, g, ff h, i, j, k, l)
    {-# INLINABLE modifier #-}
instance Has i (a, b, c, d, e, f, g, h, i, j, k, l) where
    getter (_, _, _, _, _, _, _, _, i, _, _, _) = i
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k, l) = (a, b, c, d, e, f, g, h, ff i, j, k, l)
    {-# INLINABLE modifier #-}
instance Has j (a, b, c, d, e, f, g, h, i, j, k, l) where
    getter (_, _, _, _, _, _, _, _, _, j, _, _) = j
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k, l) = (a, b, c, d, e, f, g, h, i, ff j, k, l)
    {-# INLINABLE modifier #-}
instance Has k (a, b, c, d, e, f, g, h, i, j, k, l) where
    getter (_, _, _, _, _, _, _, _, _, _, k, _) = k
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k, l) = (a, b, c, d, e, f, g, h, i, j, ff k, l)
    {-# INLINABLE modifier #-}
instance Has l (a, b, c, d, e, f, g, h, i, j, k, l) where
    getter (_, _, _, _, _, _, _, _, _, _, _, l) = l
    {-# INLINABLE getter #-}
    modifier ff (a, b, c, d, e, f, g, h, i, j, k, l) = (a, b, c, d, e, f, g, h, i, j, k, ff l)
    {-# INLINABLE modifier #-}
