{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

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
 logInAnyReaderHasLogger s = asks get >>= logWithLogger s

 queryInAnyReaderHasSQL :: (Has SqlBackEnd r, MonadReader r m) => Query -> m a
 queryInAnyReaderHasSQL q = asks get >>= queryWithSQL q
 ...

 -- now you want to use these effects together
 ...
 logger <- initLogger  ...
 sql <- initSqlBackEnd ...

 (`runReader` (logger, sql)) $ do
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

module Data.Has where

-- | A type class for extensible product.
--
-- We provide instances for tuples up to 8 elements by default.
-- You can define your own instance of 'Has', but most of the time tuples will do fine.
--
class Has a t where
    get :: t -> a
    modify :: (a -> a) -> t -> t

instance Has a a where
    get = id
    {-# INLINABLE get #-}
    modify = id
    {-# INLINABLE modify #-}

instance Has a (a, b) where
    get (a, _) = a
    {-# INLINABLE get #-}
    modify f (a, b) = (f a, b)
    {-# INLINABLE modify #-}
instance Has b (a, b) where
    get (_, b) = b
    {-# INLINABLE get #-}
    modify f (a, b) = (a, f b)
    {-# INLINABLE modify #-}

--------------------------------------------------------------------------------

instance Has a (a, b, c) where
    get (a, _, _) = a
    {-# INLINABLE get #-}
    modify f (a, b, c) = (f a, b, c)
    {-# INLINABLE modify #-}
instance Has b (a, b, c) where
    get (_, b, _) = b
    {-# INLINABLE get #-}
    modify f (a, b, c) = (a, f b, c)
    {-# INLINABLE modify #-}
instance Has c (a, b, c) where
    get (_, _, c) = c
    {-# INLINABLE get #-}
    modify f (a, b, c) = (a, b, f c)
    {-# INLINABLE modify #-}

--------------------------------------------------------------------------------

instance Has a (a, b, c, d) where
    get (a, _, _, _) = a
    {-# INLINABLE get #-}
    modify f (a, b, c, d) = (f a, b, c, d)
    {-# INLINABLE modify #-}
instance Has b (a, b, c, d) where
    get (_, b, _, _) = b
    {-# INLINABLE get #-}
    modify f (a, b, c, d) = (a, f b, c, d)
    {-# INLINABLE modify #-}
instance Has c (a, b, c, d) where
    get (_, _, c, _) = c
    {-# INLINABLE get #-}
    modify f (a, b, c, d) = (a, b, f c, d)
    {-# INLINABLE modify #-}
instance Has d (a, b, c, d) where
    get (_, _, _, d) = d
    {-# INLINABLE get #-}
    modify f (a, b, c, d) = (a, b, c, f d)
    {-# INLINABLE modify #-}

--------------------------------------------------------------------------------

instance Has a (a, b, c, d, e) where
    get (a, _, _, _, _) = a
    {-# INLINABLE get #-}
    modify f (a, b, c, d, e) = (f a, b, c, d, e)
    {-# INLINABLE modify #-}
instance Has b (a, b, c, d, e) where
    get (_, b, _, _, _) = b
    {-# INLINABLE get #-}
    modify f (a, b, c, d, e) = (a, f b, c, d, e)
    {-# INLINABLE modify #-}
instance Has c (a, b, c, d, e) where
    get (_, _, c, _, _) = c
    {-# INLINABLE get #-}
    modify f (a, b, c, d, e) = (a, b, f c, d, e)
    {-# INLINABLE modify #-}
instance Has d (a, b, c, d, e) where
    get (_, _, _, d, _) = d
    {-# INLINABLE get #-}
    modify f (a, b, c, d, e) = (a, b, c, f d, e)
    {-# INLINABLE modify #-}
instance Has e (a, b, c, d, e) where
    get (_, _, _, _, e) = e
    {-# INLINABLE get #-}
    modify f (a, b, c, d, e) = (a, b, c, d, f e)
    {-# INLINABLE modify #-}

--------------------------------------------------------------------------------

instance Has a (a, b, c, d, e, f) where
    get (a, _, _, _, _, _) = a
    {-# INLINABLE get #-}
    modify ff (a, b, c, d, e, f) = (ff a, b, c, d, e, f)
    {-# INLINABLE modify #-}
instance Has b (a, b, c, d, e, f) where
    get (_, b, _, _, _, _) = b
    {-# INLINABLE get #-}
    modify ff (a, b, c, d, e, f) = (a, ff b, c, d, e, f)
    {-# INLINABLE modify #-}
instance Has c (a, b, c, d, e, f) where
    get (_, _, c, _, _, _) = c
    {-# INLINABLE get #-}
    modify ff (a, b, c, d, e, f) = (a, b, ff c, d, e, f)
    {-# INLINABLE modify #-}
instance Has d (a, b, c, d, e, f) where
    get (_, _, _, d, _, _) = d
    {-# INLINABLE get #-}
    modify ff (a, b, c, d, e, f) = (a, b, c, ff d, e, f)
    {-# INLINABLE modify #-}
instance Has e (a, b, c, d, e, f) where
    get (_, _, _, _, e, _) = e
    {-# INLINABLE get #-}
    modify ff (a, b, c, d, e, f) = (a, b, c, d, ff e, f)
    {-# INLINABLE modify #-}
instance Has f (a, b, c, d, e, f) where
    get (_, _, _, _, _, f) = f
    {-# INLINABLE get #-}
    modify ff (a, b, c, d, e, f) = (a, b, c, d, e, ff f)
    {-# INLINABLE modify #-}

--------------------------------------------------------------------------------

instance Has a (a, b, c, d, e, f, g) where
    get (a, _, _, _, _, _, _) = a
    {-# INLINABLE get #-}
    modify ff (a, b, c, d, e, f, g) = (ff a, b, c, d, e, f, g)
    {-# INLINABLE modify #-}
instance Has b (a, b, c, d, e, f, g) where
    get (_, b, _, _, _, _, _) = b
    {-# INLINABLE get #-}
    modify ff (a, b, c, d, e, f, g) = (a, ff b, c, d, e, f, g)
    {-# INLINABLE modify #-}
instance Has c (a, b, c, d, e, f, g) where
    get (_, _, c, _, _, _, _) = c
    {-# INLINABLE get #-}
    modify ff (a, b, c, d, e, f, g) = (a, b, ff c, d, e, f, g)
    {-# INLINABLE modify #-}
instance Has d (a, b, c, d, e, f, g) where
    get (_, _, _, d, _, _, _) = d
    {-# INLINABLE get #-}
    modify ff (a, b, c, d, e, f, g) = (a, b, c, ff d, e, f, g)
    {-# INLINABLE modify #-}
instance Has e (a, b, c, d, e, f, g) where
    get (_, _, _, _, e, _, _) = e
    {-# INLINABLE get #-}
    modify ff (a, b, c, d, e, f, g) = (a, b, c, d, ff e, f, g)
    {-# INLINABLE modify #-}
instance Has f (a, b, c, d, e, f, g) where
    get (_, _, _, _, _, f, _) = f
    {-# INLINABLE get #-}
    modify ff (a, b, c, d, e, f, g) = (a, b, c, d, e, ff f, g)
    {-# INLINABLE modify #-}
instance Has g (a, b, c, d, e, f, g) where
    get (_, _, _, _, _, _, g) = g
    {-# INLINABLE get #-}
    modify ff (a, b, c, d, e, f, g) = (a, b, c, d, e, f, ff g)
    {-# INLINABLE modify #-}

--------------------------------------------------------------------------------

instance Has a (a, b, c, d, e, f, g, h) where
    get (a, _, _, _, _, _, _, _) = a
    {-# INLINABLE get #-}
    modify ff (a, b, c, d, e, f, g, h) = (ff a, b, c, d, e, f, g, h)
    {-# INLINABLE modify #-}
instance Has b (a, b, c, d, e, f, g, h) where
    get (_, b, _, _, _, _, _, _) = b
    {-# INLINABLE get #-}
    modify ff (a, b, c, d, e, f, g, h) = (a, ff b, c, d, e, f, g, h)
    {-# INLINABLE modify #-}
instance Has c (a, b, c, d, e, f, g, h) where
    get (_, _, c, _, _, _, _, _) = c
    {-# INLINABLE get #-}
    modify ff (a, b, c, d, e, f, g, h) = (a, b, ff c, d, e, f, g, h)
    {-# INLINABLE modify #-}
instance Has d (a, b, c, d, e, f, g, h) where
    get (_, _, _, d, _, _, _, _) = d
    {-# INLINABLE get #-}
    modify ff (a, b, c, d, e, f, g, h) = (a, b, c, ff d, e, f, g, h)
    {-# INLINABLE modify #-}
instance Has e (a, b, c, d, e, f, g, h) where
    get (_, _, _, _, e, _, _, _) = e
    {-# INLINABLE get #-}
    modify ff (a, b, c, d, e, f, g, h) = (a, b, c, d, ff e, f, g, h)
    {-# INLINABLE modify #-}
instance Has f (a, b, c, d, e, f, g, h) where
    get (_, _, _, _, _, f, _, _) = f
    {-# INLINABLE get #-}
    modify ff (a, b, c, d, e, f, g, h) = (a, b, c, d, e, ff f, g, h)
    {-# INLINABLE modify #-}
instance Has g (a, b, c, d, e, f, g, h) where
    get (_, _, _, _, _, _, g, _) = g
    {-# INLINABLE get #-}
    modify ff (a, b, c, d, e, f, g, h) = (a, b, c, d, e, f, ff g, h)
    {-# INLINABLE modify #-}
instance Has h (a, b, c, d, e, f, g, h) where
    get (_, _, _, _, _, _, _, h) = h
    {-# INLINABLE get #-}
    modify ff (a, b, c, d, e, f, g, h) = (a, b, c, d, e, f, g, ff h)
    {-# INLINABLE modify #-}
