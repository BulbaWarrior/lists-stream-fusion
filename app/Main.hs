{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# OPTIONS_GHC -ddump-simpl #-}
{-# OPTIONS_GHC -ddump-rule-rewrites -fenable-rewrite-rules #-}

module Main where

import qualified GHC.Base

data Stream a = forall s . Stream (s -> Step a s) s

data Step a s
  = Done
  | Yield a s
  | Skip s
  deriving (Show, Functor)

empty_s :: Stream a
empty_s = Stream (const Done) ()

cons_s :: a -> Stream a -> Stream a
cons_s x (Stream next s) = Stream next' (Left x)
  where
    next' (Left y)  = Yield y (Right s)
    next' (Right s) = Right <$> next s

build_s :: (forall a b. (a -> b -> b) -> b -> b) -> Stream a
build_s g = g cons_s empty_s

{-# RULES "stream/build"
      forall (g :: forall a b. (a -> b -> b) -> b -> b).
        stream (GHC.Base.build g) = build_s g
   #-}

map_s :: (a -> b) -> Stream a -> Stream b
map_s f (Stream next0 s0) = Stream next s0
  where
    next s = case next0 s of
      Done       -> Done
      Skip s'    -> Skip s'
      Yield x s' -> Yield (f x) s'

filter_s :: (a -> Bool) -> Stream a -> Stream a
filter_s p (Stream next0 s0) = Stream next s0
  where
    next s = case next0 s0 of
      Done -> Done
      Skip s' -> Skip s'
      Yield x s' | p x -> Yield x s'
                 | otherwise -> Skip s'
{-# INLINE append_s #-}
append_s :: Stream a -> Stream a -> Stream a
append_s (Stream next0 s0) (Stream next1 s1) = Stream next (Left s0)
  where
    next (Left s) = case next0 s of
      Done       -> Skip (Right s1)
      Skip s'    -> Skip (Left s')
      Yield a s' -> Yield a (Left s')

    next (Right s) = case next1 s of
      Done       -> Done
      Skip s'    -> Skip (Right s')
      Yield a s' -> Yield a (Right s')

append :: [a] -> [a] -> [a]
append a b = unstream $ append_s (stream a) (stream b)

zip_s :: Stream a -> Stream b -> Stream (a, b)
zip_s (Stream nextA sA0) (Stream nextB sB0) = Stream next (sA0, sB0, Nothing)
  where
    next (sA, sB, Nothing) =
      case nextA sA of
        Done        -> Done
        Skip sA'    -> Skip (sA', sB, Nothing)
        Yield a sA' -> Skip (sA', sB, Just a)
    next (sA, sB, Just a) =
      case nextB sB of
        Done        -> Done
        Skip sB'    -> Skip (sA, sB', Just a)
        Yield b sB' -> Yield (a, b) (sA, sB', Nothing)


concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = unstream . concatMap_s (stream . f) . stream

concatMap_s :: (a -> Stream b) -> Stream a -> Stream b
concatMap_s f (Stream next0 s0) = Stream next (s0, Nothing)
  where
    next (s, Nothing) = case next0 s of
      Done       -> Done
      Skip s'    -> Skip (s', Nothing)
      Yield x s' -> Skip (s', Just (f x))

    next (s, Just (Stream next1 s1)) = case next1 s1 of
      Done        -> Skip (s, Nothing)
      Skip s1'    -> Skip (s, Just (Stream next1 s1'))
      Yield x s1' -> Yield x (s, Just (Stream next1 s1'))

{-# INLINE foldl_s #-}
foldl_s :: (b -> a -> b) -> b -> Stream a -> b
foldl_s f z (Stream next s0) = go z s0
  where
    go z s = case next s of
      Done       -> z
      Skip s'    -> go z s'
      Yield x s' -> go (f z x) s'

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z = foldl_s f z . stream

return_s :: a -> Stream a
return_s x = Stream next True
  where
    next True  = Yield x False
    next False = Done

enumFromTo_s :: (Enum a, Ord a) => a -> a -> Stream a
enumFromTo_s l h = Stream next l
  where
    next s
      | s > h = Done
      | otherwise = Yield s (succ s)

enumFromTo :: (Enum a, Ord a) => a -> a -> [a]
enumFromTo l h = unstream $ enumFromTo_s l h

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

example :: Num a => [a] -> [a] -> a
example xs ys = foldl_s (+) 0 (append_s (stream xs) (stream ys))

{-# RULES "stream/unstream" forall x. stream (unstream x) = x; #-}
{-# NOINLINE stream #-}
{-# NOINLINE unstream #-}


stream :: [a] -> Stream a
stream xs0 = Stream next xs0
  where
    next []     = Done
    next (x:xs) = Yield x xs

unstream :: Stream a -> [a]
unstream (Stream next s0) = unfold s0
  where
    unfold s = case next s of
      Done       -> []
      Skip s'    -> unfold s'
      Yield x s' -> x:(unfold s')

main :: IO ()
example1 = foldl_s (+) 0 (append_s (enumFromTo_s 1 (10^8)) (enumFromTo_s 1 (10^8)))
example2 = sum' $ append [1..10^7] [1..10^7]

main = do print $ example1
