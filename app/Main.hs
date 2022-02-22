{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -ddump-simpl #-}
{-# OPTIONS_GHC -ddump-rule-rewrites -fenable-rewrite-rules #-}

module Main where


data Stream a = forall s . Stream (s -> Step a s) s

data Step a s = Done
              | Yield a s
              | Skip s deriving Show

map_s :: (a -> b) -> Stream a -> Stream b
map_s f (Stream next0 s0) = Stream next s0
  where
    next s = case next0 s of
      Done -> Done
      Skip s' -> Skip s'
      Yield x s' -> Yield (f x) s'

filter_s :: (a -> Bool) -> Stream a -> Stream a
filter_s p (Stream next0 s0) = Stream next s0
  where
    next s = case next0 s0 of
      Done -> Done
      Skip s' -> Skip s'
      Yield x s' | p x -> Yield x s'
                 | otherwise -> Skip s'

append_s :: Stream a -> Stream a -> Stream a
append_s (Stream next0 s0) (Stream next1 s1) = Stream next (Left s0)
  where
    next (Left s) = case next0 s of
      Done -> Skip (Right s1)
      Skip s' -> Skip (Left s')
      Yield a s' -> Yield a (Left s')

    next (Right s) = case next1 s of
      Done -> Done
      Skip s' -> Skip (Right s')
      Yield a s' -> Yield a (Right s')

zip_s :: Stream a -> Stream b -> Stream (a, b)
zip_s (Stream nextA sA0) (Stream nextB sB0) = Stream next (sA0, sB0, Nothing)
  where
    next (sA, sB, Nothing) =
      case nextA sA of
        Done -> Done
        Skip sA' -> Skip (sA', sB, Nothing)
        Yield a sA' -> Skip (sA', sB, Just a)
    next (sA, sB, Just a) =
      case nextB sB of
        Done -> Done
        Skip sB' -> Skip (sA, sB', Just a)
        Yield b sB' -> Yield (a, b) (sA, sB', Nothing)


concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = unstream . concatMap_s (stream . f) . stream

concatMap_s :: (a -> Stream b) -> Stream a -> Stream b
concatMap_s f (Stream next0 s0) = Stream next (s0, Nothing)
  where
    next (s, Nothing) = case next0 s of
      Done -> Done
      Skip s' -> Skip (s', Nothing)
      Yield x s' -> Skip (s', Just (f x))

    next (s, Just (Stream next1 s1)) = case next1 s1 of
      Done -> Skip (s, Nothing)
      Skip s1' -> Skip (s, Just (Stream next1 s1'))
      Yield x s1' -> Yield x (s, Just (Stream next1 s1'))

foldl_s :: (b -> a -> b) -> b -> Stream a -> b
foldl_s f z (Stream next s0) = go z s0
  where
    go z s = case next s of
      Done -> z
      Skip s' -> go z s'
      Yield x s' -> go (f z x) s'

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z = foldl_s f z . stream

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

example :: Num a => [a] -> [a] -> a
example xs ys = foldl_s (+) 0 (append_s (stream xs) (stream ys))

{-# RULES "stream/unstream" forall x. stream (unstream x) = x; #-}
{-# NOINLINE stream #-}
{-# NOINLINE unstream #-}
{-# INLINE append_s #-}
stream :: [a] -> Stream a
stream xs0 = Stream next xs0
  where
    next [] = Done
    next (x:xs) = Yield x xs

unstream :: Stream a -> [a]
unstream (Stream next s0) = unfold s0
  where
    unfold s = case next s of
      Done -> []
      Skip s' -> unfold s'
      Yield x s' -> x:(unfold s')

main :: IO ()
main = print $ example [1..5] [1..10]-- unstream (stream (unstream (stream [1, 2, 3])))
