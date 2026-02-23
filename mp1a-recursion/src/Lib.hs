--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding
  ( cycle,
    drop,
    foldl,
    foldr,
    iterate,
    map,
    repeat,
    replicate,
    reverse,
    take,
    zip,
    zipWith,
    (++),
  )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Metadata for autograder
--- -----------------------
tag1 = 21923

tag2 = 44437

tag3 = 24929

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake n (x : xs)
  | n > 0 = x : mytake (n - 1) xs
  | otherwise = []

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop n l@(x : xs)
  | n > 0 = mydrop (n - 1) xs
  | otherwise = l

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev xx = revHelp xx []
  where
    revHelp [] a = a
    revHelp (x : xs) a = revHelp xs (x : a)

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] yy = yy
app (x : xs) yy = x : app xs yy

--- ### inclist

-- don't forget to put the type declaration or you will lose points!

inclist :: (Num a) => [a] -> [a]
inclist [] = []
inclist (x : xs) = (x + 1) : inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: (Num a) => [a] -> a
sumlist xx = summer xx 0
  where
    summer [] acc = acc
    summer (x : xs) acc = summer xs $ acc + x

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a, b)]
myzip [] _ = []
myzip _ [] = []
myzip (x : xs) (y : ys) = (x, y) : myzip xs ys

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs xx yy = zipSum (myzip xx yy)
  where
    zipSum [] = []
    zipSum ((x, y) : ls) = (x + y) : zipSum ls

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1 : ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = nh 0
  where
    nh a = a : nh (a + 1)

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0 : 1 : addpairs fib (tail fib)

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: (Ord a) => a -> [a] -> [a]
add n [] = [n]
add n s@(x : xs)
  | n > x = x : add n xs
  | n < x = n : s
  | otherwise = s

--- ### union

-- don't forget to put the type declaration or you will lose points!

union :: (Ord a) => [a] -> [a] -> [a]
union [] yy = yy
union xx [] = xx
union xx@(x : xs) yy@(y : ys)
  | x < y = x : union xs yy
  | y < x = y : union xx ys
  | x == y = x : union xs ys

--- ### intersect

-- don't forget to put the type declaration or you will lose points!

intersect :: (Ord a) => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect xx@(x : xs) yy@(y : ys)
  | x == y = x : intersect xs ys
  | x < y = intersect xs yy
  | x > y = intersect xx ys

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: (Ord a) => [a] -> [[a]]
powerset [] = [[]]
powerset (x : xs) = union baseSet $ P.map (add x) baseSet
  where
    baseSet = powerset xs

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: (Num a) => [a] -> [a]
inclist' = P.map (+ 1)

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' = P.foldr (+) 0
