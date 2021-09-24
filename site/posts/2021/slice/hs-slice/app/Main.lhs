---
title: "Slicing Vectors"
---

**Preamble**

This is a [literate haskell
document](https://wiki.haskell.org/Literate_programming). It was developed using
GHC version `8.10.4` with the following extensions:

\begin{code}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-} -- `*` is not type
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
\end{code}

And these are the imports:

\begin{code}
module Main where

import Data.Kind
import Data.Proxy
\end{code}

The Problem
===========

It all started with a text book problem asking to implement:

\begin{code}
splitInHalves :: [a] -> ([a], [a])
\end{code}

One possible implementation could be:

\begin{code}
splitInHalves xs = splitAt (length xs `div` 2) xs
\end{code}

Of course, this has one big problem:

\begin{code}
badSplit :: ([Int], [Int])
badSplit = splitInHalves $ repeat 69
\end{code}

*I.e.*, when the list is infinite, this code compiles without raising any
warnings or errors.

Can I write a safe version of this function?
============================================

As the problem arises when lists are infinite, it seems natural that the
solution would try to forbid them.

Let's first try a simple solution that fails controllably when the list is too
long at runtime.

\begin{code}
safeSplitInHalves :: Int -> [a] -> Maybe ([a], [a])
safeSplitInHalves limit xs = splitAt <$> fmap (`div` 2) (safeLength 0 xs) <*> pure xs
	where
	safeLength acc [] = Just acc
	safeLength acc (_ : ys) =
		if acc > limit
			then Nothing
			else safeLength (acc + 1) ys
\end{code}

Now, nothing prevents someone to still write:

\begin{code}
safeBadSplit :: Int -> Maybe ([Int], [Int])
safeBadSplit = (`safeSplitInHalves` repeat 69)
\end{code}

It still type checks fine, but at the end is just a fancy way of writing
`const Nothing` that uses more memory.

Can I write a function that fails to type check for the `badSplit`?
===================================================================

Perhaps I shouldn't, but I can certainly try.

The cause of problems with the previous version is that I don't know if the
list is finite at **compile** time. This is why the next solution uses
*vectors*, which could be thought as list with known length at compile time. The
way to implement *vectors* in Haskell is well established.

I start by defining Peano naturals' type:

\begin{code}
-- | Peano naturals
data Nat where
	Z :: Nat
	S :: Nat -> Nat

-- some constants
one :: Nat
one = S Z

two :: Nat
two = S one

three :: Nat
three = S two

four :: Nat
four = S three

-- some Kind constants
type Zero = 'Z
type One = 'S Zero
type Two = 'S One
type Three = 'S Two
type Four = 'S Three
type Five = 'S Four
\end{code}

Now I can define vectors of known length:

\begin{code}
-- | A Vector has known length at compile time
data Vect :: Nat -> Type -> Type where
	VNil :: Vect 'Z a
	(:#) :: a -> Vect n a -> Vect ('S n) a
infixr 5 :#

-- | Example vector
example :: Vect Four Char
example = '1' :# '2' :# '3' :# '4' :# VNil
\end{code}

In this case `example` is known to be of length 4 at compile time. Trying to add
more elements in its definition would result in a type error.

One example of using *vector* is safely computing the head of a vector known to
contain at least one element:

\begin{code}
-- | Safe head of a vector
vHead :: Vect ('S n) a -> a
vHead (x :# _) = x
\end{code}

Writing increasingly complex functions
======================================

Still with my goal in mind, I decided to try to write some more functions over
`Vect`.

Take One, Drop One
------------------

One function, over lists, that plays nicer with Haskell laziness could be:

\begin{code}
-- | Return half of the elements taking one, dropping the next, etc...
skipHalfTheElements :: [a] -> [a]
skipHalfTheElements [] = []
skipHalfTheElements [_] = []
skipHalfTheElements (x : _ : xs) = x : skipHalfTheElements xs
\end{code}

I can write one that works on `Vect` like:

\begin{code}

-- | Compute the half of a natural number
type family NatHalf (n :: Nat) :: Nat where
	NatHalf 'Z = 'Z
	NatHalf ('S 'Z) = 'Z
	NatHalf ('S ('S n)) = 'S (NatHalf n)

-- Grab every other value
takeOneDropOne :: Vect n a -> Vect (NatHalf n) a
takeOneDropOne VNil = VNil
takeOneDropOne (_ :# VNil) = VNil
takeOneDropOne (x :# (_ :# xs)) = x :# takeOneDropOne xs

-- I know the result has length Two
skippedExample :: Vect Two Char
skippedExample = takeOneDropOne example
\end{code}

As its signature suggests, `takeOneDropOne` takes a `Vect` with `n` element and
returns one with half the input length. For this function to work, I need to
provide a way to compute the half, and that is what `NatHalf` does.

`NatHalf` is a type level function that takes types of kind `Nat` and returns
their half. I could have made a mistake writing it, but if it's correct then, at
compile time, I know that the output of `takeOneDropOne` has half the length of
the input.

Droping elements from a vector
------------------------------

Next I implemented `vDrop` that works like `drop :: Int -> [a] -> [a]` but for
`Vect`. To know the length of the result at compile time, the type checker also
needs to know the amount that will be dropped and that the original vector has
enough elements.

For the later, I define, the usual type level addition of naturals:

\begin{code}
type family (:+:) (n :: Nat) (m :: Nat) :: Nat where
	(:+:) 'Z m = m
	(:+:) ('S n) m = 'S (n :+: m)
\end{code}

The amount to drop can be passed using a singleton type. A singleton type is a
type that only has one constructor. That means that given the type, there is
only one way to construct the value.

`SNat n` is a type indexed by a natural that is also a singleton. This is also
the familiar way of implementing it.

\begin{code}
-- | Singleton type indexed by naturals.
data SNat :: Nat -> Type where
	SZ :: SNat 'Z
	SS :: SNat n -> SNat ('S n)

-- | Some synonyms
type SZero = SNat 'Z
type SOne = SNat ( 'S 'Z )
type STwo = SNat ( 'S ('S 'Z ))
type SThree = SNat ( 'S ( 'S ('S 'Z )))
type SFour = SNat ( 'S ('S ( 'S ('S 'Z ))))
type SFive = SNat ( 'S ('S ('S ( 'S ('S 'Z )))))
\end{code}

Because knowing that a value has some type `SNat n` means knowing how it's
built. The translation from type to value can be "automated" with a type class.

\begin{code}
class Singleton a where
	reify :: Proxy a -> a

instance Singleton (SNat 'Z) where
	reify Proxy = SZ

instance Singleton (SNat n) => Singleton (SNat ('S n)) where
	reify Proxy = SS (reify Proxy)
\end{code}

The `data Proxy a = Proxy` is a technique to bring a type to the context without bringing any
particular value. With this instance we can define some constants easily:

\begin{code}

szero :: SZero
szero = reify Proxy

sone :: SOne
sone = reify Proxy

stwo :: STwo
stwo = reify Proxy

sthree :: SThree
sthree = reify Proxy

sfour :: SFour
sfour = reify Proxy

sfive :: SFive
sfive = reify Proxy

\end{code}

Now I've all the pieces to build the `vDrop` function that follows:

\begin{code}

-- | Type safe drop function
vDrop :: SNat n -> Vect (n :+: m) a -> Vect m a
vDrop SZ xs = xs
vDrop (SS n) (_ :# xs) = vDrop n xs

-- | Example using vDrop
droppedExample :: Vect One Char
droppedExample = vDrop sthree example

\end{code}

Taking elements from a vector
-----------------------------

Writing `vTake` turned out to be harder than `vDrop` and required an *ad hoc*
construction to turn it type safe:

\begin{code}

-- | LTE n m, proof that n <= m
data LTE :: Nat -> Nat -> Type where
	LTEZero :: LTE 'Z m
	LTESucc :: LTE n m -> LTE ('S n) ('S m)

\end{code}

The `LTE n m` type, indexed by two naturals, can only be constructed if `n <=
m`. This is known way to represent it. It could be read as:

* Use `LTEZero` to prove that `0 <= m`
* To construct a proof that `S n_ <= S m`, provide a proof that `n <= m`.

With this representation there is only one way to proof that `n <= m`. This
means that `LTE n m` is a singleton hence:

\begin{code}

instance Singleton (LTE 'Z m) where
	reify _ = LTEZero

instance Singleton (LTE n m) => Singleton (LTE ('S n) ('S m)) where
	reify _ = LTESucc (reify Proxy)

\end{code}

\begin{code}

-- | Take n elements from a vector of length t with n <= t.
vTake :: LTE n t -> Vect t a -> Vect n a
vTake LTEZero _ = VNil
vTake (LTESucc p) (x :# xs) = x :# vTake p xs

-- Example using `vTake`
takedExample :: Vect Three Char
takedExample = vTake (reify Proxy) example

\end{code}

I could use `reify Proxy` in this case instead of `sthree`, which is nice. Also,
the amount to take, can be deduced from the proof that we have enough elements
to take, so I don't need to pass a `SNat n`.

Can I slice vectors type-safely?
================================

Yes, and I figured this one out before `vTake`.

Before approaching the halving of a `Vect`, I took a look at the, perhaps, more
general problem of slicing vectors. Slicing is a term python or R programmers
use to describe the action of extracting a sub-collection from a collection
(usually lists or arrays).


My solution also uses an *ad hoc* type to represent, at the type level, the
values that I want to *slice out*.

\begin{code}

-- | Indices for slices
data Slice (k :: Nat) :: Nat -> Vect k Bool -> Type where
	-- | Extract nothing from an empty Vect
	SliceNil :: Slice 'Z 'Z 'VNil
	-- | Include the next value
	SliceInc :: Slice t i is -> Slice ('S t) ('S i) ('True ':# is)
	-- | Remove the next value
	SliceRem :: Slice t i is -> Slice ('S t) i      ('False ':# is)
\end{code}

A type of `Slice Two One '['False, 'True]` means extract a total of `One` value
from a `Vect` of length `Two` where the first value is *removed* and the second
*included*.

I don't think I *need* `Vect k Bool` to index this type, and that `[Bool]` is
enough if I'm careful writing the constructors. But using a `Vect k Bool`
makes breaking the invariant a type error.

This type also happens to be a singleton.

\begin{code}
instance Singleton (Slice 'Z 'Z 'VNil) where
	reify Proxy = SliceNil

instance Singleton (Slice t i is) => Singleton (Slice ( 'S t ) i ( 'False ':# is) ) where
	reify Proxy = SliceRem (reify Proxy)

instance Singleton (Slice t i is) => Singleton (Slice ( 'S t ) ('S i) ( 'True ':# is) ) where
	reify Proxy = SliceInc (reify Proxy)
\end{code}

With these definitions `vSlice` is easy to write. It takes a `Slice t i is` and
a `Vect t a` as inputs and returns a `Vect i a` where the i-th element of `Vect
t a` is present in `Vect i a` if the i-th element of `is` is `True`.

\begin{code}
-- | Slices the i indices from a vector
vSlice :: Slice t i is -> Vect t a -> Vect i a
vSlice SliceNil _ = VNil
vSlice (SliceInc is) (x :# xs) = x :# vSlice is xs
vSlice (SliceRem is) (_ :# xs) = vSlice is xs

-- | Example using slice
slicedExample1 :: Vect Two Char
slicedExample1 = vSlice (SliceInc (SliceRem (SliceRem (SliceInc SliceNil)))) example
\end{code}

Nice. The `Slice` value has to be carefully crafted, but the type checker ensures that
exactly two values are selected.

Special Slices
--------------

But perhaps there are special slices that don't require us to construct the full
value by hand.

**The null slice**

One of them is the slice that selects nothing. Because it's always possible to
select nothing, `Vect 'Z a` is a terminal object.

The type of a slice that selects nothing can be computed from the length of the
input vector. The following type level function computes a `Vect n Bool` where
every element is `False`.

\begin{code}
type family DropAll (n :: Nat) :: Vect n Bool where
	DropAll 'Z = 'VNil
	DropAll ('S n) = 'False ':# DropAll n
\end{code}

With this function we can compute, at compile time, the type of a slice that
selects nothing, like the one used in the following function:

\begin{code}

-- | The null slice
vEliminate :: Slice n 'Z (DropAll n) -> Vect n a -> Vect 'Z a
vEliminate = vSlice

-- | Select nothing example
elimintadExample :: Vect 'Z Char
elimintadExample = vEliminate (reify Proxy) example
\end{code}

**The identity slice**

Conversely, another slice that we could surely take is the slice that selects
everything.

\begin{code}
type family TakeAll (n :: Nat) :: Vect n Bool where
	TakeAll 'Z = 'VNil
	TakeAll ('S n) = 'True ':# TakeAll n

-- | The identity slice
vDuplicate :: Slice n n (TakeAll n) -> Vect n a -> Vect n a
vDuplicate = vSlice

duplicatedExample :: Vect Four Char
duplicatedExample = vDuplicate (reify Proxy) example
\end{code}

This is very similar to the previous case.

More Special Slices
-------------------

In the previous two functions, we could say the we already know the result before
applying the function. The output of the next slices depends less trivially on
on their input.

**First Half Slice**

One thing, that I can do with any vector is look at its first half. Can I write
a function that does this and doesn't require to explicitly construct the
`Slice`?

Yes, and it turns out to be more involved that the previous functions.
Concretely, it involved writing five new type level functions; the last one
actually computing the type of the slice.

That is, given for example `2 :: Nat` it returns `Slice 2 1 [True, False]`,
which is to select the first half.

\begin{code}

-- | Less than equal over nats
type family LTEB (n :: Nat) (m :: Nat) :: Bool where
	LTEB 'Z m = 'True
	LTEB ( 'S n ) 'Z = 'False
	LTEB ( 'S n ) ( 'S m ) = LTEB n m

-- | Add an element to the end of a vector
type family Snoc (xs :: Vect k a) (y :: a) :: Vect ('S k) a where
	Snoc 'VNil y = y ':# 'VNil
	Snoc (x ':# xs) y = x ':# Snoc xs y

-- | Reverse a vector
type family Reverse (xs :: Vect nx a) :: Vect nx a where
	Reverse 'VNil = 'VNil
	Reverse (x ':# xs) = Snoc (Reverse xs) x

-- | Auxiliar Recursive function that selects the first half of a vector of
-- length m
type family FirstHalfIdxAux (n :: Nat) (m :: Nat) :: Vect m Bool where
	FirstHalfIdxAux n 'Z = 'VNil
	FirstHalfIdxAux n ('S m) = LTEB ('S m) (NatHalf n) ':# FirstHalfIdxAux n m

type family FirstHalfIdx (n :: Nat) :: Vect n Bool where
	FirstHalfIdx n = Reverse (FirstHalfIdxAux n n)

\end{code}

It important to note that the correctness of the following code dependes on the
correctness of the previous functions. In other words, if there is an error
above then the code below could be wrong despite type checking.

Anyway, with these functions I can finally select the first half:

\begin{code}
-- | The first half of a vector
vFirstHalf :: Slice t (NatHalf t) (FirstHalfIdx t) -> Vect t a -> Vect (NatHalf t) a
vFirstHalf = vSlice

firstHalfExample :: Vect Two Char
firstHalfExample = vFirstHalf (reify Proxy) example
\end{code}

**Second Half Slice**

After the first half, it comes the second half. And the *law of halves* is that
the two have to recombine into the original object when combined in the
proper order.

This translates into five new type level functions, that accomplish the goal of
computing the type of the slice that selects the second half.

\begin{code}
type family NatHalfC (n :: Nat) :: Nat where
	NatHalfC 'Z = 'Z
	NatHalfC ('S 'Z) = 'S 'Z
	NatHalfC ('S ('S n)) = 'S (NatHalfC n)

-- | Type level not
type family NOTB (n :: Bool) :: Bool where
	NOTB 'True = 'False
	NOTB 'False = 'True

-- | Greater than over Nat
type family GTB (n :: Nat) (m :: Nat) :: Bool where
	GTB n m = NOTB (LTEB n m)

type family SecondHalfIdxAux (n :: Nat) (m :: Nat) :: Vect m Bool where
	SecondHalfIdxAux n 'Z = 'VNil
	SecondHalfIdxAux n ('S m) = GTB ('S m) (NatHalf n) ':# SecondHalfIdxAux n m

type family SecondHalfIdx (n :: Nat) :: Vect n Bool where
	SecondHalfIdx n = Reverse (SecondHalfIdxAux n n)
\end{code}

With all this help, I can write the function that extracts the second half:

\begin{code}
-- | The second half of a vector
vSecondHalf :: Slice t (NatHalfC t) (SecondHalfIdx t) -> Vect t a -> Vect (NatHalfC t) a
vSecondHalf = vSlice

secondHalfExample :: Vect Two Char
secondHalfExample = vSecondHalf (reify Proxy) example
\end{code}

What about the original problem?
================================

With all these tools it is now easy to write a safe solution to the original
problem:

\begin{code}
-- | Split a vector in two halves
vHalve :: Slice t (NatHalf t) (FirstHalfIdx t)
	-> Slice t (NatHalfC t) (SecondHalfIdx t)
	-> Vect t a -> (Vect (NatHalf t) a, Vect (NatHalfC t) a)
vHalve fh sh xs = (vFirstHalf fh xs, vSecondHalf sh xs)

halvedExample :: (Vect Two Char, Vect Two Char)
halvedExample = vHalve (reify Proxy) (reify Proxy) example

\end{code}

Finally a type safe solution. I still need those `(reify Proxy)` arguments.

How does this work in the "real" world?
=======================================

So far all examples have been based on the `example` vector which is known at
compile time because it is defined in the code. In that regard, the original
`splitInHalves` function can also split any list you can type.

The type safe functions, to this point, have only helped the type checker to
find errors on the values I've provided at compile time.

To interact with "the real world", I would need to read vectors from it. The
length of these vectors are unknown at compile time. How does it work? How do
these functions help?


A case study
------------

To explore the answers to these questions, I'll consider the following program:

1. Ask for a `n :: Nat`
2. Ask for `n` Integers and store them in a `v :: Vect n Integer`
3. Ask for a second `m :: Nat`
4. Ensure that `m <= n`
5. Take `m` elements from `v` to form new vector.

I started with the simple case of converting an `Integer` to a `Nat`. Integers
cacan be easly read from `stdin`:

\begin{code}
intToNat :: Integer -> Maybe Nat
intToNat n
	| n < 0 = Nothing
	| n == 0 = Just Z
	| otherwise = S <$> intToNat (n - 1)
\end{code}

The conversion might fail, but the failure can be catched by pattern matching
the `Nothing`. If it doesn't, then I have obtained a `Nat` from the user.

The function `vTake`, necessary for the last step, requires a `SNat n`, and I
have a `Nat`. To promote that `Nat` into a `SNat n` I used the next function
that takes as its first argument a function that needs to handle all possible
`SNat n`.

\begin{code}
-- | Construct un SNat n from the nat n
promoteNat :: forall k . (forall (n :: Nat) . SNat n -> k) -> Nat -> k
promoteNat f Z = f SZ
promoteNat f (S n) = promoteNat (f . SS) n
\end{code}

I can use that `SNat n`, for any value of `n`, to ask the user for the vector:

\begin{code}

-- | Read Vect of a from stdin
readVec :: Read a => SNat n -> IO (Vect n a)
readVec SZ = pure VNil
readVec (SS n) = do
	vecElem <- read <$> getLine
	(vecElem :#) <$> readVec n
\end{code}

So far I have the vector, then I can ask for `m :: Nat` in a similar way as
before. Once I have both `n :: Nat` and `m :: Nat`, I need to verify that `m <=
n` by constructing `LTE m n`:

\begin{code}

-- | Run the function if we can prove that n < m
promoteLTE :: (LTE n m -> k) -> SNat n -> SNat m -> Maybe k
promoteLTE f SZ _ = Just $ f LTEZero
promoteLTE _ (SS _) SZ = Nothing
promoteLTE f (SS n) (SS m) = promoteLTE (f . LTESucc) n m

\end{code}

All I need to do now is put these pieces in the right order:

**Obtain a `SNat n` from the user**

\begin{code}

step0 :: IO ()
step0 = do
	putStr "Vector length: \n ?"
	mVecLen <- intToNat . (read :: String -> Integer) <$> getLine

	case promoteNat step1 <$> mVecLen of
		Nothing -> putStrLn "Failed to obtain Nat"
		Just action -> action
\end{code}

**Obtain a `Vect n Integer` and `SNat m`**

\begin{code}
-- Having obtained a valid natural n, read n integers from `Stdin`
-- Then ask for how many elements to take
step1 :: forall (n :: Nat) . SNat n -> IO ()
step1 n = do
	putStrLn "Enter elemnts"
	vec <- readVec n :: IO (Vect n Integer)

	putStrLn $ "Input: " ++ show vec

	putStr "Number to Take: \n ?"
	mTake <- intToNat . (read :: String -> Integer) <$> getLine

	case promoteNat (step2 vec n) <$> mTake of
		Nothing -> putStrLn "Failed to obtain Nat"
		Just action -> action
\end{code}

**Check if `m <= n`**

\begin{code}

-- If the elements to take n are a valid natural then
-- Make sure it is less than then length of the vector
step2 :: Vect k Integer -> SNat k -> SNat n -> IO ()
step2 vec k n = case promoteLTE (step3 vec) n k of
	Nothing -> putStrLn "Not LTE"
	Just action -> action
\end{code}

**Take the `m` elements**

\begin{code}
-- If we take less than or equal elements, proceed to take.
-- No checks needed.
step3 :: Vect m Integer -> LTE n m -> IO ()
step3 vec lte = putStrLn $ "Taken: " ++ show (vTake lte vec)
\end{code}

And that's it.

Conclusion
==========

Using the type safe function `vTake` required to carefully check that all
preconditions were met without using `undefined`. So, I could say the code is
safer.

On the other hand, the resulting code is more "complex" and I've transferred some
of the risk to the type level functions. Those could be wrong in some cases and
would require testing.


Finally, this code forbids taking more than what you have; and that could be a
very important constraint to enforce in some applications.

References
==========

Some of the techniques used in this document come from "Dependent Type
Programming with Singletons" by R. A. Eisenberg and S. Weirich.

Idris' base library source code was also used for reference.

Haskell's singleton libraray's source code was also consulted.

Appendix
========

This is the rest of the code that drives the previous post, together with some
functions that I wrote and ened up not using.

\begin{code}
main :: IO ()
main = do
	putStrLn $ "Example: " ++ show example
	putStrLn $ "Take one, drop one: " ++ show skippedExample
	putStrLn $ "Dropped: " ++ show droppedExample
	putStrLn $ "Sliced: " ++ show slicedExample1
	putStrLn $ "Eliminated: " ++ show elimintadExample
	putStrLn $ "Duplicated: " ++ show duplicatedExample
	putStrLn $ "First Half: " ++ show firstHalfExample
	putStrLn $ "Second Half: " ++ show secondHalfExample
	putStrLn $ "Halves: " ++ show halvedExample
	putStrLn $ "Taked: " ++ show takedExample

	step0

instance Show a => Show (Vect n a) where
	show VNil = "VNil"
	show (x :# xs) = show x ++ " :# " ++ show xs

printVec :: Show a => Vect n a -> String
printVec v = "{" ++ printRec v ++ "}"

printRec :: Show a => Vect n a -> String
printRec VNil = ""
printRec (x :# VNil) = show x
printRec (x :# xs@(_ :# _)) = show x ++ " ; " ++ printRec xs

instance Show (Slice 'Z 'Z 'VNil) where
	show SliceNil = "SliceNil"

instance Show (Slice t i is) => Show (Slice ( 'S t ) i ( 'False ':# is) ) where
	show (SliceRem is) = "SliceRem " ++ show is

instance Show (Slice t i is) => Show (Slice ( 'S t ) ('S i) ( 'True ':# is) ) where
	show (SliceInc is) = "SliceInc " ++ show is

-- | Less than over Nat
type family LTB (n :: Nat) (m :: Nat) :: Bool where
	LTB n m = LTEB ('S n) m

-- | Greater than equal over Nat
type family GTEB (n :: Nat) (m :: Nat) :: Bool where
	GTEB 'Z m = 'False
	GTEB ( 'S n ) 'Z = 'True
	GTEB ( 'S n ) ( 'S m ) = GTEB n m

promoteVec :: SNat n -> [a] -> Maybe (Vect n a)
promoteVec SZ [] = Just VNil
promoteVec SZ (_ : _) = Nothing
promoteVec (SS _) [] = Nothing
promoteVec (SS n) (x : xs) = (x :#) <$> promoteVec n xs

demoteSNat :: SNat n -> Nat
demoteSNat SZ = Z
demoteSNat (SS n) = S (demoteSNat n)

natToInt :: Nat -> Integer
natToInt Z = 0
natToInt (S n) = 1 + natToInt n

promoteLTE1 :: forall (n :: Nat) k . (forall (m :: Nat) . LTE n m -> k) -> SNat n -> Integer -> Maybe k
promoteLTE1 f n m
	| m < 0 = Nothing
	| m == 0 = case n of
		SZ -> Just $ f LTEZero
		_ -> Nothing
	| otherwise = case n of
		SZ -> Just $ f LTEZero
		(SS n') -> promoteLTE1 (f . LTESucc) n' (m - 1)

promoteLTE2 :: forall k . (forall (n :: Nat) (m :: Nat) . LTE n m -> k) -> Integer -> Integer -> Maybe k
promoteLTE2 f n m
	| n < 0 && m < 0 = Nothing
	| n > m = Nothing
	| n == 0 = Just $ f LTEZero
	| otherwise = promoteLTE2 (f . LTESucc) (n - 1) m
\end{code}
