{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Kind
import Data.Proxy

data Nat where
	Z :: Nat
	S :: Nat -> Nat

one :: Nat
one = S Z

two :: Nat
two = S one

three :: Nat
three = S two

four :: Nat
four = S three

type Zero = 'Z
type One = 'S 'Z
type Two = 'S ('S 'Z)
type Three = 'S ('S ('S 'Z))
type Four = 'S ('S ('S ('S 'Z)))

type family NatHalf (n :: Nat) :: Nat where
	NatHalf 'Z = 'Z
	NatHalf ('S 'Z) = 'Z
	NatHalf ('S ('S n)) = 'S (NatHalf n)

type family NatHalfC (n :: Nat) :: Nat where
	NatHalfC 'Z = 'Z
	NatHalfC ('S 'Z) = 'S 'Z
	NatHalfC ('S ('S n)) = 'S (NatHalfC n)

data Vect :: Nat -> Type -> Type where
	VNil :: Vect 'Z a
	(:#) :: a -> Vect n a -> Vect ('S n) a

infixr 5 :#

vSample :: Vect Four Char
vSample = '1' :# '2' :# '3' :# '4' :# VNil

vHead :: Vect ('S n) a -> a
vHead (x :# _) = x

-- Grab every other value
vHalve' :: Vect n a -> Vect (NatHalf n) a
vHalve' VNil = VNil
vHalve' (_ :# VNil) = VNil
vHalve' (x :# (_ :# xs)) = x :# vHalve' xs

type family (:+:) (n :: Nat) (m :: Nat) :: Nat where
	(:+:) 'Z m = m
	(:+:) ('S n) m = 'S (n :+: m)

data SNat :: Nat -> Type where
	SZ :: SNat 'Z
	SS :: SNat n -> SNat ('S n)

type SZero = SNat 'Z
type SOne = SNat ( 'S 'Z )
type STwo = SNat ( 'S ('S 'Z ))
type SThree = SNat ( 'S ( 'S ('S 'Z )))
type SFour = SNat ( 'S ('S ( 'S ('S 'Z ))))
type SFive = SNat ( 'S ('S ('S ( 'S ('S 'Z )))))

class Singleton a where
	reify :: Proxy a -> a

instance Singleton (SNat 'Z) where
	reify Proxy = SZ

instance Singleton (SNat n) => Singleton (SNat ('S n)) where
	reify Proxy = SS (reify Proxy)

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

vDrop :: SNat n -> Vect (n :+: m) a -> Vect m a
vDrop SZ xs = xs
vDrop (SS n) (_ :# xs) = vDrop n xs

-- Indicies for slices
data Indices (k :: Nat) :: Nat -> Vect k Bool -> Type where
	INil :: Indices 'Z 'Z 'VNil
	IInc :: Indices t i is -> Indices ('S t) ('S i) ('True ':# is)
	IRem :: Indices t i is -> Indices ('S t) i      ('False ':# is)

instance Singleton (Indices 'Z 'Z 'VNil) where
	reify Proxy = INil

instance Singleton (Indices t i is) => Singleton (Indices ( 'S t ) i ( 'False ':# is) ) where
	reify Proxy = IRem (reify Proxy)

instance Singleton (Indices t i is) => Singleton (Indices ( 'S t ) ('S i) ( 'True ':# is) ) where
	reify Proxy = IInc (reify Proxy)

-- Selects the indices
vSlice :: Indices t i is -> Vect t a -> Vect i a
vSlice INil _ = VNil
vSlice (IInc is) (x :# xs) = x :# vSlice is xs
vSlice (IRem is) (_ :# xs) = vSlice is xs

vSample2 :: Vect Two Char
vSample2 = vSlice (IInc (IRem (IInc (IRem INil)))) vSample

type family DropAll (n :: Nat) :: Vect n Bool where
	DropAll 'Z = 'VNil
	DropAll ('S n) = 'False ':# DropAll n

vEliminate :: Indices n 'Z (DropAll n) -> Vect n a -> Vect 'Z a
vEliminate = vSlice

vSample3 :: Vect 'Z Char
vSample3 = vEliminate (IRem  (IRem  (IRem  (IRem INil)))) vSample

type family TakeAll (n :: Nat) :: Vect n Bool where
	TakeAll 'Z = 'VNil
	TakeAll ('S n) = 'True ':# TakeAll n

vDuplicate :: Indices n n (TakeAll n) -> Vect n a -> Vect n a
vDuplicate = vSlice

vSample4 :: Vect ('S ('S ('S ('S 'Z)))) Char
vSample4 = vDuplicate (IInc (IInc  (IInc  (IInc INil)))) vSample

-- type family FilterStep (b :: Bool) (xs :: a) :: Maybe a where
-- 	FilterStep 'False x = 'Nothing
-- 	FilterStep 'True x = 'Just x

type family NOTB (n :: Bool) :: Bool where
	NOTB 'True = 'False
	NOTB 'False = 'True

type family LTEB (n :: Nat) (m :: Nat) :: Bool where
	LTEB 'Z m = 'True
	LTEB ( 'S n ) 'Z = 'False
	LTEB ( 'S n ) ( 'S m ) = LTEB n m

type family GTB (n :: Nat) (m :: Nat) :: Bool where
	GTB n m = NOTB (LTEB n m)

type family LTB (n :: Nat) (m :: Nat) :: Bool where
	LTB n m = LTEB ('S n) m

type family GTEB (n :: Nat) (m :: Nat) :: Bool where
	GTEB 'Z m = 'False
	GTEB ( 'S n ) 'Z = 'True
	GTEB ( 'S n ) ( 'S m ) = GTEB n m

-- Add an element to the end of a vector
type family Snoc (xs :: Vect nx a) (y :: a) :: Vect ('S nx) a where
	Snoc 'VNil y = y ':# 'VNil
	Snoc (x ':# xs) y = x ':# Snoc xs y

type family Reverse (xs :: Vect nx a) :: Vect nx a where
	Reverse 'VNil = 'VNil
	Reverse (x ':# xs) = Snoc (Reverse xs) x

type family FirstHalfIdxAux (n :: Nat) (m :: Nat) :: Vect m Bool where
	FirstHalfIdxAux n 'Z = 'VNil
	FirstHalfIdxAux n ('S m) = LTEB ('S m) (NatHalf n) ':# FirstHalfIdxAux n m

type family FirstHalfIdx (n :: Nat) :: Vect n Bool where
	FirstHalfIdx n = Reverse (FirstHalfIdxAux n n)

type family SecondHalfIdxAux (n :: Nat) (m :: Nat) :: Vect m Bool where
	SecondHalfIdxAux n 'Z = 'VNil
	SecondHalfIdxAux n ('S m) = GTB ('S m) (NatHalf n) ':# SecondHalfIdxAux n m

type family SecondHalfIdx (n :: Nat) :: Vect n Bool where
	SecondHalfIdx n = Reverse (SecondHalfIdxAux n n)


-- Example
--
-- vFirstHalf (reify Proxy) vSample
vFirstHalf :: Indices t (NatHalf t) (FirstHalfIdx t) -> Vect t a -> Vect (NatHalf t) a
vFirstHalf = vSlice

-- Example
--
-- vSecondHalf (reify Proxy) vSample
vSecondHalf :: Indices t (NatHalfC t) (SecondHalfIdx t) -> Vect t a -> Vect (NatHalfC t) a
vSecondHalf = vSlice

-- Example
--
-- vHalve (reify Proxy) (reify Proxy) vSample
vHalve :: Indices t (NatHalf t) (FirstHalfIdx t)
	-> Indices t (NatHalfC t) (SecondHalfIdx t)
	-> Vect t a -> (Vect (NatHalf t) a, Vect (NatHalfC t) a)
vHalve fh sh xs = (vFirstHalf fh xs, vSecondHalf sh xs)

case1 :: IO ()
case1 = putStrLn "one"

case2 :: IO ()
case2 = putStrLn "two"

main :: IO ()
main = do
	putStr $ unlines
		[ "1. Foo"
		, "2. Bar" ]

	selecteOption <- (read :: String -> Int) <$> getLine

	case selecteOption of
		1 -> case1
		2 -> case2
		_ -> error "Wrong option"

instance Show a => Show (Vect n a) where
	show VNil = "VNil"
	show (x :# xs) = show x ++ " :# " ++ show xs

printVec :: Show a => Vect n a -> String
printVec v = "{" ++ printRec v ++ "}"

printRec :: Show a => Vect n a -> String
printRec VNil = ""
printRec (x :# VNil) = show x
printRec (x :# xs@(_ :# _)) = show x ++ " ; " ++ printRec xs

instance Show (Indices 'Z 'Z 'VNil) where
	show INil = "INil"

instance Show (Indices t i is) => Show (Indices ( 'S t ) i ( 'False ':# is) ) where
	show (IRem is) = "IRem " ++ show is

instance Show (Indices t i is) => Show (Indices ( 'S t ) ('S i) ( 'True ':# is) ) where
	show (IInc is) = "IInc " ++ show is
