{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Types.I#`con_info 224743
-- XFAIL: *
module Nofib.WheelSieve1 where
-- Mark I lazy wheel-sieve.
-- Colin Runciman (colin@cs.york.ac.uk); March 1996.
-- See article "Lazy wheel sieves and spirals of primes" (to appear, JFP).

import GHC.Types
import GHC.Bool
import GHC.Base
import GHC.Num
import GHC.List


data Wheel = Wheel Int [Int]

primes :: [Int]
primes = sieve wheels primes squares

sieve (Wheel s ns:ws) ps qs =
  [n' | o <- s:(enumFromThenTo'Int (s*2) (s*3) ((head ps-1)*s)),
        n <- ns,
        n'<- [n+o], noFactor n'] 
  ++
  sieve ws (tail ps) (tail qs)
  where
  noFactor = if s<=2 then const True else notDivBy ps qs

notDivBy (p:ps) (q:qs) n =
  q > n || n `modInt` p > 0 && notDivBy ps qs n
notDivBy _ _ _ = True

squares :: [Int]
squares = [p*p | p<-primes]

wheels :: [Wheel]
wheels = Wheel 1 [1] : zipWith nextSize wheels primes 

nextSize (Wheel s ns) p =
  Wheel (s*p) ns'
  where
  ns' = [n' | o <- enumFromThenTo'Int 0 s ((p-1)*s),
              n <- ns,
              n' <- [n+o], n'`modInt`p > 0]

test = primes !! 20000

enumFromThenTo'Int :: Int -> Int -> Int -> [Int]
enumFromThenTo'Int from@(I# f) thin@(I# i) to =
  let !step = (i -# f) in go step from to
  where
  go step from@(I# m) to@(I# n) =
    if m ># n then [] else
      from : go step (I# (m +# step)) to


