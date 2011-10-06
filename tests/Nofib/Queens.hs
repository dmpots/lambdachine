{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
-- XFAIL: *
module Nofib.Queens where
-- !!! count the number of solutions to the "n queens" problem.
-- (grabbed from LML dist)

import GHC.Types
import GHC.Bool
import GHC.Base
import GHC.Num
import GHC.List

test = nsoln 10 == 724

enumFromTo'Int :: Int -> Int -> [Int]
enumFromTo'Int from@(I# m) to@(I# n) =
  if m ># n then [] else
    from : enumFromTo'Int (I# (m +# 1#)) to

nsoln nq = length (gen nq)
 where
    safe :: Int -> Int -> [Int] -> Bool
    safe x d []    = True
    safe x d (q:l) = x /= q && x /= q+d && x /= q-d && safe x (d+1) l

    gen :: Int -> [[Int]]
    gen 0 = [[]]
    gen n = [ (q:b) | b <- gen (n-1), q <- enumFromTo'Int 1 nq, safe q 1 b]
