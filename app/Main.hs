module Main where

import Heap (Heap, HCell(..), BuiltIn(..))
import Heap (run, write, dump, puts, makeStorage)
import Reducer (reduceToWHNF)

main :: IO ()
main = run program (makeStorage 16) >> return ()

program :: Heap ()
program = do
    -- (\x -> fst (\y -> y, 14)) 11 7
    write 0 (HA 3 1)
    write 1 (HN 7)
    write 2 (HN 11)
    write 3 (HA 4 2)
    write 4 (HL 'x' 5)
    write 5 (HA 6 7)
    write 6 (HB BIFst)
    write 7 (HP 8 9)
    write 8 (HL 'y' 10)
    write 9 (HN 14)
    write 10 (HV 'y')

    puts "starting heap"
    dump
    puts ""

    -- (\x -> fst (\y -> y, 14)) 11 7
    -- should reduce to
    -- fst (\y -> y, 14) 7
    -- (\y -> y) 7
    -- 7
    reduceToWHNF 0

    puts "final heap"
    dump
    puts ""


