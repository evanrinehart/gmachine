module Reducer where

import Heap
    
reduceToWHNF :: Addr -> Heap ()
reduceToWHNF addr = unlessM (objectIsWHNF addr) $ do
    puts ("perform findAndReduceRedex under " ++ show addr)
    findAndReduceRedexUnder addr

    puts "intermediate heap"
    dump
    puts ""

    gc

    puts "intermediate heap after gc"
    dump
    puts ""

    reduceToWHNF addr

-- starting at addr look for outermost reducible expression and reduce it.
findAndReduceRedexUnder :: Addr -> Heap ()
findAndReduceRedexUnder addr = do
    cell <- peek addr
    case cell of
        HA l r -> do
            -- consider 1 level app-node (lambda, 1-arg built-in)
            cellL <- peek l
            case cellL of
                HL x body -> reduceLambda addr body x r
                HB BIFst  -> reduceFst addr r
                HB BISnd  -> reduceSnd addr r
                HB BIPlus -> error "not fully-applied built-in +"
                HB BICond -> error "not fully-applied built-in cond"
                HA l2 r2 -> do
                    -- consider 2 level app-node (2-arg built-in)
                    cellL2 <- peek l2
                    case cellL2 of
                        HB BIPlus -> reducePlus addr r2 r
                        HB BICond -> error "not fully-applied built-in cond"
                        HA l3 r3  -> do
                            -- consider 3 level app-node (3-arg built-in)
                            cellL3 <- peek l3
                            case cellL3 of
                                HB BICond -> reduceCond addr r3 r2 r
                                -- go down 1 and try again
                                _         -> findAndReduceRedexUnder l
                        -- go down 1 and try again
                        _ -> findAndReduceRedexUnder l
                _ -> error ("bad app node: " ++ show (HA l r) ++ " cellL = " ++ show cellL)
        _ -> error "redex not found"

reduceFst :: Addr -> Addr -> Heap ()
reduceFst dest argAddr = do
    reduceToWHNF argAddr
    cell <- peek argAddr
    case cell of
        HP l _ -> do
            reduceToWHNF l
            copy dest l -- section 12.4.3
        _ -> error "fst failed"

reduceSnd :: Addr -> Addr -> Heap ()
reduceSnd dest argAddr = do
    reduceToWHNF argAddr
    cell <- peek argAddr
    case cell of
        HP _ r -> do
            reduceToWHNF r
            copy dest r -- section 12.4.3
        _ -> error "snd failed"

reduceCond :: Addr -> Addr -> Addr -> Addr -> Heap ()
reduceCond dest arg1 arg2 arg3 = do
    reduceToWHNF arg1
    cell <- peek arg1
    case cell of
        HN 0 -> do
            reduceToWHNF arg3
            copy dest arg3 -- section 12.4.3
        HN _ -> do
            reduceToWHNF arg2
            copy dest arg2 -- section 12.4.3
        _ -> error "cond failed"

reducePlus :: Addr -> Addr -> Addr -> Heap ()
reducePlus dest argAddr1 argAddr2 = do
    reduceToWHNF argAddr1
    cell1 <- peek argAddr1
    case cell1 of
        HN n1 -> do
            reduceToWHNF argAddr2
            cell2 <- peek argAddr2
            case cell2 of
                HN n2 -> write dest (HN (n1 + n2))
                _     -> error "plus failed (arg2)"
        _ -> error "plus failed (arg1)"

reduceLambda :: Addr -> Addr -> Char -> Addr -> Heap ()
reduceLambda dest bodyAddr x argAddr = do
    cell <- peek bodyAddr
    case cell of
        HV y
            | x == y -> do
                reduceToWHNF argAddr
                copy dest argAddr -- section 12.4.3
            | otherwise -> error "impossible"
        _ -> do
            addr <- cloneSubstituting bodyAddr x argAddr
            copy dest addr

matchOrClone :: Char -> Addr -> Addr -> Heap Addr
matchOrClone x sourceAddr argAddr = do
    cell <- peek sourceAddr
    case cell of
        HV y
            | x == y    -> return argAddr
            | otherwise -> return sourceAddr
        _ -> cloneSubstituting sourceAddr x argAddr

cloneSubstituting :: Addr -> Char -> Addr -> Heap Addr
cloneSubstituting sourceAddr x argAddr = do
    cell <- peek sourceAddr
    case cell of
        HN _ -> return sourceAddr
        HB _ -> return sourceAddr
        HV _ -> return sourceAddr -- this would only happen for unmatching vars
        HL y body
            | x == y -> return sourceAddr -- shadowed variable, "static" lambda
            | otherwise -> do
                addr <- matchOrClone x body argAddr
                newCell (HL y addr)
        HP l r -> do
            addr1 <- matchOrClone x l argAddr
            addr2 <- matchOrClone x r argAddr
            newCell (HP addr1 addr2)
        HA l r -> do
            addr1 <- matchOrClone x l argAddr
            addr2 <- matchOrClone x r argAddr
            newCell (HA addr1 addr2)
        OO -> error "shouldn't be cloning non-existent cells"

objectIsWHNF :: Addr -> Heap Bool
objectIsWHNF addr = do
    cell <- peek addr
    case cell of
        HA _ _ -> pure False
        _      -> pure True

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM test act = do
    b <- test
    if b then pure () else act
