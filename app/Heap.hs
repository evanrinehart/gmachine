module Heap where

import Control.Monad.State
import Data.Vector (Vector)
import qualified Data.Vector as V

type Addr = Int

data HCell =
      HV !Char
    | HL !Char !Addr
    | HA !Addr !Addr 
    | HN !Int 
    | HP !Addr !Addr
    | HB !BuiltIn
    | OO -- blank cell, unused
        deriving Show

data BuiltIn =
      BIPlus  -- requires 2 number args
    | BIFst   -- requires 1 pair arg
    | BISnd   -- requires 1 pair arg
    | BICond  -- requires 1 number arg and 2 more args
    deriving Show

type Storage = Vector HCell

-- type for computations using a heap
type Heap a = StateT Storage IO a

-- run a heap computation given the initial heap storage
run :: Heap a -> Storage -> IO (a, Storage)
run act vec = runStateT act vec

-- clear heap cells which are not reachable from address 0
gc :: Heap ()
gc = do
    used <- gets (reachableFrom 0)
    modify (clearUnused used)

-- show the contents of the heap
dump :: Heap ()
dump = do
    v <- get
    let acts = V.imap (\i cell -> putStrLn (show i ++ ": " ++ show cell)) v
    liftIO (sequence_ (V.toList acts))

-- print a message
puts :: String -> Heap ()
puts msg = liftIO (putStrLn msg)

-- read cell at address
peek :: Addr -> Heap HCell
peek addr = gets (\vec -> vec V.! addr)

-- write cell to address
write :: Addr -> HCell -> Heap ()
write dest cell = modify g where
    g vec = vec V.// [(dest,cell)]

-- copy cell from one address to the other
copy  :: Addr -> Addr -> Heap ()
copy dest src = peek src >>= write dest

-- allocate a new cell with the given data, returns the address
newCell :: HCell -> Heap Addr
newCell cell = do
    maybeAddr <- gets unusedAddr
    case maybeAddr of
        Nothing   -> error "out of memory"
        Just addr -> do
            write addr cell
            pure addr

-- make blank storage of a certain size
makeStorage :: Int -> Storage
makeStorage size = V.replicate size OO

reachableFrom :: Addr -> Storage -> [Addr]
reachableFrom start vec = go start [] where
    go addr seen
        | addr `elem` seen = seen
        | otherwise        = f addr seen
    f addr seen = case vec V.! addr of
        HL _ body -> go body (addr : seen)
        HA l r -> go r (go l (addr : seen))
        HP l r -> go r (go l (addr : seen))
        OO -> seen
        _  -> addr : seen

clearUnused :: [Addr] -> Storage -> Storage
clearUnused used vec = V.imap g vec where
    g i x = if i `elem` used then x else OO

isBlank :: HCell -> Bool
isBlank OO = True
isBlank _  = False

unusedAddr :: Storage -> Maybe Int
unusedAddr vec = V.findIndex isBlank vec

