{-# LANGUAGE TemplateHaskell, DeriveTraversable, ScopedTypeVariables #-}
module Main where
import Prelude as P
import Control.Lens as C
import Graphics.Image as I
import Data.Maybe (fromMaybe)
import Data.List (findIndex)
import Debug.Trace (trace)
import Control.Monad (liftM2)
import System.Random

{-
optimizations: use ids instead of imgs for the collapse, when done map each id to img  
 
 
 
-}

newtype Tile = Tile {_image :: Image RPU RGB Double}
    deriving Eq
type Constraint a = (Int, Int) -> (Int, Int) -> Tile -> Tile -> Bool -- takes two indexes and gives func to compare
 -- takes two indexes and gives func to compare
type Grid a = [[a]]  -- rows and cols (x,y)
(!!!) :: Grid a -> (Int, Int) -> a
g !!! l = g !! fst l !! snd l 

type Domain a = [a] -- list of tiles that are possibilities

$(makeLenses ''Tile)

tileGen :: Int -> ((Int, Int) -> Pixel RGB Double) -> Tile 
tileGen sz = Tile . makeImageR RPU (sz, sz) 

between :: (Ord a) => a -> a -> a -> Bool
between l x u = x >= l && x <= u 

defaultTile, horizTile, vertTile, crossTile :: Tile -- make the tiles
defaultTile = tileGen 90 (const (PixelRGB 0 0 0))
horizTile = tileGen 90 (\(x, _) -> if between (90`div`3) x (2*90`div`3) then PixelRGB 255 255 255 else PixelRGB 0 0 0)
vertTile  = tileGen 90 (\(_, y) -> if between (90`div`3) y (2*90`div`3) then PixelRGB 255 255 255 else PixelRGB 0 0 0)
crossTile = tileGen 90 (\(x, y) -> if between (90`div`3) x (2*90`div`3) || between (90`div`3) y (2*90`div`3) then PixelRGB 255 255 255 else PixelRGB 0 0 0)
                                                 
upRightTile, upLeftTile:: Tile 
upRightTile = tileGen 90 (\(x, y) -> if between (90`div`3) x (2*90`div`3) || between (90`div`3) y (2*90`div`3) then PixelRGB 255 255 255 else PixelRGB 0 0 0)
upLeftTile  = tileGen 90 (\(x, y) -> if between (90`div`3) x (2*90`div`3) || between (90`div`3) y (2*90`div`3) then PixelRGB 255 255 255 else PixelRGB 0 0 0)

tilelist :: [Tile]
tilelist = [horizTile ,vertTile, crossTile]


main :: IO ()
main = do 
    g <- getStdGen
    let res = wfc g (genGrd 20) cst 
    let wfcres = combineTiles $ toTiles res
    print $ length <$> concat res
    write ("res",  wfcres)

toTiles :: Grid (Domain Tile) -> [Tile]
toTiles g = (\dt -> avgImg (view image <$> dt)) <$> concat g

avgImg :: [Image RPU RGB Double] -> Tile
avgImg [] = trace "dumfuk, wave function collapsed" defaultTile
avgImg [i] = Tile i
avgImg (i:is) = defaultTile -- (i + avgImg is)/2

write :: (String, Tile) -> IO () 
write (name, tile) = writeImage ("images/" ++ name ++ ".png") (view image tile)


combineTiles :: [Tile] -> Tile
combineTiles tl = add tl canvas 0
    where canvas = tileGen (cols((view image . head) tl) * width) (\(_,_) -> PixelRGB 0 0 0) 
          width  = (ceiling . sqrt . fromIntegral . length) tl
          add :: [Tile] -> Tile -> Int -> Tile
          add [] acc  _    = acc
          add (t:ts) acc i = add ts (Tile (superimpose ((i `div` width)*cols (view image t),(i `mod` width)*cols (view image t)) (view image t) (view image acc))) (i+1)


cst :: [Constraint Tile]
cst = [noAdj]

c12, noAdj :: (Int, Int) -> (Int, Int) -> Tile -> Tile -> Bool -- takes two indexes and gives func to compare

c12 (0,0) (0,1) = (==)
c12 _ _ = const $ const False

noAdj _ _ = (==) -- universal constraint??

genGrd :: Int -> Grid (Domain Tile)
genGrd sz = replicate sz (replicate sz tilelist) -- domains start all possibilities

minDex :: Grid (Domain Tile) -> (Int, Int) -- (row,col)
minDex g = (ai `div` f, ai `mod` f) 
    where ai = fromMaybe (-1) (findIndex (\l -> length l == minlen) cg)
          minlen = non1min (length <$> cg)
          cg = concat g
          f = (length . head) g

non1min :: [Int] -> Int
non1min = foldr (\i acc -> if i <= 1 then acc else min i acc) 9999

pop :: (RandomGen g) => g -> Domain Tile -> (Domain Tile, g) 
pop g d = let (i, ng) = randomR (0, length d -1) g in ([d!!i], ng)

succs :: (Int, Int) -> Grid a -> [(Int, Int)] -- get succs of a list
succs (r,c) g = [(nr, nc) | nr <- [r-1, r, r+1], between 0 nr (length . head $ g),
                            nc <- [c-1, c, c+1], between 0 nc (length . head $ g), 
                           (nr == r) /= (nc == c)] --xor bitch

wfc :: RandomGen g => g -> Grid (Domain Tile) -> [Constraint Tile] -> Grid (Domain Tile)
wfc g inp cs | all (all ((<= 1) . length)) inp = inp
             | otherwise                       = wfc ng (collapse (over (idx n) (const nd) inp) cs n (succs n inp)) cs
    where (nd, ng) = pop g (inp !!! n) 
          n = minDex inp

collapse :: Grid (Domain Tile) -> [Constraint Tile] -> (Int, Int) -> [(Int, Int)] -> Grid (Domain Tile)
collapse inp cs n = foldr (liftM2 over idx prune) inp -- fold over the array and prune forall successors
    where prune :: (Int, Int) -> Domain Tile -> Domain Tile -- remove vals in target that violate constraint forall values 
          prune s d = foldr (\csr -> filter (\e -> none (csr n s e) (inp !!! n))) d cs

idx :: (Int, Int) -> ASetter (Grid (Domain Tile)) (Grid (Domain Tile)) (Domain Tile) (Domain Tile)
idx s = element (fst s) . element (snd s)
