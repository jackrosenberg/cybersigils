{-# LANGUAGE TemplateHaskell, DeriveTraversable, ScopedTypeVariables #-}
module Main where
import Prelude as P
import Control.Lens as C
import Graphics.Image as I
import Data.Maybe (fromMaybe)
import Data.List (findIndex, nub)
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

defaultTile, collapseTile, horizTile, vertTile, crossTile :: Tile -- make the tiles
defaultTile = tileGen 90 (const (PixelRGB 0 0 0))
collapseTile = tileGen 90 (const (PixelRGB 1 0 0))
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
    let res = wfc g cst (genGrd 10) 
    let wfcres = combineTiles $ toTiles res
    print $ length <$> concat res
    write ("res",  wfcres)

toTiles :: Grid (Domain Tile) -> [Tile]
toTiles g = (\dt -> avgImg (view image <$> dt)) <$> concat g

avgImg :: [Image RPU RGB Double] -> Tile
avgImg [] = trace "dumfuk, wave function collapsed" collapseTile
avgImg [i] = Tile i
avgImg (i:is) = defaultTile -- (i + avgImg is)/2

write :: (String, Tile) -> IO () 
write (name, tile) = writeImage ("images/" ++ name ++ ".png") (view image tile)


combineTiles :: [Tile] -> Tile
combineTiles tl = Tile $ foldr (\(im, (r,c)) acc -> superimpose (r*rows im, c*cols im) im acc) canvas imsWidx
    where imsWidx = P.zip (_image <$> tl) [(r, c) | r <- [0..(width -1)], c <- [0..(width -1)]] 
          width  = (ceiling . sqrt . fromIntegral . length) tl
          canvas = canvasSize Wrap (incBy (width, width)) di
          di =  defaultTile^.image
          incBy (fm, fn) = (rows di * fm, cols di * fn)

cst :: [Constraint Tile]
cst = [noAdj]

c12, noAdj, cross :: Constraint Tile

c12 (0,0) (0,1) = (==)
c12 _ _ = const $ const False

cross (r1,c1) (r2,c2) t1 t2 = r1 == r2 && c1 == c2+1 && t1 == crossTile && t2 /= horizTile

noAdj _ _ = (/=) -- universal constraint??

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

succs :: Grid a -> (Int, Int) -> [(Int, Int)] -- get succs of a list
succs g (r,c)  = [(nr, nc) | nr <- [r-1, r, r+1], between 0 nr (length . head $ g),
                             nc <- [c-1, c, c+1], between 0 nc (length . head $ g), 
                            (nr == r) /= (nc == c)] --xor bitch

wfc :: RandomGen g => g -> [Constraint Tile] -> Grid (Domain Tile) -> Grid (Domain Tile)
wfc g cs inp = let (g, f) = collapse cs (over (idx n) (const nd) inp, [n]) in g
    where (nd, ng) = pop g (inp !!! n) 
          n = minDex inp

collapse :: [Constraint Tile] -> (Grid (Domain Tile), [(Int, Int)]) -> (Grid (Domain Tile), [(Int, Int)])
collapse cs (inp, []) = (inp, [])
collapse cs (inp,n:fr) = if null fr' then (gr, []) else trace ("n: " ++ show n ++ " fr: " ++ show fr) collapse cs (gr, fr++fr')
    where 
          (gr, fr') = foldr (\s (g,f) -> (over (idx s) (snd . prune s) g, if fst $ prune s (inp !!!n) then nub $ succs g s ++ f else f)) (inp, []) (succs inp n) -- fold over the array and prune forall successors

          prune :: (Int, Int) -> Domain Tile -> (Bool, Domain Tile) -- remove vals in target that violate constraint forall values and if they changed
          prune s d = let res = foldr (\csr -> filter (\e -> none (csr n s e) (inp !!! n))) d cs in (res == d, res)

idx :: (Int, Int) -> ASetter (Grid (Domain Tile)) (Grid (Domain Tile)) (Domain Tile) (Domain Tile)
idx s = element (fst s) . element (snd s)


again :: Int -> (a->a) -> a -> a
again 1 f a = f a 
again n f a = f $ again (n-1) f a


