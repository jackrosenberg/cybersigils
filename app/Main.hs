{-# LANGUAGE TemplateHaskell, DeriveTraversable #-}
module Main where
import Prelude as P
import Control.Lens as C
import Graphics.Image as I
import Data.Maybe (fromMaybe)
import Data.List (findIndex)

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
    let res = wfc grd cst
    let wfcres = combineTiles $ toTiles res
    print $ length <$> concat res
    print $ minDex res
    write ("res",  wfcres)

toTiles :: Grid (Domain Tile) -> [Tile]
toTiles g = (\dt -> avgImg (view image <$> dt)) <$> concat g

avgImg :: [Image RPU RGB Double] -> Tile
avgImg [] = error "dumfuk"
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
cst = [c12]

c12 :: (Int, Int) -> (Int, Int) -> Tile -> Tile -> Bool -- takes two indexes and gives func to compare
c12 (0,0) (0,1) = (==)
c12 _ _ = const $ const False

grd :: Grid (Domain Tile)
grd = replicate 3 (replicate 3 tilelist) -- domains start all possibilities

minDex :: Grid (Domain Tile) -> (Int, Int) -- (row,col)
minDex g = (ai `div` f, ai `mod` f) 
    where ai = fromMaybe (-1) (findIndex (\l -> length l == minlen) cg)
          minlen = P.minimum (length <$> cg)
          cg = concat g
          f = (length . head) g

pop :: Domain Tile -> Domain Tile -- make random
pop = (:[]) . last


wfc :: Grid (Domain Tile) -> [Constraint Tile] -> Grid (Domain Tile)
wfc inp cs = collapse (over (element r . element c) pop inp) cs [(r,c)]
    where (r,c) = minDex inp

collapse :: Grid (Domain Tile) -> [Constraint Tile] -> [(Int, Int)] -> Grid (Domain Tile)
collapse inp _ [] = inp
collapse inp cs (og@(r,c):q) = over (element tr . element tc) prune inp -- replace with list of succs
    where hb = head cs
          tar@(tr, tc) = (r, c+1)
          prune :: Domain Tile -> Domain Tile -- remove all values in target that violate constraint forall values of neighbor
          prune [] = []
          prune (d:ds) = if all (hb og tar d) (inp !!! og) then prune ds else d : prune ds












