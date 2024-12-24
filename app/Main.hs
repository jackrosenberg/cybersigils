{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Main where
import Prelude as P
import Control.Lens as C
import Graphics.Image as I

newtype Tile = Tile {_image :: Image RPU RGB Double}
    deriving Eq
type Constraint a = (Int, Int) -> (a -> a -> Bool) -- takes two indexes and compares the tiles on their indexes if allowed

type Grid a = [[a]]  -- rows and cols (x,y)

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
cst = []
grd :: Grid (Domain Tile)
grd = replicate 3 (replicate 3 tilelist) -- domains start all possibilities

wfc :: Grid (Domain Tile) -> [Constraint Tile] -> Grid (Domain Tile)
wfc inp cs = over (element 1 . element 1) ((:[]) . head) inp
