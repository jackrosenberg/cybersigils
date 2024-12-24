{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Main where
import Prelude as P
import Control.Lens as C
import Graphics.Image as I

newtype Tile = Tile {_image :: Image RPU RGB Double}
    deriving Eq
type Constraint a = (Int, Int) -> (a -> a -> Bool) -- takes two indexes and compares the tiles on their indexes if allowed

newtype Grid a = Grid { _grid :: [[Domain a]] } -- rows and cols (x,y)

newtype Domain a = Domain {_list :: [a]} -- list of tiles that are possibilities
    deriving Foldable

$(makeLenses ''Domain) -- allows setting and getting of the list in Domain
$(makeLenses ''Tile)
$(makeLenses ''Grid)

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
    let wfcres = combineTiles $ toTiles (wfc grd cst )
    write ("res",  wfcres)

toTiles :: Grid Tile -> [Tile]
toTiles g = (\dt -> avgImg (view image <$> (dt^.list))) <$> concat (g^.grid)

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
grd :: Grid Tile
grd = Grid { _grid = replicate 3 (replicate 3 Domain { _list = tilelist}) } -- domains start all possibilities

wfc :: Grid Tile -> [Constraint Tile] -> Grid Tile
wfc inp cs = over mapped (const crossTile) inp 
