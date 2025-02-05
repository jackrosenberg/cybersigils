{-# LANGUAGE TemplateHaskell, DeriveTraversable, ScopedTypeVariables #-}
module Main where
import Prelude as P
import Control.Lens as C
import Data.Maybe (fromMaybe)
import Data.List (findIndex, nub, findIndices)
import Debug.Trace (trace)
import Control.Monad (liftM2)
import System.Random 
import Graphics.Gloss
import GHC.Float (int2Float)
import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Game (Key (SpecialKey), Event (EventKey), KeyState (Down, Up), SpecialKey (KeySpace))

data World = World
    { _keys :: S.Set Key
    , _grid :: Grid (Domain Tile)
    }


newtype Tile = Tile {_image :: Picture}
    deriving Eq
type Constraint a = (Int, Int) -> (Int, Int) -> Tile -> Tile -> Bool -- takes two indexes and gives func to compare
 -- takes two indexes and gives func to compare
type Grid a = [[a]]  -- rows and cols (x,y)

(!!!) :: Grid a -> (Int, Int) -> a
g !!! l = g !! fst l !! snd l 

type Domain a = [a] -- list of tiles that are possibilities

$(makeLenses ''Tile)
$(makeLenses ''World)

between :: (Ord a) => a -> a -> a -> Bool
between l x u = x >= l && x <= u 


horizTile ,vertTile, crossTile, collapseTile, defaultTile :: Tile
horizTile = Tile { _image = pictures [color black $ rectangleSolid 90 90, color white $ rectangleSolid 90 30] }
vertTile  = Tile { _image = pictures [color black $ rectangleSolid 90 90, color white $ rectangleSolid 30 90] }
crossTile = Tile { _image = pictures [color black $ rectangleSolid 90 90, color white $ rectangleSolid 30 90, color white $ rectangleSolid 90 30]}

collapseTile = Tile { _image = color red $ rectangleSolid 90 90   }
defaultTile  = Tile { _image = color black $ rectangleSolid 90 90 }


handleInput :: Event -> World -> World
handleInput (EventKey k Down _ _) env = env { _keys = S.insert k (_keys env)}
handleInput (EventKey k Up _ _)   env = env { _keys = S.delete k (_keys env)}
handleInput _ env = env -- Ignore non-keypresses for simplicity

update :: RandomGen g => g -> Float -> World -> World
update g _ world
    | S.member (SpecialKey KeySpace) (_keys world) = over grid (const res) world
    | otherwise = world
    where (res, _) = wfco ng cst (world^.grid)
          (ng, _) = split g

tilelist :: [Tile]
tilelist = [horizTile ,vertTile, crossTile]

render :: World -> Picture
render w = translate (-(45*int2Float (sz-1))) (-(45*int2Float (sz-1))) $ pictures b
    where 
     res = w^.grid  
     l = zip [(r, c) | r <- [0..(sz -1)], c <- [0..(sz -1)]] (concat res)
     fr = minDeces res
     b = (\((r, c), d) -> let col = if (r,c) `elem` fr then green else light orange in translate (int2Float r*90) (int2Float c*90) (toTl d col ^. image)) <$> l 

------------------------------------------------------
sz = 9
window = InWindow "" (90*sz, 90*sz) (0, 0)
background = light black
cst = [noAdj]   :: [Constraint Tile]
grd = genGrd sz :: Grid (Domain Tile)
------------------------------------------------------


main :: IO ()
main = do
    g <- getStdGen
    play window background 30 (World {_keys = S.empty , _grid = grd}) render handleInput (update g)
    

toTl :: Domain Tile -> Color -> Tile
toTl [] _= trace "dumfuk, wave function collapsed" collapseTile
toTl [i] _ = i
toTl l@(i:is) c = Tile {_image = (scale 0.5 0.5 . translate (-45) (-45) . color c . text . show . length) l}
noAdj, cross :: Constraint Tile

cross (r1,c1) (r2,c2) t1 t2 =  r1 == r2 && (t1 /= crossTile) && (t2 /= horizTile)

noAdj _ _ = (==) -- universal constraint??

genGrd :: Int -> Grid (Domain Tile)
genGrd sz = replicate sz (replicate sz tilelist) -- domains start all possibilitiesidcs

minDeces :: Grid (Domain Tile) -> [(Int, Int)] -- (row,col)
minDeces g = (\ai -> (ai `div` f, ai `mod` f)) <$> ids
    where ids = findIndices (\l -> length l == minlen) cg
          minlen = non1min (length <$> cg)
          cg = concat g
          f = (length . head) g

non1min :: [Int] -> Int
non1min = foldr (\i acc -> if i <= 1 then acc else min i acc) 9999

pop :: (RandomGen g) => g -> Domain Tile -> (Domain Tile, g) 
pop g d = trace ("i: " <> show i <> " ld: " <> show (length d-1)) ([d!!i], ng)
        where (i, ng) = randomR (0, length d-1) g
 
succs :: Grid a -> (Int, Int) -> [(Int, Int)] -- get succs of a list
succs g (r,c)  = [(nr, nc) | nr <- [r-1, r, r+1], between 0 nr ((length . head $ g) -1),
                             nc <- [c-1, c, c+1], between 0 nc ((length . head $ g) -1), 
                            (nr == r) /= (nc == c)] --xor bitch

idx :: (Int, Int) -> ASetter (Grid (Domain Tile)) (Grid (Domain Tile)) (Domain Tile) (Domain Tile)
idx s = element (fst s) . element (snd s)

done :: Grid (Domain Tile) -> Bool
done  = all $ all $ (<=1) . length

-- Wfc once:
-- top level function, pops a random tile with the least entropy and calls collapse on it
wfco :: RandomGen g => g -> [Constraint Tile] -> Grid (Domain Tile) -> (Grid (Domain Tile), g)
wfco g cs inp | done inp  = (inp, g)
              | otherwise = let r = fst $ collapse cs (over (idx n) (const nd) inp, succs inp n) in (r, nnng)
    where (nd, nnng) = pop ng (inp !!! n) 
          idcs = minDeces inp
          (n, ng) = let (ni, ng) = randomR (0, length idcs -1) g in (idcs !! ni, ng)

collapse :: [Constraint Tile] -> (Grid (Domain Tile), [(Int, Int)]) -> (Grid (Domain Tile), [(Int, Int)])
collapse _  (inp, [])  = (inp, [])
collapse cs (inp,n:fr) = collapse cs (nGrid, nub $ fr ++ nFr)
    where 
          (nGrid, nFr) = foldr (\suc (gr,cf) -> let (shorter, nd) = prune gr suc (gr!!!n) in (over (idx n) (const nd) gr, if shorter then cf ++ succs inp n else cf)) (inp, []) (succs inp n) -- fold over the array and prune forall successors

          prune :: Grid (Domain Tile) -> (Int, Int) -> Domain Tile -> (Bool, Domain Tile) -- remove all vals from d that s doesnt allow
          prune cg s d = let res = foldr (\csr acc -> filter (\v -> (not . all (csr n s v)) (cg !!! s)) acc) d cs in (length d > length res, res)
