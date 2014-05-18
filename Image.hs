{-----------------------------------------------------------------------------
                            Image
-----------------------------------------------------------------------------}

module Image
( Image,
  Shape (..),
  Pos (..),
  row,
  col,
  fromIndex,
  toIndex,
  nrow,
  ncol,
  readImage,
  writeImage,
  fromVector,
  toVector,
  extent,
  getElem,
  setElem,
  newImage,
  contains
) where

import qualified Data.Vector.Unboxed    as V
import qualified Data.ByteString.Lazy   as B
import qualified Data.Binary.IEEE754    as B
import qualified Data.Binary.Get        as B
import qualified Data.Binary.Put        as B

-- A position on a 2D plane
-- The two Ints are row and column respectively
data Pos = Pos Int Int deriving (Eq)

-- A shape of an image
-- The two Ints are numer of rows and number of columns respectively
data Shape = Shape Int Int deriving (Show, Eq)

-- Represents a 2D image of 32-bit floats
data Image = Image (V.Vector Float) Shape deriving (Show)

----------------------------------------------------------
-- Shape
----------------------------------------------------------

-- Returns the number of rows in a shape
nrow :: Shape -> Int
nrow (Shape r _) = r

-- Returns the number of columns in a shape
ncol :: Shape -> Int
ncol (Shape _ c) = c

----------------------------------------------------------
-- Pos
----------------------------------------------------------

-- Returns the row from a position 
row :: Pos -> Int
row (Pos r _) = r

-- Returns the column from a position
col :: Pos -> Int
col (Pos _ c) = c

-- Converts a Pos (2D) to an array index (1D)
fromIndex :: Int -> Shape -> Pos
fromIndex idx sh = Pos row col
    where col = mod idx $ ncol sh
          row = div idx $ ncol sh

-- Converts an array index (1D) into a Pos (2D)
toIndex :: Pos -> Shape -> Int
toIndex (Pos row col) (Shape _ ncol) = (row * ncol) + col

-- Is the position contained within the shape
contains :: Shape -> Pos -> Bool
contains (Shape sr sc) (Pos pr pc) | pr < 0     = False
                                   | pr >= sr   = False
                                   | pc < 0     = False
                                   | pc >= sc   = False
                                   | otherwise  = True

----------------------------------------------------------
-- Image
----------------------------------------------------------

-- Given an image and a position, returns the value at that position  
getElem :: Image -> Pos -> Float
getElem (Image vec sh) p =  vec V.! (toIndex p sh)

-- Given an image, a position, and a vale, returns an image with the
-- value at the given position, and all other pixels the same as the
-- input image
setElem :: Image -> Pos -> Float -> Image
setElem (Image vec sh) p val = (Image (vec V.// [(pos1d, val)]) sh)
    where pos1d = toIndex p sh

-- Given a shape and a value, returns an image of the given shape with
-- all pixels set to the given value
newImage :: Shape -> Float -> Image
newImage sh val = Image (V.replicate ((nrow sh) * (ncol sh)) val) sh

-- Reads a vector from a file
readVecFloat :: FilePath -> IO (V.Vector Float)
readVecFloat f = do
    input <- B.readFile f 
    return $ V.fromList $ listOfFloat32le input
    where listOfFloat32le input
            | B.null input = []
            | otherwise = let (val, rest, _) = B.runGetState B.getFloat32le input 0
                        in val : listOfFloat32le rest

-- Writes a vector to a file
writeVecFloat :: FilePath -> V.Vector Float -> IO()
writeVecFloat f xs = do
    B.writeFile f $ B.runPut $ putRawFloats xs
    where putRawFloats xs = do
              V.mapM_ B.putFloat32le xs

-- Given a vector, calculates the square root of the size. If the image is
-- a square the size (along one-dimension) is returned, otherwise "error"
-- is called
checkSquare :: V.Vector Float -> IO Int
checkSquare xs = do
    let sz = V.length xs
    let singledim = floor . sqrt $ fromIntegral sz
    if (singledim * singledim /= sz)
        then do error "Image is not square"
        else return singledim

-- Reads an image from disk
readImage :: FilePath -> IO (Image)
readImage f = do
    vec <- readVecFloat f
    dim <- checkSquare vec
    return $ Image vec $ Shape dim dim

-- Writes an image to disk
writeImage :: FilePath -> Image -> IO()
writeImage f (Image v _) = writeVecFloat f v

-- Converts a vector to an image
fromVector :: V.Vector Float -> Shape -> Image
fromVector v sh = Image v sh

-- Converts an image to a vector
toVector :: Image -> V.Vector Float
toVector (Image v _) = v

-- Returns the extent (or shape) of an image
extent :: Image -> Shape
extent (Image _ sh) = sh
