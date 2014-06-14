{-----------------------------------------------------------------------------
                            Main
-----------------------------------------------------------------------------}

import qualified Data.Time.Clock.POSIX  as PT
import qualified Image                  as I
import qualified HogbomClean            as C

-- | Given a shape returns a string representation
shapeToString :: I.Shape -> String
shapeToString sh = (show ncol) ++ "x" ++ (show nrow)
    where ncol = I.ncol sh
          nrow = I.nrow sh

-- Main
main :: IO ()
main = do
    -- Read images and calculate dimensions
    putStrLn "Reading dirty image and psf image"
    dirty <- I.readImage dirtyFile
    psf <- I.readImage psfFile

    -- Report some numbers
    putStrLn $ "Iterations = " ++ (show niters)
    putStrLn $ "Image dimensions = " ++ (shapeToString $ I.extent dirty)
    putStrLn $ "PSF dimensions   = " ++ (shapeToString $ I.extent psf)

    -- Call Hogbom Clean
    putStrLn "+++++ Processing (Haskell) +++++"
    start <- PT.getPOSIXTime
    (model, residual) <- C.deconvolve dirty psf niters gain threshold
    stop <- PT.getPOSIXTime
    let time = realToFrac $ (stop - start) :: Double
    putStrLn $ "    Time " ++ (show time) ++ " (s)"
    putStrLn $ "    Time per cycle " ++ (show $ time / (fromIntegral niters) * 1000.0) ++ " (ms)"
    putStrLn $ "    Cleaning rate  " ++ (show $ (fromIntegral niters) / time)
        ++ " (iterations per second)"
    putStrLn "Done"

    -- Write model and residual images
    putStrLn "Writing model..."
    I.writeImage "model.img" model
    putStrLn "Writing residual..."
    I.writeImage "residual.img" residual

    where dirtyFile = "dirty.img"
          psfFile = "psf.img"
          niters = 1000::Int
          gain = 0.1
          threshold = 0.00001
