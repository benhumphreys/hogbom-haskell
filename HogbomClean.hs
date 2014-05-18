{-----------------------------------------------------------------------------
                       Hogbom Clean (Deconvolution)
-----------------------------------------------------------------------------}

module HogbomClean (deconvolve) where

import qualified Data.Vector.Unboxed    as V
import qualified Image                  as I

-- Find the absolute peak value in an image, returning both
-- the value and the position
--
-- This function essentially breaks encapsulation of the Image
-- (not that it is very well encapsulated to start with), but it
-- does this for a small efficiency and simplicity gain
findPeak :: I.Image -> (Float, I.Pos)
findPeak img = (maxVal, maxPos2d)
    where absVals = V.map abs $ I.toVector img
          maxPos = V.maxIndex absVals
          maxVal = absVals V.! maxPos
          maxPos2d = I.fromIndex maxPos $ I.extent img

-- This function gets passed to map to process each pixel
subPsfPixel ::  I.Image ->  -- PSF image
                I.Shape ->  -- Residual image shape
                Float ->    -- Gain
                Int ->      -- diffx
                Int ->      -- diffy
                Int  ->     -- Index location of pixel
                Float ->    -- Input pixel value
                Float       -- Output pixel value
subPsfPixel psf resShape gain diffx diffy pos val | inBounds = newval
                                                  | otherwise = val
    where resPos = I.fromIndex pos resShape
          x = I.col resPos
          y = I.row resPos
          psfShape = I.extent psf
          psfPos = I.Pos (y - diffy) (x - diffx)
          inBounds = I.contains psfShape psfPos
          psfVal = I.getElem psf psfPos
          newval = val - ((abs val) * gain * psfVal)

-- Subtracts the PSF from the residual image
subtractPSF :: I.Image ->       -- PSF image
               I.Image ->       -- Residual image
               I.Pos ->         -- Peak position
               I.Pos ->         -- PSF peak position
               Float ->         -- Gain
               I.Image          -- Return value: new residual image
subtractPSF psf residual peakpos psfpeakpos gain = I.fromVector newvec resShape
    where resShape = I.extent residual
          rx = I.col peakpos
          ry = I.row peakpos
          px = I.col psfpeakpos
          py = I.row psfpeakpos
          diffx = rx - px
          diffy = ry - py
          f = subPsfPixel psf resShape gain diffx diffy
          newvec = V.imap f $ I.toVector residual

-- The deconvolution minor cycle loop
deconvolveLoop :: I.Image ->    -- Residual image
                  I.Image ->    -- Model image
                  I.Image ->    -- PSF image
                  I.Pos ->      -- PSF peak position
                  Int ->        -- iter (zero based)
                  Int ->        -- niters
                  Float ->      -- gain
                  Float ->      -- Threshold
                  IO (I.Image, I.Image)     -- return value: (model, residual)
deconvolveLoop residual model psf psfPeakPos iter niters gain threshold = do
    if (iter < niters)
        then do 
            -- Find peak in residual image
            let (peakVal, peakPos) = findPeak residual
            putStrLn $ "Iteration: " ++ (show $ iter + 1)
                ++ " - Maximum = " ++ (show peakVal)
                ++ " at location " ++ (show $ I.col peakPos)
                ++ ", " ++ (show $ I.row peakPos)

            if ((abs peakVal) >= threshold)
                then do
                    -- Add to model
                    let oldval = I.getElem model peakPos
                    let newval = oldval + (peakVal * gain)
                    let newmodel = I.setElem model peakPos newval

                    -- Subtract the PSF from the residual image
                    let newresidual = subtractPSF psf residual peakPos psfPeakPos gain

                    -- Loop
                    deconvolveLoop newresidual newmodel psf psfPeakPos (iter + 1) niters gain threshold

                else return (model, residual)
          
        else return (model, residual)

-- Deconvolution entry function
deconvolve :: I.Image ->        -- Dirty image
              I.Image ->        -- PSF image
              Int ->            -- Maximum number of iterations
              Float ->          -- Gain
              Float ->          -- Threshold
              IO (I.Image, I.Image)   -- Return value: (model, residual)
deconvolve dirty psf niters gain threshold = do
    -- Initialise model and residual images
    let residual = dirty
    let model = I.newImage (I.extent dirty) 0.0

    -- Find the peak of the PSF
    let (psfPeakVal, psfPeakPos) = findPeak psf
    putStrLn $ "Found peak of PSF: Maximum = " ++ (show psfPeakVal)
         ++ " at location " ++ (show $ I.col psfPeakPos)
         ++ ", " ++ (show $ I.row psfPeakPos)
    deconvolveLoop residual model psf psfPeakPos 0 niters gain threshold
