Haskell Hogbom Clean Benchmark
==============================

The Haskell Hogbom Clean Benchmark implements the kernel of the
[Hogbom Clean](http://cdsads.u-strasbg.fr/abs/1974A%26AS...15..417H)
deconvolution algorithm. This simple benchmark was initially written to
benchmark the Intel Xeon Phi accelerator. This particular instance of the
benchmark measures the performance of Haskell.

This benchmark is quite minimal and actually omits the final step, convolution
of the model with the clean beam, but this involves the similar operations to
the other steps as far as the CPU is concerned.

Execution of the Hogbom Clean benchmark will require the existence of the point
spread function (PSF) image and the dirty image (the image to be cleaned) the
working directory.  These can be downloaded from here:

* http://www.atnf.csiro.au/people/Ben.Humphreys/dirty.img
* http://www.atnf.csiro.au/people/Ben.Humphreys/psf.img

The benchmark is built with Cabal. To checkout, build and execute:

    git clone git@github.com:benhumphreys/hogbom-haskell.git
    cd hogbom-haskell
    wget http://www.atnf.csiro.au/people/Ben.Humphreys/dirty.img
    wget http://www.atnf.csiro.au/people/Ben.Humphreys/psf.img
    cabal sandbox init
    cabal install --only-dependencies
    cabal configure
    cabal build
    dist/build/Main/Main
