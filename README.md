Haskell Hogbom Clean Benchmark
==============================

The Haskell Hogbom Clean Benchmark benchmark implements the kernel of the
[Hogbom Clean](http://cdsads.u-strasbg.fr/abs/1974A%26AS...15..417H)
deconvolution algorithm. This simple benchmark was initially written to
benchmark the Intel Xeon Phi accelerator, previously known as Many Integrated
Cores (MIC). This particular instance of the benchmark measures the performance
of Haskell.

This benchmark is quite minimal and actually omits the final step, convolution
of the model with the clean beam, but this involves the similar operations to
the other steps as far as the CPU is concerned.

Execution of both the tHogbomClean benchmarks will require the existence of the
point spread function (PSF) image and the dirty image (the image to be cleaned)
the working directory.  These can be downloaded from here:

* http://www.atnf.csiro.au/people/Ben.Humphreys/dirty.img
* http://www.atnf.csiro.au/people/Ben.Humphreys/psf.img

The benchmark can be built with either Cabal or Make. It is executed without
any command line arguments.
