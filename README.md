EchoNet2Fish
============

**EchoNet2Fish** is a package of functions for the [R programming language](http://www.r-project.org/).  **EchoNet2Fish** estimates fish abundance from acoustic surveys combined with catch in nets (typically midwater trawls or gill nets).
An example of how to use the functions in **EchoNet2Fish** is given in this [vignette](https://rawgit.com/JVAdams/EchoNet2Fish/master/vignettes/Intro.html).

- - -

You should be able to access the functions by installing them directly from within R.

    install.packages("devtools")
    devtools::install_github("JVAdams/EchoNet2Fish", build_vignettes=TRUE)
    library(EchoNet2Fish)

If you don't already have `Rtools` and `devtools`, you will need to download and install (as administrator, if using a PC) `Rtools` from [CRAN](http://cran.r-project.org/bin/windows/Rtools/) then run the following lines of code before submitting the code above:

    find_rtools()
    install.packages("devtools")

An alternative approach for Windows users is to download this [zip file](https://github.com/JVAdams/EchoNet2Fish/raw/master/) and install the package from the R menu:
- Packages
- Install package(s) from local zip files...
	
- - -

_U.S. Geological Survey_ (USGS) Computer Program **EchoNet2Fish** version 0.2.2.9000. 
Written by Jean V. Adams, [USGS - Great Lakes Science Center](http://www.glsc.usgs.gov/), Ann Arbor, Michigan, USA. 
Written in programming language R (R Core Team, 2015, www.R-project.org), version 3.2.2 (2015-08-14). 
Run on a PC with Intel(R) Core(TM) I7-4600m CPU, 2.90 GHz processor, 16.0 GB RAM, and Microsoft Windows 7 Enterprise operating system 2009 Service Pack 1. 
Source code is available from Jean V. Adams on [GitHub](https://github.com/JVAdams/EchoNet2Fish), _jvadams (at) usgs (dot) gov_.

_Disclaimer:_ Although this program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the United States Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.
