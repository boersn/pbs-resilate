## PBSresilate: Explorations of complexity and resilience ##
&copy; Fisheries and Oceans Canada (2011-2018)

This R package uses visual tools to investigate properties of complex models. In particular, it deals with resilience theory developed by C.S. (Buzz) Holling and his colleagues. Where possible, we employ graphical methods in R designed for three or more dimensions, such as **rgl** and **rggobi**. We also take advantage of differential equation solvers available in other R packages. 

**PBSresilate** belongs to a suite of R packages described at [http://github.com/pbs-software/](http://github.com/pbs-software/). It depends particularly on **PBSmodelling** ([http://github.com/pbs-software/pbs-modelling/](http://github.com/pbs-software/pbs-modelling/)). 


Users interested in Buzz Holling and his work on resilience may wish to consult the web links shown below:<br>
* [Buzz Holling - Wikipedia](https://en.wikipedia.org/wiki/C._S._Holling)<br>
* [Resilience Alliance](http://www.resalliance.org)<br>
* [Volvo Environment Prize 2008](http://www.environment-prize.com/laureates/by-year/2008/crawford-buzz-holling/)

**PBSresilate** represents just one of a series of R packages developed at the Pacific Biological Station (<a href="http://www.pac.dfo-mpo.gc.ca/science/facilities-installations/index-eng.html#pbs">PBS</a>) in Nanaimo, British Columbia. A more advanced version of **PBSresilate** might be available at <a href="https://github.com/pbs-software">pbs-software on GitHub</a>. Any evolving package (Windows binary and source tarball) is built after using CRAN's rigorous `R CMD check --as-cran` routine (using R-devel on a **Windows 7** 64-bit system) and posted to <a href="https://drive.google.com/drive/folders/0B2Bkic2Qu5LGOGx1WkRySVYxNFU?usp=sharing">Google Drive</a>. Most of the time, the revision on <a href="https://github.com/pbs-software/pbs-resilate">GitHub</a> can be built (supposedly) in R using `devtools::install_github("pbs-software/pbs-resilate")`; however, not every revision has been checked for CRAN worthiness.