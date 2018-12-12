README
================
Joey Heffner
December 12, 2018

fecScrape
=========

The fecScrape package provides functions to interface with the [OpenFEC API](https://api.open.fec.gov/developers/). OpenFEC API allows you to access funding data on candidates and committees. This package allows users to scrape individual- and aggregated-level donation data, plot these data to examine the timecourse of donation as well as geographic spread of donations, and does some basic summary statistics.

Thanks
======

During this class project, we came across another package which inspired our own code and thoughts on how to develop this package. Please check out the [tidyusafec](https://github.com/stephenholzman/tidyusafec) package, written by Stephen Holzman.

Installation
============

To finish when the R package is complete.

Functions
=========

To Do: Create a naming schematic for the functions such that all functions which interface with the OpenFEC API use the "query\_" prefix and all functions which plot data use the "plot\_" prefix.

\[x\] choose\_cand \[x\] get\_contributions\_all \[x\] get\_itemized\_contributions \[x\] plot\_donations \[x\] query\_candidate\_list \[x\] query\_openfec \[x\] top\_cities \[x\] top\_occupations

Example: 2018 Senate race between Bato & Cruz
=============================================

Step 1: Scrape candidates running in an election of interest.
-------------------------------------------------------------

For our example, we will focus on the recent 2018 Sentate race in Texas between [Beto O'Rourke](https://betofortexas.com) and [Ted Cruz](https://www.cruz.senate.gov). We chose these this specific election because both Beto and Cruz raised tens of millions of dollars for this election cycle.

``` r
# Test
```
