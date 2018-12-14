README
================
Gabri, Pablo, Joey
December 12, 2018

# fecScrape

The fecScrape package provides functions to interface with the [OpenFEC
API](https://api.open.fec.gov/developers/). OpenFEC API allows you to
access funding data on candidates and committees. This package allows
users to scrape individual- and aggregated-level donation data, plot
these data to examine the timecourse of donation as well as geographic
spread of donations, and does some basic summary statistics.

# Thanks

During this class project, we came across another package which inspired
our own code and thoughts on how to develop this package. Please check
out the [tidyusafec](https://github.com/stephenholzman/tidyusafec)
package, written by Stephen Holzman.

# Installation

To finish when the R package is complete.

# Functions

To Do: Create a naming schematic for the functions such that all
functions which interface with the OpenFEC API use the “query\_” prefix
and all functions which plot data use the “plot\_” prefix.

\[x\] choose\_cand \[x\] get\_contributions\_all \[x\]
get\_itemized\_contributions \[x\] plot\_donations \[x\]
query\_candidate\_list \[x\] query\_openfec \[x\] top\_cities \[x\]
top\_occupations

# Example: 2018 Senate race between Beto & Cruz

## Step 1: Scrape candidates running in an election of interest.

For our example, we will focus on the recent 2018 Sentate race in Texas
between [Beto O’Rourke](https://betofortexas.com) and [Ted
Cruz](https://www.cruz.senate.gov). We chose these this specific
election because both Beto and Cruz raised tens of millions of dollars
for this election cycle.

``` r
# Find and select candidates
my_api <- "jFTYk34OsWkFoEHLcUDa7G1Ax4GCyhJyAgCwB8oz"

tx_data <- query_candidate_list(
  api_key = my_api, 
  state = "TX", 
  election_year = 2018, 
  office = "S"
)
head(tx_data)

# Select candidates of interest
tx_chosen_data <- choose_cand(tx_data, 3, 4)
head(tx_chosen_data)
```

## Step 2: Find individual donations for specified candidates

``` r
# Find all individual donations to each candidates' primary committee
tx_indiv_data <- query_contributions_all(
  input_candlist = tx_chosen_data, 
  api_key = my_api
)
```

## Step 3: Plot average donations

``` r
tx_avg_donation <- plot_avg_donation(tx_indiv_data)
tx_avg_donation
```

## Step 4: Plot cummulative donations

``` r
tx_cum_donation <- plot_cum_donation(tx_indiv_data)
tx_cum_donation
```

## Step 5: Plot cities of donators

``` r
tx_cities_donation <- plot_top_cities(3, tx_indiv_data)
tx_cities_donation
```

## Step 6: Plot occputations of donators

``` r
tx_occup_donation <- plot_occupations(4, tx_indiv_data)
tx_occup_donation
```
