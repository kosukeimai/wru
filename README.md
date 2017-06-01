# wru: Who Are You? Bayesian Prediction of Racial Category Using Surname and Geolocation [![Build Status](https://travis-ci.org/kosukeimai/wru.svg?branch=master)](https://travis-ci.org/kosukeimai/wru) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/wru)](https://cran.r-project.org/package=wru)


This R package implements the methods proposed in Imai, K. and Khanna, K. (2016). "[Improving Ecological Inference by Predicting Individual Ethnicity from Voter Registration Record.](http://imai.princeton.edu/research/race.html)" Political Analysis, Vol. 24, No. 2 (Spring), pp. 263-272. doi: 10.1093/pan/mpw001.

### Using wru

Here is a simple example that predicts the race/ethnicity of voters based only on their surnames. 
```r
library(wru)
data(voters)
predict_race(voter.file = voters, surname.only = T)
```

The above produces the following output, where the last five columns are probabilistic race/ethnicity predictions (e.g., 'pred.his' is the probability of being Hispanic/Latino):
```r
# "Proceeding with surname-only predictions ..."
# VoterID    surname state CD county  tract block precinct age sex party PID pred.whi   pred.bla   pred.his   pred.asi pred.oth
#       1     Khanna    NJ 12    021 004000  3001        6  29   0   Ind   0   0.0676 0.00430000 0.00820000 0.86680000  0.05310
#       2       Imai    NJ 12    021 004501  1025           40   0   Dem   1   0.0812 0.00240000 0.06890000 0.73750000  0.11000
#       3    Velasco    NY 12    061 004800  6001           33   0   Rep   2   0.0594 0.00260000 0.82270000 0.10510000  0.01020
#       4    Fifield    NJ 12    021 004501  1025           27   0   Dem   1   0.9355 0.00220000 0.02850000 0.00780000  0.02590
#       5       Zhou    NJ 12    021 004501  1025           28   1   Dem   1   0.0098 0.00180000 0.00065000 0.98200000  0.00575
#       6   Ratkovic    NJ 12    021 004000  1025           35   0   Ind   0   0.9187 0.01083333 0.01083333 0.01083333  0.04880
#       7    Johnson    NY  9    061 015100  4000           25   0   Dem   1   0.5897 0.34630000 0.02360000 0.00540000  0.03500
#       8      Lopez    NJ 12    021 004501  1025           33   0   Rep   2   0.0486 0.00570000 0.92920000 0.01020000  0.00630
#       9 Wantchekon    NJ 12    021 004501  1025           50   0   Rep   2   0.6665 0.08530000 0.13670000 0.07970000  0.03180
#      10      Morse    DC  0    001 001301  3005           29   1   Rep   2   0.9054 0.04310000 0.02060000 0.00720000  0.02370
```

In order to predict race/ethnicity based on surnames AND geolocation, first request a U.S. Census API key [here](http://api.census.gov/data/key_signup.html). Once you have an API key, you can use the package to download relevant Census geographic data on demand and condition race/ethnicity predictions on geolocation (county, tract, or block).

The following example predicts the race/ethnicity of voters based on their surnames, Census tract of residence (census.geo = "tract"), and which party registration (party = "PID"). Note that a valid API key must be provided in the input parameter 'census.key' in order for the function to download the relevant tract-level data.
```r
library(wru)
data(voters)
predict_race(voter.file = voters, census.geo = "tract", census.key = "", party = "PID")
```

The above returns the following output.
```
# VoterID    surname state CD county  tract block precinct age sex party PID    pred.whi     pred.bla     pred.his    pred.asi    pred.oth
#       1     Khanna    NJ 12    021 004000  3001        6  29   0   Ind   0 0.081856291 0.0021396565 0.0110451405 0.828313291 0.076645621
#       6   Ratkovic    NJ 12    021 004000  1025           35   0   Ind   0 0.916936771 0.0044432219 0.0120276229 0.008532929 0.058059455
#       4    Fifield    NJ 12    021 004501  1025           27   0   Dem   1 0.895620643 0.0022078678 0.0139457411 0.023345853 0.064879895
#       5       Zhou    NJ 12    021 004501  1025           28   1   Dem   1 0.003164229 0.0006092345 0.0001072684 0.991261466 0.004857802
#       2       Imai    NJ 12    021 004501  1025           40   0   Dem   1 0.029936354 0.0009275220 0.0129831039 0.850040743 0.106112277
#       8      Lopez    NJ 12    021 004501  1025           33   0   Rep   2 0.231046860 0.0016485574 0.6813780115 0.053180270 0.032746301
#       9 Wantchekon    NJ 12    021 004501  1025           50   0   Rep   2 0.817841573 0.0063677130 0.0258733496 0.107254103 0.042663261
#       3    Velasco    NY 12    061 004800  6001           33   0   Rep   2 0.223924118 0.0002913000 0.4451163607 0.313431417 0.017236805
#       7    Johnson    NY  9    061 015100  4000           25   0   Dem   1 0.241417483 0.6900686166 0.0293556870 0.011105140 0.028053073
#      10      Morse    DC  0    001 001301  3005           29   1   Rep   2 0.983300770 0.0006116706 0.0034070782 0.004823439 0.007857042
```

It is also possible to pre-download Census geographic data, which can save time when running predict_race(). The example dataset 'voters'  includes people in DC, NJ, and NY. The following example subsets voters in DC and NJ, and then uses get_census_data() to download Census geographic data in these two states (input parameter 'key' requires valid API key). Census data is assigned to an object named census.dc.nj. The predict_race() statement predicts the race/ethnicity of voters in DC and NJ using the pre-saved Census data (census.data = census.dc.nj). This example conditions race/ethnicity predictions on voters' surnames, block of residence (census.geo = "block"), age (age = TRUE), and party registration (party = "PID").

Please note that the input parameters 'age' and 'sex' must have the same values in get_census_data() and predict_race(), i.e., TRUE in both or FALSE in both. In this case, predictions are conditioned on age but not sex, so age = TRUE and sex = FALSE in both the get_census_data() and predict_race() statements.
```r
library(wru)
data(voters)
voters.dc.nj <- voters[c(-3, -7), ]  # remove two NY cases from dataset
census.dc.nj <- get_census_data(key = "", state = c("DC", "NJ"), age = TRUE, sex = FALSE)  # create Census data object covering DC and NJ 
predict_race(voter.file = voters.dc.nj, census.geo = "block", census.data = census.dc.nj, age = TRUE, sex = FALSE, party = "PID")
```

The last two lines above are equivalent to the following:
```r
predict_race(voter.file = voters.dc.nj, census.geo = "block", census.key = "", age = TRUE, sex = FALSE, party = "PID")
```
 
Using pre-downloaded Census data may be useful for the following reasons:
* You can save a lot of time in future runs of predict_race() if the relevant Census data has already been saved; 
* The machines used to run predict_race() may not have internet access; 
* You can obtain timely snapshots of Census geographic data that match your voter file.

Downloading data using get_census_data() may take a long time, especially at the block level or in large states. If block-level Census data is not required, downloading Census data at the tract level will save time. Similarly, if tract-level Census data is not required, county-level data may be specified in order to save time.

```r
library(wru)
data(voters)
voters.dc.nj <- voters[c(-3, -7), ]  # remove two NY cases from dataset
census.dc.nj2 <- get_census_data(key = "", state = c("DC", "NJ"), age = TRUE, sex = FALSE, census.geo = "tract")  
predict_race(voter.file = voters.dc.nj, census.geo = "tract", census.data = census.dc.nj2, party = "PID", age = TRUE, sex = FALSE)
predict_race(voter.file = voters.dc.nj, census.geo = "county", census.data = census.dc.nj2, age = TRUE, sex = FALSE)  # Pr(Race | Surname, County)
predict_race(voter.file = voters.dc.nj, census.geo = "tract", census.data = census.dc.nj2, age = TRUE, sex = FALSE)  # Pr(Race | Surname, Tract)
predict_race(voter.file = voters.dc.nj, census.geo = "county", census.data = census.dc.nj2, party = "PID", age = TRUE, sex = FALSE)  # Pr(Race | Surname, County, Party)
predict_race(voter.file = voters.dc.nj, census.geo = "tract", census.data = census.dc.nj2, party = "PID", age = TRUE, sex = FALSE)  # Pr(Race | Surname, Tract, Party)
```

Or you can also use the census_geo_api() to maually construct a census object. The example below creates a census object with county-level and tract-level data in DC and NJ, while avoiding downloading block-level data. Note that this function has the input parameter 'state' that requires a two-letter state abbreviation to proceed.
```r
censusObj2  = list()

county.dc <- census_geo_api(key = "", state = "DC", geo = "county", age = TRUE, sex = FALSE)
tract.dc <- census_geo_api(key = "", state = "DC", geo = "tract", age = TRUE, sex = FALSE)
censusObj2[["DC"]] <- list(state = "DC", county = county.dc, tract = tract.dc, age = TRUE, sex = FALSE)

tract.nj <- census_geo_api(key = "", state = "NJ", geo = "tract", age = TRUE, sex = FALSE)
county.nj <- census_geo_api(key = "", state = "NJ", geo = "county", age = TRUE, sex = FALSE)
censusObj2[["NJ"]] <- list(state = "NJ", county = county.nj, tract = tract.nj, age = TRUE, sex = FALSE)
```

Note: The age and sex parameters must be consistent when creating the Census object and using that Census object in the predict_race function. If one of these parameters is TRUE in the Census object, it must also be TRUE in the predict_race function.

After saving the data in censusObj2 above, we can condition race/ethnicity predictions on different combinations of input variables, without having to re-download the relevant Census data.
```r
predict_race(voter.file = voters.dc.nj, census.geo = "county", census.data = censusObj2, age = TRUE, sex = FALSE)  # Pr(Race | Surname, County)
predict_race(voter.file = voters.dc.nj, census.geo = "tract", census.data = censusObj2, age = TRUE, sex = FALSE)  # Pr(Race | Surname, Tract)
predict_race(voter.file = voters.dc.nj, census.geo = "county", census.data = censusObj2, party = "PID", age = TRUE, sex = FALSE)  # Pr(Race | Surname, County, Party)
predict_race(voter.file = voters.dc.nj, census.geo = "tract", census.data = censusObj2, party = "PID", age = TRUE, sex = FALSE)  # Pr(Race | Surname, Tract, Party)
```
