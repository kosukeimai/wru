# wru: Who Are You? Bayesian Prediction of Racial Category Using Surname and Geolocation [![Build Status](https://travis-ci.org/kosukeimai/wru.svg?branch=master)](https://travis-ci.org/kosukeimai/wru) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/wru)](https://cran.r-project.org/package=wru)


The R package implements the methods proposed in Imai, Kosuke and Kabir Khanna. (2016). ``[Improving Ecological Inference by Predicting Individual Ethnicity from Voter Registration Record.](http://imai.princeton.edu/research/race.html)'' Political Analysis, Vol. 24, No. 2 (Spring), pp. 263-272. doi: 10.1093/pan/mpw001 

### Using wru

To start using the package, get Census Data API Key from [http://api.census.gov/data/key_signup.html](http://api.census.gov/data/key_signup.html)

Here is an simple example that predict the race only based on the surnames. 
```r
library(wru)
data(voters)
race.pred(voters, surname.only = T)
```

The above produce the following output:
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

Once you have the key, you can dive right in. The package downloads relevant data on demand.

For instance, to get the race of someone with name last name Smith, write in where `---' should be replaced with the Census Data API Key you obtained: 

```r
library(wru)
data(voters)
race.pred(voter.file = voters, races = c("white", "black", "latino", "asian", "other"), census.geo = "tract", census.key = '---', party = "PID")
```

which returns the predicted probabilities for each racial category:

```
#    VoterID    surname state CD county  tract block precinct age sex party PID    pred.whi     pred.bla     pred.his    pred.asi    pred.oth
#        1     Khanna    NJ 12    021 004000  3001        6  29   0   Ind   0 0.081856291 0.0021396565 0.0110451405 0.828313291 0.076645621
#        6   Ratkovic    NJ 12    021 004000  1025           35   0   Ind   0 0.916936771 0.0044432219 0.0120276229 0.008532929 0.058059455
#        4    Fifield    NJ 12    021 004501  1025           27   0   Dem   1 0.895620643 0.0022078678 0.0139457411 0.023345853 0.064879895
#        5       Zhou    NJ 12    021 004501  1025           28   1   Dem   1 0.003164229 0.0006092345 0.0001072684 0.991261466 0.004857802
#        2       Imai    NJ 12    021 004501  1025           40   0   Dem   1 0.029936354 0.0009275220 0.0129831039 0.850040743 0.106112277
#        8      Lopez    NJ 12    021 004501  1025           33   0   Rep   2 0.231046860 0.0016485574 0.6813780115 0.053180270 0.032746301
#        9 Wantchekon    NJ 12    021 004501  1025           50   0   Rep   2 0.817841573 0.0063677130 0.0258733496 0.107254103 0.042663261
#        3    Velasco    NY 12    061 004800  6001           33   0   Rep   2 0.223924118 0.0002913000 0.4451163607 0.313431417 0.017236805
#        7    Johnson    NY  9    061 015100  4000           25   0   Dem   1 0.241417483 0.6900686166 0.0293556870 0.011105140 0.028053073
#       10      Morse    DC  0    001 001301  3005           29   1   Rep   2 0.983300770 0.0006116706 0.0034070782 0.004823439 0.007857042
```

It is also possible to pre-download and save the census data for the intended states. For example, voters data only involves people in DC, NJ and NY:
```r
library(wru)
data(voters)
censusObjs <- getCensusData('---', state = c("NY", "DC", "NJ"), demo = FALSE)   # Note: '---' is your census key
race.pred(voter.file = voters, races = c("white", "black", "latino", "asian", "other"), census.geo = "tract", census.data = censusObjs, party = "PID")
```

Please note that the input parameter $demo$ needs to be consistent in $getCensusData()$ and $race.pred()$, i.e. both FALSE or both TRUE.

The result is same as the above that accesses online census data, instead of using a pre-downloaded census object.

The feature of using a pre-downloaded census object is useful for the following reasons:
(1) the machine runs race.pred may not have internet access. 
(2) No redundent download of the census data for each state. 
(3) Timely snapshots of the census data that match the voter data.
