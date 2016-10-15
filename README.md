# wru: Who Are You? Bayesian Prediction of Racial Category Using Surname and Geolocation [![Build Status](https://travis-ci.org/kosukeimai/wru.svg?branch=master)](https://travis-ci.org/kosukeimai/wru)

The R package implements the methods proposed in Imai, Kosuke and Kabir Khanna. (2016). ``[Improving Ecological Inference by Predicting Individual Ethnicity from Voter Registration Record.](http://imai.princeton.edu/research/race.html)'' Political Analysis, Vol. 24, No. 2 (Spring), pp. 263-272. doi: 10.1093/pan/mpw001 

### Using wru

To start using the package, get Census Data API Key from [http://api.census.gov/data/key_signup.html](http://api.census.gov/data/key_signup.html)

Once you have the key, you can dive right in. The package downloads relevant data on demand.

For instance, to get the race of someone with name last name Smith, write in where `---' should be replaced with the Census Data API Key you obtained: 

```r
race.pred(voters = data.frame(surname="Smith"), races = c("white", "black", "latino"), census = "tract", 
          census.key = "---", demo = TRUE, surname.only=TRUE)
```
which returns the predicted probabilities for each racial category:

```
# surname pred.whi pred.bla pred.his
# 1   Smith   0.7335   0.2222   0.0156
```

This package contains a sample data frame `voters`. Some function calls in the documentation reference this object.
