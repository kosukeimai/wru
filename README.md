# wru: Who Are You? Bayesian Prediction of Racial Category Using Surname and Geolocation

The R package implements the methods proposed in Imai, Kosuke and Kabir Khanna. (2016). ``Improving Ecological Inference by Predicting Individual Ethnicity from Voter Registration Record.'' Political Analysis. doi: 10.1093/pan/mpw001 available at http://imai.princeton.edu/research/race.html

## Using wru

To start using the package, get Census Data API Key from [http://api.census.gov/data/key_signup.html](http://api.census.gov/data/key_signup.html)

Once you have the key, you can dive right in. The package downloads relevant data on demand.

For instance, to get the race of someone with name last name Smith, write in: 

```r
race.pred(voters = data.frame(surname="Smith"), races = c("white", "black", "latino"), census = "tract", census.key = "65b30624e3c6d0e4b6ebfbde95dd951b6732c69b", demo = TRUE, surname.only=TRUE)
```
Which returns:
```
# surname pred.whi pred.bla pred.his
# 1   Smith   0.7335   0.2222   0.0156
```

