# Set random seed
set.seed(12345)

data(voters)

# Need a valid census key
k <- NULL

if (! is.null(k)) {
  predict_race(voter.file = voters, census.geo = "tract", census.key = k, party = "PID")
}

