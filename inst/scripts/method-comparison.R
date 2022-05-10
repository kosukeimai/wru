devtools::load_all()

library(future)
library(tictoc)

# availableCores() -- 12

data(voters)
plan(sequential)

tic()
tract_sequential <- suppressMessages(
  predict_race(voter.file = voters, census.geo = "tract", census.key = NULL, party = "PID")
)
toc()
# 105.061 sec elapsed

tic()
tract_sequential_counties <- suppressMessages(
  predict_race(voter.file = voters, census.geo = "tract", census.key = NULL, party = "PID", use_counties = TRUE)
)
toc()
# 7.673 sec elapsed

# Now in parallel
plan(multisession)

tic()
tract_multisession <- suppressMessages(
  predict_race(voter.file = voters, census.geo = "tract", census.key = NULL, party = "PID")
)
toc()
# 24.49 sec elapsed

tic()
tract_multisession_counties <- suppressMessages(
  predict_race(voter.file = voters, census.geo = "tract", census.key = NULL, party = "PID", use_counties = TRUE)
)
toc()
# 7.928 sec elapsed

all.equal(
  tract_sequential,
  tract_sequential_counties
)

all.equal(
  tract_multisession,
  tract_multisession_counties
)

all.equal(
  tract_sequential,
  tract_multisession
)

