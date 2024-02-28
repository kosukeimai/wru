voters <- data.frame(
  VoterID = as.character(1:10),
  surname = c("Khanna", "Imai", "Rivera", "Fifield", "Zhou", "Ratkovic", "Johnson", "Lopez", "Wantchekon", "Morse"),
  state = c("NJ", "NJ", "NY", "NJ", "NJ", "NJ", "NY", "NJ", "NJ", "DC"),
  CD = c("12", "12", "12", "12", "12", "12", "9", "12", "12", "0"),
  county = c("021", "021", "061", "021", "021", "021", "061", "021", "021", "001"),
  tract = c("004000", "004501", "004800", "004501", "004501", "004000", "014900", "004501", "004501", "001301"),
  block = c("3001", "1025", "6001", "1025", "1025", "1025", "4000", "1025", "1025", "3005"),
  precinct = c("6", "", "", "", "", "", "", "", "", ""),
  age = c(29, 40, 33, 27, 28, 35, 25, 33, 50, 29),
  sex = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 1),
  party = c("Ind", "Dem", "Rep", "Dem", "Dem", "Ind", "Dem", "Rep", "Rep", "Rep"),
  PID = c("0", "1", "2", "1", "1", "0", "1", "2", "2", "2"),
  place = c("74000", "60900", "51000", "60900", "60900", "60900", "51000", "60900", "60900", "50000"),
  last = c("Khanna", "Imai", "Rivera", "Fifield", "Zhou", "Ratkovic", "Johnson", "Lopez", "Wantchekon", "Morse"),
  first = c("Kabir", "Kosuke", "Carlos", "Ben", "Yang-Yang", "Marc", "Frank", "Gabriel", "Leonard", "Julia")
)

usethis::use_data(voters, overwrite = TRUE)
