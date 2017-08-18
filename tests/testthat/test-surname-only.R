# Set random seed
set.seed(12345)

data(voters)
predict_race(voter.file = voters, surname.only = T)
