library(dplyr)
library(piggyback)
# dev version fixes problem with private repo
# install_github("ropensci/piggyback")

Sys.setenv("GITHUB_TOKEN"= Sys.getenv("GITHUB_PAT"))



#load("system.rda")

saveRDS(first_c, "wru-data-first_c.rds")
saveRDS(last_c, "wru-data-last_c.rds")
saveRDS(mid_c, "wru-data-mid_c.rds")
saveRDS(census_last_c, file="wru-data-census_last_c.rds")

pb_new_release(
  repo = "kosukeimai/wru",
  tag = "v2.0.0"
)

list.files(pattern = "wru-data") %>% 
  pb_upload(
    repo = "kosukeimai/wru", 
    tag = "v2.0.0"
  )

# validate/verify
pb_list(
  repo = "kosukeimai/wru", 
  tag = "v2.0.0"
)

#              file_name     size           timestamp    tag     owner    repo
# 1 wru-data-first_c.rds  9086951 2022-05-06 21:02:26 v0.0.1 solivella wruData
# 2  wru-data-last_c.rds 16083217 2022-05-06 21:06:19 v0.0.1 solivella wruData
# 3   wru-data-mid_c.rds 11261397 2022-05-06 21:08:51 v0.0.1 solivella wruData

