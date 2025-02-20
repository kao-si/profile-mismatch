
# Resume Audit Study

# Create resume submission schedule

library(tidyverse)

# Submission Schedule ####

# Create 1500 post ids for each region
# That is 4500 post ids and 9000 resume ids in total

# Create column "res_type"
# Randomly pair "balance" with either "intern" or "skill" within a job post

res_type <- character(length = 9000)

set.seed(1000)

for (i in seq(1, 8999, by = 2)) {
  res_type[c(i, i + 1)] <- sample(c("balance", sample(c("intern", "skill"), 1)))
}

# Create submission schedule
rand <- data.frame(
  res_id = 1:9000,
  post_id = rep(1:4500, each = 2),
  sub_order = rep(1:2, time = 4500),
  res_type = res_type
)

# Balance check
table(rand$res_type, rand$sub_order)

# Export to .csv Files ####

# rand1 <- filter(rand, post_id <= 1500)
# rand2 <- filter(rand, post_id >= 1501, post_id <= 3000)
# rand3 <- filter(rand, post_id >= 3001)

# readr::write_csv(rand, "rand.csv")
# readr::write_csv(rand1, "rand1.csv")
# readr::write_csv(rand2, "rand2.csv")
# readr::write_csv(rand3, "rand3.csv")
