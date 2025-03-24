
# Resume Audit Study

# Import raw data from data input scripts to .rds files

source("Resume-Study_02_Input-Functions.R")

source("Resume-Study_Data/Data Input Scripts/res-level_part1_gd.R")

source("Resume-Study_Data/Data Input Scripts/res-level_part2_bj.R")

source("Resume-Study_Data/Data Input Scripts/res-level_part3_sh.R")

source("Resume-Study_Data/Data Input Scripts/post-level_part1_gd.R")

source("Resume-Study_Data/Data Input Scripts/post-level_part2_bj.R")

source("Resume-Study_Data/Data Input Scripts/post-level_part3_sh.R")

write_rds(dat_res, "Resume-Study_Data/res-level_raw.rds")

write_rds(dat_post, "Resume-Study_Data/post-level_raw.rds")

rm(list = ls())
