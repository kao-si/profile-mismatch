
# Resume Audit Study

# Wrangle data for analysis

library(tidyverse)
library(lubridate)

# Import Raw Data ####

dat_res <- read_rds("Resume-Study_Data/res-level_raw.rds")

dat_post <- read_rds("Resume-Study_Data/post-level_raw.rds")

# Tidy Resume Level Data ####

# Exclude non-usable posts
dat_res <- dat_res %>%
filter(!str_detect(cmt, "error"))

# Replace empty strings with NA
dat_res <- dat_res %>%
mutate(across(where(is.character), ~ na_if(., "")))

# Recode response variables
dat_res <- dat_res %>%
mutate(
    rsp = case_when(
        is.na(rsp) ~ "no",
        TRUE ~ rsp
    ),
    rsp_call = case_when(
        is.na(rsp_call) ~ "no",
        TRUE ~ rsp_call
    ),
    rsp_wechat = case_when(
        is.na(rsp_wechat) ~ "no",
        TRUE ~ rsp_wechat
    )
)

# Construction of time variables
dat_res <- dat_res %>%
mutate(
    subtime2 = case_when(
        as.numeric(substr(subday, 1, 2)) < 9
        ~ ymd_hms(paste0("2025", subday, subtime, "00")),
        TRUE ~ ymd_hms(paste0("2024", subday, subtime, "00"))
    ),
    rsp_time2 = case_when(
        as.numeric(substr(rsp_day, 1, 2)) < 9
        ~ ymd_hms(paste0("2025", rsp_day, rsp_time, "00")),
        TRUE ~ ymd_hms(paste0("2024", rsp_day, rsp_time, "00"))
    ),
    sub_year = year(subtime2),
    sub_month = month(subtime2, label = TRUE),
    sub_wday = wday(subtime2, label = TRUE),
    sub_hour = hour(subtime2),
    rsp_year = year(rsp_time2),
    rsp_month = month(rsp_time2, label = TRUE),
    rsp_wday = wday(rsp_time2, label = TRUE),
    rsp_hour = hour(rsp_time2)
)

# Tidy Post Level Data ####

# Replace empty strings with NA
dat_post <- dat_post %>%
mutate(across(where(is.character), ~ na_if(., "")))

# Join Resume and Post Level Data ####

# Rename variables in resume level data
dat_res <- dat_res %>%
rename(
    res_ptitle = ptitle,
    res_fname = fname,
    res_rname = rname
)

# Remove and rename variables in post level data
dat_post <- dat_post %>%
select(-c(pcat, pcity)) %>%
rename(
    post_ptitle = ptitle,
    post_fname = fname,
    post_rname = rname
)

# Join data
dat <- dat_res %>%
left_join(
    dat_post,
    by = "post_id",
    relationship = "many-to-one"
)

# Fill missing values in post level data from resume level data
dat <- dat %>%
mutate(
    post_ptitle = coalesce(post_ptitle, res_ptitle),
    post_fname = coalesce(post_fname, res_fname),
    post_rname = coalesce(post_rname, res_rname)
)

# Correct errors in recruiter gender identification
dat <- dat %>%
mutate(
    rmale = case_when(
        str_detect(post_rname, "先生") & (!rmale == "yes" | is.na(rmale))
        ~ "yes",
        str_detect(post_rname, "女士") & (!rmale == "no" | is.na(rmale))
        ~ "no",
        TRUE ~ rmale
    )
)
