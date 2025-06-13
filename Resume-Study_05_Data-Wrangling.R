
# Resume Audit Study

# Wrangle data for analysis

library(tidyverse)
library(lubridate)

# Import Raw Data ####

dat_res <- read_rds("Resume-Study_Data/res-level_raw.rds")
# version: 2025-04-07 20:53

dat_post <- read_rds("Resume-Study_Data/post-level_raw.rds")
# version: 2025-04-07 20:53

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

# Further Processing ####

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

# Fill blank values in rmale
# Gender identification done by three RAs independently

# Load the coding file
c_rmale <- readxl::read_xlsx("Resume-Study_Data/rmale_na.xlsx") %>%
mutate(
    c_rmale = case_when(
        c_rmale == 1 ~ "yes",
        c_rmale == 0 ~ "no"
    )
)

# Join the coding file with the main data
dat <- dat %>%
left_join(
    c_rmale[c("post_id", "c_rmale")],
    by = "post_id",
    relationship = "many-to-one"
) %>%
mutate(
    rmale = coalesce(rmale, c_rmale),
    c_rmale = NULL
)

# Response category
# Each categorization was confirmed by two RAs

# Load the coding file
c_rsp_cat <- readxl::read_xlsx("Resume-Study_Data/rsp_cat.xlsx") %>%
mutate(
    c_rsp_cat = case_when(
        c_rsp_cat %in% c("dc", "nc") ~ "no_outcome",
        TRUE ~ c_rsp_cat
    )
)

# Join the coding file with the main data
dat <- dat %>%
left_join(
    c_rsp_cat[c("res_id", "c_rsp_cat")],
    by = "res_id",
    relationship = "one-to-one"
) %>%
mutate(
    rsp_cat = case_when(
        rsp == "no" ~ "no_response",
        rsp == "yes" ~ c_rsp_cat
    ),
    c_rsp_cat = NULL
)

# Create a set of outcome variables
# rsp: "yes" means receiving any response
dat <- dat %>%
mutate(
    # rsp2: "yes" means receiving any response but refusal
    rsp2 = case_when(
        rsp_cat %in% c("no_response", "refusal") ~ "no",
        TRUE ~ "yes"
    ),
    # rsp3: "yes" means receiving request for contact or invitation
    rsp3 = case_when(
        rsp_cat %in% c("no_response", "refusal", "no_outcome") ~ "no",
        TRUE ~ "yes"
    ),
    # rsp4: "yes" means receiving invitation
    rsp4 = case_when(
        rsp_cat == "invitation" ~ "yes",
        TRUE ~ "no"
    )
)

# Correct for some random errors
dat$psal_mon[dat$psal_mon == 122] <- 12 # n = 1
dat$fpostno[dat$fpostno == 0] <- NA # post_id == 2525
dat$fhrno[dat$fhrno == 0] <- NA # post_id == 2525
dat$falbum[dat$falbum == "NO"] <- "no" # n = 1
dat$post_rname[dat$post_rname == "珊瑚跨境"] <- NA # post_id == 668

# Drop and rename variables
dat <- dat %>%
mutate(
    subday = NULL,
    subtime = NULL,
    rsp_day = NULL,
    rsp_time = NULL,
    res_ptitle = NULL,
    res_fname = NULL,
    res_rname = NULL,
) %>%
rename(
    sub_time = subtime2,
    rsp_time = rsp_time2,
    ptitle = post_ptitle,
    fname = post_fname,
    rname = post_rname,
)

# Create a dummy variable for submissions that involve change of post
dat <- dat %>%
mutate(
    post_change = case_when(
        str_detect(post_id, "_") ~ "yes",
        TRUE ~ "no"
    ),
    # Keep the origional post_id variable
    post_id_orig = post_id,
    # Remove the suffix of post_id
    post_id = str_remove(post_id, "_.*")
)

# Recode res_type
dat <- dat %>%
mutate(
    res_type2 = case_when(
        res_type == "balance" ~ "balance",
        TRUE ~ "unbalance"
    )
)

# Factorize sub_order
dat$sub_order <- factor(dat$sub_order)

# Create a dummy variable for profile picture used in the resumes
dat <- dat %>%
mutate(
    res_pic = case_when(
        preg == "beijing" & res_type2 == "balance" ~ "wwj",
        preg == "beijing" & res_type2 == "unbalance" ~ "lzy",
        preg == "guangdong" & res_type2 == "balance" ~ "lzy",
        preg == "guangdong" & res_type2 == "unbalance" ~ "wwj",
        preg == "shanghai" & res_type2 == "balance" ~ "lzy",
        preg == "shanghai" & res_type2 == "unbalance" ~ "wwj"
    )
)

# Transform continuous variables to discrete variables
dat <- dat %>%
mutate(
    psal_lower_d = cut_number(psal_lower, n = 3,
    labels = c("low", "medium", "high")),
    psal_upper_d = cut_number(psal_upper, n = 3,
    labels = c("low", "medium", "high")),
    pcandno_d = cut_number(pcandno, n = 3,
    labels = c("low", "medium", "high")),
    fpostno_d = cut_number(fpostno, n = 3,
    labels = c("low", "medium", "high")),
    fhrno_d = cut_interval(fhrno, n = 3,
    labels = c("low", "medium", "high")),
    rpostno_d = cut_number(rpostno, n = 3,
    labels = c("low", "medium", "high"))
)
