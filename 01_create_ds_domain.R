
library(sdtm.oak)
library(pharmaverseraw)

study_ct <- read.csv("/cloud/project/sdtm_ct.csv")

ds_raw <- pharmaverseraw::ds_raw

ds_raw <- admiral::convert_blanks_to_na(ds_raw)

#derive oak_id_vars
ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
                       )

# creating DS Domain 

#Mapping IT.DSSTDAT to DSSTDTC in ISO8601 Format

ds <- assign_datetime(
  raw_dat = ds_raw,
  raw_var = "IT.DSSTDAT" ,
  tgt_var = "DSSTDTC" ,
  raw_fmt = list(c("m-d-y", "mm-dd-yyyy")),
)

#Mapping null to DSDECOD

ds_raw$DSDECOD_SRC <- ifelse(
  is.na(ds_raw$OTHERSP),
  ds_raw$`IT.DSDECOD`,
  NA
)

ds <- ds %>%
assign_no_ct(
  raw_dat = ds_raw,
  raw_var = "DSDECOD_SRC",
  tgt_var = "DSDECOD"
)

#Mapping DSCAT 

ds_raw$DSCAT_SRC <- ifelse(
  ds_raw$`IT.DSDECOD` == "Randomized",
  "PROTOCOL MILESTONE",
  "DISPOSITION EVENT"
)

ds <- ds %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "DSCAT_SRC",
    tgt_var = "DSCAT",
    id_vars = oak_id_vars()
  )

#Mapping DSTERM

ds <- ds %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "OTHERSP",
    tgt_var = "DSDECOD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "OTHERSP",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  )

# Mapping DSCAT

ds <- ds %>%
  left_join(
    ds_raw %>% select(oak_id, OTHERSP),
    by = "oak_id"
  ) %>%
  mutate(
    DSCAT = ifelse(!is.na(OTHERSP), "OTHER EVENT", DSCAT)
  ) %>%
  select(-OTHERSP)

# Mapping DSTERM

ds <- ds %>%
  left_join(
    ds_raw %>% select(oak_id, OTHERSP, `IT.DSTERM`),
    by = "oak_id"
  ) %>%
  mutate(
    DSTERM = ifelse(is.na(OTHERSP), `IT.DSTERM`, DSTERM)
  ) %>%
  select(-OTHERSP, -`IT.DSTERM`)

# Mapping DSDTCOL & DSTMCOL to DSDTC in ISO8601 Format

ds <- ds %>% assign_datetime(
  raw_dat = ds_raw,
  raw_var = c("DSDTCOL", "DSTMCOL"),
  tgt_var = "DSDTC",
  raw_fmt = c("m-d-y", "H:M")
)
  
ds <- ds %>%
  left_join(
    ds_raw %>% select(oak_id, STUDY, PATNUM),
    by = "oak_id"
  ) %>%
  mutate(
    STUDYID = STUDY,
    DOMAIN = "DS",
    USUBJID = paste0(STUDYID, "-", PATNUM)
  ) %>%
  select(-STUDY, -PATNUM)
ds <- derive_seq(tgt_var = "DSSEQ",
           tgt_dat = ds,
           rec_vars = c("DSSTDTC", "DSDTC"))

#VISIT & VISITNUM
library(stringr)
ds <- ds %>%
  left_join(
    ds_raw %>% select(oak_id, INSTANCE),
    by = "oak_id"
  ) %>%
  mutate(
    VISIT = INSTANCE,
    VISITNUM = case_when(
      INSTANCE == "Baseline" ~ 0,
      TRUE ~ as.numeric(str_extract(INSTANCE, "\\d+"))
    )
  ) %>%
  select(-INSTANCE)
  
ds <- ds %>%
  mutate(DSSTDY = NA_real_)

ds <- ds %>% select("STUDYID", "DOMAIN", "USUBJID", "DSSEQ",
              "DSTERM", "DSDECOD", "DSCAT","VISITNUM", "VISIT", "DSDTC", "DSSTDTC", "DSSTDY")

  
  
  
