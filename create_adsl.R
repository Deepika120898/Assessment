install.packages("admiral")

library(admiral)
library(dplyr)
library(pharmaversesdtm)
library(lubridate)
library(stringr)

dm <- pharmaversesdtm::dm
ds <- pharmaversesdtm::ds
ex <- pharmaversesdtm::ex
ae <- pharmaversesdtm::ae
lb <- pharmaversesdtm::lb

dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)
lb <- convert_blanks_to_na(lb)

# DM domain is used as base

adsl <- dm %>%
  select(-DOMAIN)

#Derive AGEGR9 & AGEGR9N into categories

agegr9_lookup <- exprs(
  ~condition,              ~AGEGR9,    ~AGEGR9N,
  AGE < 18,                "<18",      1,
  between(AGE, 18, 50),    "18 - 50",  2,
  AGE > 50,                ">50",      3
)

adsl <- adsl %>%
  derive_vars_cat(definition = agegr9_lookup)
 
# creating TRTSDTM & TRTSTMF
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    highest_imputation = "h",
    time_imputation = "00:00:00",
    flag_imputation = "time",
    ignore_seconds_flag = TRUE
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    highest_imputation = "h",
    time_imputation = "00:00:00",
    flag_imputation = "none",
    ignore_seconds_flag = TRUE
  )

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    by_vars = exprs(STUDYID, USUBJID),
    filter_add = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first"
  ) %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    by_vars = exprs(STUDYID, USUBJID),
    filter_add = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last"
  )

#creating ITTFL

adsl <- adsl %>%
  mutate(
    ITTFL = if_else(!is.na(ARM), "Y", "N")
  )

#Creating LSTAVLDT 

library(dplyr)

# VS last alive date
vs_last <- vs %>%
  filter((!is.na(VSSTRESN) | !is.na(VSSTRESC)) & !is.na(VSDTC)) %>%
  mutate(VS_LSTDT = as.Date(substr(VSDTC, 1, 10))) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(VS_LSTDT = max(VS_LSTDT, na.rm = TRUE), .groups = "drop")

# AE last onset date
ae_last <- ae %>%
  filter(!is.na(AESTDTC)) %>%
  mutate(AE_LSTDT = as.Date(substr(AESTDTC, 1, 10))) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(
    AE_LSTDT = if (all(is.na(AE_LSTDT))) as.Date(NA) else max(AE_LSTDT, na.rm = TRUE),
    .groups = "drop"
  )

# DS last disposition date
ds_last <- ds %>%
  filter(!is.na(DSSTDTC)) %>%
  mutate(DS_LSTDT = as.Date(substr(DSSTDTC, 1, 10))) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(DS_LSTDT = max(DS_LSTDT, na.rm = TRUE), .groups = "drop")

# EX last valid treatment date
ex_last <- ex %>%
  filter(EXDOSE > 0 | (EXDOSE == 0 & grepl("PLACEBO", EXTRT, ignore.case = TRUE))) %>%
  filter(!is.na(EXENDTC)) %>%
  mutate(EX_LSTDT = as.Date(substr(EXENDTC, 1, 10))) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(EX_LSTDT = max(EX_LSTDT, na.rm = TRUE), .groups = "drop")

# Merge to ADSL and derive LSTALVDT
adsl <- adsl %>%
  left_join(vs_last, by = c("STUDYID", "USUBJID")) %>%
  left_join(ae_last, by = c("STUDYID", "USUBJID")) %>%
  left_join(ds_last, by = c("STUDYID", "USUBJID")) %>%
  left_join(ex_last, by = c("STUDYID", "USUBJID")) %>%
  rowwise() %>%
  mutate(
    LSTALVDT = max(c(VS_LSTDT, AE_LSTDT, DS_LSTDT, EX_LSTDT), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    LSTALVDT = ifelse(is.infinite(LSTALVDT), NA, LSTALVDT)
  ) %>%
  select(-VS_LSTDT, -AE_LSTDT, -DS_LSTDT, -EX_LSTDT)


