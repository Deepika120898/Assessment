suppressPackageStartupMessages({
  library(dplyr)
  library(gtsummary)
  library(gt)
  library(pharmaverseadam)
})

dir.create("question_3_tlg", showWarnings = FALSE, recursive = TRUE)

adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# Choose row variable:
#   "AESOC"  -> summary by system organ class
#   "AETERM" -> summary by preferred term

row_var <- "AESOC"

#denominator population from adsl

adsl_denoms <- adsl %>%
  filter(!is.na(ACTARM), !is.na(USUBJID)) %>%
  distinct(USUBJID, ACTARM)

# Prepare TEAE analysis dataset
# Count subjects with >=1 TEAE per row_var within treatment

teae_subj <- adae %>%
  filter(TRTEMFL == "Y", !is.na(USUBJID), !is.na(ACTARM)) %>%
  filter(!is.na(.data[[row_var]])) %>%
  distinct(USUBJID, ACTARM, .data[[row_var]])

#population alignment

teae_subj <- teae_subj %>%
  semi_join(adsl_denoms, by = c("USUBJID", "ACTARM"))

#gtsummary table creation

tbl_ae <- teae_subj %>%
  select(ACTARM, all_of(row_var)) %>%
  tbl_summary(
    by = ACTARM,
    statistic = all_categorical() ~ "{n} ({p}%)",
    percent = "column",
    missing = "no",
    sort = all_categorical() ~ "frequency",
    label = list(
      all_of(row_var) ~ ifelse(
        row_var == "AESOC",
        "Primary System Organ Class",
        "Preferred Term"
      )
    )
  ) %>%
  add_overall(
    last = TRUE,
    col_label = "**Total**<br>N = {style_number(N)}"
  ) %>%
  modify_header(
    label ~ ifelse(
      row_var == "AESOC",
      "**Primary System Organ Class**",
      "**Reported Term for the Adverse Event**"
    )
  ) %>%
  bold_labels() %>%
  modify_caption("**Summary of Treatment-Emergent Adverse Events (TEAEs)**")

#HTML Output

tbl_ae %>%
  as_gt() %>%
  gt::gtsave(filename = "question_3_tlg/ae_summary_table.html")

tbl_ae
