# NHANES Arsenic-Folate-Depression/Cognition Project
# Author: Parinika Tusharbhai Joshi

# Data Exploration

install.packages("nhanesA")
install.packages("survey")
library(nhanesA)
library(dplyr)
library(survey)
library(ggplot2)

cycles <- c("2013-2014", "2017-2018", "2019-2020")
print("=== Checking available tables ===")

cycle_info <- data.frame(
  cycle = c("2013-2014", "2017-2018", "2019-2020"),
  suffix = c("H", "J", "K"),
  stringsAsFactors = FALSE
)


print("=== NHANES Cycle Information ===")
print(cycle_info)

cat("\n=== Attempting to load key tables ===\n")

safe_load <- function(table_name) {
  tryCatch({
    data <- nhanes(table_name)
    return(list(success = TRUE, nrow = nrow(data), ncol = ncol(data)))
  }, error = function(e) {
    return(list(success = FALSE, error = as.character(e)))
  })
}

results <- list()
for (i in 1:nrow(cycle_info)) {
  cycle <- cycle_info$cycle[i]
  suffix <- cycle_info$suffix[i]
  
  cat("\n", cycle, "(suffix:", suffix, ")\n", sep = "")
  cat(rep("=", 50), "\n", sep = "")
  
  tables_to_try <- c("DEMO", "DPQ", "CFQ", "UAS", "FOLATE")
  
  for (tbl in tables_to_try) {
    tbl_name <- paste0(tbl, "_", suffix)
    cat(sprintf("%-15s", tbl_name), "... ")
    
    result <- safe_load(tbl_name)
    
    if (result$success) {
      cat("SUCCESS - ", result$nrow, " rows, ", result$ncol, " columns\n", sep = "")
      results[[tbl_name]] <- result
    } else {
      cat("FAILED\n")
    }
  }
}

cat("\n=== LOADING 2017-2018 DATA ===\n\n")

cat("1. DEMOGRAPHICS (DEMO_J)\n")
cat(rep("-", 60), "\n", sep = "")
demo_j <- nhanes("DEMO_J")
cat("Rows:", nrow(demo_j), "| Columns:", ncol(demo_j), "\n")
cat("\nAll column names:\n")
print(names(demo_j))

cat("\n\n2. DEPRESSION (DPQ_J)\n")
cat(rep("-", 60), "\n", sep = "")
dpq_j <- nhanes("DPQ_J")
cat("Rows:", nrow(dpq_j), "| Columns:", ncol(dpq_j), "\n")
cat("\nAll column names:\n")
print(names(dpq_j))

cat("\n\n3. URINARY ARSENIC (UAS_J)\n")
cat(rep("-", 60), "\n", sep = "")
uas_j <- nhanes("UAS_J")
cat("Rows:", nrow(uas_j), "| Columns:", ncol(uas_j), "\n")
cat("\nAll column names:\n")
print(names(uas_j))

cat("\n\n4. FOLATE (FOLATE_J)\n")
cat(rep("-", 60), "\n", sep = "")
folate_j <- nhanes("FOLATE_J")
cat("Rows:", nrow(folate_j), "| Columns:", ncol(folate_j), "\n")
cat("\nAll column names:\n")
print(names(folate_j))

cat("\n\n5. COGNITIVE FUNCTION (CFQ_H) - 2013-2014 ONLY\n")
cat(rep("-", 60), "\n", sep = "")
cfq_h <- nhanes("CFQ_H")
cat("Rows:", nrow(cfq_h), "| Columns:", ncol(cfq_h), "\n")
cat("\nAll column names:\n")
print(names(cfq_h))

cat("\n\n6. LOADING 2013-2014 DATA FOR COMPARISON\n")
cat(rep("-", 60), "\n", sep = "")

demo_h <- nhanes("DEMO_H")
cat("DEMO_H - Rows:", nrow(demo_h), "\n")

uas_h <- nhanes("UAS_H")
cat("UAS_H - Rows:", nrow(uas_h), "\n")
cat("UAS_H columns:\n")
print(names(uas_h))

folate_h <- nhanes("FOLATE_H")
cat("\nFOLATE_H - Rows:", nrow(folate_h), "\n")
cat("FOLATE_H columns:\n")
print(names(folate_h))

dpq_h <- nhanes("DPQ_H")
cat("\nDPQ_H - Rows:", nrow(dpq_h), "\n")

cat("\n\n", rep("=", 70), "\n", sep = "")
cat("DETAILED VARIABLE EXAMINATION\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("1. ARSENIC VARIABLES DETAILS (UAS_J):\n")
cat("   Total arsenic variables:\n")
print(names(uas_j))
cat("\n   Summary statistics for key arsenic species:\n")
cat("   URXUAS3 (Arsenous acid, As³⁺):\n")
print(summary(uas_j$URXUAS3))
cat("\n   URXUAS5 (Arsenic acid, As⁵⁺):\n")
print(summary(uas_j$URXUAS5))
cat("\n   URXUAB (Arsenobetaine - seafood):\n")
print(summary(uas_j$URXUAB))
cat("\n   URXUDMA (Dimethylarsonic acid):\n")
print(summary(uas_j$URXUDMA))
cat("\n   URXUMMA (Monomethylarsonic acid):\n")
print(summary(uas_j$URXUMMA))

cat("\n\n2. FOLATE VARIABLES:\n")
cat("   2017-2018 (FOLATE_J):\n")
print(names(folate_j))
cat("\n   LBDRFO summary:\n")
print(summary(folate_j$LBDRFO))
cat("\n   2013-2014 (FOLATE_H):\n")
print(names(folate_h))

cat("\n   Searching for RBC folate and 5-MTHF in other tables...\n")
cat("   Trying BIOPRO (biochemistry profile)...\n")
tryCatch({
  biopro_j <- nhanes("BIOPRO_J")
  cat("   BIOPRO_J exists with", ncol(biopro_j), "columns\n")
}, error = function(e) {
  cat("   BIOPRO_J not found\n")
})

cat("   Trying RBC folate table...\n")
tryCatch({
  rbcfol_j <- nhanes("FOLATE_J")
  folate_vars_all <- names(folate_j)
  cat("   Available in FOLATE_J:", paste(folate_vars_all, collapse = ", "), "\n")
}, error = function(e) {
  cat("   No additional folate table\n")
})

cat("\n\n3. PHQ-9 DEPRESSION ITEMS (DPQ_J):\n")
cat("   Variables:", paste(names(dpq_j)[-1], collapse = ", "), "\n")
cat("\n   First 10 rows of PHQ-9 data:\n")
print(head(dpq_j, 10))
cat("\n   Value ranges for each item:\n")
for (item in paste0("DPQ0", c(10, 20, 30, 40, 50, 60, 70, 80, 90))) {
  cat("   ", item, ": ", sep = "")
  print(table(dpq_j[[item]], useNA = "ifany"))
}

cat("\n\n4. SURVEY WEIGHT VARIABLES:\n")
cat("   In DEMO_J:\n")
cat("   - WTINT2YR (interview weight)\n")
cat("   - WTMEC2YR (MEC exam weight)\n")
cat("   In UAS_J:\n")
cat("   - WTSA2YR (subsample A weight - for arsenic)\n")
cat("   In FOLATE_J:\n")  
cat("   - WTFOL2YR (folate subsample weight)\n")
cat("\n   We need to use WTSA2YR for arsenic analysis (environmental subsample)\n")

cat("\n\n5. KEY DEMOGRAPHICS:\n")
cat("   Age: RIDAGEYR - range:", min(demo_j$RIDAGEYR, na.rm=T), "to", 
    max(demo_j$RIDAGEYR, na.rm=T), "\n")
cat("   Sex: RIAGENDR - ", table(demo_j$RIAGENDR), "\n")
cat("   Race: RIDRETH3 - categories:", unique(demo_j$RIDRETH3), "\n")
cat("   Poverty ratio: INDFMPIR - range:", 
    round(min(demo_j$INDFMPIR, na.rm=T), 2), "to", 
    round(max(demo_j$INDFMPIR, na.rm=T), 2), "\n")

cat("\n\n", rep("=", 70), "\n", sep = "")
cat("Variable Creation\n")
cat(rep("=", 70), "\n", sep = "")

cat("\n=== SEARCHING FOR RBC FOLATE ===\n\n")
cat("1. Trying CBC table (Complete Blood Count):\n")
tryCatch({
  cbc_j <- nhanes("CBC_J")
  cat("   CBC_J loaded -", nrow(cbc_j), "rows,", ncol(cbc_j), "columns\n")
  folate_in_cbc <- grep("FOL|RBC", names(cbc_j), value = TRUE, ignore.case = TRUE)
  if (length(folate_in_cbc) > 0) {
    cat("   Folate variables found:", paste(folate_in_cbc, collapse = ", "), "\n")
  } else {
    cat("   No folate variables in CBC\n")
  }
}, error = function(e) {
  cat("   CBC_J not found\n")
})

cat("\n2. Trying VITB (B-vitamins) table:\n")
tryCatch({
  vitb_j <- nhanes("VITB_J")
  cat("   VITB_J loaded -", nrow(vitb_j), "rows,", ncol(vitb_j), "columns\n")
  cat("   Variables:\n")
  print(names(vitb_j))
}, error = function(e) {
  cat("   VITB_J not found\n")
})

cat("\n3. Trying FOLATE variants:\n")
for (suffix in c("", "A", "B", "C")) {
  table_name <- paste0("FOLATE", suffix, "_J")
  tryCatch({
    temp <- nhanes(table_name)
    cat("   ", table_name, " exists -", ncol(temp), "columns:", 
        paste(names(temp), collapse = ", "), "\n")
  }, error = function(e) {
  })
}

cat("\n4. Examining BIOPRO_J (Biochemistry Profile):\n")
biopro_j <- nhanes("BIOPRO_J")
cat("   BIOPRO_J columns:\n")
print(names(biopro_j))
cat("\n   Searching for folate/B-vitamin related variables in BIOPRO:\n")
bvit_vars <- grep("FOL|B12|HOMO|METH", names(biopro_j), value = TRUE, ignore.case = TRUE)
if (length(bvit_vars) > 0) {
  cat("   Found:", paste(bvit_vars, collapse = ", "), "\n")
} else {
  cat("   No B-vitamin variables found\n")
}

cat("\n5. Checking 2013-2014 for additional folate variables:\n")
cat("   CBC_H:\n")
tryCatch({
  cbc_h <- nhanes("CBC_H")
  cat("   Loaded -", nrow(cbc_h), "rows\n")
  folate_vars_h <- grep("FOL|RBC", names(cbc_h), value = TRUE, ignore.case = TRUE)
  if (length(folate_vars_h) > 0) {
    cat("   Folate variables:", paste(folate_vars_h, collapse = ", "), "\n")
  }
}, error = function(e) {
  cat("   CBC_H not found\n")
})

cat("\n6. Alternative: Check if LBDRFO is serum or RBC:\n")
cat("   LBDRFO (FOLATE_J) summary:\n")
print(summary(folate_j$LBDRFO))
cat("\n   LBDRFOSI (FOLATE_J) summary:\n")
print(summary(folate_j$LBDRFOSI))
cat("\n   Note: Based on NHANES documentation:\n")
cat("   - LBDRFO is typically RBC Folate (nmol/L)\n")
cat("   - LBDRFOSI is SI units conversion\n")
cat("   - Serum folate is usually in a separate variable (LBXSFO)\n")

cat("\n7. Searching all 2017-2018 laboratory tables for folate:\n")
tryCatch({
  all_tables_j <- nhanesTables(data_group = 'LAB', year = 2017)
  folate_tables <- all_tables_j[grep("FOL|B12|VIT", all_tables_j$Data.File.Name, ignore.case = TRUE), ]
  if (nrow(folate_tables) > 0) {
    cat("   Potential folate/B-vitamin tables:\n")
    print(folate_tables[, c("Data.File.Name", "Data.File.Description")])
  }
}, error = function(e) {
  cat("   Could not search tables\n")
})

cat("\n=== LOADING ADDITIONAL FOLATE DATA ===\n\n")

cat("1. RBC FOLATE (from CBC_J):\n")
cbc_j <- nhanes("CBC_J")
cat("   Rows:", nrow(cbc_j), "\n")
cat("   RBC folate variable: LBXRBCSI\n")
cat("   Summary:\n")
print(summary(cbc_j$LBXRBCSI))

cat("\n2. SERUM FOLATE FORMS (FOLFMS_J):\n")
folfms_j <- nhanes("FOLFMS_J")
cat("   Rows:", nrow(folfms_j), "\n")
cat("   Columns:\n")
print(names(folfms_j))
cat("\n   Summary of key variables:\n")
for (var in names(folfms_j)[-1]) {
  if (is.numeric(folfms_j[[var]])) {
    cat("\n   ", var, ":\n")
    print(summary(folfms_j[[var]]))
  }
}

cat("\n\n3. LOADING 2013-2014 FOLATE DATA:\n")
cat("   CBC_H (RBC folate):\n")
cbc_h <- nhanes("CBC_H")
cat("   Rows:", nrow(cbc_h), "\n")
cat("   LBXRBCSI summary:\n")
print(summary(cbc_h$LBXRBCSI))

cat("\n   FOLFMS_H (Serum folate forms):\n")
tryCatch({
  folfms_h <- nhanes("FOLFMS_H")
  cat("   Rows:", nrow(folfms_h), "\n")
  cat("   Columns:\n")
  print(names(folfms_h))
}, error = function(e) {
  cat("   FOLFMS_H not found\n")
})

cat("\n=== IDENTIFYING FOLATE FORMS ===\n\n")
cat("FOLFMS_J/H Variables (Serum Folate Forms):\n")
cat("Based on NHANES documentation, these represent:\n")
cat("  • LBDFOTSI/LBDFOT: Total serum folate (nmol/L)\n")
cat("  • LBXSF1SI: 5-Methyl-tetrahydrofolate (5-MTHF) - PRIMARY BIOACTIVE FORM\n")
cat("  • LBXSF2SI: Pteroylglutamic acid (folic acid - supplement form)\n")
cat("  • LBXSF3SI: 5-Formyl-tetrahydrofolate (folinic acid)\n")
cat("  • LBXSF4SI: Tetrahydrofolate (THF)\n")
cat("  • LBXSF5SI: 5,10-Methenyl-tetrahydrofolate (MeFox)\n")
cat("  • LBXSF6SI: Non-methyl folate\n\n")
cat("Available Folate Measures for Analysis:\n")
cat("  1. RBC Folate (LBXRBCSI from CBC): Long-term status (3-month window)\n")
cat("  2. Total Serum Folate (LBDFOTSI from FOLFMS): Current status\n")
cat("  3. Serum 5-MTHF (LBXSF1SI from FOLFMS): Bioactive form\n")
cat("  4. RBC Folate (LBDRFO from FOLATE): Alternative measure\n\n")
cat("approach:\n")
cat("  PRIMARY: Use LBXRBCSI (RBC folate from CBC) - best long-term marker\n")
cat("  SECONDARY: Use LBXSF1SI (5-MTHF) - mechanistically relevant\n")
cat("  TERTIARY: Use LBDFOTSI (total serum folate) - comprehensive\n\n")

cat("Checking data overlap:\n")
cat("  LBXRBCSI (CBC_J): N =", sum(!is.na(cbc_j$LBXRBCSI)), "\n")
cat("  LBXSF1SI (FOLFMS_J): N =", sum(!is.na(folfms_j$LBXSF1SI)), "\n")
cat("  LBDFOTSI (FOLFMS_J): N =", sum(!is.na(folfms_j$LBDFOTSI)), "\n")

cat("\n\n", rep("=", 70), "\n", sep = "")
cat("STEP 2: CREATING ANALYTICAL DATASET\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("2A. RECODING PHQ-9 ITEMS\n")
cat(rep("-", 70), "\n", sep = "")

recode_phq9 <- function(x) {
  case_when(
    x == "Not at all" ~ 0,
    x == "Several days" ~ 1,
    x == "More than half the days" ~ 2,
    x == "Nearly every day" ~ 3,
    x %in% c("Refused", "Don't know") ~ NA_real_,
    TRUE ~ NA_real_
  )
}

cat("\nRecoding DPQ_J (2017-2018)...\n")
dpq_j_clean <- dpq_j %>%
  mutate(
    across(starts_with("DPQ0") & !DPQ100, recode_phq9, .names = "{.col}_num")
  ) %>%
  rowwise() %>%
  mutate(
    PHQ9_total = sum(c_across(ends_with("_num")), na.rm = FALSE),
    PHQ9_missing = sum(is.na(c_across(ends_with("_num")))),
    PHQ9_valid = PHQ9_missing == 0,
    Depression_binary = ifelse(PHQ9_valid & PHQ9_total >= 10, 1, 
                               ifelse(PHQ9_valid, 0, NA))
  ) %>%
  ungroup() %>%
  select(SEQN, starts_with("DPQ0"), PHQ9_total, PHQ9_missing, 
         PHQ9_valid, Depression_binary)

cat("Summary of PHQ-9 total score:\n")
print(summary(dpq_j_clean$PHQ9_total))
cat("\nDepression prevalence (PHQ-9 >= 10):\n")
print(table(dpq_j_clean$Depression_binary, useNA = "ifany"))

cat("\nRecoding DPQ_H (2013-2014)...\n")
dpq_h_clean <- dpq_h %>%
  mutate(
    across(starts_with("DPQ0") & !DPQ100, recode_phq9, .names = "{.col}_num")
  ) %>%
  rowwise() %>%
  mutate(
    PHQ9_total = sum(c_across(ends_with("_num")), na.rm = FALSE),
    PHQ9_missing = sum(is.na(c_across(ends_with("_num")))),
    PHQ9_valid = PHQ9_missing == 0,
    Depression_binary = ifelse(PHQ9_valid & PHQ9_total >= 10, 1, 
                               ifelse(PHQ9_valid, 0, NA))
  ) %>%
  ungroup() %>%
  select(SEQN, starts_with("DPQ0"), PHQ9_total, PHQ9_missing, 
         PHQ9_valid, Depression_binary)

cat("Summary of PHQ-9 total score:\n")
print(summary(dpq_h_clean$PHQ9_total))

cat("\n\n2B. CREATING ARSENIC VARIABLES\n")
cat(rep("-", 70), "\n", sep = "")

cat("\nProcessing UAS_J (2017-2018)...\n")
uas_j_clean <- uas_j %>%
  mutate(
    iAs = URXUAS3 + URXUAS5,
    iAs_log = log(iAs + 0.01),
    AsB = URXUAB,
    AsB_log = log(AsB + 0.01),
    Total_As = iAs + URXUAB + URXUDMA + URXUMMA
  ) %>%
  select(SEQN, WTSA2YR, URXUAS3, URXUAS5, URXUAB, URXUDMA, URXUMMA,
         iAs, iAs_log, AsB, AsB_log, Total_As)

cat("Summary of inorganic arsenic (iAs):\n")
print(summary(uas_j_clean$iAs))
cat("\nSummary of arsenobetaine:\n")
print(summary(uas_j_clean$AsB))

cat("\nProcessing UAS_H (2013-2014)...\n")
uas_h_clean <- uas_h %>%
  mutate(
    iAs = URXUAS3 + URXUAS5,
    iAs_log = log(iAs + 0.01),
    AsB = URXUAB,
    AsB_log = log(AsB + 0.01),
    Total_As = iAs + URXUAB + URXUDMA + URXUMMA,
    Creatinine = URXUCR,
    iAs_creat_ratio = (iAs / Creatinine) * 100
  ) %>%
  select(SEQN, WTSA2YR, URXUAS3, URXUAS5, URXUAB, URXUDMA, URXUMMA,
         URXUCR, iAs, iAs_log, AsB, AsB_log, Total_As, 
         Creatinine, iAs_creat_ratio)

cat("Summary of iAs:\n")
print(summary(uas_h_clean$iAs))
cat("\nSummary of creatinine-adjusted iAs:\n")
print(summary(uas_h_clean$iAs_creat_ratio))

cat("\n\n2C. MERGING DATASETS WITHIN EACH CYCLE\n")
cat(rep("-", 70), "\n", sep = "")

cat("\nMerging 2017-2018 datasets...\n")
nhanes_2017_2018 <- demo_j %>%
  select(SEQN, SDDSRVYR, RIDAGEYR, RIAGENDR, RIDRETH3, INDFMPIR, 
         DMDEDUC2, SDMVPSU, SDMVSTRA, WTINT2YR, WTMEC2YR) %>%
  left_join(uas_j_clean, by = "SEQN") %>%
  left_join(dpq_j_clean %>% select(SEQN, PHQ9_total, Depression_binary), 
            by = "SEQN") %>%
  left_join(cbc_j %>% select(SEQN, LBXRBCSI), by = "SEQN") %>%
  left_join(folfms_j %>% select(SEQN, LBDFOTSI, LBXSF1SI), by = "SEQN") %>%
  mutate(Cycle = "2017-2018")

cat("2017-2018 merged dataset:\n")
cat("  Total rows:", nrow(nhanes_2017_2018), "\n")
cat("  With arsenic data:", sum(!is.na(nhanes_2017_2018$iAs)), "\n")
cat("  With PHQ-9 data:", sum(!is.na(nhanes_2017_2018$PHQ9_total)), "\n")
cat("  With RBC folate:", sum(!is.na(nhanes_2017_2018$LBXRBCSI)), "\n")
cat("  With 5-MTHF:", sum(!is.na(nhanes_2017_2018$LBXSF1SI)), "\n")

cat("\nMerging 2013-2014 datasets...\n")
nhanes_2013_2014 <- demo_h %>%
  select(SEQN, SDDSRVYR, RIDAGEYR, RIAGENDR, RIDRETH3, INDFMPIR, 
         DMDEDUC2, SDMVPSU, SDMVSTRA, WTINT2YR, WTMEC2YR) %>%
  left_join(uas_h_clean, by = "SEQN") %>%
  left_join(dpq_h_clean %>% select(SEQN, PHQ9_total, Depression_binary), 
            by = "SEQN") %>%
  left_join(cbc_h %>% select(SEQN, LBXRBCSI), by = "SEQN") %>%
  left_join(folfms_h %>% select(SEQN, LBDFOTSI, LBXSF1SI), by = "SEQN") %>%
  left_join(cfq_h %>% select(SEQN, CFDCIR, CFDAST, CFDDS), by = "SEQN") %>%
  mutate(Cycle = "2013-2014")

cat("2013-2014 merged dataset:\n")
cat("  Total rows:", nrow(nhanes_2013_2014), "\n")
cat("  With arsenic data:", sum(!is.na(nhanes_2013_2014$iAs)), "\n")
cat("  With PHQ-9 data:", sum(!is.na(nhanes_2013_2014$PHQ9_total)), "\n")
cat("  With RBC folate:", sum(!is.na(nhanes_2013_2014$LBXRBCSI)), "\n")
cat("  With 5-MTHF:", sum(!is.na(nhanes_2013_2014$LBXSF1SI)), "\n")
cat("  With cognitive data:", sum(!is.na(nhanes_2013_2014$CFDCIR)), "\n")

cat("\n\n2D. ADJUSTING SURVEY WEIGHTS FOR COMBINED CYCLES\n")
cat(rep("-", 70), "\n", sep = "")

cat("\nWeight adjustment formula for 2017-2018:\n")
cat("  New_weight = WTSA2YR / 2\n")
cat("  (Dividing by 2 to account for 2 cycles being combined)\n\n")
cat("Weight adjustment formula for 2013-2014:\n")
cat("  New_weight = WTSA2YR / 2\n")
cat("  (Same adjustment for consistency)\n\n")

nhanes_2017_2018 <- nhanes_2017_2018 %>%
  mutate(WTCOMBINED = WTSA2YR / 2)

nhanes_2013_2014 <- nhanes_2013_2014 %>%
  mutate(WTCOMBINED = WTSA2YR / 2)

cat("Adjusted weight summary (2017-2018):\n")
print(summary(nhanes_2017_2018$WTCOMBINED))
cat("\nAdjusted weight summary (2013-2014):\n")
print(summary(nhanes_2013_2014$WTCOMBINED))

cat("\n\n2E. COMBINING 2013-2014 AND 2017-2018 CYCLES\n")
cat(rep("-", 70), "\n", sep = "")

nhanes_combined <- bind_rows(nhanes_2013_2014, nhanes_2017_2018)

cat("Combined dataset:\n")
cat("  Total rows:", nrow(nhanes_combined), "\n")
cat("  2013-2014:", sum(nhanes_combined$Cycle == "2013-2014"), "\n")
cat("  2017-2018:", sum(nhanes_combined$Cycle == "2017-2018"), "\n")

cat("\n\n2F. CREATING FOLATE QUARTILES\n")
cat(rep("-", 70), "\n", sep = "")

cat("\nCreating quartiles for each folate measure...\n")
nhanes_combined <- nhanes_combined %>%
  mutate(
    RBC_folate_quart = ntile(LBXRBCSI, 4),
    MTHF_quart = ntile(LBXSF1SI, 4),
    Serum_folate_quart = ntile(LBDFOTSI, 4),
    RBC_folate_cat = factor(RBC_folate_quart, 
                            levels = 1:4,
                            labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)")),
    MTHF_cat = factor(MTHF_quart,
                      levels = 1:4, 
                      labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)")),
    Serum_folate_cat = factor(Serum_folate_quart,
                              levels = 1:4,
                              labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"))
  )

cat("\nRBC Folate quartile distribution:\n")
print(table(nhanes_combined$RBC_folate_cat, useNA = "ifany"))
cat("\nRBC Folate values by quartile:\n")
print(nhanes_combined %>%
        group_by(RBC_folate_cat) %>%
        summarise(
          N = n(),
          Min = min(LBXRBCSI, na.rm = TRUE),
          Median = median(LBXRBCSI, na.rm = TRUE),
          Max = max(LBXRBCSI, na.rm = TRUE)
        ))

cat("\n5-MTHF quartile distribution:\n")
print(table(nhanes_combined$MTHF_cat, useNA = "ifany"))
cat("\n5-MTHF values by quartile:\n")
print(nhanes_combined %>%
        group_by(MTHF_cat) %>%
        summarise(
          N = n(),
          Min = min(LBXSF1SI, na.rm = TRUE),
          Median = median(LBXSF1SI, na.rm = TRUE),
          Max = max(LBXSF1SI, na.rm = TRUE)
        ))

cat("\n\n2G. CREATING ANALYTIC SAMPLE\n")
cat(rep("-", 70), "\n", sep = "")

cat("\nFiltering to complete cases for main analysis...\n")
cat("Required variables:\n")
cat("  - Arsenic (iAs)\n")
cat("  - Depression (PHQ9_total)\n")
cat("  - RBC Folate (LBXRBCSI)\n")
cat("  - Demographics (age, sex, race, income)\n")
cat("  - Survey weights (WTCOMBINED)\n\n")

analytic_depression <- nhanes_combined %>%
  filter(
    !is.na(iAs),
    !is.na(PHQ9_total),
    !is.na(LBXRBCSI),
    !is.na(RIDAGEYR),
    !is.na(RIAGENDR),
    !is.na(RIDRETH3),
    !is.na(INDFMPIR),
    !is.na(WTCOMBINED),
    !is.na(AsB)
  )

cat("Analytic sample for DEPRESSION analysis:\n")
cat("  N =", nrow(analytic_depression), "\n")
cat("  Depression cases (PHQ-9 >= 10):", 
    sum(analytic_depression$Depression_binary == 1, na.rm = TRUE), "\n")
cat("  By cycle:\n")
print(table(analytic_depression$Cycle))

analytic_cognitive <- nhanes_combined %>%
  filter(
    Cycle == "2013-2014",
    RIDAGEYR >= 60,
    !is.na(iAs),
    !is.na(CFDCIR),
    !is.na(LBXRBCSI),
    !is.na(WTCOMBINED)
  )

cat("\nAnalytic sample for COGNITIVE analysis:\n")
cat("  N =", nrow(analytic_cognitive), "\n")
cat("  Age range:", min(analytic_cognitive$RIDAGEYR), "-", 
    max(analytic_cognitive$RIDAGEYR), "\n")

sample_sizes <- data.frame(
  Stage = c("Total combined", "With arsenic", "With depression", 
            "With RBC folate", "Complete cases (depression)",
            "2013-2014 age 60+", "Complete cases (cognitive)"),
  N = c(
    nrow(nhanes_combined),
    sum(!is.na(nhanes_combined$iAs)),
    sum(!is.na(nhanes_combined$PHQ9_total)),
    sum(!is.na(nhanes_combined$LBXRBCSI)),
    nrow(analytic_depression),
    sum(nhanes_combined$Cycle == "2013-2014" & nhanes_combined$RIDAGEYR >= 60, na.rm=TRUE),
    nrow(analytic_cognitive)
  )
)

cat("\n\nSAMPLE SIZE FLOW:\n")
print(sample_sizes)

cat("\n\n", rep("=", 70), "\n", sep = "")
cat("STEP 3: DESCRIPTIVE STATISTICS - WEIGHTED TABLE 1\n")
cat(rep("=", 70), "\n\n", sep = "")

library(tableone)
library(survey)

cat("3A. SETTING UP SURVEY DESIGN OBJECT\n")
cat(rep("-", 70), "\n", sep = "")

svy_depression <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTCOMBINED,
  nest = TRUE,
  data = analytic_depression
)

cat("Survey design created successfully\n")
cat("  Sample size:", nrow(analytic_depression), "\n")
cat("  Number of strata:", length(unique(analytic_depression$SDMVSTRA)), "\n")
cat("  Number of PSUs:", length(unique(analytic_depression$SDMVPSU)), "\n\n")

cat("3B. PREPARING VARIABLES FOR TABLE 1\n")
cat(rep("-", 70), "\n", sep = "")

analytic_depression <- analytic_depression %>%
  mutate(
    Age_group = case_when(
      RIDAGEYR < 30 ~ "18-29",
      RIDAGEYR < 45 ~ "30-44",
      RIDAGEYR < 60 ~ "45-59",
      TRUE ~ "60+"
    ),
    Sex = factor(RIAGENDR, levels = 1:2, labels = c("Male", "Female")),
    Race_ethnicity = factor(RIDRETH3, 
                            levels = c(3, 4, 6, 1, 2, 7),
                            labels = c("NH White", "NH Black", "NH Asian", 
                                       "Mexican American", "Other Hispanic", 
                                       "Other/Multi")),
    Education = factor(DMDEDUC2,
                       levels = 1:5,
                       labels = c("<9th grade", "9-11th grade", "HS/GED",
                                  "Some college", "College+")),
    Poverty_ratio_cat = case_when(
      INDFMPIR < 1.3 ~ "<1.3 (Low)",
      INDFMPIR < 3.5 ~ "1.3-3.5 (Middle)",
      TRUE ~ ">=3.5 (High)"
    ),
    iAs_quartile = ntile(iAs, 4),
    iAs_cat = factor(iAs_quartile, 
                     levels = 1:4,
                     labels = c("Q1", "Q2", "Q3", "Q4")),
    AsB_high = ifelse(AsB > quantile(AsB, 0.75), "High (>75th)", "Low-Medium"),
    Depression_severity = case_when(
      PHQ9_total < 5 ~ "None (0-4)",
      PHQ9_total < 10 ~ "Mild (5-9)",
      PHQ9_total < 15 ~ "Moderate (10-14)",
      PHQ9_total < 20 ~ "Moderately severe (15-19)",
      TRUE ~ "Severe (20-27)"
    )
  )

cat("Variable recoding complete\n\n")

cat("3C. CREATING WEIGHTED TABLE 1 BY RBC FOLATE QUARTILE\n")
cat(rep("-", 70), "\n", sep = "")

vars_table1 <- c(
  "RIDAGEYR", "Age_group", "Sex", "Race_ethnicity", "Education", 
  "INDFMPIR", "Poverty_ratio_cat",
  "iAs", "iAs_log", "iAs_cat", "AsB", "AsB_high",
  "URXUDMA", "URXUMMA",
  "LBXRBCSI", "LBXSF1SI", "LBDFOTSI",
  "PHQ9_total", "Depression_binary", "Depression_severity"
)

cat_vars <- c(
  "Age_group", "Sex", "Race_ethnicity", "Education", 
  "Poverty_ratio_cat", "iAs_cat", "AsB_high",
  "Depression_binary", "Depression_severity"
)

cat("\nGenerating survey-weighted descriptive statistics...\n\n")

table1_overall <- svyCreateTableOne(
  vars = vars_table1,
  strata = "RBC_folate_cat",
  data = svy_depression,
  factorVars = cat_vars,
  test = TRUE,
  addOverall = TRUE
)

cat("WEIGHTED TABLE 1: Baseline Characteristics by RBC Folate Quartile\n")
cat(rep("=", 70), "\n\n")

print(table1_overall, 
      smd = TRUE,
      test = TRUE,
      contDigits = 2,
      catDigits = 1,
      pDigits = 3,
      explain = TRUE,
      printToggle = TRUE)

cat("\n\n3D. TESTING FOR TREND ACROSS FOLATE QUARTILES\n")
cat(rep("-", 70), "\n", sep = "")

test_trend_continuous <- function(var_name, design) {
  formula_str <- paste0(var_name, " ~ RBC_folate_quart")
  model <- svyglm(as.formula(formula_str), design = design, 
                  family = gaussian())
  
  coef_trend <- coef(model)["RBC_folate_quart"]
  se_trend <- sqrt(vcov(model)["RBC_folate_quart", "RBC_folate_quart"])
  t_stat <- coef_trend / se_trend
  p_value <- 2 * pt(abs(t_stat), df = degf(design), lower.tail = FALSE)
  
  return(list(
    variable = var_name,
    coefficient = coef_trend,
    p_value = p_value
  ))
}

cat("\nTesting for linear trend (p-for-trend):\n\n")
trend_vars <- c("RIDAGEYR", "INDFMPIR", "iAs", "iAs_log", "AsB", 
                "PHQ9_total", "LBXSF1SI", "LBDFOTSI")

trend_results <- list()
for (var in trend_vars) {
  if (var %in% names(analytic_depression)) {
    result <- test_trend_continuous(var, svy_depression)
    trend_results[[var]] <- result
    cat(sprintf("%-15s: β = %7.4f, p = %.4f\n", 
                var, result$coefficient, result$p_value))
  }
}

cat("\n\nTesting trend for depression (binary):\n")
depression_trend <- svyglm(
  Depression_binary ~ RBC_folate_quart, 
  design = svy_depression,
  family = quasibinomial()
)
summary(depression_trend)

cat("\n\nGeometric mean of iAs by folate quartile:\n")
geo_means <- analytic_depression %>%
  group_by(RBC_folate_cat) %>%
  summarise(
    N = n(),
    Weighted_N = sum(WTCOMBINED),
    Arithmetic_mean = weighted.mean(iAs, WTCOMBINED, na.rm = TRUE),
    Geometric_mean = exp(weighted.mean(log(iAs), WTCOMBINED, na.rm = TRUE)),
    Median = median(iAs, na.rm = TRUE),
    .groups = "drop"
  )
print(geo_means)

cat("\n\n", rep("=", 70), "\n", sep = "")
cat("KEY FINDINGS FROM DESCRIPTIVE ANALYSIS\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("1. SAMPLE CHARACTERISTICS:\n")
cat("   • Total weighted N: 201 million (representing US population)\n")
cat("   • Unweighted N: 3,004 individuals\n")
cat("   • Depression prevalence (PHQ-9 >=10): 7.3% overall\n\n")
cat("2. DEMOGRAPHIC TRENDS BY FOLATE QUARTILE:\n")
cat("   • Age: DECREASES with higher folate (p < 0.001)\n")
cat("     - Q1: 51.2 years -> Q4: 42.2 years (9 year difference)\n")
cat("   • Income: No significant trend (p = 0.792)\n")
cat("   • Interpretation: Younger individuals have higher folate\n\n")
cat("3. ARSENIC EXPOSURE BY FOLATE QUARTILE:\n")
cat("   • Inorganic arsenic (iAs) INCREASES with folate (p = 0.008)\n")
cat("     - Q1: 0.89 μg/L -> Q4: 0.97 μg/L\n")
cat("   • Log(iAs) also increases (p = 0.011)\n")
cat("   • Arsenobetaine: No trend (p = 0.207)\n")
cat("   • IMPORTANT: This suggests potential confounding or\n")
cat("     that people with higher arsenic may consume more folate\n\n")
cat("4. FOLATE MEASURES BY RBC FOLATE QUARTILE:\n")
cat("   • 5-MTHF DECREASES (p < 0.001): 44.0 -> 34.5 nmol/L\n")
cat("   • Total serum folate DECREASES (p < 0.001): 48.0 -> 36.7 nmol/L\n")
cat("   • NOTE: RBC and serum folate NOT perfectly correlated\n\n")
cat("5. DEPRESSION BY FOLATE QUARTILE:\n")
cat("   • PHQ-9 score: No significant trend (p = 0.223)\n")
cat("   • Depression prevalence: No trend (p = 0.957)\n")
cat("     - Q1: 7.7% -> Q4: 7.3%\n")
cat("   • Suggests no main effect, BUT interaction may exist\n\n")

cat("\n\n", rep("=", 70), "\n", sep = "")
cat("STEP 4 PREVIEW: REGRESSION ANALYSIS PLAN\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("Next steps:\n")
cat("1. Main effect models:\n")
cat("   • Model 1: Arsenic -> Depression (unadjusted)\n")
cat("   • Model 2: + demographics (age, sex, race)\n")
cat("   • Model 3: + SES (income, education)\n")
cat("   • Model 4: + arsenobetaine (full model)\n\n")
cat("2. Effect modification analysis:\n")
cat("   • Interaction model: iAs × RBC_folate_quartile\n")
cat("   • Stratified models by folate quartile\n")
cat("   • Predicted probabilities plots\n\n")
cat("3. Sensitivity analyses:\n")
cat("   • Creatinine-adjusted arsenic (2013-2014 only)\n")
cat("   • Alternative folate measures (5-MTHF, serum folate)\n")
cat("   • Exclude high arsenobetaine (>75th percentile)\n\n")

svy_depression <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTCOMBINED,
  nest = TRUE,
  data = analytic_depression
)

cat("\n\n", rep("=", 70), "\n", sep = "")
cat("STEP 4: MAIN EFFECT MODELS - ARSENIC -> DEPRESSION\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("4A. LOGISTIC REGRESSION MODELS (Depression Binary: PHQ-9 >= 10)\n")
cat(rep("-", 70), "\n", sep = "")

cat("\nMODEL 1: Unadjusted (Arsenic only)\n")
cat(rep("-", 40), "\n", sep = "")
model1_binary <- svyglm(
  Depression_binary ~ iAs_log,
  design = svy_depression,
  family = quasibinomial()
)
cat("Summary:\n")
print(summary(model1_binary))

coef1 <- coef(model1_binary)["iAs_log"]
se1 <- sqrt(vcov(model1_binary)["iAs_log", "iAs_log"])
or1 <- exp(coef1)
ci1_lower <- exp(coef1 - 1.96 * se1)
ci1_upper <- exp(coef1 + 1.96 * se1)
cat("\nResults (per unit increase in log(iAs)):\n")
cat(sprintf("   OR = %.3f (95%% CI: %.3f - %.3f)\n", or1, ci1_lower, ci1_upper))

cat("\n\nMODEL 2: + Demographics (age, sex, race/ethnicity)\n")
cat(rep("-", 40), "\n", sep = "")
model2_binary <- svyglm(
  Depression_binary ~ iAs_log + RIDAGEYR + RIAGENDR + RIDRETH3,
  design = svy_depression,
  family = quasibinomial()
)
cat("Summary:\n")
print(summary(model2_binary))

coef2 <- coef(model2_binary)["iAs_log"]
se2 <- sqrt(vcov(model2_binary)["iAs_log", "iAs_log"])
or2 <- exp(coef2)
ci2_lower <- exp(coef2 - 1.96 * se2)
ci2_upper <- exp(coef2 + 1.96 * se2)
cat("\nResults (per unit increase in log(iAs)):\n")
cat(sprintf("   OR = %.3f (95%% CI: %.3f - %.3f)\n", or2, ci2_lower, ci2_upper))

cat("\n\nMODEL 3: + Socioeconomic Status (income-to-poverty ratio)\n")
cat(rep("-", 40), "\n", sep = "")
model3_binary <- svyglm(
  Depression_binary ~ iAs_log + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR,
  design = svy_depression,
  family = quasibinomial()
)
cat("Summary:\n")
print(summary(model3_binary))

coef3 <- coef(model3_binary)["iAs_log"]
se3 <- sqrt(vcov(model3_binary)["iAs_log", "iAs_log"])
or3 <- exp(coef3)
ci3_lower <- exp(coef3 - 1.96 * se3)
ci3_upper <- exp(coef3 + 1.96 * se3)
cat("\nResults (per unit increase in log(iAs)):\n")
cat(sprintf("   OR = %.3f (95%% CI: %.3f - %.3f)\n", or3, ci3_lower, ci3_upper))

cat("\n\nMODEL 4: FULL MODEL + Arsenobetaine (seafood confounder)\n")
cat(rep("-", 40), "\n", sep = "")
model4_binary <- svyglm(
  Depression_binary ~ iAs_log + RIDAGEYR + RIAGENDR + RIDRETH3 + 
    INDFMPIR + AsB_log,
  design = svy_depression,
  family = quasibinomial()
)
cat("Summary:\n")
print(summary(model4_binary))

coef4 <- coef(model4_binary)["iAs_log"]
se4 <- sqrt(vcov(model4_binary)["iAs_log", "iAs_log"])
or4 <- exp(coef4)
ci4_lower <- exp(coef4 - 1.96 * se4)
ci4_upper <- exp(coef4 + 1.96 * se4)
cat("\nResults (per unit increase in log(iAs)):\n")
cat(sprintf("   OR = %.3f (95%% CI: %.3f - %.3f)\n", or4, ci4_lower, ci4_upper))

cat("\n\n4B. LINEAR REGRESSION MODELS (PHQ-9 Total Score)\n")
cat(rep("-", 70), "\n", sep = "")

cat("\nMODEL 1: Unadjusted\n")
cat(rep("-", 40), "\n", sep = "")
model1_cont <- svyglm(
  PHQ9_total ~ iAs_log,
  design = svy_depression,
  family = gaussian()
)
cat("Summary:\n")
print(summary(model1_cont))

cat("\n\nMODEL 4: FULL MODEL\n")
cat(rep("-", 40), "\n", sep = "")
model4_cont <- svyglm(
  PHQ9_total ~ iAs_log + RIDAGEYR + RIAGENDR + RIDRETH3 + 
    INDFMPIR + AsB_log,
  design = svy_depression,
  family = gaussian()
)
cat("Summary:\n")
print(summary(model4_cont))

cat("\n\n4C. SUMMARY TABLE: ARSENIC EFFECT ACROSS ALL MODELS\n")
cat(rep("-", 70), "\n", sep = "")

model_summary <- data.frame(
  Model = c("Model 1: Unadjusted",
            "Model 2: + Demographics",
            "Model 3: + SES",
            "Model 4: Full (+ Arsenobetaine)"),
  OR = c(
    sprintf("%.3f", exp(coef(model1_binary)["iAs_log"])),
    sprintf("%.3f", exp(coef(model2_binary)["iAs_log"])),
    sprintf("%.3f", exp(coef(model3_binary)["iAs_log"])),
    sprintf("%.3f", exp(coef(model4_binary)["iAs_log"]))
  ),
  CI_95 = c(
    sprintf("(%.3f - %.3f)", ci1_lower, ci1_upper),
    sprintf("(%.3f - %.3f)", ci2_lower, ci2_upper),
    sprintf("(%.3f - %.3f)", ci3_lower, ci3_upper),
    sprintf("(%.3f - %.3f)", ci4_lower, ci4_upper)
  ),
  P_value_binary = c(
    sprintf("%.3f", summary(model1_binary)$coefficients["iAs_log", "Pr(>|t|)"]),
    sprintf("%.3f", summary(model2_binary)$coefficients["iAs_log", "Pr(>|t|)"]),
    sprintf("%.3f", summary(model3_binary)$coefficients["iAs_log", "Pr(>|t|)"]),
    sprintf("%.3f", summary(model4_binary)$coefficients["iAs_log", "Pr(>|t|)"])
  ),
  Beta_continuous = c(
    sprintf("%.3f", coef(model1_cont)["iAs_log"]),
    "—",
    "—",
    sprintf("%.3f", coef(model4_cont)["iAs_log"])
  ),
  P_value_cont = c(
    sprintf("%.3f", summary(model1_cont)$coefficients["iAs_log", "Pr(>|t|)"]),
    "—",
    "—",
    sprintf("%.3f", summary(model4_cont)$coefficients["iAs_log", "Pr(>|t|)"])
  )
)

cat("\nTable 2: Main Effect of Arsenic on Depression\n\n")
print(model_summary, row.names = FALSE)

cat("\n\nInterpretation:\n")
cat("• OR > 1: Higher arsenic -> Higher odds of depression\n")
cat("• β > 0: Higher arsenic -> Higher PHQ-9 scores\n")
cat("• Per unit increase in log(iAs) = 2.7× increase in iAs\n\n")

cat("\n\n", rep("=", 70), "\n", sep = "")
cat("STEP 5: EFFECT MODIFICATION - DOES FOLATE BUFFER ARSENIC?\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("KEY QUESTION: Does the effect of arsenic on depression differ\n")
cat("by folate status, even though there's no overall main effect?\n\n")

cat("5A. INTERACTION MODEL (Product Term Approach)\n")
cat(rep("-", 70), "\n", sep = "")

cat("\nFULL MODEL WITH INTERACTION: iAs × RBC_folate_quartile\n")
cat(rep("-", 40), "\n", sep = "")

interaction_binary <- svyglm(
  Depression_binary ~ iAs_log * RBC_folate_quart + 
    RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + AsB_log,
  design = svy_depression,
  family = quasibinomial()
)
cat("Summary:\n")
print(summary(interaction_binary))

cat("\n\nTesting interaction significance:\n")
cat("Wald test for interaction term (iAs_log:RBC_folate_quart)\n\n")

interact_coef <- coef(interaction_binary)["iAs_log:RBC_folate_quart"]
interact_se <- sqrt(vcov(interaction_binary)["iAs_log:RBC_folate_quart", 
                                             "iAs_log:RBC_folate_quart"])
interact_p <- summary(interaction_binary)$coefficients["iAs_log:RBC_folate_quart", "Pr(>|t|)"]

cat(sprintf("Interaction coefficient: β = %.4f (SE = %.4f)\n", interact_coef, interact_se))
cat(sprintf("P-value: %.4f\n", interact_p))

if (interact_p < 0.10) {
  cat("\nSIGNIFICANT INTERACTION DETECTED (p < 0.10)\n")
  cat("-> Effect of arsenic on depression DIFFERS by folate level\n\n")
} else {
  cat("\nNo significant interaction (p >= 0.10)\n")
  cat("-> Effect of arsenic does not differ by folate level\n\n")
}

cat("\n\nLINEAR MODEL WITH INTERACTION (PHQ-9 Continuous)\n")
cat(rep("-", 40), "\n", sep = "")
interaction_cont <- svyglm(
  PHQ9_total ~ iAs_log * RBC_folate_quart + 
    RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + AsB_log,
  design = svy_depression,
  family = gaussian()
)
cat("Summary:\n")
print(summary(interaction_cont))

cat("\n\n5B. STRATIFIED ANALYSIS (Separate Models by Folate Quartile)\n")
cat(rep("-", 70), "\n", sep = "")

cat("\nRunning fully adjusted models within each folate quartile...\n\n")

stratified_results <- list()
quartiles <- c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)")

for (i in 1:4) {
  cat(sprintf("\n%s: %s\n", LETTERS[i], quartiles[i]))
  cat(rep("-", 40), "\n", sep = "")
  
  svy_subset <- subset(svy_depression, RBC_folate_quart == i)
  n_subset <- sum(svy_depression$variables$RBC_folate_quart == i, na.rm = TRUE)
  cat(sprintf("Sample size: %d\n", n_subset))
  
  if (n_subset < 50) {
    cat("Warning: Small sample size in this stratum\n")
  }
  
  model_subset <- svyglm(
    Depression_binary ~ iAs_log + RIDAGEYR + RIAGENDR + RIDRETH3 + 
      INDFMPIR + AsB_log,
    design = svy_subset,
    family = quasibinomial()
  )
  
  coef_subset <- coef(model_subset)["iAs_log"]
  se_subset <- sqrt(vcov(model_subset)["iAs_log", "iAs_log"])
  or_subset <- exp(coef_subset)
  ci_lower_subset <- exp(coef_subset - 1.96 * se_subset)
  ci_upper_subset <- exp(coef_subset + 1.96 * se_subset)
  p_subset <- summary(model_subset)$coefficients["iAs_log", "Pr(>|t|)"]
  
  cat(sprintf("OR = %.3f (95%% CI: %.3f - %.3f), p = %.4f\n", 
              or_subset, ci_lower_subset, ci_upper_subset, p_subset))
  
  stratified_results[[i]] <- list(
    quartile = quartiles[i],
    n = n_subset,
    OR = or_subset,
    CI_lower = ci_lower_subset,
    CI_upper = ci_upper_subset,
    p_value = p_subset
  )
}

cat("\n\n5C. SUMMARY: ARSENIC EFFECT BY FOLATE QUARTILE\n")
cat(rep("-", 70), "\n", sep = "")

stratified_table <- data.frame(
  Folate_Quartile = sapply(stratified_results, function(x) x$quartile),
  N = sapply(stratified_results, function(x) x$n),
  OR = sprintf("%.3f", sapply(stratified_results, function(x) x$OR)),
  CI_95 = sprintf("(%.3f - %.3f)", 
                  sapply(stratified_results, function(x) x$CI_lower),
                  sapply(stratified_results, function(x) x$CI_upper)),
  P_value = sprintf("%.4f", sapply(stratified_results, function(x) x$p_value))
)

cat("\nTable 3: Stratified Analysis - Arsenic Effect by Folate Quartile\n\n")
print(stratified_table, row.names = FALSE)

cat("\n\nTesting for heterogeneity across folate strata:\n")
ORs <- sapply(stratified_results, function(x) log(x$OR))
SEs <- sapply(stratified_results, function(x) {
  (log(x$CI_upper) - log(x$CI_lower)) / (2 * 1.96)
})

weights <- 1 / (SEs^2)
pooled_OR <- sum(weights * ORs) / sum(weights)
Q <- sum(weights * (ORs - pooled_OR)^2)
Q_df <- length(ORs) - 1
Q_p <- pchisq(Q, df = Q_df, lower.tail = FALSE)

cat(sprintf("Cochran's Q = %.2f (df = %d), p = %.4f\n", Q, Q_df, Q_p))

if (Q_p < 0.10) {
  cat("Significant heterogeneity detected\n")
  cat("-> Arsenic effect differs across folate quartiles\n")
} else {
  cat("No significant heterogeneity\n")
  cat("-> Arsenic effect is similar across folate quartiles\n")
}

cat("\n\n", rep("=", 70), "\n", sep = "")
cat("INTERPRETATION OF EFFECT MODIFICATION ANALYSIS\n")
cat(rep("=", 70), "\n\n")

cat("KEY FINDINGS:\n\n")

cat("1. INTERACTION TERM:\n")
if (interact_p < 0.10) {
  cat(sprintf("   Significant (p = %.4f)\n", interact_p))
  cat("   -> Folate modifies the arsenic-depression relationship\n\n")
} else {
  cat(sprintf("   Not significant (p = %.4f)\n", interact_p))
  cat("   -> No statistical evidence for effect modification\n\n")
}

cat("2. STRATIFIED MODELS:\n")
cat("   Arsenic OR by folate quartile:\n")
for (i in 1:4) {
  cat(sprintf("   - %s: OR = %.3f (p = %.4f)\n", 
              stratified_results[[i]]$quartile,
              stratified_results[[i]]$OR,
              stratified_results[[i]]$p_value))
}
cat("\n")

cat("3. PATTERN:\n")
or_q1 <- stratified_results[[1]]$OR
or_q4 <- stratified_results[[4]]$OR

if (or_q1 > 1.2 & or_q4 < 1) {
  cat("   PROTECTIVE PATTERN: Higher folate attenuates arsenic effect\n")
  cat("   -> Low folate (Q1): Arsenic increases depression risk\n")
  cat("   -> High folate (Q4): No arsenic effect (protective buffering)\n\n")
} else if (abs(or_q1 - or_q4) < 0.3) {
  cat("   -> Consistent effects across folate levels (no modification)\n\n")
} else {
  cat("   -> Mixed pattern - interpretation requires caution\n\n")
}

setwd("C:/Desktop/SEM 4/Causal Inference/Causal Project")

dir.create("tables", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)

cat("Working directory set. Output folders created.\n\n")

cat("\n\n", rep("=", 70), "\n", sep = "")
cat("STEP 6: COGNITIVE FUNCTION ANALYSIS (2013-2014 ONLY)\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("6A. CREATING COGNITIVE COMPOSITE Z-SCORE\n")
cat(rep("-", 70), "\n", sep = "")

cat("\nCognitive tests in NHANES 2013-2014 (age 60+):\n")
cat("  • CFDCIR: CERAD delayed recall (0-10, higher = better)\n")
cat("  • CFDAST: Animal Fluency (# animals in 60s, higher = better)\n")
cat("  • CFDDS: Digit Symbol Substitution (0-105, higher = better)\n\n")

cat("Checking cognitive variables in analytic_cognitive:\n")
cat("  N with CFDCIR:", sum(!is.na(analytic_cognitive$CFDCIR)), "\n")
cat("  N with CFDAST:", sum(!is.na(analytic_cognitive$CFDAST)), "\n")
cat("  N with CFDDS:", sum(!is.na(analytic_cognitive$CFDDS)), "\n\n")

analytic_cognitive <- analytic_cognitive %>%
  mutate(
    CFDCIR_z = scale(CFDCIR)[,1],
    CFDAST_z = scale(CFDAST)[,1],
    CFDDS_z = scale(CFDDS)[,1],
    Cognitive_composite = rowMeans(cbind(CFDCIR_z, CFDAST_z, CFDDS_z), 
                                   na.rm = FALSE),
    N_missing_tests = rowSums(is.na(cbind(CFDCIR, CFDAST, CFDDS))),
    Cognitive_valid = N_missing_tests == 0
  )

cat("Cognitive composite created:\n")
cat("  Valid composites:", sum(analytic_cognitive$Cognitive_valid, na.rm=TRUE), "\n")
cat("  Mean composite:", round(mean(analytic_cognitive$Cognitive_composite, na.rm=TRUE), 3), "\n")
cat("  SD composite:", round(sd(analytic_cognitive$Cognitive_composite, na.rm=TRUE), 3), "\n\n")

analytic_cognitive_complete <- analytic_cognitive %>%
  filter(Cognitive_valid == TRUE, !is.na(iAs), !is.na(LBXRBCSI))

cat("Final cognitive analysis sample:\n")
cat("  N =", nrow(analytic_cognitive_complete), "\n\n")

svy_cognitive <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTCOMBINED,
  nest = TRUE,
  data = analytic_cognitive_complete
)

cat("Survey design created for cognitive analysis\n\n")

cat("6B. MAIN EFFECT MODELS - ARSENIC -> COGNITIVE FUNCTION\n")
cat(rep("-", 70), "\n", sep = "")

cat("\nMODEL 1: Unadjusted\n")
cog_model1 <- svyglm(
  Cognitive_composite ~ iAs_log,
  design = svy_cognitive,
  family = gaussian()
)
print(summary(cog_model1))

cat("\n\nMODEL 2: + Demographics\n")
cog_model2 <- svyglm(
  Cognitive_composite ~ iAs_log + RIDAGEYR + RIAGENDR + RIDRETH3,
  design = svy_cognitive,
  family = gaussian()
)
print(summary(cog_model2))

cat("\n\nMODEL 3: + SES\n")
cog_model3 <- svyglm(
  Cognitive_composite ~ iAs_log + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR,
  design = svy_cognitive,
  family = gaussian()
)
print(summary(cog_model3))

cat("\n\nMODEL 4: Full Model + Arsenobetaine\n")
cog_model4 <- svyglm(
  Cognitive_composite ~ iAs_log + RIDAGEYR + RIAGENDR + RIDRETH3 + 
    INDFMPIR + AsB_log,
  design = svy_cognitive,
  family = gaussian()
)
print(summary(cog_model4))

cog_results <- data.frame(
  Model = c("Model 1: Unadjusted", "Model 2: + Demographics", 
            "Model 3: + SES", "Model 4: Full"),
  Beta = c(
    sprintf("%.3f", coef(cog_model1)["iAs_log"]),
    sprintf("%.3f", coef(cog_model2)["iAs_log"]),
    sprintf("%.3f", coef(cog_model3)["iAs_log"]),
    sprintf("%.3f", coef(cog_model4)["iAs_log"])
  ),
  SE = c(
    sprintf("%.3f", sqrt(vcov(cog_model1)["iAs_log", "iAs_log"])),
    sprintf("%.3f", sqrt(vcov(cog_model2)["iAs_log", "iAs_log"])),
    sprintf("%.3f", sqrt(vcov(cog_model3)["iAs_log", "iAs_log"])),
    sprintf("%.3f", sqrt(vcov(cog_model4)["iAs_log", "iAs_log"]))
  ),
  P_value = c(
    sprintf("%.4f", summary(cog_model1)$coefficients["iAs_log", "Pr(>|t|)"]),
    sprintf("%.4f", summary(cog_model2)$coefficients["iAs_log", "Pr(>|t|)"]),
    sprintf("%.4f", summary(cog_model3)$coefficients["iAs_log", "Pr(>|t|)"]),
    sprintf("%.4f", summary(cog_model4)$coefficients["iAs_log", "Pr(>|t|)"])
  )
)

cat("\n\nTable 4: Main Effect of Arsenic on Cognitive Function\n")
print(cog_results, row.names = FALSE)

cat("\n\n6C. EFFECT MODIFICATION - FOLATE × ARSENIC FOR COGNITION\n")
cat(rep("-", 70), "\n", sep = "")

cat("\nINTERACTION MODEL\n")
cog_interaction <- svyglm(
  Cognitive_composite ~ iAs_log * RBC_folate_quart + 
    RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + AsB_log,
  design = svy_cognitive,
  family = gaussian()
)
print(summary(cog_interaction))

interact_coef_cog <- coef(cog_interaction)["iAs_log:RBC_folate_quart"]
interact_p_cog <- summary(cog_interaction)$coefficients["iAs_log:RBC_folate_quart", "Pr(>|t|)"]

cat(sprintf("\nInteraction: β = %.4f, p = %.4f\n", interact_coef_cog, interact_p_cog))

cat("\n\nSTRATIFIED ANALYSIS BY FOLATE QUARTILE\n\n")

stratified_cog_results <- list()
for (i in 1:4) {
  cat(sprintf("%s: Quartile %d\n", LETTERS[i], i))
  
  svy_subset <- subset(svy_cognitive, RBC_folate_quart == i)
  n_subset <- sum(svy_cognitive$variables$RBC_folate_quart == i, na.rm = TRUE)
  cat(sprintf("  N = %d\n", n_subset))
  
  model_subset <- svyglm(
    Cognitive_composite ~ iAs_log + RIDAGEYR + RIAGENDR + RIDRETH3 + 
      INDFMPIR + AsB_log,
    design = svy_subset,
    family = gaussian()
  )
  
  beta_subset <- coef(model_subset)["iAs_log"]
  se_subset <- sqrt(vcov(model_subset)["iAs_log", "iAs_log"])
  ci_lower <- beta_subset - 1.96 * se_subset
  ci_upper <- beta_subset + 1.96 * se_subset
  p_subset <- summary(model_subset)$coefficients["iAs_log", "Pr(>|t|)"]
  
  cat(sprintf("  β = %.3f (95%% CI: %.3f to %.3f), p = %.4f\n\n", 
              beta_subset, ci_lower, ci_upper, p_subset))
  
  stratified_cog_results[[i]] <- list(
    quartile = i,
    n = n_subset,
    beta = beta_subset,
    CI_lower = ci_lower,
    CI_upper = ci_upper,
    p_value = p_subset
  )
}

cat("\n\n", rep("=", 70), "\n", sep = "")
cat("STEP 7: SENSITIVITY ANALYSES\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("7A. SENSITIVITY ANALYSIS: 5-MTHF AS EFFECT MODIFIER\n")
cat(rep("-", 70), "\n", sep = "")

cat("\nRepeating effect modification analysis with 5-MTHF (bioactive folate)\n")
cat("instead of RBC folate...\n\n")

n_mthf <- sum(!is.na(analytic_depression$LBXSF1SI) & 
                !is.na(analytic_depression$MTHF_quart))

cat("Depression sample with 5-MTHF data: N =", n_mthf, "\n\n")

svy_mthf <- subset(svy_depression, !is.na(MTHF_quart))

cat("INTERACTION MODEL: iAs × 5-MTHF (Depression)\n")
cat(rep("-", 40), "\n", sep = "")

mthf_interaction_dep <- svyglm(
  Depression_binary ~ iAs_log * MTHF_quart + 
    RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + AsB_log,
  design = svy_mthf,
  family = quasibinomial()
)

print(summary(mthf_interaction_dep))

mthf_interact_p <- summary(mthf_interaction_dep)$coefficients["iAs_log:MTHF_quart", "Pr(>|t|)"]
cat(sprintf("\nInteraction p-value: %.4f\n", mthf_interact_p))

cat("\n\nSTRATIFIED BY 5-MTHF QUARTILES\n\n")

mthf_stratified <- list()
for (i in 1:4) {
  cat(sprintf("5-MTHF Quartile %d:\n", i))
  
  svy_subset <- subset(svy_mthf, MTHF_quart == i)
  n_subset <- sum(svy_mthf$variables$MTHF_quart == i, na.rm = TRUE)
  cat(sprintf("  N = %d\n", n_subset))
  
  model_subset <- svyglm(
    Depression_binary ~ iAs_log + RIDAGEYR + RIAGENDR + RIDRETH3 + 
      INDFMPIR + AsB_log,
    design = svy_subset,
    family = quasibinomial()
  )
  
  coef_subset <- coef(model_subset)["iAs_log"]
  se_subset <- sqrt(vcov(model_subset)["iAs_log", "iAs_log"])
  or_subset <- exp(coef_subset)
  ci_lower <- exp(coef_subset - 1.96 * se_subset)
  ci_upper <- exp(coef_subset + 1.96 * se_subset)
  p_subset <- summary(model_subset)$coefficients["iAs_log", "Pr(>|t|)"]
  
  cat(sprintf("  OR = %.3f (95%% CI: %.3f - %.3f), p = %.4f\n\n", 
              or_subset, ci_lower, ci_upper, p_subset))
  
  mthf_stratified[[i]] <- list(
    quartile = i,
    OR = or_subset,
    CI_lower = ci_lower,
    CI_upper = ci_upper,
    p = p_subset
  )
}

cat("\n\n7B. SENSITIVITY: EXCLUDE HIGH ARSENOBETAINE (>75th %ile)\n")
cat(rep("-", 70), "\n", sep = "")

asb_75 <- quantile(analytic_depression$AsB, 0.75, na.rm = TRUE)
cat(sprintf("\n75th percentile of arsenobetaine: %.2f μg/L\n\n", asb_75))

analytic_low_asb <- analytic_depression %>%
  filter(AsB <= asb_75)

cat("Sample after excluding high arsenobetaine:\n")
cat("  Original N:", nrow(analytic_depression), "\n")
cat("  Low AsB N:", nrow(analytic_low_asb), "\n\n")

svy_low_asb <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTCOMBINED,
  nest = TRUE,
  data = analytic_low_asb
)

cat("MAIN EFFECT (Low AsB subsample)\n")
model_low_asb <- svyglm(
  Depression_binary ~ iAs_log + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR,
  design = svy_low_asb,
  family = quasibinomial()
)

or_low_asb <- exp(coef(model_low_asb)["iAs_log"])
se_low_asb <- sqrt(vcov(model_low_asb)["iAs_log", "iAs_log"])
p_low_asb <- summary(model_low_asb)$coefficients["iAs_log", "Pr(>|t|)"]

cat(sprintf("\nOR = %.3f (95%% CI: %.3f - %.3f), p = %.4f\n", 
            or_low_asb,
            exp(coef(model_low_asb)["iAs_log"] - 1.96*se_low_asb),
            exp(coef(model_low_asb)["iAs_log"] + 1.96*se_low_asb),
            p_low_asb))

cat("\nCompare to full sample: OR = 1.182, p = 0.508\n")

cat("\n\n7C. SENSITIVITY: CREATININE-ADJUSTED ARSENIC (2013-2014)\n")
cat(rep("-", 70), "\n", sep = "")

analytic_2013_creat <- analytic_depression %>%
  filter(Cycle == "2013-2014", !is.na(iAs_creat_ratio))

cat("\n2013-2014 sample with creatinine adjustment:\n")
cat("  N =", nrow(analytic_2013_creat), "\n\n")

if (nrow(analytic_2013_creat) > 0) {
  svy_creat <- svydesign(
    ids = ~SDMVPSU,
    strata = ~SDMVSTRA,
    weights = ~WTCOMBINED,
    nest = TRUE,
    data = analytic_2013_creat
  )
  
  cat("MODEL: Creatinine-adjusted arsenic\n")
  
  analytic_2013_creat$iAs_creat_log <- log(analytic_2013_creat$iAs_creat_ratio + 0.01)
  
  svy_creat <- svydesign(
    ids = ~SDMVPSU,
    strata = ~SDMVSTRA,
    weights = ~WTCOMBINED,
    nest = TRUE,
    data = analytic_2013_creat
  )
  
  model_creat <- svyglm(
    Depression_binary ~ iAs_creat_log + RIDAGEYR + RIAGENDR + 
      RIDRETH3 + INDFMPIR + AsB_log,
    design = svy_creat,
    family = quasibinomial()
  )
  
  print(summary(model_creat))
  
  or_creat <- exp(coef(model_creat)["iAs_creat_log"])
  p_creat <- summary(model_creat)$coefficients["iAs_creat_log", "Pr(>|t|)"]
  
  cat(sprintf("\nOR (creatinine-adjusted) = %.3f, p = %.4f\n", or_creat, p_creat))
  cat("Compare to unadjusted: OR = 1.182, p = 0.508\n")
}

cat("\n\n", rep("=", 70), "\n", sep = "")
cat("SUMMARY: SENSITIVITY ANALYSES\n")
cat(rep("=", 70), "\n\n")

cat("TABLE 5: Sensitivity Analysis Results\n\n")

sens_summary <- data.frame(
  Analysis = c(
    "Primary (RBC folate)",
    "Alternative (5-MTHF)",
    "Exclude high AsB",
    "Creatinine-adjusted (2013-14)"
  ),
  N = c(
    nrow(analytic_depression),
    n_mthf,
    nrow(analytic_low_asb),
    nrow(analytic_2013_creat)
  ),
  Main_Effect_OR = c(
    "1.182",
    "—",
    sprintf("%.3f", or_low_asb),
    ifelse(nrow(analytic_2013_creat) > 0, sprintf("%.3f", or_creat), "—")
  ),
  Main_Effect_P = c(
    "0.508",
    "—",
    sprintf("%.4f", p_low_asb),
    ifelse(nrow(analytic_2013_creat) > 0, sprintf("%.4f", p_creat), "—")
  ),
  Interaction_P = c(
    "0.728",
    sprintf("%.4f", mthf_interact_p),
    "—",
    "—"
  )
)

print(sens_summary, row.names = FALSE)

cat("\n\nKEY FINDINGS:\n")
cat("• Results consistent across different folate measures\n")
cat("• No effect after excluding high seafood consumers\n")
cat("• Creatinine adjustment does not change conclusions\n")
cat("• Overall: NO evidence for arsenic-depression relationship\n\n")

cat("\n\n", rep("=", 70), "\n", sep = "")
cat("STEP 8: SAVING OUTPUTS & CREATING VISUALIZATIONS\n")
cat(rep("=", 70), "\n\n", sep = "")

library(ggplot2)
library(gridExtra)

cat("8A. SAVING TABLES TO CSV FILES\n")
cat(rep("-", 70), "\n\n", sep = "")

table1_export <- print(table1_overall, printToggle = FALSE, noSpaces = TRUE)
write.csv(table1_export, "tables/Table1_Baseline_Characteristics.csv", row.names = TRUE)
cat("Table 1 saved: Baseline characteristics by folate quartile\n")

write.csv(model_summary, "tables/Table2_Main_Effects_Depression.csv", row.names = FALSE)
cat("Table 2 saved: Main effects - Depression\n")

write.csv(stratified_table, "tables/Table3_Stratified_Depression.csv", row.names = FALSE)
cat("Table 3 saved: Stratified analysis - Depression\n")

write.csv(cog_results, "tables/Table4_Main_Effects_Cognition.csv", row.names = FALSE)
cat("Table 4 saved: Main effects - Cognition\n")

write.csv(sens_summary, "tables/Table5_Sensitivity_Analyses.csv", row.names = FALSE)
cat("Table 5 saved: Sensitivity analyses\n")

cog_strat_table <- data.frame(
  Folate_Quartile = paste0("Q", 1:4),
  N = sapply(stratified_cog_results, function(x) x$n),
  Beta = sprintf("%.3f", sapply(stratified_cog_results, function(x) x$beta)),
  CI_95 = sprintf("(%.3f to %.3f)", 
                  sapply(stratified_cog_results, function(x) x$CI_lower),
                  sapply(stratified_cog_results, function(x) x$CI_upper)),
  P_value = sprintf("%.4f", sapply(stratified_cog_results, function(x) x$p_value))
)
write.csv(cog_strat_table, "tables/Table6_Stratified_Cognition.csv", row.names = FALSE)
cat("Table 6 saved: Stratified analysis - Cognition\n\n")

cat("8B. CREATING FOREST PLOT - DEPRESSION\n")
cat(rep("-", 70), "\n\n", sep = "")

forest_data_dep <- data.frame(
  Quartile = factor(paste0("Q", 1:4, " (", 
                           c("Lowest", "Low-Med", "Med-High", "Highest"), ")"),
                    levels = paste0("Q", 4:1, " (", 
                                    c("Highest", "Med-High", "Low-Med", "Lowest"), ")")),
  OR = sapply(stratified_results, function(x) x$OR),
  CI_lower = sapply(stratified_results, function(x) x$CI_lower),
  CI_upper = sapply(stratified_results, function(x) x$CI_upper),
  P_value = sapply(stratified_results, function(x) x$p_value)
)

forest_data_dep <- forest_data_dep[4:1, ]

p_forest_dep <- ggplot(forest_data_dep, aes(x = OR, y = Quartile)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_errorbar(aes(xmin = CI_lower, xmax = CI_upper), 
                width = 0.2, linewidth = 0.8, color = "#2C3E50", orientation = "y") +
  geom_point(size = 4, color = "#E74C3C") +
  scale_x_log10(breaks = c(0.25, 0.5, 1, 2, 4, 8)) +
  labs(
    title = "Effect of Arsenic on Depression by RBC Folate Quartile",
    subtitle = "Odds Ratios (95% CI) from Survey-Weighted Logistic Regression",
    x = "Odds Ratio (log scale)",
    y = "RBC Folate Quartile"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  ) +
  annotate("text", x = 8, y = 0.5, 
           label = "No significant\ninteraction\n(p = 0.728)", 
           hjust = 1, vjust = 0, size = 3.5, color = "gray30")

ggsave("figures/Figure1_Forest_Depression.png", p_forest_dep, 
       width = 10, height = 6, dpi = 300)
cat("Figure 1 saved: Forest plot - Depression\n\n")

cat("8C. CREATING FOREST PLOT - COGNITION\n")
cat(rep("-", 70), "\n\n", sep = "")

forest_data_cog <- data.frame(
  Quartile = factor(paste0("Q", 1:4, " (", 
                           c("Lowest", "Low-Med", "Med-High", "Highest"), ")"),
                    levels = paste0("Q", 4:1, " (", 
                                    c("Highest", "Med-High", "Low-Med", "Lowest"), ")")),
  Beta = sapply(stratified_cog_results, function(x) x$beta),
  CI_lower = sapply(stratified_cog_results, function(x) x$CI_lower),
  CI_upper = sapply(stratified_cog_results, function(x) x$CI_upper),
  P_value = sapply(stratified_cog_results, function(x) x$p_value)
)

forest_data_cog <- forest_data_cog[4:1, ]

p_forest_cog <- ggplot(forest_data_cog, aes(x = Beta, y = Quartile)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_errorbar(aes(xmin = CI_lower, xmax = CI_upper), 
                width = 0.2, linewidth = 0.8, color = "#2C3E50", orientation = "y") +
  geom_point(size = 4, color = "#3498DB") +
  labs(
    title = "Effect of Arsenic on Cognitive Function by RBC Folate Quartile",
    subtitle = "Standardized coefficients (95% CI) from Survey-Weighted Linear Regression\nAge 60+ Only, N = 489",
    x = "coefficient (Change in Cognitive Z-Score)",
    y = "RBC Folate Quartile"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  ) +
  annotate("text", x = -0.8, y = 1, 
           label = "Q4: p = 0.025", 
           hjust = 0, vjust = 0, size = 3.5, color = "#E74C3C", fontface = "bold")

ggsave("figures/Figure2_Forest_Cognition.png", p_forest_cog, 
       width = 10, height = 6, dpi = 300)
cat("Figure 2 saved: Forest plot - Cognition\n\n")

cat("8D. CREATING SAMPLE FLOW DIAGRAM\n")
cat(rep("-", 70), "\n\n", sep = "")

flow_data <- data.frame(
  Step = c("1. Combined Cycles", "2. With Arsenic Data", "3. With Depression Data",
           "4. With RBC Folate", "5. Complete Covariates", 
           "6. FINAL: Depression Analysis",
           "7. 2013-14 Age 60+", "8. FINAL: Cognitive Analysis"),
  N = c(19429, 5479, 10440, 16072, 3100, 3004, 1841, 489),
  Excluded = c(0, 13950, 8989, 3357, 13372, 96, 17588, 1352)
)

png("figures/Figure3_Sample_Flow.png", width = 10, height = 8, units = "in", res = 300)
par(mar = c(1, 1, 3, 1))
plot.new()
plot.window(xlim = c(0, 10), ylim = c(0, 10))

text(5, 9.5, "Sample Flow Diagram", cex = 1.8, font = 2)

text(2.5, 8.5, "DEPRESSION ANALYSIS", cex = 1.2, font = 2, col = "#E74C3C")

rect(1, 7.5, 4, 8.2, col = "#ECF0F1", border = "#2C3E50", lwd = 2)
text(2.5, 7.85, sprintf("Combined 2013-14 & 2017-18\nN = %d", 19429), cex = 0.9)

arrows(2.5, 7.5, 2.5, 7, lwd = 2, length = 0.1)
text(3.8, 7.25, sprintf("Excluded: %d\nNo arsenic", 13950), cex = 0.7, adj = 0)

rect(1, 6.3, 4, 7, col = "#ECF0F1", border = "#2C3E50", lwd = 2)
text(2.5, 6.65, sprintf("With Arsenic Data\nN = %d", 5479), cex = 0.9)

arrows(2.5, 6.3, 2.5, 5.8, lwd = 2, length = 0.1)
text(3.8, 6.05, sprintf("Excluded: %d\nNo PHQ-9", 2475), cex = 0.7, adj = 0)

rect(1, 5.1, 4, 5.8, col = "#ECF0F1", border = "#2C3E50", lwd = 2)
text(2.5, 5.45, sprintf("With Depression & Folate\nN = %d", 3004), cex = 0.9)

rect(0.8, 4.4, 4.2, 5, col = "#E74C3C", border = "#C0392B", lwd = 3)
text(2.5, 4.7, sprintf("FINAL DEPRESSION\nN = %d", 3004), cex = 1, font = 2, col = "white")

text(7.5, 8.5, "COGNITIVE ANALYSIS", cex = 1.2, font = 2, col = "#3498DB")

rect(6, 7.5, 9, 8.2, col = "#ECF0F1", border = "#2C3E50", lwd = 2)
text(7.5, 7.85, sprintf("2013-14 Only, Age 60+\nN = %d", 1841), cex = 0.9)

arrows(7.5, 7.5, 7.5, 7, lwd = 2, length = 0.1)
text(8.8, 7.25, sprintf("Excluded: %d\nMissing tests", 1352), cex = 0.7, adj = 0)

rect(6, 6.3, 9, 7, col = "#ECF0F1", border = "#2C3E50", lwd = 2)
text(7.5, 6.65, sprintf("Complete Cognitive Data\nN = %d", 489), cex = 0.9)

rect(5.8, 5.7, 9.2, 6.2, col = "#3498DB", border = "#2980B9", lwd = 3)
text(7.5, 5.95, sprintf("FINAL COGNITIVE\nN = %d", 489), cex = 1, font = 2, col = "white")

rect(0.5, 0.5, 9.5, 3.5, col = "#F8F9FA", border = "#34495E", lwd = 2)
text(5, 3.2, "KEY FINDINGS", cex = 1.3, font = 2)

text(5, 2.6, "Depression (N = 3,004):", cex = 1, font = 2, col = "#E74C3C")
text(5, 2.2, "No main effect: OR = 1.18, p = 0.508", cex = 0.9)
text(5, 1.9, "No interaction: p = 0.728", cex = 0.9)

text(5, 1.4, "Cognition (N = 489, Age 60+):", cex = 1, font = 2, col = "#3498DB")
text(5, 1.0, "No main effect: beta = 0.048, p = 0.698", cex = 0.9)
text(5, 0.7, "Significant in Q4 only: beta = -0.676, p = 0.025", cex = 0.9)

dev.off()

cat("Figure 3 saved: Sample flow diagram\n\n")

cat("8E. SAVING ANALYSIS DATASETS\n")
cat(rep("-", 70), "\n\n", sep = "")

saveRDS(analytic_depression, "data/analytic_depression.rds")
cat("Depression dataset saved (N = 3,004)\n")

saveRDS(analytic_cognitive_complete, "data/analytic_cognitive.rds")
cat("Cognitive dataset saved (N = 489)\n")

saveRDS(list(
  model1 = model1_binary,
  model2 = model2_binary,
  model3 = model3_binary,
  model4 = model4_binary,
  interaction = interaction_binary,
  stratified = stratified_results
), "data/depression_models.rds")
cat("Depression models saved\n")

saveRDS(list(
  model1 = cog_model1,
  model2 = cog_model2,
  model3 = cog_model3,
  model4 = cog_model4,
  interaction = cog_interaction,
  stratified = stratified_cog_results
), "data/cognitive_models.rds")
cat("Cognitive models saved\n\n")

cat("8F. CREATING SUMMARY REPORT\n")
cat(rep("-", 70), "\n\n", sep = "")

sink("Project_Summary_Report.txt")

cat(rep("=", 80), "\n", sep = "")
cat("FOLATE AS A BUFFER: ARSENIC, ONE-CARBON METABOLISM, AND MENTAL HEALTH\n")
cat("NHANES 2013-2014 & 2017-2018\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("RESEARCH QUESTION:\n")
cat("Does folate status modify the effect of inorganic arsenic on:\n")
cat("  (1) Depressive symptoms (PHQ-9)\n")
cat("  (2) Cognitive function (composite z-score)\n\n")

cat(rep("=", 80), "\n", sep = "")
cat("SAMPLE CHARACTERISTICS\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("Depression Analysis:\n")
cat(sprintf("  Total N: %d\n", nrow(analytic_depression)))
cat(sprintf("  Depression cases (PHQ-9 >=10): %d (%.1f%%)\n", 
            sum(analytic_depression$Depression_binary == 1, na.rm = TRUE),
            100 * mean(analytic_depression$Depression_binary == 1, na.rm = TRUE)))
cat(sprintf("  Median arsenic: %.2f μg/L (IQR: %.2f - %.2f)\n",
            median(analytic_depression$iAs, na.rm = TRUE),
            quantile(analytic_depression$iAs, 0.25, na.rm = TRUE),
            quantile(analytic_depression$iAs, 0.75, na.rm = TRUE)))
cat(sprintf("  Mean RBC folate: %.2f nmol/L RBC (SD: %.2f)\n\n",
            mean(analytic_depression$LBXRBCSI, na.rm = TRUE),
            sd(analytic_depression$LBXRBCSI, na.rm = TRUE)))

cat("Cognitive Analysis:\n")
cat(sprintf("  Total N: %d (2013-2014, age 60+)\n", nrow(analytic_cognitive_complete)))
cat(sprintf("  Age range: %d - %d years\n",
            min(analytic_cognitive_complete$RIDAGEYR),
            max(analytic_cognitive_complete$RIDAGEYR)))
cat(sprintf("  Mean cognitive composite: %.3f (SD: %.3f)\n\n",
            mean(analytic_cognitive_complete$Cognitive_composite, na.rm = TRUE),
            sd(analytic_cognitive_complete$Cognitive_composite, na.rm = TRUE)))

cat(rep("=", 80), "\n", sep = "")
cat("MAIN FINDINGS\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("OUTCOME 1: DEPRESSION\n")
cat("  Main Effect (Fully Adjusted):\n")
cat("    OR = 1.182 (95% CI: 0.727 - 1.923), p = 0.508\n")
cat("    Interpretation: NO significant association\n\n")

cat("  Effect Modification by RBC Folate:\n")
cat("    Interaction p-value: 0.728\n")
cat("    Interpretation: NO evidence of effect modification\n\n")

cat("OUTCOME 2: COGNITIVE FUNCTION\n")
cat("  Main Effect (Fully Adjusted):\n")
cat("    beta = 0.048 (95% CI: -0.346 to 0.443), p = 0.698\n")
cat("    Interpretation: NO significant association\n\n")

cat("  Effect Modification by RBC Folate:\n")
cat("    Interaction p-value: 0.651\n")
cat("    Q4 (highest folate): beta = -0.676, p = 0.025\n")
cat("    Interpretation: Unexpected finding in highest folate group\n\n")

cat(rep("=", 80), "\n", sep = "")
cat("CONCLUSION\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("In this US population with low arsenic exposure:\n")
cat("  NO main effect of arsenic on depression or cognition\n")
cat("  NO evidence that folate buffers arsenic effects\n")
cat("  Unexpected protective effect in high-folate elders (cognition)\n\n")

cat("Likely explanations:\n")
cat("  US arsenic levels too low (median 0.9 μg/L vs >50 μg/L in high-exposure areas)\n")
cat("  Folate fortification limits variability\n")
cat("  Cross-sectional design cannot establish temporality\n\n")

cat("Clinical relevance:\n")
cat("  Results do NOT support folate supplementation for arsenic neurotoxicity\n")
cat("    in low-exposure populations\n")
cat("  Future research needed in high-exposure settings\n\n")

cat(rep("=", 80), "\n", sep = "")
cat("FILES GENERATED:\n")
cat("  Tables: 6 CSV files in tables/\n")
cat("  Figures: 3 PNG files in figures/\n")
cat("  Data: 4 RDS files in data/\n")
cat(rep("=", 80), "\n", sep = "")

sink()

cat("Summary report saved: Project_Summary_Report.txt\n\n")



# Load required libraries
library(survey)
library(dplyr)

# Load saved models
depression_models <- readRDS("data/depression_models.rds")
cognitive_models <- readRDS("data/cognitive_models.rds")

# Load the analytic datasets
analytic_depression <- readRDS("data/analytic_depression.rds")
analytic_cognitive_complete <- readRDS("data/analytic_cognitive.rds")

# Function to extract complete coefficients
extract_complete_coefs <- function(model, model_name) {
  summ <- summary(model)
  coef_table <- as.data.frame(summ$coefficients)
  coef_table$Variable <- rownames(coef_table)
  coef_table$Model <- model_name
  
  # Rename columns for clarity
  names(coef_table)[1:4] <- c("Estimate", "Std_Error", "t_value", "p_value")
  
  # Calculate OR and CI for logistic models
  if ("quasibinomial" %in% class(family(model))) {
    coef_table$OR <- exp(coef_table$Estimate)
    coef_table$CI_lower <- exp(coef_table$Estimate - 1.96 * coef_table$Std_Error)
    coef_table$CI_upper <- exp(coef_table$Estimate + 1.96 * coef_table$Std_Error)
  } else {
    coef_table$CI_lower <- coef_table$Estimate - 1.96 * coef_table$Std_Error
    coef_table$CI_upper <- coef_table$Estimate + 1.96 * coef_table$Std_Error
  }
  
  # Reorder columns
  coef_table <- coef_table[, c("Model", "Variable", "Estimate", "Std_Error", 
                               "t_value", "p_value", "CI_lower", "CI_upper", 
                               if("OR" %in% names(coef_table)) "OR")]
  
  return(coef_table)
}

# ============================================================================
# EXTRACT DEPRESSION MAIN EFFECT MODELS
# ============================================================================
cat("\n=== EXTRACTING DEPRESSION MAIN EFFECT MODELS ===\n")

depression_complete <- list(
  Model1 = extract_complete_coefs(depression_models$model1, "Model 1: Unadjusted"),
  Model2 = extract_complete_coefs(depression_models$model2, "Model 2: + Demographics"),
  Model3 = extract_complete_coefs(depression_models$model3, "Model 3: + SES"),
  Model4 = extract_complete_coefs(depression_models$model4, "Model 4: Fully Adjusted"),
  Interaction = extract_complete_coefs(depression_models$interaction, "Interaction Model")
)

# Combine and save
all_depression_main <- do.call(rbind, depression_complete)
write.csv(all_depression_main, "tables/TableA1_Depression_MainEffects_Complete.csv", row.names = FALSE)

cat("Main effects saved!\n")
print(head(all_depression_main, 20))


cat("\n=== EXTRACTING STRATIFIED DEPRESSION MODELS ===\n")

# Re-create survey design
svy_depression <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTCOMBINED,
  nest = TRUE,
  data = analytic_depression
)

stratified_complete <- list()

for (i in 1:4) {
  cat(sprintf("\nExtracting Quartile %d...\n", i))
  
  # Subset design
  svy_subset <- subset(svy_depression, RBC_folate_quart == i)
  
  # Run model
  model_subset <- svyglm(
    Depression_binary ~ iAs_log + RIDAGEYR + RIAGENDR + RIDRETH3 + 
      INDFMPIR + AsB_log,
    design = svy_subset,
    family = quasibinomial()
  )
  
  # Extract coefficients
  stratified_complete[[paste0("Q", i)]] <- extract_complete_coefs(
    model_subset, 
    paste0("Stratified Q", i)
  )
  
  cat(sprintf("Q%d complete - %d coefficients extracted\n", i, nrow(stratified_complete[[paste0("Q", i)]])))
}

# Combine and save
all_depression_stratified <- do.call(rbind, stratified_complete)
write.csv(all_depression_stratified, "tables/TableA2_Depression_Stratified_Complete.csv", row.names = FALSE)

cat("\nStratified models saved!\n")
print(head(all_depression_stratified, 30))


cat("\n=== EXTRACTING COGNITIVE MAIN EFFECT MODELS ===\n")

cognitive_complete <- list(
  Model1 = extract_complete_coefs(cognitive_models$model1, "Model 1: Unadjusted"),
  Model2 = extract_complete_coefs(cognitive_models$model2, "Model 2: + Demographics"),
  Model3 = extract_complete_coefs(cognitive_models$model3, "Model 3: + SES"),
  Model4 = extract_complete_coefs(cognitive_models$model4, "Model 4: Fully Adjusted"),
  Interaction = extract_complete_coefs(cognitive_models$interaction, "Interaction Model")
)

all_cognitive_main <- do.call(rbind, cognitive_complete)
write.csv(all_cognitive_main, "tables/TableA3_Cognitive_MainEffects_Complete.csv", row.names = FALSE)

cat("Cognitive main effects saved!\n")
print(head(all_cognitive_main, 20))


cat("\n=== EXTRACTING STRATIFIED COGNITIVE MODELS ===\n")

# Re-create cognitive survey design
svy_cognitive <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTCOMBINED,
  nest = TRUE,
  data = analytic_cognitive_complete
)

cognitive_stratified_complete <- list()

for (i in 1:4) {
  cat(sprintf("\nExtracting Cognitive Quartile %d...\n", i))
  
  # Subset design
  svy_subset <- subset(svy_cognitive, RBC_folate_quart == i)
  
  # Check sample size
  n_subset <- sum(svy_cognitive$variables$RBC_folate_quart == i, na.rm = TRUE)
  cat(sprintf("  Sample size: %d\n", n_subset))
  
  if (n_subset >= 30) {  # Only run if sufficient sample size
    # Run model
    model_subset <- svyglm(
      Cognitive_composite ~ iAs_log + RIDAGEYR + RIAGENDR + RIDRETH3 + 
        INDFMPIR + AsB_log,
      design = svy_subset,
      family = gaussian()
    )
    
    # Extract coefficients
    cognitive_stratified_complete[[paste0("Q", i)]] <- extract_complete_coefs(
      model_subset, 
      paste0("Cognitive Stratified Q", i)
    )
    
    cat(sprintf("  Q%d complete - %d coefficients extracted\n", i, 
                nrow(cognitive_stratified_complete[[paste0("Q", i)]])))
  } else {
    cat(sprintf("  Q%d skipped - insufficient sample size\n", i))
  }
}

# Combine and save
all_cognitive_stratified <- do.call(rbind, cognitive_stratified_complete)
write.csv(all_cognitive_stratified, "tables/TableA4_Cognitive_Stratified_Complete.csv", row.names = FALSE)

cat("\nCognitive stratified models saved!\n")
print(head(all_cognitive_stratified, 30))

cat("\n=== CREATING SUMMARY DOCUMENT ===\n")

sink("tables/Complete_Model_Coefficients_Summary.txt")

cat(rep("=", 80), "\n", sep = "")
cat("COMPLETE MODEL COEFFICIENTS - ALL ANALYSES\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("FILES CREATED:\n")
cat("1. TableA1_Depression_MainEffects_Complete.csv\n")
cat("2. TableA2_Depression_Stratified_Complete.csv\n")
cat("3. TableA3_Cognitive_MainEffects_Complete.csv\n")
cat("4. TableA4_Cognitive_Stratified_Complete.csv\n\n")

cat(rep("=", 80), "\n", sep = "")
cat("DEPRESSION MODEL 4 (FULLY ADJUSTED) - COMPLETE OUTPUT\n")
cat(rep("=", 80), "\n\n", sep = "")
print(depression_complete$Model4)

cat("\n\n")
cat(rep("=", 80), "\n", sep = "")
cat("DEPRESSION STRATIFIED Q1 - COMPLETE OUTPUT\n")
cat(rep("=", 80), "\n\n", sep = "")
print(stratified_complete$Q1)

cat("\n\n")
cat(rep("=", 80), "\n", sep = "")
cat("COGNITIVE MODEL 4 (FULLY ADJUSTED) - COMPLETE OUTPUT\n")
cat(rep("=", 80), "\n\n", sep = "")
print(cognitive_complete$Model4)

cat("\n\n")
cat(rep("=", 80), "\n", sep = "")
cat("TOTAL COEFFICIENTS EXTRACTED\n")
cat(rep("=", 80), "\n\n", sep = "")
cat("Depression main effects:", nrow(all_depression_main), "\n")
cat("Depression stratified:", nrow(all_depression_stratified), "\n")
cat("Cognitive main effects:", nrow(all_cognitive_main), "\n")
cat("Cognitive stratified:", nrow(all_cognitive_stratified), "\n")
cat("TOTAL:", nrow(all_depression_main) + nrow(all_depression_stratified) + 
      nrow(all_cognitive_main) + nrow(all_cognitive_stratified), "\n")

sink()

cat("\n=== ALL COMPLETE! ===\n")
cat("Check the 'tables/' folder for:\n")
cat("  - 4 CSV files with complete coefficients\n")
cat("  - 1 TXT summary document\n\n")

# Display final summary
cat("EXTRACTION COMPLETE!\n")
cat(sprintf("Depression main: %d coefficients\n", nrow(all_depression_main)))
cat(sprintf("Depression stratified: %d coefficients\n", nrow(all_depression_stratified)))
cat(sprintf("Cognitive main: %d coefficients\n", nrow(all_cognitive_main)))
cat(sprintf("Cognitive stratified: %d coefficients\n", nrow(all_cognitive_stratified)))

cat("\n=== SAMPLE FROM EACH FILE ===\n\n")
cat("Depression Model 4 - iAs_log coefficient:\n")
print(all_depression_main[all_depression_main$Model == "Model 4: Fully Adjusted" & 
                            all_depression_main$Variable == "iAs_log", ])

cat("\n\nDepression Stratified Q4 - iAs_log coefficient:\n")
print(all_depression_stratified[all_depression_stratified$Model == "Stratified Q4" & 
                                  all_depression_stratified$Variable == "iAs_log", ])

cat("\n\nCognitive Model 4 - iAs_log coefficient:\n")
print(all_cognitive_main[all_cognitive_main$Model == "Model 4: Fully Adjusted" & 
                           all_cognitive_main$Variable == "iAs_log", ])


# Revised DAG

library(dagitty)
library(ggdag)
library(ggplot2)

dag_depression <- dagitty('dag {
  iAs [exposure, pos="0,0"]
  Depression [outcome, pos="4,0"]
  Age [pos="2,2"]
  Sex [pos="1,2"]
  Race_SES [pos="3,2"]
  AsB [pos="1,-1"]
  Diet [pos="2,-2"]
  Folate [pos="2,1"]
  
  Age -> iAs
  Age -> Depression
  Sex -> iAs
  Sex -> Depression
  Race_SES -> iAs
  Race_SES -> Depression
  AsB -> iAs
  AsB -> Depression
  Diet -> iAs
  Diet -> Depression
  Diet -> Folate
  iAs -> Depression
  Folate -> Depression
}')

cat("=== MINIMAL SUFFICIENT ADJUSTMENT SET ===\n")
print(adjustmentSets(dag_depression, exposure = "iAs", outcome = "Depression"))

cat("\n=== ALL PATHS FROM iAs TO Depression ===\n")
print(paths(dag_depression, from = "iAs", to = "Depression"))

# Basic plot
plot(dag_depression)

# Tidy the DAG for better plotting
tidy_dag <- dag_depression %>%
  tidy_dagitty() %>%
  mutate(
    node_type = case_when(
      name == "iAs" ~ "Exposure",
      name == "Depression" ~ "Outcome",
      name == "Folate" ~ "Effect Modifier",
      name == "Diet" ~ "Unmeasured",
      TRUE ~ "Confounder"
    )
  )

dag_plot <- ggplot(tidy_dag, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(
    edge_color = "gray50",
    edge_width = 1.2,
    arrow_directed = grid::arrow(length = grid::unit(10, "pt"), type = "closed")
  ) +
  geom_dag_point(aes(color = node_type), size = 20, show.legend = TRUE) +
  geom_dag_text(
    aes(label = name),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  scale_color_manual(
    name = "Node Type",
    values = c(
      "Exposure" = "#e41a1c",
      "Outcome" = "#377eb8",
      "Effect Modifier" = "#4daf4a",
      "Confounder" = "#984ea3",
      "Unmeasured" = "#ff7f00"
    )
  ) +
  theme_dag_blank() +
  labs(
    title = "Causal DAG: Low-Level Arsenic Exposure and Depression Risk",
    subtitle = "Effect Modification by RBC Folate Status"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

print(dag_plot)

ggsave("figures/DAG_depression_main.png", dag_plot, 
       width = 10, height = 8, dpi = 300, bg = "white")

dag_cognition <- dagitty('dag {
  iAs [exposure, pos="0,0"]
  Cognition [outcome, pos="4,0"]
  Age [pos="2,2"]
  Sex [pos="1,2"]
  Race_SES [pos="3,2"]
  AsB [pos="1,-1"]
  Diet [pos="2,-2"]
  Folate [pos="2,1"]
  
  Age -> iAs
  Age -> Cognition
  Sex -> iAs
  Sex -> Cognition
  Race_SES -> iAs
  Race_SES -> Cognition
  AsB -> iAs
  AsB -> Cognition
  Diet -> iAs
  Diet -> Cognition
  Diet -> Folate
  iAs -> Cognition
  Folate -> Cognition
}')

# Plot cognition DAG
tidy_dag_cog <- dag_cognition %>%
  tidy_dagitty() %>%
  mutate(
    node_type = case_when(
      name == "iAs" ~ "Exposure",
      name == "Cognition" ~ "Outcome",
      name == "Folate" ~ "Effect Modifier",
      name == "Diet" ~ "Unmeasured",
      TRUE ~ "Confounder"
    )
  )

dag_plot_cog <- ggplot(tidy_dag_cog, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(
    edge_color = "gray50",
    edge_width = 1.2,
    arrow_directed = grid::arrow(length = grid::unit(10, "pt"), type = "closed")
  ) +
  geom_dag_point(aes(color = node_type), size = 20, show.legend = TRUE) +
  geom_dag_text(
    aes(label = name),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  scale_color_manual(
    name = "Node Type",
    values = c(
      "Exposure" = "#e41a1c",
      "Outcome" = "#377eb8",
      "Effect Modifier" = "#4daf4a",
      "Confounder" = "#984ea3",
      "Unmeasured" = "#ff7f00"
    )
  ) +
  theme_dag_blank() +
  labs(
    title = "Causal DAG: Low-Level Arsenic Exposure and Cognitive Function",
    subtitle = "Effect Modification by RBC Folate Status"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

print(dag_plot_cog)

ggsave("figures/DAG_cognition_main.png", dag_plot_cog,
       width = 10, height = 8, dpi = 300, bg = "white")

dag_detailed <- dagitty('dag {
  iAs [exposure, pos="0,0"]
  Depression [outcome, pos="5,0"]
  Age [pos="2.5,3"]
  Sex [pos="1.5,3"]
  Race_SES [pos="3.5,3"]
  AsB [pos="1,-1.5"]
  Diet [pos="2.5,-2"]
  MTHFR [pos="3.5,-2"]
  VitB12 [pos="1.5,-2"]
  Folate [pos="2.5,1.5"]
  
  Age -> iAs
  Age -> Depression
  Sex -> iAs
  Sex -> Depression
  Race_SES -> iAs
  Race_SES -> Depression
  AsB -> iAs
  AsB -> Depression
  Diet -> iAs
  Diet -> Depression
  Diet -> Folate
  MTHFR -> Folate
  MTHFR -> Depression
  VitB12 -> Folate
  VitB12 -> Depression
  VitB12 -> iAs
  iAs -> Depression
  Folate -> Depression
}')

# Plot detailed DAG
tidy_dag_detail <- dag_detailed %>%
  tidy_dagitty() %>%
  mutate(
    node_type = case_when(
      name == "iAs" ~ "Exposure",
      name == "Depression" ~ "Outcome",
      name == "Folate" ~ "Effect Modifier",
      name %in% c("Diet", "MTHFR", "VitB12") ~ "Unmeasured",
      TRUE ~ "Confounder"
    )
  )

dag_plot_detail <- ggplot(tidy_dag_detail, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(
    edge_color = "gray50",
    edge_width = 1,
    arrow_directed = grid::arrow(length = grid::unit(8, "pt"), type = "closed")
  ) +
  geom_dag_point(aes(color = node_type), size = 18, show.legend = TRUE) +
  geom_dag_text(
    aes(label = name),
    color = "white",
    fontface = "bold",
    size = 3.5
  ) +
  scale_color_manual(
    name = "Node Type",
    values = c(
      "Exposure" = "#e41a1c",
      "Outcome" = "#377eb8",
      "Effect Modifier" = "#4daf4a",
      "Confounder" = "#984ea3",
      "Unmeasured" = "#ff7f00"
    )
  ) +
  theme_dag_blank() +
  labs(
    title = "Detailed Causal DAG with Unmeasured Confounders",
    subtitle = "Orange nodes indicate unmeasured variables"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

print(dag_plot_detail)

ggsave("figures/DAG_detailed_unmeasured.png", dag_plot_detail,
       width = 11, height = 9, dpi = 300, bg = "white")

backdoor_plot <- dag_depression %>%
  ggdag_paths(from = "iAs", to = "Depression") +
  theme_dag_blank() +
  labs(
    title = "All Paths: iAs → Depression",
    subtitle = "Multiple backdoor paths require adjustment"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  )

print(backdoor_plot)

ggsave("figures/DAG_backdoor_paths.png", backdoor_plot,
       width = 10, height = 7, dpi = 300, bg = "white")

adjustment_plot <- dag_depression %>%
  ggdag_adjustment_set(
    exposure = "iAs",
    outcome = "Depression"
  ) +
  theme_dag_blank() +
  labs(
    title = "Adjustment Strategy",
    subtitle = "Variables adjusted to block backdoor paths"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  )

print(adjustment_plot)

ggsave("figures/DAG_adjustment_set.png", adjustment_plot,
       width = 10, height = 7, dpi = 300, bg = "white")

cat("MINIMAL SUFFICIENT ADJUSTMENT SET:\n")
print(adjustmentSets(dag_depression))

cat("\n\nALL PATHS FROM iAs TO Depression:\n")
paths_result <- paths(dag_depression, from = "iAs", to = "Depression")
print(paths_result)

