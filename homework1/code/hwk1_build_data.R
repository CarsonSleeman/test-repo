library(tidyverse)

data_dir <- "../data"
results_dir <- "../results"
dir.create(results_dir, showWarnings = FALSE)

enroll_path <- file.path(data_dir, "CPSC_Enrollment_Info_2015_01.csv")
sa_path     <- file.path(data_dir, "MA_Cnty_SA_2015_01.csv")

enroll <- read_csv(enroll_path, show_col_types = FALSE) %>%
  rename(
    contract = `Contract Number`,
    plan_id  = `Plan ID`,
    ssa      = `SSA State County Code`,
    fips     = `FIPS State County Code`,
    state    = State,
    county   = County,
    enroll   = Enrollment
  ) %>%
  mutate(
    enroll   = parse_number(as.character(enroll)),
    contract = as.character(contract),
    plan_id  = as.character(plan_id),
    ssa      = as.character(ssa),
    fips     = as.character(fips)
  )

sa <- read_csv(sa_path, show_col_types = FALSE) %>%
  rename(
    contract  = `Contract ID`,
    plan_type = `Plan Type`,
    eghp      = EGHP,
    ssa       = SSA,
    fips      = FIPS,
    notes     = Notes
  ) %>%
  mutate(
    contract = as.character(contract),
    ssa = as.character(ssa),
    fips = as.character(fips),
    plan_type = as.character(plan_type),
    eghp = as.character(eghp),
    notes = as.character(notes)
  ) %>%
  distinct(contract, ssa, fips, .keep_all = TRUE)

merged <- enroll %>%
  inner_join(sa, by = c("contract", "ssa", "fips"))

table1 <- merged %>%
  distinct(contract, plan_id, plan_type) %>%
  count(plan_type, name = "n_plans") %>%
  arrange(desc(n_plans))

merged_clean <- merged %>%
  mutate(plan_id_num = suppressWarnings(as.integer(plan_id))) %>%
  filter(
    !str_to_lower(eghp) %in% c("y","yes","1","true"),
    !str_detect(str_to_upper(plan_type), "SNP"),
    !str_detect(str_to_upper(coalesce(notes, "")), "SNP"),
    !(plan_id_num >= 800 & plan_id_num <= 899)
  )

table2 <- merged_clean %>%
  distinct(contract, plan_id, plan_type) %>%
  count(plan_type, name = "n_plans") %>%
  arrange(desc(n_plans))

table3 <- merged %>%
  group_by(plan_type) %>%
  summarise(avg_enroll = mean(enroll, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_enroll))

write_csv(table1, file.path(results_dir, "table1_plan_count_all.csv"))
write_csv(table2, file.path(results_dir, "table2_plan_count_exclusions.csv"))
write_csv(table3, file.path(results_dir, "table3_avg_enroll_all.csv"))

cat("Done!\n")
