# 10_participant_descriptives: who the participants are (descriptive), and
# the exclusion counts.

source("R/_setup.R")

pall <- readRDS(file.path(derived_dir, "participants.rds"))
included <- pall %>% filter(included)
mods <- readRDS(file.path(derived_dir, "moderators.rds")) %>%
  semi_join(included, by = "pid")
# "none" (prefer not to say) counts as no answer
clean_sv <- function(s) s %>%
  mutate(across(where(is.character), ~ na_if(str_trim(.x), "none")))
sv <- read_survey(data_dir) %>% semi_join(included, by = "pid") %>% clean_sv()
n_inc <- nrow(included)
n_sv  <- nrow(sv)

n_ans <- function(x) sum(!is.na(x))
yrs  <- sv$q_years_analysis
exp4 <- suppressWarnings(as.integer(sv$q_expertise_oci))

# expertise shares among surveyed included participants
joint <- sum(yrs %in% c("3-5", "6-10", "More than 10") & exp4 >= 3, na.rm = TRUE)
grad_or_6 <- sum(sv$q_education %in% c("Master's", "Doctorate") |
                   yrs %in% c("6-10", "More than 10"), na.rm = TRUE)
write_csv(tibble(measure = c("3+ years and Proficient+ (joint)",
                             "Master's/Doctorate or 6+ years",
                             "Proficient+ (self-rated)"),
                 n = c(joint, grad_or_6, sum(exp4 >= 3, na.rm = TRUE)),
                 of = c(n_ans(yrs), n_ans(yrs), n_ans(exp4))),
          file.path(out_tables, str_glue("participant_expert_joint_{cfg$set}.csv")))

# skill battery among included, full form only
sk <- mods$skill_score8[!is.na(mods$skill_score8)]
write_csv(tibble(n_included = n_inc, n_scored = length(sk),
                 items = length(skill_keys),
                 min = min(sk), median = median(sk),
                 p25 = unname(quantile(sk, .25)),
                 p75 = unname(quantile(sk, .75))),
          file.path(out_tables, str_glue("skill_battery_{cfg$set}.csv")))

# Exclusions by criterion and assigned lean; decision "all" rows carry the
# per-criterion totals.
crit_map <- c(`no AI engagement` = "no substantive AI engagement")
exd <- pall %>% filter(!included)
stopifnot("unmapped exclusion label" = all(exd$exclusion %in% names(crit_map)))
exd <- exd %>% mutate(criterion = unname(crit_map[exclusion]))
excl <- bind_rows(
  exd %>% count(criterion) %>% mutate(decision = "all", side = "all"),
  exd %>%
    pivot_longer(all_of(paste0("lean_d", 1:5)), names_to = "decision",
                 values_to = "side", names_prefix = "lean_") %>%
    mutate(side = coalesce(side, "none")) %>%
    count(criterion, decision, side)) %>%
  select(criterion, decision, side, n)
write_csv(excl, file.path(out_tables, str_glue("exclusions_{cfg$set}.csv")))

# Balance rows: included vs the no-AI completers (excluded for no
# AI engagement). One row per categorical level (count and answered base per
# group) or per scale (mean, SD, base).
noai_p  <- pall %>% filter(!included)
sv_noai <- read_survey(data_dir) %>% semi_join(noai_p, by = "pid") %>% clean_sv()
mods_noai <- build_moderators(read_survey(data_dir), noai_p)

cat_row <- function(measure, level, f) {
  iv <- f(sv); nv <- f(sv_noai)
  tibble(measure, level, stat = "n_pct",
         inc_v1 = sum(iv, na.rm = TRUE), inc_v2 = NA_real_,
         inc_base = sum(!is.na(iv)),
         noai_v1 = sum(nv, na.rm = TRUE), noai_v2 = NA_real_,
         noai_base = sum(!is.na(nv)))
}
num_row <- function(measure, vi, vn) {
  vi <- vi[!is.na(vi)]; vn <- vn[!is.na(vn)]
  tibble(measure, level = "mean (SD)", stat = "mean_sd",
         inc_v1 = mean(vi), inc_v2 = sd(vi), inc_base = length(vi),
         noai_v1 = mean(vn), noai_v2 = sd(vn), noai_base = length(vn))
}
int4 <- function(x) suppressWarnings(as.integer(x))
any_of_lv <- function(x, lv) x %in% lv | if_else(is.na(x), NA, FALSE)

bal <- bind_rows(
  tibble(measure = "With a pre-task survey",
         level = "With a pre-task survey", stat = "n_pct",
         inc_v1 = n_sv, inc_v2 = NA_real_, inc_base = n_inc,
         noai_v1 = nrow(sv_noai), noai_v2 = NA_real_,
         noai_base = nrow(noai_p)),
  cat_row("Age band", "18--24", \(d) d$q_age_band == "18-24"),
  cat_row("Age band", "25--34", \(d) d$q_age_band == "25-34"),
  cat_row("Age band", "35--44", \(d) d$q_age_band == "35-44"),
  cat_row("Age band", "45--54", \(d) d$q_age_band == "45-54"),
  cat_row("Gender", "Man",        \(d) d$q_gender == "Man"),
  cat_row("Gender", "Woman",      \(d) d$q_gender == "Woman"),
  cat_row("Gender", "Non-binary", \(d) d$q_gender == "Non-binary"),
  cat_row("Education", "Master's or Doctorate",
          \(d) any_of_lv(d$q_education, c("Master's", "Doctorate"))),
  cat_row("Education", "Bachelor's", \(d) d$q_education == "Bachelor's"),
  cat_row("Education", "High school or some college",
          \(d) any_of_lv(d$q_education, c("High school", "Some college"))),
  cat_row("Field", "Data science / ML / analytics",
          \(d) d$field_bucket == "data science / ML / analytics"),
  cat_row("Field", "Computer science / software",
          \(d) d$field_bucket == "computer science / software"),
  cat_row("Field", "Math / engineering / physical sciences",
          \(d) d$field_bucket == "mathematics / engineering / physical sciences"),
  cat_row("Field", "Other applied fields",
          \(d) d$field_bucket == "other applied fields"),
  cat_row("Years in analysis roles", "6 or more",
          \(d) any_of_lv(d$q_years_analysis, c("6-10", "More than 10"))),
  cat_row("Years in analysis roles", "3--5", \(d) d$q_years_analysis == "3-5"),
  cat_row("Years in analysis roles", "2 or less",
          \(d) any_of_lv(d$q_years_analysis, c("1-2", "Less than 1"))),
  cat_row("Causal-inference expertise (self-rated)", "Proficient or Expert",
          \(d) int4(d$q_expertise_oci) >= 3),
  cat_row("Causal-inference expertise (self-rated)", "Basic",
          \(d) int4(d$q_expertise_oci) == 2),
  cat_row("Causal-inference expertise (self-rated)", "Novice",
          \(d) int4(d$q_expertise_oci) == 1),
  cat_row("AI use (days/week)", "4 or more",
          \(d) any_of_lv(d$q_ai_use_days, c("4-5", "6-7"))),
  cat_row("AI use (days/week)", "2--3", \(d) d$q_ai_use_days == "2-3"),
  cat_row("AI use (days/week)", "1 or less",
          \(d) any_of_lv(d$q_ai_use_days, c("0", "1"))),
  num_row("Trust in AI (3 items, 1--4)", mods$trust, mods_noai$trust),
  num_row("Open-minded thinking (AOT; 7 items, 1--7)", mods$aot, mods_noai$aot),
  num_row("Need for cognition (6 items, 1--5)", mods$ncs, mods_noai$ncs),
  num_row("Skill battery (8 items, 0--8)", mods$skill_score8, mods_noai$skill_score8))
write_csv(bal, file.path(out_tables, str_glue("participant_balance_{cfg$set}.csv")))
print(as.data.frame(bal), digits = 3, row.names = FALSE)
