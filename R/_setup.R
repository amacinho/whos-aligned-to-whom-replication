# Shared setup, sourced first by every script in R/. Run the scripts from the
# package root (run_all.R does this).

suppressPackageStartupMessages({
  library(tidyverse)
  library(clubSandwich)
})

if (!file.exists(file.path("data", "participants.csv")))
  stop("run from the package root (the folder that holds data/ and R/)")

# Analysis set. The confirmatory set is "main". The registered disengagement
# sensitivity reruns scripts 01-06 with IDLE_CUTOFF=50, which drops sessions
# whose idle share exceeds 50% of the work period; its outputs carry the set
# name "main_idle50".
set_name <- "main"
max_idle <- suppressWarnings(as.numeric(Sys.getenv("IDLE_CUTOFF", "")))
if (!is.na(max_idle)) set_name <- str_glue("{set_name}_idle{max_idle}")

data_dir    <- "data"
derived_dir <- file.path("output", "derived", set_name)
out_tables  <- file.path("output", "tables")
out_figures <- file.path("output", "figures")
for (d in c(derived_dir, out_tables, out_figures))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

cfg <- list(
  set              = set_name,
  max_pct_idle_env = max_idle,
  primary_pool     = c("d1", "d2", "d3", "d4"),
  missing_verdicts = c("", "unclear", "conflicting")
)
cat(sprintf("[analysis set: %s]\n", cfg$set))

# ---- readers -----------------------------------------------------------------
# Each reader checks that its key is unique.
read_participants <- function(data_dir) {
  p <- read_csv(file.path(data_dir, "participants.csv"), show_col_types = FALSE,
                col_types = cols(.default = col_double(), pid = col_character(),
                                 lean_d1 = col_character(), lean_d2 = col_character(),
                                 lean_d3 = col_character(), lean_d4 = col_character(),
                                 lean_d5 = col_character(),
                                 schedule_slot = col_integer(),
                                 prompted = col_logical()))
  stopifnot("duplicate pid in participants.csv" = !anyDuplicated(p$pid))
  p %>% mutate(lean = str_c(coalesce(lean_d1, ""), coalesce(lean_d2, ""),
                            coalesce(lean_d3, ""), coalesce(lean_d4, ""),
                            coalesce(lean_d5, "")), .after = lean_d5)
}

read_decisions <- function(data_dir) {
  d <- read_csv(file.path(data_dir, "decisions.csv"), show_col_types = FALSE,
                col_types = cols(.default = col_character()))
  stopifnot("duplicate (pid, decision) in decisions.csv" =
              !anyDuplicated(d[c("pid", "decision")]),
            "not exactly 5 decisions per participant" = all(table(d$pid) == 5))
  d
}

# Closed survey items, all read as character; scale construction below owns
# the numeric coercions.
read_survey <- function(data_dir) {
  s <- read_csv(file.path(data_dir, "survey.csv"), show_col_types = FALSE,
                col_types = cols(.default = col_character()))
  stopifnot("duplicate pid in survey.csv" = !anyDuplicated(s$pid))
  s
}

read_coded <- function(name)
  read_csv(file.path(data_dir, "coded", paste0(name, ".csv")),
           col_types = cols(.default = col_character()))

# Result table written by an earlier script. numbers.R, tables.R and the
# figure scripts read only these.
read_result_table <- function(name, tables_dir = out_tables, set = cfg$set) {
  f <- file.path(tables_dir, paste0(name, "_", set, ".csv"))
  if (!file.exists(f))
    stop("missing result table ", f, "; run the script that writes it")
  read_csv(f, show_col_types = FALSE)
}

# ---- sample and exclusions (preregistration, section 3) ------------------------
# Participants who never prompted the agent are excluded from the analysis sample (the no-AI group).
#   exclusion  NA for included participants, otherwise the reason
#   included   no exclusion applies; the row enters every analysis
# Deliverable numbers come with the interval converted to the percentage
# scale using the participant's own percent-per-unit factor
# (pct_change / point_estimate); a zero or sign-mismatched factor stays NA,
# which downstream treats as an unsettled interval.
annotate_participants <- function(raw, cfg) {
  raw %>%
    mutate(
      exclusion = case_when(
        !prompted ~ "no AI engagement",
        TRUE      ~ NA_character_),
      included = is.na(exclusion),
      scale_factor = if_else(!is.na(point_estimate) & point_estimate != 0 &
                               !is.na(pct_change) &
                               pct_change / point_estimate > 0,
                             pct_change / point_estimate, NA_real_),
      ci_low_pct  = ci_low  * scale_factor,
      ci_high_pct = ci_high * scale_factor)
}

# ---- survey scales ---------------------------------------------------------------
num  <- function(x) suppressWarnings(as.numeric(x))
rev5 <- function(x) 6 - num(x)  # reverse a 1-5 item
rev7 <- function(x) 8 - num(x)  # reverse a 1-7 item

# Skill battery answer keys. The short form asked only b1, b2 and b4; the full forms asked all eight. skill_score8 (full
# form only, NA for short-form respondents) is the registered score; a skipped
# item scores as incorrect. skill_score (the three common items) is the
# cross-form reference.
skill_keys <- c(q_b1_mean_reversion = "bounce",        q_b2_unobservables = "unmeasured",
                q_b3_mediator       = "remove_effect", q_b4_ci_meaning    = "procedure",
                q_b5_clustering     = "too_narrow",    q_b6_controls      = "poverty_only",
                q_b7_collider       = "spurious",      q_q3_code          = "interaction")
skill_common3 <- c("q_b1_mean_reversion", "q_b2_unobservables", "q_b4_ci_meaning")

score_skill <- function(df, keys = names(skill_keys)) rowSums(
  vapply(keys,
         \(k) str_trim(coalesce(df[[k]], "")) == skill_keys[[k]],
         logical(nrow(df))))

# full form administered = any of the five full-form-only items answered
skill_full_form <- function(df) rowSums(
  vapply(setdiff(names(skill_keys), skill_common3),
         \(k) str_trim(coalesce(df[[k]], "")) != "",
         logical(nrow(df)))) > 0

# Participant-level moderators and covariates, in natural units; models
# center and scale per fit.
build_moderators <- function(survey_n, sample_p) {
  survey_n %>%
    semi_join(sample_p, by = "pid") %>%
    mutate(skill_score  = score_skill(pick(everything()), skill_common3),
           skill_score8 = if_else(skill_full_form(pick(everything())),
                                  score_skill(pick(everything())),
                                  NA_real_)) %>%
    transmute(
      pid,
      skill_score, skill_score8,
      # trust: mean of three reliance items, each 1-4
      trust = (num(q_trust_reliable) + num(q_trust_integrity_rely) + num(q_trust_interests)) / 3,
      # NCS-6, items 3 and 4 reverse-coded
      ncs = (num(q_ncs1) + num(q_ncs2) + rev5(q_ncs3) + rev5(q_ncs4) +
             num(q_ncs5) + num(q_ncs6)) / 6,
      # AOT-7 (1-7 agree), items 4-7 reverse-coded
      aot = (num(q_aot1) + num(q_aot2) + num(q_aot3) + rev7(q_aot4) +
             rev7(q_aot5) + rev7(q_aot6) + rev7(q_aot7)) / 7,
      # ordinal bands -> midpoints
      age = recode(q_age_band, "18-24" = 21, "25-34" = 29.5, "35-44" = 39.5,
                   "45-54" = 49.5, "55-64" = 59.5, "65+" = 70,
                   .default = NA_real_),
      years_analysis = recode(q_years_analysis, "Less than 1" = 0.5, "1-2" = 1.5,
                              "3-5" = 4, "6-10" = 8, "More than 10" = 12,
                              .default = NA_real_),
      ai_use_days = recode(q_ai_use_days, "0" = 0, "1" = 1, "2-3" = 2.5,
                           "4-5" = 4.5, "6-7" = 6.5, .default = NA_real_),
      gender_man = as.numeric(q_gender == "Man"),
      education = recode(q_education, "High school" = 1, "Some college" = 2,
                         "Bachelor's" = 3, "Master's" = 4, "Doctorate" = 5,
                         .default = NA_real_),
      # self-rated causal-inference expertise, 1 = Novice .. 4 = Expert
      expertise_oci = num(q_expertise_oci))
}

# ---- moderator model ---------------------------------------------------------------
# One moderator: 1{H=A} ~ decision + leanA * X_centered, CR2 cluster-robust by
# participant, Satterthwaite dof. X is centered on the participant mean and,
# except for the gender indicator, scaled by the participant-level SD.
fit_moderator <- function(trials, mods, xname) {
  d <- trials %>%
    inner_join(mods %>% select(pid, X = all_of(xname)), by = "pid") %>%
    filter(!is.na(X))
  x_p <- d$X[!duplicated(d$pid)]
  d$Xc <- d$X - mean(x_p)
  if (xname != "gender_man") d$Xc <- d$Xc / sd(x_p)
  m  <- lm(HA ~ decision + leanA * Xc, data = d)
  ct <- coef_test(m, vcov = "CR2", cluster = d$pid, test = "Satterthwaite")
  ci <- conf_int(m, vcov = "CR2", cluster = d$pid, test = "Satterthwaite", level = .95)
  rows <- c("leanA", "leanA:Xc")
  tibble(moderator = xname,
         term     = c("flip at avg participant",
                      if (xname == "gender_man") "interaction (per unit X)"
                      else "interaction (per SD X)"),
         estimate = ct$beta[match(rows, ct$Coef)],
         se       = ct$SE[match(rows, ct$Coef)],
         ci_lo    = ci$CI_L[match(rows, ci$Coef)],
         ci_hi    = ci$CI_U[match(rows, ci$Coef)],
         df       = ct$df_Satt[match(rows, ct$Coef)],
         p_two    = ct$p_Satt[match(rows, ct$Coef)],
         n_part   = n_distinct(d$pid), n_trials = nrow(d))
}

# d5 recommendation score: roll out 1, extension 0.5, discontinue 0; anything
# else is unresolved (NA)
d5_score <- function(judge_choice) case_when(judge_choice == "A"         ~ 1,
                                             judge_choice == "extension" ~ 0.5,
                                             judge_choice == "B"         ~ 0,
                                             TRUE                        ~ NA_real_)

# Wilson score interval for k of n
wilson <- function(k, n, z = qnorm(.975)) {
  p <- k / n
  (p + z^2 / (2 * n) + c(-1, 1) * z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) /
    (1 + z^2 / n)
}

# Colors for the post-task ownership scale (display scale 1 = entirely the
# AI assistant's .. 6 = entirely mine)
ownership_cols <- c(`1` = "#7A3A04", `2` = "#DE7A11", `3` = "#FFE0B0",
                    `4` = "#A3CDEE", `5` = "#3E7CB1", `6` = "#0E2A47")
