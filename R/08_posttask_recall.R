# 08_posttask_recall: post-task recall of the delivered choices and estimate
# (descriptive), and the survey-vs-judge mismatch summary.
#
# For each decision: does the participant's post-task report of their choice
# match what they delivered (judge_choice)? Outcomes per trial:
#   match / mismatch  recalled code vs delivered, both codeable
#   no recall         picked "none"
#   other             an "Other" answer the human coding did not map to a side
#   unanswered        item left blank
# Coded "Other" answers (recall_other_codes.csv): qualifier-only uses the
# side picked alongside the text, maps-to-A/B uses that side.

source("R/_setup.R")

participants <- readRDS(file.path(derived_dir, "participants.rds"))
survey <- read_survey(data_dir)

recalled <- survey %>%
  select(pid,
         d1_ab = p_d1_ab, d2_ab = p_d2_ab, d3_ab = p_d3_ab, d4_ab = p_d4_ab,
         d5_ab = p_d5_ab,
         d1_raw = p_d1_choice, d2_raw = p_d2_choice, d3_raw = p_d3_choice,
         d4_raw = p_d4_choice, d5_raw = p_d5_recommendation,
         d1_picked = p_d1_ab_picked, d2_picked = p_d2_ab_picked,
         d3_picked = p_d3_ab_picked, d4_picked = p_d4_ab_picked,
         d5_picked = p_d5_ab_picked) %>%
  pivot_longer(-pid, names_to = c("decision", ".value"), names_sep = "_") %>%
  rename(recalled = ab, recalled_raw = raw, recalled_picked = picked)

adjud <- read_coded("recall_other_codes") %>% select(pid, decision, adj = label)

delivered <- read_decisions(data_dir) %>%
  transmute(pid, decision,
            delivered = case_when(judge_choice %in% c("A", "B") ~ judge_choice,
                                  judge_choice == "extension"   ~ "extension",
                                  TRUE                          ~ NA_character_))

recall <- recalled %>%
  inner_join(delivered, by = c("pid", "decision")) %>%
  inner_join(participants %>% select(pid, included, exclusion), by = "pid") %>%
  left_join(adjud, by = c("pid", "decision")) %>%
  mutate(
    recalled_adj = case_when(
      coalesce(adj, "") == "qualifier-only" ~ recalled_picked,
      coalesce(adj, "") == "maps-to-A"      ~ "A",
      coalesce(adj, "") == "maps-to-B"      ~ "B",
      TRUE                                  ~ recalled),
    outcome = case_when(
      coalesce(recalled_raw, "") == ""      ~ "unanswered",
      coalesce(recalled_adj, "") == "" &
        recalled_raw == "none"              ~ "no recall",
      coalesce(recalled_adj, "") == ""      ~ "other",
      is.na(delivered)                      ~ "delivered unresolved",
      recalled_adj == delivered             ~ "match",
      TRUE                                  ~ "mismatch"))
recall_cols <- function(d) d %>%
  select(pid, decision, included, exclusion, recalled_raw, recalled,
         recalled_picked, adj, recalled_adj, delivered, outcome)
write_csv(recall_cols(recall),
          file.path(out_tables, paste0("posttask_recall_all_", cfg$set, ".csv")))
recall <- recall %>% filter(included)
write_csv(recall_cols(recall),
          file.path(out_tables, paste0("posttask_recall_", cfg$set, ".csv")))
print(recall %>% count(decision, outcome) %>%
        pivot_wider(names_from = outcome, values_from = n, values_fill = 0) %>%
        as.data.frame(), row.names = FALSE)

# ---- estimate recall ---------------------------------------------------------
# The recalled number can be in percent, as a proportion, or in the
# participant's native units; each recall is read all three ways against the
# delivered percent estimate and the closest reading is kept.
est <- survey %>%
  transmute(pid,
            recalled_raw = suppressWarnings(as.numeric(p_estimate_point)),
            rec_ci_lo    = suppressWarnings(as.numeric(p_estimate_ci_low)),
            rec_ci_hi    = suppressWarnings(as.numeric(p_estimate_ci_high)),
            hedged       = coalesce(p_estimate_dontrecall, "") != "") %>%
  inner_join(participants %>%
               select(pid, included, delivered_pct = pct_change, scale_factor),
             by = "pid") %>%
  filter(!is.na(delivered_pct))

mult <- c(percent = 1, proportion = 100, native = NA)
best <- est %>%
  mutate(percent    = recalled_raw,
         proportion = recalled_raw * 100,
         native     = recalled_raw * scale_factor) %>%
  pivot_longer(c(percent, proportion, native),
               names_to = "reading", values_to = "recalled_pct") %>%
  filter(!is.na(recalled_pct)) %>%
  group_by(pid) %>%
  slice_min(abs(recalled_pct - delivered_pct), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(multiplier = if_else(reading == "native", scale_factor, mult[reading]),
         rec_lo_pct = rec_ci_lo * multiplier,
         rec_hi_pct = rec_ci_hi * multiplier,
         err = recalled_pct - delivered_pct)
est <- est %>%
  left_join(best %>% select(pid, reading, recalled_pct, rec_lo_pct, rec_hi_pct, err),
            by = "pid") %>%
  filter(included)
write_csv(est %>% select(pid, included, hedged, delivered_pct, recalled_raw,
                         reading, recalled_pct, rec_lo_pct, rec_hi_pct, err),
          file.path(out_tables, paste0("posttask_estimate_recall_", cfg$set, ".csv")))
cat("estimate recall:", sum(!is.na(est$recalled_raw)), "numeric recalls of",
    nrow(est), "surveyed\n")

# ---- survey vs LLM judge -------------------------------------------------------
# Trials where an included participant's survey-reported side disagrees with
# the LLM judge before human adjudication; judge_upheld = the adjudicated
# code kept the judge's value. Two universes: side_vs_side (both give a
# side) and with_unclear (judge "unclear" rows included).
dj <- read_decisions(data_dir) %>%
  select(pid, decision, judge_choice, judge_choice_machine)
mm <- read_result_table("posttask_recall_all") %>%
  filter(included) %>%
  inner_join(dj, by = c("pid", "decision")) %>%
  mutate(surv = recode(recalled_adj, E = "extension"),
         mach = judge_choice_machine) %>%
  filter(surv %in% c("A", "B", "extension"), !is.na(mach), surv != mach)
mm_row <- function(d, universe) tibble(
  universe, n = nrow(d),
  judge_upheld = sum(d$judge_choice == d$mach),
  corrected_to_survey = sum(d$judge_choice == d$surv),
  other = sum(d$judge_choice != d$mach & d$judge_choice != d$surv))
write_csv(bind_rows(mm_row(mm %>% filter(mach != "unclear"), "side_vs_side"),
                    mm_row(mm, "with_unclear")),
          file.path(out_tables, paste0("judge_mismatch_", cfg$set, ".csv")))
