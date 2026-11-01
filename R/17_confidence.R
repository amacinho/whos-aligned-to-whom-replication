# 17_confidence: post-task confidence that the recommendation is the right
# call (p_confidence, 0-100), descriptive.
#   1. confidence by group: included AI users vs no-AI completers
#   2. AI users: Spearman rho between the share of d1-d4 trials on the lean
#      side and confidence
#   3. AI users whose choices matched the lean on all four analytical
#      decisions vs the rest

source("R/_setup.R")

participants <- readRDS(file.path(derived_dir, "participants.rds"))
trials <- readRDS(file.path(derived_dir, "trials.rds"))
sv <- read_survey(data_dir) %>%
  transmute(pid, confidence = suppressWarnings(as.integer(p_confidence))) %>%
  filter(!is.na(confidence))

grp <- participants %>%
  mutate(group = case_when(included ~ "AI users (included)",
                           exclusion == "no AI engagement" ~ "no-AI completers",
                           TRUE ~ NA_character_)) %>%
  filter(!is.na(group)) %>%
  inner_join(sv, by = "pid")

by_group <- grp %>% group_by(group) %>%
  summarise(n = n(), mean = mean(confidence), sd = sd(confidence),
            median = median(confidence), .groups = "drop")
print(as.data.frame(by_group), digits = 3, row.names = FALSE)

align <- trials %>% group_by(pid) %>%
  summarise(n_trials = n(), align_rate = mean(HA == leanA), .groups = "drop")
ai <- grp %>% filter(group == "AI users (included)") %>%
  inner_join(align, by = "pid")
ct <- cor.test(ai$align_rate, ai$confidence, method = "spearman", exact = FALSE)

al4 <- trials %>% group_by(pid) %>%
  summarise(n_trials = n(), n_aligned = sum(HA == leanA), .groups = "drop") %>%
  inner_join(ai %>% select(pid, confidence), by = "pid") %>%
  mutate(all_four = n_trials == 4 & n_aligned == 4)

conf_ai   <- grp$confidence[grp$group == "AI users (included)"]
conf_noai <- grp$confidence[grp$group == "no-AI completers"]
summary_row <- tibble(
  n_ai = by_group$n[by_group$group == "AI users (included)"],
  mean_ai = by_group$mean[by_group$group == "AI users (included)"],
  median_ai = by_group$median[by_group$group == "AI users (included)"],
  n_noai = by_group$n[by_group$group == "no-AI completers"],
  mean_noai = by_group$mean[by_group$group == "no-AI completers"],
  median_noai = by_group$median[by_group$group == "no-AI completers"],
  q25_ai = quantile(conf_ai, .25), q75_ai = quantile(conf_ai, .75),
  q25_noai = quantile(conf_noai, .25), q75_noai = quantile(conf_noai, .75),
  n_all_four_aligned = sum(al4$all_four),
  mean_all_four_aligned = mean(al4$confidence[al4$all_four]),
  n_not_all_four = sum(!al4$all_four),
  mean_not_all_four = mean(al4$confidence[!al4$all_four]),
  spearman_rho = unname(ct$estimate), spearman_p = ct$p.value)
write_csv(summary_row, file.path(out_tables, paste0("confidence_summary_", cfg$set, ".csv")))
print(as.data.frame(summary_row), digits = 3, row.names = FALSE)
