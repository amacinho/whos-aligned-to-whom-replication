# 04_h2_h3: d5 recommendation outcomes, H2 (policy shift) and H3
# (departure from the verdict the participant's own interval supports), with
# the registered H3 falsification checks.
#
# Recommendation scored 0 (discontinue), 0.5 (extension), 1 (roll out);
# an unresolved d5 is missing (participant out of H2 and H3). H2: one-sided
# Welch t across the d5 lean arms, Wilcoxon robustness check. H3: among
# settled intervals, one-sided Fisher exact on departure under incongruent vs
# congruent lean, Newcombe-Wilson 95% CI for the risk difference.
# Falsification: (i) two-sided Welch t of percent-scale estimates across d5
# arms; (ii) two-sided Fisher of the settlement rate across d5 arms.

source("R/_setup.R")

included <- readRDS(file.path(derived_dir, "participants.rds")) %>% filter(included)

d5 <- read_decisions(data_dir) %>%
  semi_join(included, by = "pid") %>%
  filter(decision == "d5") %>%
  mutate(score = d5_score(judge_choice),
         arm = if_else(lean == "A", "rollout-lean", "discontinue-lean"))

# percent-scale estimate and interval (see annotate_participants)
reported_effects <- included %>%
  select(pid, est = pct_change, lo = ci_low_pct, hi = ci_high_pct)

# ---- H2 -----------------------------------------------------------------------
h2   <- d5 %>% filter(!is.na(score))
roll <- h2 %>% filter(arm == "rollout-lean")     %>% pull(score)
disc <- h2 %>% filter(arm == "discontinue-lean") %>% pull(score)
cat(nrow(d5), "d5 trials,", sum(is.na(d5$score)), "unresolved\n")

welch      <- t.test(roll, disc)                               # two-sided 95% CI
welch_one  <- t.test(roll, disc, alternative = "greater")      # registered one-sided
wilcox_one <- wilcox.test(roll, disc, alternative = "greater",
                          correct = TRUE, exact = FALSE)
h2_res <- tibble(diff  = mean(roll) - mean(disc),
                 ci_lo = welch$conf.int[1], ci_hi = welch$conf.int[2],
                 p_one_welch = welch_one$p.value, p_one_wilcoxon = wilcox_one$p.value,
                 n_rollout_lean = length(roll), n_discontinue_lean = length(disc))
write_csv(h2_res, file.path(out_tables, str_glue("h2_verdict_{cfg$set}.csv")))
print(as.data.frame(h2_res), digits = 3, row.names = FALSE)

# ---- exploratory: d5 lean agreement vs the pooled d1-d4 rate -------------------
# x = share of the participant's d1-d4 trials on the lean side; y = d5 lean
# agreement (1 = lean side, 0.5 = extension, 0 = counter side). Paired t on
# x - y, two-sided.
pooled_share <- readRDS(file.path(derived_dir, "trials.rds")) %>%
  group_by(pid) %>%
  summarise(x = mean(HA == leanA), n_d14 = n(), .groups = "drop")
d5_agree <- h2 %>% transmute(pid, y = if_else(lean == "A", score, 1 - score))
paired <- inner_join(pooled_share, d5_agree, by = "pid") %>% mutate(d = x - y)
tt <- t.test(paired$d)
d5p <- tibble(pooled_rate = mean(paired$x), d5_rate = mean(paired$y),
              mean_diff = mean(paired$d),
              ci_lo = tt$conf.int[1], ci_hi = tt$conf.int[2],
              p_t_two = tt$p.value, n_pairs = nrow(paired))
write_csv(d5p, file.path(out_tables, str_glue("h2_d5_vs_pooled_{cfg$set}.csv")))

# ---- H3 -----------------------------------------------------------------------
# implied verdict from the percent-scale interval against the 3-4% bar
h3 <- h2 %>%
  inner_join(reported_effects, by = "pid") %>%
  mutate(valid_ci = !is.na(lo) & !is.na(hi) & lo <= hi,
         implied = case_when(valid_ci & hi < 3 ~ "discontinue",
                             valid_ci & lo > 4 ~ "rollout",
                             TRUE              ~ NA_character_),
         settled = !is.na(implied),
         congruence = case_when(implied == "discontinue" & lean == "B" ~ "congruent",
                                implied == "rollout"     & lean == "A" ~ "congruent",
                                settled                                ~ "incongruent"),
         # departure: the final recommendation is not the implied verdict
         # (an extension counts as a departure)
         departed = (implied == "rollout"     & score != 1) |
                    (implied == "discontinue" & score != 0))
st <- h3 %>% filter(settled)
cat(sum(h3$settled), "of", nrow(h3), "resolved trials have an interval that settles a verdict\n")

cells <- st %>%
  group_by(congruence) %>%
  summarise(n = n(), departed = sum(departed), held = n - departed,
            departed_extension = sum(departed & score == 0.5),
            .groups = "drop")
inc <- cells %>% filter(congruence == "incongruent")
con <- cells %>% filter(congruence == "congruent")
# rows sorted congruent first; "less" tests congruent < incongruent odds
fisher_one <- cells %>% select(departed, held) %>% as.matrix() %>%
  fisher.test(alternative = "less")
risk_diff <- inc$departed / inc$n - con$departed / con$n
w_inc <- wilson(inc$departed, inc$n)
w_con <- wilson(con$departed, con$n)
nw <- c(risk_diff - sqrt((inc$departed / inc$n - w_inc[1])^2 + (w_con[2] - con$departed / con$n)^2),
        risk_diff + sqrt((w_inc[2] - inc$departed / inc$n)^2 + (con$departed / con$n - w_con[1])^2))
h3_res <- tibble(risk_diff = risk_diff, ci_lo = nw[1], ci_hi = nw[2],
                 p_one_fisher = fisher_one$p.value,
                 n_incongruent = inc$n, n_congruent = con$n,
                 n_departed_incongruent = inc$departed,
                 n_departed_congruent = con$departed,
                 n_departed_incongruent_extension = inc$departed_extension)
write_csv(h3_res, file.path(out_tables, str_glue("h3_departure_{cfg$set}.csv")))
print(as.data.frame(h3_res), digits = 3, row.names = FALSE)

# ---- H3 falsification checks ----------------------------------------------------
fal_i <- reported_effects %>%
  inner_join(h2 %>% select(pid, arm), by = "pid") %>%
  filter(!is.na(est)) %>%
  { t.test(est ~ arm, data = .)$p.value }
# settlement rate in the full H2 sample (resolved d5)
fal_ii <- h2 %>%
  select(pid, arm) %>%
  left_join(reported_effects, by = "pid") %>%
  mutate(valid_ci = !is.na(lo) & !is.na(hi) & lo <= hi,
         settled  = valid_ci & (hi < 3 | lo > 4)) %>%
  { m <- table(factor(.$arm, c("discontinue-lean", "rollout-lean")),
               factor(.$settled, c(FALSE, TRUE)))
    fisher.test(m)$p.value }
fal <- tibble(check = c("(i) pct-scale estimates differ by d5 arm (Welch, two-sided)",
                        "(ii) settlement rate differs by d5 arm (Fisher, two-sided)"),
              p = c(fal_i, fal_ii))
write_csv(fal, file.path(out_tables, str_glue("h3_falsification_{cfg$set}.csv")))
print(as.data.frame(fal), digits = 3, row.names = FALSE)
