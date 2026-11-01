# 06_h4: H4, switching under an opposed vs an aligned lean, plus the coder
# agreement summary for the transcript coding.
#
# Among qualifying d1-d4 trials (the participant took a side before the
# agent voiced its assigned lean, and the agent then voiced it), a switch is
# a final choice opposite the coded initial side. Effect = the opposed
# coefficient of lm(switched ~ 0 + decision + opposed), CR2 by participant,
# Satterthwaite, two-sided 95% CI. The confirmatory test is a randomization
# test of the sharp null: the assignment schedule is regenerated 10,000
# times under the registered scheme (blocks of all 32 configurations as
# adjacent complement pairs; pair order and within-pair order shuffled per
# block), each session is reassigned by its schedule slot,
# alignment is recomputed and the effect re-estimated; one-sided p with the
# observed draw included. Four registered falsification checks,
# Holm-adjusted over the family of four.
#
# Trial admission: agreed, adjudicated and human-only trials use the human
# label; trials coded only by the LLM judge use its label.

source("R/_setup.R")

participants <- readRDS(file.path(derived_dir, "participants.rds"))

h4_checks <- c("1 pre-expression side-taking ~ assigned side",
               "2 subsequent expression ~ alignment",
               "3 side-to-expression gap (episodes) ~ alignment",
               "4 share aligned vs one half (randomization)")

h4_all <- read_coded("transcript_codes") %>%
  mutate(decision = str_to_lower(decision)) %>%
  inner_join(participants %>% select(pid, included), by = "pid") %>%
  filter(included)

# Coder agreement over the included sessions, all five decisions. Agreement
# is the agree flag (the human and the LLM status match); kappa is Cohen's
# kappa on the three-category H4 status.
dual <- h4_all %>% filter(coalesce(agree, "") != "")
cats <- union(dual$human_h4_status, dual$machine_h4_status)
p_h  <- table(factor(dual$human_h4_status, cats)) / nrow(dual)
p_m  <- table(factor(dual$machine_h4_status, cats)) / nrow(dual)
p_o  <- mean(dual$agree == "1")
p_e  <- sum(p_h * p_m)
write_csv(tibble(
  n_sessions_total        = n_distinct(h4_all$pid),
  n_sessions_dual         = n_distinct(dual$pid),
  n_sessions_machine_only = n_distinct(h4_all$pid[h4_all$h4_final_source == "machine only"]),
  n_trials_dual           = nrow(dual),
  n_agree                 = sum(dual$agree == "1"),
  n_disagree              = sum(dual$agree == "0"),
  agree_share             = p_o,
  kappa_status            = (p_o - p_e) / (1 - p_e)),
  file.path(out_tables, paste0("coder_agreement_", cfg$set, ".csv")))

h4_raw <- h4_all %>% filter(decision %in% paste0("d", 1:4))
trials <- h4_raw %>%
  filter(h4_final_source %in% c("agreed", "adjudicated", "human only",
                                "machine only")) %>%
  mutate(took_pre    = h4_took_side_pre == "1",
         voiced      = h4_lean_voiced == "1",
         aligned_pre = if_else(took_pre & h4_initial_side %in% c("A", "B"),
                               h4_initial_side == lean, NA))
incl_p     <- participants %>% filter(included)
n_expected <- 4L * nrow(incl_p)
cat(sprintf("settled d1-d4 trials: %d of %d expected\n", nrow(trials), n_expected))

# ---- schedule slots -------------------------------------------------------------
# Each participant's slot in the randomization schedule.
sched <- read_csv(file.path(data_dir, "materials", "lean_schedule.csv"),
                  col_types = cols(position = col_integer(),
                                   lean = col_character()))
slots <- read_participants(data_dir) %>% select(pid, lean, schedule_slot)
slot_lean <- sched$lean[match(slots$schedule_slot, sched$position)]
if (anyNA(slot_lean) || any(slot_lean != slots$lean) || anyDuplicated(slots$schedule_slot))
  stop("lean configuration does not match the schedule at schedule_slot")
positions <- slots %>% select(pid, position = schedule_slot)

qual_all <- trials %>%
  filter(h4_final %in% c("opposed", "aligned")) %>%
  mutate(opposed  = h4_final == "opposed",
         # a switch is a final choice on the side OPPOSITE the initial side;
         # a hybrid/other result is not the opposite side
         switched = case_when(
           !h4_initial_side %in% c("A", "B") ~ NA_real_,
           result %in% c("A", "B") ~ as.numeric(result != h4_initial_side),
           result == "O"           ~ 0,
           TRUE                    ~ NA_real_))
qual <- qual_all %>%
  filter(!is.na(switched)) %>%
  left_join(positions %>% select(pid, position), by = "pid") %>%
  mutate(dec_i = as.integer(str_sub(decision, 2)))
stopifnot(n_distinct(qual$opposed) == 2, !anyNA(qual$position))

# falsification 4 sample: sided before any lean expression
sided <- trials %>%
  filter(took_pre, h4_initial_side %in% c("A", "B")) %>%
  left_join(positions %>% select(pid, position), by = "pid") %>%
  mutate(dec_i = as.integer(str_sub(decision, 2))) %>%
  filter(!is.na(position))

# ---- registered estimator -----------------------------------------------------
m  <- lm(switched ~ 0 + decision + opposed, data = qual)
ct <- coef_test(m, vcov = "CR2", cluster = qual$pid, test = "Satterthwaite")
ci <- conf_int(m, vcov = "CR2", cluster = qual$pid,
               test = "Satterthwaite", level = .95)
k  <- match("opposedTRUE", ct$Coef)

# ---- randomization test -------------------------------------------------------
a_first <- grep("^A", apply(as.matrix(expand.grid(rep(list(c("A", "B")), 5))),
                            1, paste0, collapse = ""), value = TRUE)
mates <- chartr("AB", "BA", a_first)
draw_schedule <- function() {
  unlist(lapply(1:3, function(b) {
    unlist(lapply(sample(16L), function(p)
      if (sample(2L, 1) == 1L) c(a_first[p], mates[p])
      else c(mates[p], a_first[p])))
  }))
}
X0 <- model.matrix(~ 0 + decision, qual)
fit_opp <- function(opp) {
  if (length(unique(opp)) < 2) return(NA_real_)
  cf <- lm.fit(cbind(X0, opp = as.numeric(opp)), qual$switched)$coefficients
  unname(cf["opp"])
}
stat_obs <- fit_opp(qual$opposed)
stopifnot(abs(stat_obs - coef(m)[["opposedTRUE"]]) < 1e-10)
share_obs <- mean(sided$h4_initial_side == sided$lean)

set.seed(20260811)
n_draws <- 10000L
stat_perm <- share_perm <- rep(NA_real_, n_draws)
for (r in seq_len(n_draws)) {
  drw <- draw_schedule()
  stat_perm[r]  <- fit_opp(qual$h4_initial_side !=
                             str_sub(drw[qual$position], qual$dec_i, qual$dec_i))
  share_perm[r] <- mean(sided$h4_initial_side ==
                          str_sub(drw[sided$position], sided$dec_i, sided$dec_i))
}
# degenerate draws (one arm after reassignment) count as exceedances
n_na_perm <- sum(is.na(stat_perm))
p_rand    <- (1 + sum(stat_perm >= stat_obs, na.rm = TRUE) + n_na_perm) /
  (n_draws + 1)

res <- tibble(estimator   = "H4 opposed - aligned switch",
              diff        = ct$beta[k], se = ct$SE[k],
              ci_lo       = ci$CI_L[k], ci_hi = ci$CI_U[k],
              df          = ct$df_Satt[k],
              switch_opposed = mean(qual$switched[qual$opposed]),
              switch_aligned = mean(qual$switched[!qual$opposed]),
              p_rand_one  = p_rand,
              n_qualifying = nrow(qual),
              n_opposed   = sum(qual$opposed),
              n_aligned   = sum(!qual$opposed),
              participants = n_distinct(qual$pid),
              n_draws_degenerate = n_na_perm)
write_csv(res, file.path(out_tables, paste0("h4_", cfg$set, ".csv")))
print(as.data.frame(res %>% mutate(across(where(is.numeric), ~ round(.x, 3)))),
      row.names = FALSE)

# ---- falsification checks (two-sided, Holm over the family of four) ------------
lpm_term <- function(d, f, term) {
  out <- tibble(estimate = NA_real_, se = NA_real_, p_two = NA_real_, n = nrow(d))
  # a constant outcome has no sampling variation: report the exact 0
  y <- model.response(model.frame(f, data = d))
  if (n_distinct(y) < 2) {
    out$estimate <- 0
    return(out)
  }
  mm <- lm(f, data = d)
  cc <- coef_test(mm, vcov = "CR2", cluster = d$pid, test = "Satterthwaite")
  j  <- match(term, cc$Coef)
  tibble(estimate = cc$beta[j], se = cc$SE[j], p_two = cc$p_Satt[j], n = nrow(d))
}
f1 <- trials %>% mutate(leanA = lean == "A", y = as.numeric(took_pre)) %>%
  lpm_term(y ~ 0 + decision + leanA, "leanATRUE")
f2 <- trials %>% filter(took_pre, !is.na(aligned_pre)) %>%
  mutate(y = as.numeric(voiced)) %>%
  lpm_term(y ~ 0 + decision + aligned_pre, "aligned_preTRUE")
# check 3 on all qualifying trials with anchors; the gap is counted in
# conversation episodes
f3 <- qual_all %>%
  mutate(gap_ep = as.numeric(h4_ep_voice) - as.numeric(h4_ep_side)) %>%
  filter(!is.na(gap_ep)) %>%
  lpm_term(gap_ep ~ 0 + decision + opposed, "opposedTRUE")
f4 <- tibble(estimate = share_obs - .5, se = NA_real_,
             p_two = (1 + sum(abs(share_perm - .5) >= abs(share_obs - .5),
                              na.rm = TRUE)) / (n_draws + 1),
             n = nrow(sided))
fal <- bind_rows(f1, f2, f3, f4) %>%
  mutate(check = h4_checks, .before = 1) %>%
  mutate(trials_settled = nrow(trials), trials_expected = n_expected)
ok <- !is.na(fal$p_two)
fal$p_holm <- NA_real_
fal$p_holm[ok] <- p.adjust(fal$p_two[ok], method = "holm", n = nrow(fal))
write_csv(fal, file.path(out_tables, paste0("h4_falsification_", cfg$set, ".csv")))
print(as.data.frame(fal %>% mutate(across(where(is.numeric), ~ round(.x, 4)))),
      row.names = FALSE)
