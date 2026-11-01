# 05_covariate_adjustment: covariate-adjusted primary estimators for H1 and H2
# (preregistration, section 4).
#
#   * Candidate pool: eight pre-task covariates (skill_score8, trust, ncs, aot,
#     years_analysis, ai_use_days, age, gender_man), trimmed by a mechanical
#     rule (no variation, or binary with minority share < 10%).
#   * Missing covariate values are mean-imputed over the analysis set, so the
#     adjusted and unadjusted estimates use the same participants.
#   * Selection: gaussian lasso predicting the outcome (HA for H1, the
#     recommendation score for H2) from decision indicators (H1 only,
#     unpenalized) plus the candidates, leave-one-participant-out
#     cross-validation, lambda.min. The selection matrix has no lean column.
#   * Held-out gate: an outer leave-one-participant-out loop reruns
#     imputation, selection and a lean-free outcome model on the training
#     part and predicts the held-out participant. The candidates enter the
#     adjusted estimator only if the held-out R2 against a covariate-free
#     baseline is positive; otherwise the set is empty and adjusted =
#     unadjusted.
#   * Adjusted estimators: the registered models with the selected covariates
#     added; inference unchanged (H1: CR2 by participant + Satterthwaite on
#     the pooled contrast; H2: score on the arm indicator, CR2 on singleton
#     clusters = HC2, Satterthwaite). The unadjusted H2 row is the registered
#     Welch test.
#   * Unadjusted rows are always written alongside; sign_flag marks a sign
#     disagreement.

source("R/_setup.R")
suppressPackageStartupMessages(library(glmnet))

trials   <- readRDS(file.path(derived_dir, "trials.rds"))
included <- readRDS(file.path(derived_dir, "participants.rds")) %>% filter(included)
mods     <- readRDS(file.path(derived_dir, "moderators.rds"))

POOL <- c("skill_score8", "trust", "ncs", "aot", "years_analysis",
          "ai_use_days", "age", "gender_man")

# covariate frame over ALL included participants (no survey -> all NA, imputed)
covs_raw <- included %>%
  select(pid) %>%
  left_join(mods %>% select(pid, all_of(POOL)), by = "pid")

# Trimming rule (observed covariate values only; no lean, no outcome)
trim_report <- map_dfr(POOL, function(v) {
  x <- covs_raw[[v]][!is.na(covs_raw[[v]])]
  reason <- if (n_distinct(x) < 2) "no variation"
    else if (n_distinct(x) == 2 && min(table(x)) / length(x) < 0.10)
      sprintf("binary, minority share %.0f%% < 10%%", 100 * min(table(x)) / length(x))
    else NA_character_
  tibble(covariate = v, n_observed = length(x),
         n_missing = nrow(covs_raw) - length(x), dropped = reason)
})
pool <- trim_report %>% filter(is.na(dropped)) %>% pull(covariate)
print(as.data.frame(trim_report), row.names = FALSE)

# Mean imputation (training-fold means when `means` is given)
impute <- function(df, vars, means = NULL) {
  if (is.null(means))
    means <- set_names(map_dbl(vars, \(v) mean(df[[v]], na.rm = TRUE)), vars)
  for (v in vars) df[[v]][is.na(df[[v]])] <- means[[v]]
  list(df = df, means = means)
}

assert_lean_free <- function(X)
  stopifnot("lean column reached the selection matrix" =
            !any(grepl("lean|arm", colnames(X), ignore.case = TRUE)))

select_h1 <- function(tr, cov_df, vars) {
  d <- tr %>% left_join(cov_df, by = "pid")
  X <- cbind(model.matrix(~ decision, d)[, -1, drop = FALSE],
             as.matrix(d[, vars]))
  assert_lean_free(X)
  pf  <- c(rep(0, nlevels(droplevels(d$decision)) - 1), rep(1, length(vars)))
  fid <- as.integer(factor(d$pid))
  cvf <- cv.glmnet(X, d$HA, foldid = fid, penalty.factor = pf,
                   standardize = TRUE, grouped = FALSE)
  b <- coef(cvf, s = "lambda.min")
  intersect(vars, rownames(b)[as.vector(b) != 0])
}

select_h2 <- function(d, vars) {
  X <- as.matrix(d[, vars])
  assert_lean_free(X)
  cvf <- cv.glmnet(X, d$score, foldid = seq_len(nrow(d)),
                   standardize = TRUE, grouped = FALSE)
  b <- coef(cvf, s = "lambda.min")
  intersect(vars, rownames(b)[as.vector(b) != 0])
}

fit_h1 <- function(d, sel) {
  m <- lm(reformulate(c("0", "decision", "decision:leanA", sel), "HA"), data = d)
  slope <- grepl(":leanA", names(coef(m)))
  lc <- linear_contrast(m, vcov = "CR2", cluster = d$pid, level = .95,
                        contrasts = matrix(ifelse(slope, 1 / sum(slope), 0), nrow = 1))
  tibble(estimate = lc$Est, se = lc$SE, ci_lo = lc$CI_L, ci_hi = lc$CI_U,
         df = lc$df, p_one = pt(lc$Est / lc$SE, lc$df, lower.tail = FALSE))
}

fit_h2 <- function(d, sel) {
  m  <- lm(reformulate(c("leanA5", sel), "score"), data = d)
  ct <- coef_test(m, vcov = "CR2", cluster = d$pid, test = "Satterthwaite")
  ci <- conf_int(m, vcov = "CR2", cluster = d$pid, test = "Satterthwaite", level = .95)
  i  <- match("leanA5", ct$Coef)
  tibble(estimate = ct$beta[i], se = ct$SE[i], ci_lo = ci$CI_L[i], ci_hi = ci$CI_U[i],
         df = ct$df_Satt[i], p_one = pt(ct$beta[i] / ct$SE[i], ct$df_Satt[i],
                                        lower.tail = FALSE))
}

# Held-out performance: outer leave-one-participant-out loop over the whole
# pipeline; outcome models without lean terms.
heldout_h1 <- function(tr, covs_raw, vars) {
  ids <- unique(tr$pid); sse_m <- sse_b <- 0; n <- 0
  for (t in ids) {
    tr_tr <- tr %>% filter(pid != t)
    tr_te <- tr %>% filter(pid == t)
    imp   <- impute(covs_raw %>% filter(pid != t), vars)
    cv_te <- impute(covs_raw %>% filter(pid == t), vars, imp$means)$df
    sel   <- select_h1(tr_tr, imp$df, vars)
    d_tr  <- tr_tr %>% left_join(imp$df, by = "pid")
    d_te  <- tr_te %>% left_join(cv_te,  by = "pid")
    m_b   <- lm(HA ~ 0 + decision, data = d_tr)
    m_m   <- if (length(sel))
               lm(reformulate(c("0", "decision", sel), "HA"), data = d_tr) else m_b
    sse_b <- sse_b + sum((d_te$HA - predict(m_b, d_te))^2)
    sse_m <- sse_m + sum((d_te$HA - predict(m_m, d_te))^2)
    n     <- n + nrow(d_te)
  }
  tibble(heldout_mse_base = sse_b / n, heldout_mse_adj = sse_m / n,
         heldout_r2_vs_base = 1 - sse_m / sse_b)
}

heldout_h2 <- function(d5c_raw, vars) {
  sse_m <- sse_b <- 0
  for (i in seq_len(nrow(d5c_raw))) {
    tr  <- d5c_raw[-i, ]; te <- d5c_raw[i, , drop = FALSE]
    imp <- impute(tr, vars)
    te  <- impute(te, vars, imp$means)$df
    sel <- select_h2(imp$df, vars)
    m_m <- if (length(sel)) lm(reformulate(sel, "score"), data = imp$df) else NULL
    sse_b <- sse_b + (te$score - mean(tr$score))^2
    sse_m <- sse_m + (te$score - if (is.null(m_m)) mean(tr$score)
                                 else predict(m_m, te))^2
  }
  tibble(heldout_mse_base = sse_b / nrow(d5c_raw),
         heldout_mse_adj = sse_m / nrow(d5c_raw),
         heldout_r2_vs_base = 1 - sse_m / sse_b)
}

# ---- H1 -------------------------------------------------------------------------
imp_full   <- impute(covs_raw, pool)
trials_cov <- trials %>% left_join(imp_full$df, by = "pid")

h1_cand <- select_h1(trials, imp_full$df, pool)
h1_ho   <- heldout_h1(trials, covs_raw, pool)
h1_gate <- h1_ho$heldout_r2_vs_base > 0
h1_sel  <- if (h1_gate) h1_cand else character(0)

h1_un  <- fit_h1(trials_cov, character(0))
h1_adj <- fit_h1(trials_cov, h1_sel)
h1_rows <- bind_rows(
  h1_un  %>% mutate(hypothesis = "H1", estimator = "unadjusted (registered)",
                    candidates = "", selected = "", gate_passed = NA,
                    .before = 1),
  h1_adj %>% mutate(hypothesis = "H1", estimator = "adjusted (outcome-only lasso; primary)",
                    candidates = if (length(h1_cand)) paste(h1_cand, collapse = "+")
                                 else "(none)",
                    selected = if (length(h1_sel)) paste(h1_sel, collapse = "+")
                               else "(none)",
                    gate_passed = h1_gate, .before = 1) %>%
             bind_cols(h1_ho)) %>%
  mutate(participants = n_distinct(trials$pid), trials = nrow(trials))

# ---- H2 -------------------------------------------------------------------------
d5 <- read_decisions(data_dir) %>%
  semi_join(included, by = "pid") %>%
  filter(decision == "d5") %>%
  mutate(score = d5_score(judge_choice),
         leanA5 = as.integer(lean == "A")) %>%
  filter(!is.na(score)) %>%
  select(pid, score, leanA5)

d5c_raw <- d5 %>% left_join(covs_raw, by = "pid")
d5c     <- d5 %>% left_join(impute(covs_raw, pool)$df, by = "pid")

h2_cand <- select_h2(impute(d5c_raw, pool)$df, pool)
h2_ho   <- heldout_h2(d5c_raw, pool)
h2_gate <- h2_ho$heldout_r2_vs_base > 0
h2_sel  <- if (h2_gate) h2_cand else character(0)

# unadjusted: the registered one-sided Welch test, two-sided 95% CI
w1 <- t.test(d5$score[d5$leanA5 == 1], d5$score[d5$leanA5 == 0],
             alternative = "greater")
w2 <- t.test(d5$score[d5$leanA5 == 1], d5$score[d5$leanA5 == 0])
h2_un <- tibble(estimate = unname(-diff(w2$estimate)), se = w2$stderr,
                ci_lo = w2$conf.int[1], ci_hi = w2$conf.int[2],
                df = unname(w2$parameter), p_one = w1$p.value)
# with an empty selected set, adjusted = unadjusted
h2_adj <- if (length(h2_sel)) fit_h2(d5c, h2_sel) else h2_un
h2_rows <- bind_rows(
  h2_un  %>% mutate(hypothesis = "H2", estimator = "unadjusted (registered Welch)",
                    candidates = "", selected = "", gate_passed = NA,
                    .before = 1),
  h2_adj %>% mutate(hypothesis = "H2", estimator = "adjusted (outcome-only lasso; primary)",
                    candidates = if (length(h2_cand)) paste(h2_cand, collapse = "+")
                                 else "(none)",
                    selected = if (length(h2_sel)) paste(h2_sel, collapse = "+")
                               else "(none)",
                    gate_passed = h2_gate, .before = 1) %>%
             bind_cols(h2_ho)) %>%
  mutate(participants = nrow(d5), trials = nrow(d5))

# ---- report ---------------------------------------------------------------------
res <- bind_rows(h1_rows, h2_rows) %>%
  relocate(hypothesis, estimator, estimate, se, ci_lo, ci_hi, df, p_one,
           participants, trials, candidates, selected, gate_passed) %>%
  group_by(hypothesis) %>%
  mutate(sign_flag = sign(estimate[1]) != sign(estimate[2])) %>%
  ungroup()
write_csv(res, file.path(out_tables, str_glue("covariate_adjustment_{cfg$set}.csv")))
res %>% select(hypothesis, estimator, estimate, ci_lo, ci_hi, p_one, selected) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  as.data.frame() %>% print(row.names = FALSE)
