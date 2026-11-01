# 03_h1_flip: H1, the pooled d1-d4 flip, registered estimator.
#
# Linear probability model lm(HA ~ 0 + decision + decision:leanA); pooled
# flip = equal-weight mean of the four decision:leanA slopes; CR2 by
# participant, Satterthwaite dof, one-sided test (flip > 0), two-sided 95%
# CI. The covariate-adjusted variant is in 05_covariate_adjustment.R.

source("R/_setup.R")

trials <- readRDS(file.path(derived_dir, "trials.rds"))
cat("trials:", nrow(trials), " participants:", n_distinct(trials$pid), "\n")

# the registered estimand needs all four decisions with both lean arms
cells <- trials %>% count(decision, leanA)
stopifnot(n_distinct(cells$decision) == 4, nrow(cells) == 8)

m     <- lm(HA ~ 0 + decision + decision:leanA, data = trials)
slope <- grepl(":leanA", names(coef(m)))
lc <- linear_contrast(m, vcov = "CR2", cluster = trials$pid, level = .95,
                      contrasts = matrix(ifelse(slope, 1 / sum(slope), 0), nrow = 1))

res <- tibble(estimator = "unadjusted H1",
              flip = lc$Est, se = lc$SE, ci_lo = lc$CI_L, ci_hi = lc$CI_U, df = lc$df,
              p_one = pt(lc$Est / lc$SE, lc$df, lower.tail = FALSE),
              participants = n_distinct(trials$pid), trials = nrow(trials))

# Registered robustness companion: HB = 1{H=B} with the lean coded from side
# B. Hybrid or other choices are 0 under both codings, so a gap between the
# two rows would flag lean-responsive hybrid choices.
tB <- trials %>% mutate(HB = as.integer(judge_choice == "B"), leanB = 1L - leanA)
mB <- lm(HB ~ 0 + decision + decision:leanB, data = tB)
slopeB <- grepl(":leanB", names(coef(mB)))
lcB <- linear_contrast(mB, vcov = "CR2", cluster = tB$pid, level = .95,
                       contrasts = matrix(ifelse(slopeB, 1 / sum(slopeB), 0), nrow = 1))
res <- bind_rows(res,
  tibble(estimator = "HB coding (registered robustness)",
         flip = lcB$Est, se = lcB$SE, ci_lo = lcB$CI_L, ci_hi = lcB$CI_U, df = lcB$df,
         p_one = pt(lcB$Est / lcB$SE, lcB$df, lower.tail = FALSE),
         participants = n_distinct(tB$pid), trials = nrow(tB)))
write_csv(res, file.path(out_tables, paste0("h1_flip_", cfg$set, ".csv")))

# Per-decision flips: risk difference with a Newcombe-Wilson 95% interval
nw_diff <- function(k1, n1, k0, n0) {
  d <- k1 / n1 - k0 / n0
  w1 <- wilson(k1, n1); w0 <- wilson(k0, n0)
  c(d,
    d - sqrt((k1 / n1 - w1[1])^2 + (w0[2] - k0 / n0)^2),
    d + sqrt((w1[2] - k1 / n1)^2 + (k0 / n0 - w0[1])^2))
}
by_dec <- trials %>%
  group_by(decision) %>%
  summarise(k1 = sum(HA[leanA == 1]), n1 = sum(leanA == 1),
            k0 = sum(HA[leanA == 0]), n0 = sum(leanA == 0),
            .groups = "drop") %>%
  rowwise() %>%
  mutate(res = list(nw_diff(k1, n1, k0, n0))) %>%
  ungroup() %>%
  mutate(flip = map_dbl(res, 1), ci_lo = map_dbl(res, 2),
         ci_hi = map_dbl(res, 3), method = "Newcombe-Wilson 95%") %>%
  select(decision, flip, ci_lo, ci_hi, k1, n1, k0, n0, method)
write_csv(by_dec, file.path(out_tables,
                            paste0("h1_flip_by_decision_", cfg$set, ".csv")))

print(as.data.frame(res %>% mutate(across(where(is.numeric), ~ round(.x, 3)))),
      row.names = FALSE)
