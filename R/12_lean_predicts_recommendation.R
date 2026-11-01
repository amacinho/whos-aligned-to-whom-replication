# 12_lean_predicts_recommendation: how well the randomly assigned lean
# configuration alone predicts the final recommendation (exploratory).
#
# Outcome: the d5 recommendation, roll out (A) = 1, discontinue (B) = 0;
# extensions and unresolved recommendations are excluded and counted.
# Predictors: the assigned lean sides, one indicator per decision (1 = side
# A). Variants d1_d4 (the four analytical leans) and d1_d5. Models: logistic
# regression (accuracy at the 0.5 cutoff) and a count rule (roll out when
# more than half the leans point to A). AUC and accuracy are in-sample,
# against the always-guess-the-majority baseline.

source("R/_setup.R")

included <- readRDS(file.path(derived_dir, "participants.rds")) %>% filter(included)

dec <- read_decisions(data_dir) %>% semi_join(included, by = "pid")
stopifnot(nrow(dec) == 5 * nrow(included), all(dec$lean %in% c("A", "B")))

leans <- dec %>%
  transmute(pid, decision, leanA = as.integer(lean == "A")) %>%
  pivot_wider(names_from = decision, values_from = leanA, names_prefix = "lean_")

d5 <- dec %>%
  filter(decision == "d5") %>%
  mutate(y = case_when(judge_choice == "A" ~ 1,
                       judge_choice == "B" ~ 0,
                       TRUE                ~ NA_real_))
n_excl_extension  <- sum(d5$judge_choice == "extension", na.rm = TRUE)
n_excl_unresolved <- sum(is.na(d5$y)) - n_excl_extension

dat <- d5 %>% filter(!is.na(y)) %>% select(pid, y) %>%
  inner_join(leans, by = "pid")
stopifnot(!anyNA(dat), n_distinct(dat$y) == 2)
baseline_acc <- max(mean(dat$y), 1 - mean(dat$y))

# rank-based (Mann-Whitney) AUC; midranks handle ties
auc_rank <- function(score, y) {
  r  <- rank(score)
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

variants <- list(d1_d4 = paste0("lean_d", 1:4),
                 d1_d5 = paste0("lean_d", 1:5))
res <- imap(variants, function(preds, vname) {
  fit <- glm(reformulate(preds, response = "y"), family = binomial, data = dat)
  p   <- fitted(fit)
  cnt <- rowSums(dat[preds])
  mid <- length(preds) / 2
  bind_rows(
    tibble(variant = vname, model = "logistic",
           auc = auc_rank(p, dat$y), accuracy = mean((p > 0.5) == dat$y)),
    tibble(variant = vname, model = "count_rule",
           auc = auc_rank(cnt, dat$y), accuracy = mean((cnt > mid) == dat$y)))
}) %>%
  list_rbind() %>%
  mutate(n = nrow(dat),
         n_excluded_extensions = n_excl_extension,
         n_excluded_unresolved = n_excl_unresolved,
         baseline_accuracy = baseline_acc)
write_csv(res, file.path(out_tables,
                         str_glue("lean_predicts_recommendation_{cfg$set}.csv")))
print(as.data.frame(res), digits = 3, row.names = FALSE)
