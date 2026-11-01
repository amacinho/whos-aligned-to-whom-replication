# 13_cite_rates: how often the post-task "why" answers repeat a talking point
# the agent gave in the session, or mention the AI at all (AI users).
#   - answer-level prevalence from human labels only, per decision (Wilson CI)
#     and decision-stratified (equal weight, delta-method CI). The
#     human-labeled answers are not a simple random sample of all answers.
#   - participant-level share with at least one citing / mentioning answer,
#     human label where present, else the LLM prelabel; exact binomial CI.
#     Participants who left every why answer blank count as not citing.
#   - Cohen's kappa, human vs LLM, on double-labeled answers.

source("R/_setup.R")

decisions <- paste0("d", 1:5)
labs <- c("cites-ai-point", "mentions-ai")
split_labels <- function(x) if (is.na(x) || x == "") character(0) else str_split(x, ";")[[1]]

# AI users who answered the post-task survey
recall_all <- read_result_table("posttask_recall_all") %>%
  distinct(pid, exclusion)
ai_ids <- recall_all %>%
  filter(coalesce(exclusion, "") != "no AI engagement") %>%
  pull(pid)

# one row per (decision, participant) with the LLM and human label sets;
# human is NULL where no human label exists
units <- read_coded("why_codes") %>%
  filter(pid %in% ai_ids) %>%
  mutate(machine = map(machine_labels, split_labels),
         human   = map2(human_coded, human_labels, \(hc, hl)
                        if (hc == "TRUE") split_labels(hl) else NULL)) %>%
  select(decision, pid, machine, human) %>%
  arrange(decision)

wilson_pt <- function(k, n) {
  if (n == 0) return(c(NA_real_, NA_real_))
  ci <- prop.test(k, n, correct = FALSE)$conf.int
  c(ci[1], ci[2])
}

out <- list()
# ---- 1) answer prevalence, human labels only, decision-stratified -------------
for (l in labs) {
  per_dec <- units %>%
    filter(!map_lgl(human, is.null)) %>%
    mutate(hit = map_lgl(human, ~ l %in% .x)) %>%
    summarise(k = sum(hit), n = n(), .by = decision) %>%
    rowwise() %>%
    mutate(p = k / n, ci_lo = wilson_pt(k, n)[1], ci_hi = wilson_pt(k, n)[2]) %>%
    ungroup()
  est <- mean(per_dec$p)
  se  <- sqrt(sum(per_dec$p * (1 - per_dec$p) / per_dec$n) / length(decisions)^2)
  out[[length(out) + 1]] <- per_dec %>%
    transmute(block = "answers_human_only", label = l, unit = decision,
              k, n, estimate = p, ci_lo, ci_hi)
  out[[length(out) + 1]] <- tibble(
    block = "answers_human_only", label = l, unit = "stratified",
    k = sum(per_dec$k), n = sum(per_dec$n), estimate = est,
    ci_lo = max(0, est - 1.96 * se), ci_hi = min(1, est + 1.96 * se))
}

# ---- 2) participants with at least one, human where labeled else LLM ----------
for (l in labs) {
  per_part <- units %>%
    mutate(lab = map2(human, machine, ~ .x %||% .y),
           hit = map_lgl(lab, ~ l %in% .x)) %>%
    summarise(any = any(hit), .by = pid) %>%
    right_join(tibble(pid = ai_ids), by = "pid") %>%
    mutate(any = coalesce(any, FALSE))
  k <- sum(per_part$any); n <- nrow(per_part)
  bt <- binom.test(k, n)
  out[[length(out) + 1]] <- tibble(
    block = "participants_composite", label = l, unit = "participant",
    k = k, n = n, estimate = k / n,
    ci_lo = bt$conf.int[1], ci_hi = bt$conf.int[2])
}

# ---- 3) Cohen's kappa, human vs LLM, double-labeled answers --------------------
for (l in labs) {
  d <- units %>%
    filter(!map_lgl(human, is.null)) %>%
    mutate(h = map_lgl(human, ~ l %in% .x),
           m = map_lgl(machine, ~ l %in% .x))
  po <- mean(d$h == d$m)
  pe <- mean(d$h) * mean(d$m) + (1 - mean(d$h)) * (1 - mean(d$m))
  out[[length(out) + 1]] <- tibble(
    block = "kappa_human_machine", label = l, unit = "answer",
    k = sum(d$h == d$m), n = nrow(d), estimate = (po - pe) / (1 - pe),
    ci_lo = NA_real_, ci_hi = NA_real_)
}

res <- bind_rows(out)
write_csv(res, file.path(out_tables, str_glue("cite_rates_{cfg$set}.csv")))
print(as.data.frame(res %>% mutate(across(where(is.numeric), ~ round(.x, 3)))),
      row.names = FALSE)
