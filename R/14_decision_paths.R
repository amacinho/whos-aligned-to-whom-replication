# 14_decision_paths: collapsed decision paths over the hand-coded sessions
# (5 decisions each), and the engagement shares quoted in the paper.
#
# Collapse rule on the lean-relative pattern (human_pattern_lc): consecutive
# elements pointing in the same lean-relative direction merge and the first
# of each run survives; toward the lean = favoring the lean side (L/l) or
# arguing against the counter side (!C/!c), toward the counter side the
# mirror; the final result element never merges.
# Engagement shares use the fuller human_pattern layer, which keeps the
# no-stance (~) and question (?) moves:
#   - clarify_or_pushback: the participant asked a question about the
#     decision (H:? move) or pushed back at the agent's voiced side (H:!L /
#     H:!l in the lean-relative layer)
#   - silent: no question, no against-move and no side-taking by the
#     participant before the result; no-stance moves do not break silence

source("R/_setup.R")

participants <- readRDS(file.path(derived_dir, "participants.rds"))

h <- read_coded("transcript_codes") %>%
  filter(coalesce(human_pattern, "") != "")
stopifnot("hand-coded sessions are complete (5 trials each)" = all(table(h$pid) == 5))
h <- h %>% left_join(participants %>% select(pid, included), by = "pid")
stopifnot(all(coalesce(h$included, FALSE)), all(h$result != "?"))

split_els <- function(pat) str_split(pat, " > ")[[1]]
pre_els   <- function(pat) { e <- split_els(pat); e[!str_starts(e, "R:")] }

# +1 toward the lean, -1 toward the counter side
dir_of <- function(els) {
  side <- str_extract(els, "[LlCc]$")
  d <- ifelse(side %in% c("L", "l"), 1L,
              ifelse(side %in% c("C", "c"), -1L, NA_integer_))
  ifelse(str_detect(els, "!"), -d, d)
}

collapse_lc <- function(pat) {
  els <- split_els(pat)
  is_res <- str_starts(els, "R:")
  stopifnot(sum(is_res) == 1 && which(is_res) == length(els))
  pre <- els[!is_res]
  if (length(pre) == 0) return(els[is_res])
  d <- dir_of(pre)
  stopifnot(!anyNA(d))
  keep <- c(TRUE, d[-1] != head(d, -1))
  paste(c(pre[keep], els[is_res]), collapse = " > ")
}

h <- h %>% mutate(
  path = vapply(human_pattern_lc, collapse_lc, character(1)),
  result_follows_lean = str_extract(path, "R:.$") %in% c("R:L", "R:l"),
  has_question = vapply(human_pattern, function(p)
    any(str_detect(pre_els(p), "^H:[?]")), logical(1)),
  has_pushback_at_lean = vapply(human_pattern_lc, function(p)
    any(pre_els(p) %in% c("H:!L", "H:!l")), logical(1)),
  has_any_h_stance = vapply(human_pattern, function(p)
    any(str_detect(pre_els(p), "^H:[!?]?D")), logical(1)))
stopifnot(all(h$has_any_h_stance[h$has_question | h$has_pushback_at_lean]))

paths <- h %>%
  count(result_follows_lean, path) %>%
  arrange(desc(result_follows_lean), desc(n), path)
write_csv(paths, file.path(out_tables, paste0("decision_paths_", cfg$set, ".csv")))

shares <- tibble(
  n_trials = nrow(h),
  n_sessions = n_distinct(h$pid),
  n_clarify_or_pushback = sum(h$has_question | h$has_pushback_at_lean),
  share_clarify_or_pushback = n_clarify_or_pushback / n_trials,
  n_silent = sum(!h$has_any_h_stance),
  share_silent = n_silent / n_trials)
write_csv(shares, file.path(out_tables, paste0("decision_paths_shares_", cfg$set, ".csv")))
print(as.data.frame(shares), digits = 3, row.names = FALSE)
