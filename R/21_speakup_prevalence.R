# 21_speakup_prevalence: how often Opus 4.8 voices an alternative when a
# scripted analyst declares a decision already made and asks only for the
# implementation (direct simulations). An LLM judge coded each reply:
# silent (complies, no commentary), mention (surfaces the alternative),
# recommend (proposes acting on it), advocate (argues for it); "unclear" is
# left out of the shares. Declarations pooled over d1-d5 and prompt wordings.
#
# Four cells: agent (stock = no lean, leaned) x declaration (against or
# matching the model's modal baseline preference). Where the agent is leaned,
# the lean always opposes the declaration: against-declarations come with a
# lean backing the model's own side, matching-declarations with a lean toward
# its non-preferred side.

source("R/_setup.R")

VERDICTS <- c("silent", "mention", "recommend", "advocate")
runs <- read_csv(file.path(data_dir, "simulations", "speakup_runs.csv"),
                 col_types = cols(.default = col_character(), error = col_logical()))

read_arm <- function(decl, ag, arm) {
  runs %>%
    filter(declaration == decl, agent == ag, !error) %>%
    pivot_longer(matches("^d[1-5]_verdict$"), names_to = "decision",
                 values_to = "verdict") %>%
    filter(!is.na(verdict), verdict %in% VERDICTS) %>%
    count(verdict) %>%
    right_join(tibble(verdict = VERDICTS), by = "verdict") %>%
    mutate(arm = arm, n = coalesce(n, 0L),
           verdict = factor(verdict, levels = VERDICTS)) %>%
    arrange(verdict) %>%
    mutate(n_judged = sum(n), share = n / n_judged) %>%
    select(arm, verdict, n, n_judged, share)
}
su <- bind_rows(
  read_arm("against", "stock", "stock_against"),
  read_arm("against", "leaned", "leaned_against"),
  read_arm("matching", "stock", "stock_matching"),
  read_arm("matching", "leaned", "leaned_matching"))
stopifnot(nrow(su) == 16)
write_csv(su, file.path(out_tables, paste0("speakup_opus_", cfg$set, ".csv")))
print(as.data.frame(su %>% mutate(share = round(share, 3))), row.names = FALSE)
