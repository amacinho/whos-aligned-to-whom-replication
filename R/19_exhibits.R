# 19_exhibits: Exhibit 1's (P09) delivered estimate and 95% interval, and
# Exhibit 2's (P52) minutes to the first prompt.

source("R/_setup.R")

# participant ids printed in the paper's exhibit headings
EXHIBIT1 <- "P09"
EXHIBIT2 <- "P52"

p <- readRDS(file.path(derived_dir, "participants.rds"))
e1 <- p %>% filter(pid == EXHIBIT1)
e2 <- p %>% filter(pid == EXHIBIT2)
stopifnot(nrow(e1) == 1, nrow(e2) == 1, e1$included, e2$included)
ex <- tibble(
  exhibit  = c("Exhibit 1", "Exhibit 1", "Exhibit 1", "Exhibit 2"),
  quantity = c("delivered estimate (%)", "95% CI low (%)", "95% CI high (%)",
               "minutes to first prompt"),
  value    = c(e1$pct_change, e1$ci_low_pct, e1$ci_high_pct, e2$first_prompt_min))
write_csv(ex, file.path(out_tables, paste0("exhibits_", cfg$set, ".csv")))
print(as.data.frame(ex), digits = 3, row.names = FALSE)
