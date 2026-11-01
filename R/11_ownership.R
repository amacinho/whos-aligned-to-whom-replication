# 11_ownership: whose judgment the final recommendation reflects (post-task
# item p_ownership, raw 1 = entirely mine .. 6 = entirely the AI
# assistant's), shown on the display scale own = 7 - raw (higher = more mine).

source("R/_setup.R")

participants <- readRDS(file.path(derived_dir, "participants.rds"))
included <- participants %>% filter(included)

own_all <- read_survey(data_dir) %>%
  transmute(pid, own = 7L - suppressWarnings(as.integer(p_ownership))) %>%
  filter(!is.na(own), own %in% 1:6)

frame <- included %>% select(pid) %>% inner_join(own_all, by = "pid")
noai  <- participants %>% filter(!included) %>%
  select(pid) %>% inner_join(own_all, by = "pid")

share_row <- function(dat, group) tibble(
  group = group,
  n = nrow(dat),
  k_mostly_entirely_own = sum(dat$own >= 5),
  share_mostly_entirely_own = mean(dat$own >= 5),
  k_more_own_than_agent = sum(dat$own >= 4),
  share_more_own_than_agent = mean(dat$own >= 4))
shares <- bind_rows(share_row(frame, "included"),
                    share_row(noai, "no AI (baseline)"))
write_csv(shares, file.path(out_tables, str_glue("ownership_shares_{cfg$set}.csv")))

dist <- bind_rows(frame %>% mutate(group = "included"),
                  noai  %>% mutate(group = "no AI (baseline)")) %>%
  count(group, own) %>%
  complete(group, own = 1:6, fill = list(n = 0L))
write_csv(dist, file.path(out_tables, str_glue("ownership_dist_{cfg$set}.csv")))
print(as.data.frame(shares), digits = 3, row.names = FALSE)
