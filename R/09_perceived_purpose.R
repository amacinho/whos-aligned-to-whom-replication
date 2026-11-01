# 09_perceived_purpose: coded answers to the open "what was this study
# about?" question (descriptive). Labels are multi-select; shares are
# per-label prevalences among labeled answers, by group (included vs the
# no-AI completers).

source("R/_setup.R")

participants <- readRDS(file.path(derived_dir, "participants.rds"))
codes <- read_coded("purpose_codes") %>%
  mutate(labels = map(coalesce(labels, ""), \(x)
                      if (x == "") character(0) else str_split(x, ";")[[1]]))

lab_tbl <- codes %>%
  mutate(label = map(labels, \(ls) if (length(ls)) ls else "none of these")) %>%
  select(pid, label) %>%
  unnest(label)

groups <- participants %>%
  transmute(pid, grp = case_when(
    included                          ~ "included",
    exclusion == "no AI engagement"   ~ "no AI (baseline)",
    TRUE                              ~ NA_character_)) %>%
  filter(!is.na(grp))

d <- lab_tbl %>% inner_join(groups, by = "pid")
n_grp <- d %>% distinct(pid, grp) %>% count(grp)

# fixed row order: an escalation ladder from reading the task literally up to
# suspecting planted behavior
ladder <- c("task-surface", "judgment-process", "ai-usage",
            "ai-reliance", "ai-influence", "planted-suspicion")
row_order <- c("task-surface", "judgment-process", "judgment-process only",
               "ai-usage", "ai-reliance", "ai-influence",
               "planted-suspicion", "none of these")
unit_labels <- codes %>% select(pid, labels)
jp_only <- unit_labels %>%
  inner_join(groups, by = "pid") %>%
  filter(map_lgl(labels,
                 ~ identical(unname(sort(unlist(.x))), "judgment-process"))) %>%
  count(grp) %>%
  mutate(label = "judgment-process only")
dist <- tidyr::crossing(label = row_order, grp = n_grp$grp) %>%
  left_join(bind_rows(d %>% count(label, grp), jp_only),
            by = c("label", "grp")) %>%
  mutate(n = coalesce(n, 0L)) %>%
  left_join(n_grp %>% rename(n_grp = n), by = "grp") %>%
  mutate(share = n / n_grp) %>%
  pivot_wider(id_cols = label, names_from = grp,
              values_from = c(n, share), names_sep = " ") %>%
  arrange(match(label, row_order))
write_csv(dist, file.path(out_tables, str_glue("posttask_purpose_{cfg$set}.csv")))

# Awareness ladder: each answer's HIGHEST ladder level partitions the answers,
# so "mentioned at least level k" is monotone in k.
top_reached <- unit_labels %>%
  inner_join(groups, by = "pid") %>%
  mutate(top = map_int(labels, ~ { m <- match(unlist(.x), ladder)
                                   if (all(is.na(m))) 0L
                                   else max(m, na.rm = TRUE) }))
ladder_tbl <- crossing(k = 2:6, grp = n_grp$grp) %>%
  left_join(n_grp %>% rename(n_labeled = n), by = "grp") %>%
  rowwise() %>%
  mutate(stage = ladder[k],
         n = sum(top_reached$top[top_reached$grp == grp] >= k),
         share = n / n_labeled) %>%
  ungroup() %>%
  select(grp, k, stage, n_labeled, n, share)
write_csv(ladder_tbl, file.path(out_tables, str_glue("purpose_ladder_{cfg$set}.csv")))
print(as.data.frame(ladder_tbl), digits = 3, row.names = FALSE)
