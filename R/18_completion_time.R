# 18_completion_time: task duration for AI users vs no-AI completers, on
# pap_window_min and work_block_min, with a Wilcoxon rank-sum p.

source("R/_setup.R")

p <- readRDS(file.path(derived_dir, "participants.rds")) %>%
  mutate(group = case_when(included ~ "AI users (included)",
                           exclusion == "no AI engagement" ~ "no-AI completers",
                           TRUE ~ NA_character_)) %>%
  filter(!is.na(group))
out <- map_dfr(c("pap_window_min", "work_block_min"), function(v) {
  w <- wilcox.test(p[[v]] ~ p$group)
  p %>% group_by(group) %>%
    summarise(n = n(), median_min = median(.data[[v]], na.rm = TRUE),
              mean_min = mean(.data[[v]], na.rm = TRUE),
              q25_min = quantile(.data[[v]], .25, na.rm = TRUE),
              q75_min = quantile(.data[[v]], .75, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(window = v, wilcoxon_p_two = w$p.value, .before = 1)
})
write_csv(out, file.path(out_tables, paste0("completion_time_", cfg$set, ".csv")))
print(as.data.frame(out), digits = 3, row.names = FALSE)
