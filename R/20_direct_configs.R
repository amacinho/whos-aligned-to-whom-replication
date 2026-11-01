# 20_direct_configs: configuration profiles of the direct model simulations
# (figure) and Opus 4.8's baseline preferences (table).
#
# A configuration is one run's judged d1-d5 choices concatenated (e.g.
# BAAAA; u = unclear, e = extension, c = conflicting). Stacked share per
# model, top five configurations colored, a single dominant "other"
# configuration named in dark ink.

source("R/_setup.R")

model_label <- c(opus = "Opus 4.8", opus5 = "Opus 5", fable = "Fable 5",
                 sonnet46 = "Sonnet 4.6", sonnet5 = "Sonnet 5",
                 sol = "GPT-5.6 Sol", terra = "GPT-5.6 Terra", k3 = "Kimi K3")

runs <- read_csv(file.path(data_dir, "simulations", "direct_runs.csv"),
                 show_col_types = FALSE,
                 col_types = cols(.default = col_character(), error = col_logical())) |>
  filter(!error)

code1 <- function(x) case_when(x %in% c("A", "B") ~ x,
                               is.na(x) ~ "?",
                               TRUE ~ tolower(substr(x, 1, 1)))
profiles <- runs |>
  mutate(config = paste0(code1(d1), code1(d2), code1(d3), code1(d4), code1(d5))) |>
  count(model, config) |>
  mutate(share = n / sum(n), .by = model)

cfg_cols <- c("#3B7BB8", "#D96B27", "#6B4FA0", "#3F8F5F", "#C09A2E")
top_cfgs <- profiles |>
  summarise(n = sum(n), .by = config) |>
  slice_max(n, n = length(cfg_cols)) |>
  pull(config)
# name a single dominant profile hidden in the gray "other" block
other_dom <- profiles |>
  filter(!config %in% top_cfgs) |>
  slice_max(share, n = 1, with_ties = FALSE, by = model) |>
  filter(share >= 0.18) |>
  transmute(model, cfg = "other",
            dom_lab = paste0("mostly ", config, " ", round(100 * share), "%"))
modal <- profiles |>
  slice_max(share, n = 1, with_ties = FALSE, by = model) |>
  select(model, modal_share = share)
fig_dat <- profiles |>
  mutate(cfg = ifelse(config %in% top_cfgs, config, "other")) |>
  summarise(share = sum(share), .by = c(model, cfg)) |>
  left_join(other_dom, by = c("model", "cfg")) |>
  mutate(cfg = factor(cfg, levels = c(top_cfgs, "other"))) |>
  left_join(modal, by = "model") |>
  mutate(model_lab = fct_reorder(model_label[model], modal_share),
         seg_lab = case_when(
           cfg != "other" & share >= 0.18 ~ paste0(cfg, " ", round(100 * share), "%"),
           cfg == "other" & !is.na(dom_lab) ~ dom_lab,
           TRUE ~ ""),
         lab_col = ifelse(cfg == "other", "gray25", "white"))

theme_set(theme_minimal(base_size = 9))
p <- ggplot(fig_dat, aes(y = model_lab, x = share, fill = cfg)) +
  geom_col(position = position_fill(reverse = TRUE), width = 0.72,
           color = "white", linewidth = 0.5) +
  geom_text(aes(label = seg_lab, color = lab_col, group = cfg),
            position = position_fill(reverse = TRUE, vjust = 0.5),
            size = 2.5, fontface = "bold") +
  scale_color_identity() +
  scale_fill_manual(values = c(setNames(cfg_cols, top_cfgs), other = "gray82"),
                    name = NULL) +
  scale_x_continuous(labels = scales::label_percent(),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = "share of judged runs", y = NULL) +
  theme(legend.position = "top", panel.grid.major.y = element_blank())
ggsave(file.path(out_figures, "direct_configs.pdf"), p, width = 6.5, height = 3.2)
ggsave(file.path(out_figures, "direct_configs.png"), p, width = 6.5, height = 3.2,
       dpi = 200, device = ragg::agg_png)

# Opus 4.8 baseline: modal side per decision and the modal full profile,
# with shares over the model's judged runs
opus_runs <- runs |> filter(model == "opus")
baseline_dec <- map_dfr(c("d1", "d2", "d3", "d4", "d5"), function(d) {
  x <- code1(opus_runs[[d]])
  tab <- sort(table(x), decreasing = TRUE)
  tibble(decision = d, side = names(tab)[1], n = as.integer(tab[1]),
         n_runs = length(x), share = as.integer(tab[1]) / length(x))
})
baseline_prof <- profiles |>
  filter(model == "opus") |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  transmute(decision = "profile", side = config, n,
            n_runs = sum(profiles$n[profiles$model == "opus"]), share)
stopifnot(nrow(opus_runs) == baseline_prof$n_runs)
write_csv(bind_rows(baseline_dec, baseline_prof),
          file.path(out_tables, paste0("direct_baseline_opus_", cfg$set, ".csv")))
