# 22_fig_headline_forest: the main-results forest (H1-H4 on one axis).

source("R/_setup.R")

# The four registered contrasts on one axis, null 0 for every row. Text
# sizes scale with text_scale (ggplot text sizes are absolute).
fig_headline_forest <- function(tables_dir = out_tables, set = cfg$set,
                                text_scale = 1) {
  ca <- read_result_table("covariate_adjustment", tables_dir, set)
  f1 <- ca %>% filter(hypothesis == "H1", str_detect(estimator, "^adjusted"))
  f2 <- ca %>% filter(hypothesis == "H2", str_detect(estimator, "^adjusted"))
  f3 <- read_result_table("h3_departure", tables_dir, set)
  f4 <- read_result_table("h4", tables_dir, set)
  stopifnot(nrow(f1) == 1, nrow(f2) == 1, nrow(f3) == 1, nrow(f4) == 1)
  fmt_p <- function(p) if_else(p < .001, "p < .001",
                               str_c("p = ", format(round(p, 3),
                                                    nsmall = 2)))
  hl <- tibble(
    row = 4:1,
    label = c(
      "H1: analytical decisions (pooled flip, d1-d4)",
      "H2: policy recommendation (d5)",
      "H3: recommending a policy incongruent\nwith own estimate (risk difference, d5)",
      "H4: switching a taken side (d1-d4)"),
    est = c(f1$estimate, f2$estimate, f3$risk_diff, f4$diff),
    lo  = c(f1$ci_lo, f2$ci_lo, f3$ci_lo, f4$ci_lo),
    hi  = c(f1$ci_hi, f2$ci_hi, f3$ci_hi, f4$ci_hi),
    p   = fmt_p(c(f1$p_one, f2$p_one, f3$p_one_fisher, f4$p_rand_one)))
  ggplot(hl, aes(est, row)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    annotate("text", x = .015, y = 4.62, hjust = 0,
             size = 3.6 * text_scale,
             label = "no effect", color = "grey45") +
    geom_linerange(aes(xmin = lo, xmax = hi), color = "grey15",
                   linewidth = 0.7) +
    geom_point(color = "grey15", size = 4.6 * text_scale) +
    geom_text(aes(label = str_glue("+{round(est * 100)}pp, {p}")),
              vjust = -1.1, size = 4.2 * text_scale, fontface = "bold",
              color = "grey15") +
    scale_y_continuous(breaks = hl$row, labels = hl$label,
                       limits = c(.6, 4.75)) +
    scale_x_continuous(labels = function(x) str_c(x * 100, "pp"),
                       breaks = seq(0, 1, .25),
                       limits = c(min(c(hl$lo, -.02)), 1),
                       expand = expansion(mult = c(.02, 0))) +
    labs(x = "effect of the agent's lean, percentage points", y = NULL) +
    theme(panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 11.5 * text_scale,
                                     color = "grey15",
                                     lineheight = 1.05, hjust = 0),
          # room for the 100pp tick label at the exact panel cap
          plot.margin = margin(5.5, 30, 5.5, 5.5))
}

theme_set(theme_minimal(base_size = 9))
p <- fig_headline_forest(text_scale = .72)
ggsave(file.path(out_figures, "headline_forest.pdf"), p, width = 6.5, height = 3.4)
ggsave(file.path(out_figures, "headline_forest.png"), p, width = 6.5, height = 3.4,
       dpi = 200, device = ragg::agg_png)
