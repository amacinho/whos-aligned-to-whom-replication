# 24_fig_speakup_barbell: the 2x2 speak-up figure (recommend-or-advocate
# share, stock vs leaned agent, by declared side relative to the model's
# baseline preference), exact binomial 95% intervals.

source("R/_setup.R")

fig_speakup_barbell <- function(tables_dir = out_tables, set = cfg$set,
                                text_scale = 1) {
  su <- read_result_table("speakup_opus", tables_dir, set)
  stopifnot(nrow(su) == 16)
  cells <- su %>%
    mutate(arm2 = if_else(str_starts(arm, "stock"), "stock", "leaned"),
           decl = if_else(str_ends(arm, "matching"),
                          "matching", "against")) %>%
    group_by(arm2, decl) %>%
    summarise(k = sum(n[verdict %in% c("recommend", "advocate")]),
              n_judged = n_judged[1], .groups = "drop") %>%
    rowwise() %>%
    mutate(share = k / n_judged,
           ci_lo = binom.test(k, n_judged)$conf.int[1],
           ci_hi = binom.test(k, n_judged)$conf.int[2]) %>%
    ungroup() %>%
    mutate(decl_lab = factor(if_else(decl == "matching",
             "declaration matches\nits baseline preference",
             "declaration against\nits baseline preference"),
             levels = c("declaration matches\nits baseline preference",
                        "declaration against\nits baseline preference")))
  ggplot(cells, aes(decl_lab, share)) +
    geom_line(aes(group = decl_lab), color = "grey60", linewidth = .8) +
    geom_linerange(aes(ymin = ci_lo, ymax = ci_hi, color = arm2),
                   linewidth = .5, alpha = .6) +
    geom_point(aes(color = arm2), size = 4 * text_scale) +
    geom_text(aes(label = sprintf("%.0f%%", 100 * share), color = arm2,
                  hjust = if_else(arm2 == "stock", 1.7, -0.7)),
              size = 3.3 * text_scale, fontface = "bold",
              show.legend = FALSE) +
    scale_color_manual(values = c(stock = "grey35", leaned = "#3E7CB1"),
                       breaks = c("stock", "leaned"), name = NULL) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1.02),
                       breaks = c(0, .25, .5, .75, 1)) +
    labs(x = NULL,
         y = "share recommending or advocating\nthe alternative") +
    theme(legend.position = "top", panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text.x = element_text(size = 8.5 * text_scale,
                                     color = "grey15", lineheight = 1.05))
}

theme_set(theme_minimal(base_size = 9))
p <- fig_speakup_barbell(text_scale = .72)
ggsave(file.path(out_figures, "speakup_barbell.pdf"), p, width = 4.4, height = 3.0)
ggsave(file.path(out_figures, "speakup_barbell.png"), p, width = 4.4, height = 3.0,
       dpi = 200, device = ragg::agg_png)
