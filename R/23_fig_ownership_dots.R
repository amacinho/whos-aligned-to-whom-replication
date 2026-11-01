# 23_fig_ownership_dots: the ownership distribution, one dot per
# participant, included participants above the no-AI completers.

source("R/_setup.R")

# Dots sorted from the mine/AI boundary outward, per-category brackets
# instead of a legend, side totals at the row ends.
fig_ownership_dots <- function(tables_dir = out_tables, set = cfg$set,
                               text_scale = 1, panel_mm = 178) {
  dist <- read_result_table("ownership_dist", tables_dir, set)
  stopifnot(nrow(dist) == 12, all(dist$own %in% 1:6))
  # one dot per participant
  rows <- dist %>%
    filter(n > 0) %>%
    uncount(n) %>%
    transmute(disp = own, y = if_else(group == "included", 1L, 0L))
  dots <- rows %>%
    mutate(side = if_else(disp >= 4, 1L, -1L)) %>%
    arrange(side * disp) %>%               # center-out within each side
    group_by(y, side) %>%
    # + 1.2 dot-widths of padding off the dashed origin line: enough that
    # the innermost dots clear it
    mutate(x = side * (row_number() - .5 + 1.2)) %>%
    ungroup()
  side_tot <- dots %>% count(y, side)
  # per-category bracket + small label above the top row / below the
  # bottom row; direct labels replace the legend
  grp_lab <- c(`1` = "entirely", `2` = "mostly", `3` = "slightly",
               `4` = "slightly", `5` = "mostly", `6` = "entirely")
  brackets <- dots %>%
    group_by(y, side, disp) %>%
    summarise(x0 = min(x) - .35, x1 = max(x) + .35, .groups = "drop") %>%
    mutate(lab  = grp_lab[as.character(disp)],
           yb   = if_else(y == 1, 1.30, -.30),   # bracket bar
           yt   = if_else(y == 1, 1.20, -.20),   # end ticks, toward the dots
           ylab = if_else(y == 1, 1.52, -.62),   # one label tier per row;
                                                 # the bottom one leaves room for leaders
           xc   = (x0 + x1) / 2,
           # label half-width in x units (~.6 per character at this size)
           hw   = nchar(lab) * .3)
  # labels sit over their bracket centers, but a label too close to the
  # dashed line or to its inner neighbor slides outward; only a moved label
  # gets a leader line back to its bracket center
  brackets <- brackets %>%
    arrange(y, side, abs(xc)) %>%
    group_by(y, side) %>%
    mutate(pos = accumulate2(abs(xc), seq_along(xc), \(prev, a, i)
                   if (i == 1) max(a, hw[1] + 1.4)
                   else max(a, prev + hw[i - 1] + hw[i] + 2.5),
                   .init = 0)[-1],
           xlab = side * pos) %>%
    ungroup() %>%
    mutate(ylead = if_else(y == 1, ylab - .09, ylab + .09),
           moved = abs(pos - abs(xc)) > .05)
  n_grp <- dist %>% group_by(group) %>% summarise(n = sum(n), .groups = "drop")
  n_inc  <- n_grp$n[n_grp$group == "included"]
  n_noai <- n_grp$n[n_grp$group != "included"]
  row_lab <- c(`1` = str_glue("included\n(n = {n_inc})"),
               `0` = str_glue("no AI (baseline)\n(n = {n_noai})"))
  # dot size scaled to the row length so neighbors touch instead of
  # overlapping; panel_mm ~ usable panel width at the render size
  dot_sz <- min(4.9, panel_mm / (2 * (max(side_tot$n) + 4)))
  max_n <- max(side_tot$n)
  ggplot(dots, aes(x, y)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(aes(fill = factor(disp, levels = 1:6)), shape = 21,
               size = dot_sz, color = "white", stroke = .5,
               show.legend = FALSE) +
    geom_text(data = side_tot, aes(x = side * (n + 2.8), label = n,
                                   color = factor(side)),
              size = 4.6 * text_scale, show.legend = FALSE) +
    geom_segment(data = brackets, aes(x = x0, xend = x1, y = yb, yend = yb),
                 color = "grey55", linewidth = .35) +
    geom_segment(data = brackets, aes(x = x0, xend = x0, y = yb, yend = yt),
                 color = "grey55", linewidth = .35) +
    geom_segment(data = brackets, aes(x = x1, xend = x1, y = yb, yend = yt),
                 color = "grey55", linewidth = .35) +
    geom_segment(data = filter(brackets, moved), aes(x = xlab, xend = xc, y = ylead, yend = yb),
                 color = "grey70", linewidth = .3) +
    geom_text(data = brackets, aes(x = xlab, y = ylab, label = lab),
              size = 3.5 * text_scale, color = "grey30") +
    annotate("text", x = -.8, y = 1.95, hjust = 1, size = 4.6 * text_scale,
             label = "← more the AI assistant's", color = "#7A3A04") +
    annotate("text", x = .8, y = 1.95, hjust = 0, size = 4.6 * text_scale,
             label = "more mine →", color = "#0E2A47") +
    scale_fill_manual(values = ownership_cols, guide = "none") +
    scale_color_manual(values = c(`-1` = "#7A3A04", `1` = "#0E2A47"),
                       guide = "none") +
    scale_y_continuous(breaks = 1:0, labels = row_lab,
                       limits = c(-1.05, 2.3)) +
    scale_x_continuous(breaks = NULL,
                       limits = c(-max_n - 4, max_n + 4)) +
    labs(x = NULL, y = NULL) +
    theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.text.y = element_text(size = 10.5 * text_scale,
                                     color = "grey20", lineheight = .95))
}

# cairo_pdf so the unicode arrows render
theme_set(theme_minimal(base_size = 9))
p <- fig_ownership_dots(text_scale = .72, panel_mm = 130)
ggsave(file.path(out_figures, "ownership_dots.pdf"), p, width = 6.5, height = 2.9,
       device = cairo_pdf)
ggsave(file.path(out_figures, "ownership_dots.png"), p, width = 6.5, height = 2.9,
       dpi = 200, device = ragg::agg_png)
