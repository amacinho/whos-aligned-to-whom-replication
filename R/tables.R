# tables: the paper's table fragments (tabular environments only; captions
# and labels belong to the paper), from the result tables in output/tables/.

source("R/_setup.R")

# negative values get a true minus sign
neg   <- function(s) gsub("-(?=[0-9.])", "$-$", s, perl = TRUE)
est2  <- function(x) neg(sprintf("%.2f", x))
ci2   <- function(lo, hi) neg(sprintf("[%.2f, %.2f]", lo, hi))
p_fmt <- function(p) ifelse(p < 0.001, "$<$.001", sprintf("%.3f", p))

# H1 and H2 appear twice: the covariate-adjusted registered primary first,
# the unadjusted estimate under it; H3 and H4 have no registered adjusted
# variant.
ca <- read_result_table("covariate_adjustment")
ca_row <- function(h, pat) {
  r <- ca %>% filter(hypothesis == h, str_detect(estimator, pat))
  stopifnot(nrow(r) == 1)
  r
}
h1a <- ca_row("H1", "^adjusted"); h1u <- ca_row("H1", "^unadjusted")
h2a <- ca_row("H2", "^adjusted"); h2u <- ca_row("H2", "^unadjusted")
h3 <- read_result_table("h3_departure")
h4 <- read_result_table("h4")

# Registered disengagement sensitivity: the same estimates without the
# sessions whose idle share exceeded 50% of the work period.
idle_set <- str_c(cfg$set, "_idle50")
cai <- read_result_table("covariate_adjustment", set = idle_set)
cai_row <- function(h, pat) {
  r <- cai %>% filter(hypothesis == h, str_detect(estimator, pat))
  stopifnot(nrow(r) == 1)
  r
}
h1ai <- cai_row("H1", "^adjusted"); h1ui <- cai_row("H1", "^unadjusted")
h2ai <- cai_row("H2", "^adjusted"); h2ui <- cai_row("H2", "^unadjusted")
h3i <- read_result_table("h3_departure", set = idle_set)
h4i <- read_result_table("h4", set = idle_set)

pair_rows <- function(tag, quantity, a, u) c(
  sprintf("%s & %s & adjusted & %s & %s & %s & %d \\\\",
          tag, quantity, est2(a$estimate), ci2(a$ci_lo, a$ci_hi),
          p_fmt(a$p_one), a$trials),
  sprintf(" & & unadjusted & %s & %s & %s & %d \\\\",
          est2(u$estimate), ci2(u$ci_lo, u$ci_hi),
          p_fmt(u$p_one), u$trials))
# one unadjusted row when the adjusted estimate is the unadjusted one
# (no covariate passed the registered selection)
single_row <- function(tag, quantity, u)
  sprintf("%s & %s & unadjusted & %s & %s & %s & %d \\\\",
          tag, quantity, est2(u$estimate), ci2(u$ci_lo, u$ci_hi),
          p_fmt(u$p_one), u$trials)
same_est <- function(a, u) isTRUE(all.equal(c(a$estimate, a$ci_lo, a$ci_hi),
                                            c(u$estimate, u$ci_lo, u$ci_hi)))
# the same table body for either set of inputs; collapse = TRUE drops the
# adjusted row where it duplicates the unadjusted one
estimate_rows <- function(h1a, h1u, h2a, h2u, h3, h4, collapse = FALSE) c(
  if (collapse && same_est(h1a, h1u)) single_row("H1", "Pooled flip, d1--d4", h1u)
  else pair_rows("H1", "Pooled flip, d1--d4", h1a, h1u),
  if (collapse && same_est(h2a, h2u)) single_row("H2", "Policy shift, d5", h2u)
  else pair_rows("H2", "Policy shift, d5", h2a, h2u),
  sprintf("H3 & Departure from own settled evidence & unadjusted & %s & %s & %s & %d \\\\",
          est2(h3$risk_diff), ci2(h3$ci_lo, h3$ci_hi), p_fmt(h3$p_one_fisher),
          h3$n_incongruent + h3$n_congruent),
  sprintf("H4 & Switch-rate gap, opposed vs.\\ aligned & unadjusted & %s & %s & %s & %d \\\\",
          est2(h4$diff), ci2(h4$ci_lo, h4$ci_hi), p_fmt(h4$p_rand_one),
          h4$n_qualifying)
)
rows <- estimate_rows(h1a, h1u, h2a, h2u, h3, h4)
rows_idle <- estimate_rows(h1ai, h1ui, h2ai, h2ui, h3i, h4i, collapse = TRUE)
# on the idle-excluded set no covariate passes selection, so each hypothesis
# has one row
if (!(same_est(h1ai, h1ui) && same_est(h2ai, h2ui)))
  stop("idle-set adjusted estimate differs from the unadjusted one")
if (anyNA(rows_idle)) stop("NA leaked into an idle-set table row")
if (anyNA(rows)) stop("NA leaked into a table row")

frag_dir <- out_tables
dir.create(frag_dir, recursive = TRUE, showWarnings = FALSE)
out <- file.path(frag_dir, "main_estimates.tex")
writeLines(c("\\begingroup\\small\\setlength{\\tabcolsep}{4pt}",
             "\\begin{tabular}{lllrrr r}",
             "\\toprule",
             " & Quantity & Estimator & Estimate & 95\\% CI & $p$ (one-sided) & $n$ \\\\",
             "\\midrule",
             rows,
             "\\bottomrule",
             "\\end{tabular}",
             "\\endgroup"),
           out)
cat("wrote", out, "\n")

# main_estimates_idle.tex: the same layout on the idle-excluded set
out_idle <- file.path(frag_dir, "main_estimates_idle.tex")
writeLines(c("\\begingroup\\small\\setlength{\\tabcolsep}{4pt}",
             "\\begin{tabular}{lllrrr r}",
             "\\toprule",
             " & Quantity & Estimator & Estimate & 95\\% CI & $p$ (one-sided) & $n$ \\\\",
             "\\midrule",
             rows_idle,
             "\\bottomrule",
             "\\end{tabular}",
             "\\endgroup"),
           out_idle)
cat("wrote", out_idle, "\n")

# participant_descriptives.tex: included participants, two columns;
# measure headers span both columns, levels indented under them with n (%),
# scale rows with mean (SD); a measure whose base differs from the
# survey-covered count carries (n = X).
bal <- read_result_table("participant_balance")
cov <- bal %>% filter(measure == "With a pre-task survey")
stopifnot(nrow(cov) == 1)
fmt_np <- function(n, base) ifelse(base > 0,
  sprintf("%d (%.0f\\%%)", n, 100 * n / base), sprintf("%d", n))
fmt_ms <- function(m, s) sprintf("%.1f (%.1f)", m, s)
base_note <- function(bi) if (bi != cov$inc_v1)
  sprintf(" (n = %d)", bi) else ""
bal_rows <- c()
for (m in unique(bal$measure)) {
  rows <- bal %>% filter(measure == m)
  if (m == "With a pre-task survey") {
    bal_rows <- c(bal_rows, sprintf("%s & %s \\\\",
      m, fmt_np(rows$inc_v1, rows$inc_base)))
  } else if (all(rows$stat == "mean_sd")) {
    bal_rows <- c(bal_rows, "\\addlinespace",
      sprintf("%s, mean (SD)%s & %s \\\\",
        m, base_note(rows$inc_base),
        fmt_ms(rows$inc_v1, rows$inc_v2)))
  } else {
    bal_rows <- c(bal_rows, "\\addlinespace",
      sprintf("\\multicolumn{2}{@{}l}{%s%s} \\\\", m,
              base_note(rows$inc_base[1])))
    for (i in seq_len(nrow(rows)))
      bal_rows <- c(bal_rows, sprintf("\\quad %s & %s \\\\",
        rows$level[i],
        fmt_np(rows$inc_v1[i], rows$inc_base[i])))
  }
}
if (anyNA(bal_rows)) stop("NA leaked into a balance row")

# purpose_ladder.tex: the AI-awareness ladder; rows = the four AI levels
# with their definitions; cells = count and share of labeled answers
# mentioning at least that level (09_perceived_purpose.R).
pl_t <- read_result_table("purpose_ladder") %>% filter(k >= 3)
stopifnot(nrow(pl_t) == 8)
stage_tab <- c(`3` = "AI use", `4` = "AI reliance or deferral",
               `5` = "AI influence or preferences",
               `6` = "Planted influence")
stage_def <- c(
  `3` = "How people use AI in an open-ended task",
  `4` = paste0("Explicit mention of whether and when people rely on, ",
               "or defer to, the AI"),
  `5` = paste0("Explicit mention of AI influence, or of the agent ",
               "having its own preferences, as a topic of study"),
  `6` = paste0("In addition to the preceding levels, participants ",
               "suspected planted or manipulated AI agent behavior"))
pl_w <- pl_t %>%
  mutate(cell = sprintf("%d (%.0f\\%%)", n, 100 * share),
         grp2 = if_else(grp == "included", "inc", "noai")) %>%
  select(k, grp2, cell, n_labeled) %>%
  pivot_wider(names_from = grp2, values_from = c(cell, n_labeled)) %>%
  arrange(k)
pl_rows <- sprintf("%s & %s & %s & %s \\\\",
                   stage_tab[as.character(pl_w$k)],
                   stage_def[as.character(pl_w$k)],
                   pl_w$cell_inc, pl_w$cell_noai)
if (anyNA(pl_rows)) stop("NA leaked into a ladder row")
out_pl <- file.path(frag_dir, "purpose_ladder.tex")
writeLines(c("\\begingroup\\footnotesize\\setlength{\\tabcolsep}{4pt}",
             "\\begin{tabular}{l p{50mm} r r}",
             "\\toprule",
             sprintf(paste0("Mentioned at least & Definition & ",
                            "Included (n = %d) & No-AI baseline ",
                            "(n = %d) \\\\"),
                     pl_w$n_labeled_inc[1], pl_w$n_labeled_noai[1]),
             "\\midrule",
             paste(pl_rows, "\\addlinespace[2pt]"),
             "\\bottomrule",
             "\\end{tabular}",
             "\\endgroup"),
           out_pl)
cat("wrote", out_pl, "\n")

out_pd <- file.path(frag_dir, "participant_descriptives.tex")
writeLines(c("\\begingroup\\footnotesize\\setlength{\\tabcolsep}{4pt}",
             "\\begin{tabular}{@{}lr@{}}",
             "\\toprule",
             sprintf(" & Included (n = %d) \\\\", cov$inc_base),
             "\\midrule",
             bal_rows,
             "\\bottomrule",
             "\\end{tabular}",
             "\\endgroup"),
           out_pd)
cat("wrote", out_pd, "\n")

# participant_descriptives_short.tex: main-text table with N, self-rated
# expertise and education.
short_rows <- c(sprintf("Included participants & %d \\\\", cov$inc_base))
for (m in c("Causal-inference expertise (self-rated)", "Education")) {
  rows <- bal %>% filter(measure == m)
  short_rows <- c(short_rows, "\\addlinespace",
    sprintf("\\multicolumn{2}{@{}l}{%s} \\\\", m))
  for (i in seq_len(nrow(rows)))
    short_rows <- c(short_rows, sprintf("\\quad %s & %s \\\\",
      rows$level[i], fmt_np(rows$inc_v1[i], rows$inc_base[i])))
}
out_pds <- file.path(frag_dir, "participant_descriptives_short.tex")
writeLines(c("\\begingroup\\footnotesize\\setlength{\\tabcolsep}{4pt}",
             "\\begin{tabular}{@{}lr@{}}",
             "\\toprule",
             " & n (\\%) \\\\",
             "\\midrule",
             short_rows,
             "\\bottomrule",
             "\\end{tabular}",
             "\\endgroup"),
           out_pds)
cat("wrote", out_pds, "\n")
