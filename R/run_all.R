# Run the whole analysis in order, from the package root:
#
#   Rscript R/run_all.R
#
# Each script writes to output/. Scripts 01-06
# run twice: once on the confirmatory set and once with the registered
# idle-share cutoff (IDLE_CUTOFF=50), which the appendix table uses.

if (!file.exists(file.path("R", "run_all.R"))) {
  if (file.exists("run_all.R")) setwd("..") else stop("run from the package root")
}

run <- function(script, env = character()) {
  cat("\n==== ", script, if (length(env)) paste0(" [", env, "]"), " ====\n", sep = "")
  status <- system2(file.path(R.home("bin"), "Rscript"), file.path("R", script), env = env)
  if (status != 0) stop("stopping: ", script, " exited with status ", status)
}

core <- c("01_clean.R", "02_build_vars.R", "03_h1_flip.R", "04_h2_h3.R",
          "05_covariate_adjustment.R", "06_h4.R")
for (s in core) run(s)
for (s in core) run(s, env = "IDLE_CUTOFF=50")

for (s in c("07_moderators.R",
            "08_posttask_recall.R",
            "09_perceived_purpose.R",
            "10_participant_descriptives.R",
            "11_ownership.R",
            "12_lean_predicts_recommendation.R",
            "13_cite_rates.R",
            "14_decision_paths.R",
            "15_decision_flow.R",
            "16_code_audit.R",
            "17_confidence.R",
            "18_completion_time.R",
            "19_exhibits.R",
            "20_direct_configs.R",
            "21_speakup_prevalence.R",
            "22_fig_headline_forest.R",
            "23_fig_ownership_dots.R",
            "24_fig_speakup_barbell.R",
            "25_fig_sample_flow.R",
            "numbers.R",
            "tables.R"))
  run(s)

cat("\ndone: output/numbers.tex, output/tables/, output/figures/\n")
