# 16_code_audit: exploratory audit of the submitted analysis code.

source("R/_setup.R")

sv <- read_coded("code_audit")
stopifnot(nrow(sv) == 61, all(sv$arm %in% c("no-AI", "AI-assisted")))

prev <- map_dfr(c(human = "human_fatal", sol = "sol_fatal",
                  gemini = "gemini_fatal"), function(col) {
  sv %>% group_by(arm) %>% summarise(
    n_sessions = n(),
    n_fatal = sum(.data[[col]] == "1", na.rm = TRUE),
    n_reviewed = sum(!is.na(.data[[col]]) & .data[[col]] != ""),
    share = n_fatal / n_sessions, .groups = "drop")
}, .id = "source")
write_csv(prev, file.path(out_tables, paste0("code_audit_", cfg$set, ".csv")))

jm <- read_coded("code_audit_judges")
write_csv(jm, file.path(out_tables, paste0("code_audit_judges_", cfg$set, ".csv")))

# self-rated causal-inference expertise >= proficient, by audit arm, among
# the arm's participants with a pre-task survey
svn <- read_survey(data_dir) %>%
  select(pid, q_expertise_oci) %>%
  filter(!is.na(q_expertise_oci), q_expertise_oci != "")
expert <- sv %>% inner_join(svn, by = "pid") %>%
  group_by(arm) %>% summarise(
    n_surveyed = n(),
    n_prof_expert = sum(as.integer(q_expertise_oci) >= 3),
    share = n_prof_expert / n_surveyed, .groups = "drop")
write_csv(expert, file.path(out_tables, paste0("code_audit_expertise_", cfg$set, ".csv")))
print(as.data.frame(prev), digits = 3, row.names = FALSE)
