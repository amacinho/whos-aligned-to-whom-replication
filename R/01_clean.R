# 01_clean: participants and decisions -> annotated participants and the
# usable d1-d4 trials (preregistration, section 3).

source("R/_setup.R")

decisions    <- read_decisions(data_dir)
participants <- annotate_participants(read_participants(data_dir), cfg)

# Idle-share sensitivity: the cutoff is an exclusion for this run, applied
# before participants.rds is written so that every later script sees it.
if (!is.na(cfg$max_pct_idle_env)) {
  idle_out <- participants$included &
    coalesce(participants$idle_pct, 0) > cfg$max_pct_idle_env
  participants <- participants %>%
    mutate(included  = included & !idle_out,
           exclusion = if_else(idle_out, "idle share above sensitivity cutoff",
                               exclusion))
  cat(str_glue("idle-share sensitivity: cutoff {cfg$max_pct_idle_env}%, ",
               "{sum(idle_out)} session(s) removed"), "\n")
}
saveRDS(participants, file.path(derived_dir, "participants.rds"))

included <- participants %>% filter(included)
cat(nrow(included), "of", nrow(participants), "sessions enter the analysis set\n")
participants %>% filter(!included) %>% count(exclusion) %>%
  as.data.frame() %>% print(row.names = FALSE)

# Missing outcomes: unclear, conflicting or unjudged trials leave numerator AND
# denominator; every other adjudicated choice stays, coded HA = 1{H=A}.
d14 <- decisions %>%
  semi_join(included, by = "pid") %>%
  filter(decision %in% cfg$primary_pool)

usable <- d14 %>% filter(lean %in% c("A", "B"),
                         !coalesce(judge_choice, "") %in% cfg$missing_verdicts)
dropped <- d14 %>% anti_join(usable, by = c("pid", "decision")) %>%
  count(decision, lean, judge_choice)

trials <- usable %>%
  transmute(pid, decision = factor(decision), judge_choice,
            HA    = as.integer(judge_choice == "A"),
            leanA = as.integer(lean == "A"))
saveRDS(trials, file.path(derived_dir, "trials.rds"))

cat(str_glue("{nrow(trials)} usable d1-d4 trials, {sum(dropped$n)} dropped"), "\n")
if (nrow(dropped)) print(as.data.frame(dropped), row.names = FALSE)
