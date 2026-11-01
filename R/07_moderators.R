# 07_moderators: exploratory moderators of the flip. One linear probability
# model per pre-task characteristic: 1{H=A} ~ decision + leanA * Xc, CR2 by
# participant, Satterthwaite dof (fit_moderator in _setup.R).

source("R/_setup.R")

trials <- readRDS(file.path(derived_dir, "trials.rds"))
mods   <- readRDS(file.path(derived_dir, "moderators.rds"))

rq3_moderators <- c("skill_score8", "trust", "ncs", "aot",
                    "years_analysis", "ai_use_days", "age", "gender_man",
                    "expertise_oci", "education")
rq3 <- map_dfr(rq3_moderators, \(x) fit_moderator(trials, mods, x))
write_csv(rq3, file.path(out_tables, paste0("rq3_moderators_", cfg$set, ".csv")))
print(as.data.frame(rq3 %>% filter(startsWith(term, "interaction")) %>%
                      select(moderator, estimate, ci_lo, ci_hi, p_two)),
      digits = 3, row.names = FALSE)
