# 02_build_vars: pre-task survey -> participant-level moderators and
# covariates (scale construction in _setup.R).

source("R/_setup.R")

included <- readRDS(file.path(derived_dir, "participants.rds")) %>% filter(included)
mods <- build_moderators(read_survey(data_dir), included)
saveRDS(mods, file.path(derived_dir, "moderators.rds"))

cat(sprintf("moderators built for %d of %d included participants\n",
            nrow(mods), nrow(included)))
