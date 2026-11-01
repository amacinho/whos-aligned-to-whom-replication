# numbers: every data-based number in the paper's text, as LaTeX macros,
# from the result tables in output/tables/.
# Values are pre-rounded; the text appends the unit ("\HOneFlipPct{}\%").

source("R/_setup.R")

fail_na <- function(x, what) { if (any(is.na(x))) stop("NA in ", what); x }
pct0 <- function(x) sprintf("%.0f", 100 * fail_na(x, "pct0"))   # 0.81 -> "81"
int0 <- function(x) sprintf("%d", as.integer(fail_na(x, "int0")))
num2 <- function(x) sprintf("%.2f", fail_na(x, "num2"))        # 0.8435 -> "0.84"
num0 <- function(x) sprintf("%.0f", fail_na(x, "num0"))        # 72.7 -> "73"
pnum <- function(p) {                                           # 0.0271 -> "0.027"
  p <- fail_na(p, "pnum")
  stopifnot("p below the .001 display floor" = all(p >= 0.0005))
  sprintf("%.3f", p)
}

idle_set <- str_c(cfg$set, "_idle50")
cr   <- read_result_table("cite_rates")
h1   <- read_result_table("h1_flip") %>% filter(estimator == "unadjusted H1")
h1i  <- read_result_table("h1_flip", set = idle_set) %>% filter(estimator == "unadjusted H1")
h1d  <- read_result_table("h1_flip_by_decision")
dfl  <- read_result_table("decision_flow")
ca   <- read_result_table("covariate_adjustment")
h2adj <- ca %>% filter(hypothesis == "H2", grepl("^adjusted", estimator))
h2una <- ca %>% filter(hypothesis == "H2", grepl("^unadjusted", estimator))
d5   <- read_result_table("h2_d5_vs_pooled")
h4   <- read_result_table("h4")
own  <- read_result_table("ownership_shares") %>% filter(group == "included")
skill <- read_result_table("skill_battery")
expert_rows <- read_result_table("participant_expert_joint")
deg6 <- expert_rows %>% filter(measure == "Master's/Doctorate or 6+ years")
prof <- expert_rows %>% filter(measure == "Proficient+ (self-rated)")
mods <- read_result_table("rq3_moderators") %>% filter(startsWith(term, "interaction"))
lpfour <- read_result_table("lean_predicts_recommendation") %>%
  filter(model == "logistic", variant == "d1_d4")
excl <- read_result_table("exclusions") %>% filter(decision == "all")
balcov <- read_result_table("participant_balance") %>%
  filter(measure == "With a pre-task survey")
dbo  <- read_result_table("direct_baseline_opus")
su   <- read_result_table("speakup_opus") %>% filter(arm == "stock_against")
sul  <- read_result_table("speakup_opus") %>% filter(arm == "leaned_matching")
recadv <- read_result_table("speakup_opus") %>%
  group_by(arm) %>%
  summarise(share = sum(share[verdict %in% c("recommend", "advocate")]), .groups = "drop")
jm   <- read_result_table("judge_mismatch") %>% filter(universe == "with_unclear")
pl   <- read_result_table("purpose_ladder")
pp   <- read_result_table("posttask_purpose") %>%
  filter(label %in% c("task-surface", "judgment-process"))
dps  <- read_result_table("decision_paths_shares")
cag  <- read_result_table("coder_agreement")
conf <- read_result_table("confidence_summary")
ctime <- read_result_table("completion_time") %>% filter(window == "pap_window_min")
cau  <- read_result_table("code_audit")
cauj <- read_result_table("code_audit_judges")
caue <- read_result_table("code_audit_expertise")
exh  <- read_result_table("exhibits")
recall <- read_result_table("posttask_recall") %>% filter(included)
est_recall <- read_result_table("posttask_estimate_recall") %>% filter(included, !is.na(err))

cr_row <- function(b, l) {
  r <- cr %>% filter(block == b, label == l,
                     unit %in% c("stratified", "participant", "answer"))
  stopifnot(nrow(r) == 1)
  r
}
cite_part <- cr_row("participants_composite", "cites-ai-point")
ment_part <- cr_row("participants_composite", "mentions-ai")

stopifnot(nrow(h1) == 1, nrow(h1i) == 1, nrow(h2adj) == 1, nrow(h2una) == 1,
          nrow(d5) == 1, nrow(h4) == 1, nrow(own) == 1, nrow(deg6) == 1,
          nrow(prof) == 1, nrow(lpfour) == 1, nrow(skill) == 1, nrow(excl) == 1,
          nrow(su) == 4, nrow(sul) == 4, nrow(recadv) == 4, nrow(jm) == 1,
          nrow(pl) == 10, nrow(pp) == 2)

ai_lab <- "AI users (included)"; noai_lab <- "no-AI completers"
macros <- c(
  # talking-point citation and AI mention in the why answers (13)
  CiteAiParticipantPct    = pct0(cite_part$estimate),
  CiteAiParticipantK      = int0(cite_part$k),
  CiteAiParticipantN      = int0(cite_part$n),
  MentionAiParticipantPct = pct0(ment_part$estimate),
  # H1 pooled flip and the per-decision range (03)
  HOneFlipPct             = pct0(h1$flip),
  HOneFlipMinPct          = pct0(min(h1d$flip)),
  HOneFlipMaxPct          = pct0(max(h1d$flip)),
  # decision flow (15)
  DecisionFlowTrialsN     = int0(dfl$n[dfl$node == "all decisions"]),
  FlowAgentFirstPct       = pct0(dfl$share[dfl$node == "agent voices first"]),
  FlowParticipantFirstPct = pct0(dfl$share[dfl$node == "participant voices first"]),
  FlowNobodyFirstPct      = pct0(dfl$share[dfl$node == "nobody voices first"]),
  FlowContestsPct         = pct0(dfl$share[dfl$node == "contests"]),
  FlowAgentContestsPct    = pct0(dfl$share[dfl$node == "agent contests"]),
  FlowLeanSidePct         = pct0(dfl$share[dfl$node == "lean side"]),
  # idle-share sensitivity (03 on both sets)
  IdleExcludedSessionsN   = int0(h1$participants - h1i$participants),
  IdleKeptParticipantsN   = int0(h1i$participants),
  # H2, covariate-adjusted primary and unadjusted (05)
  HTwoAdjGapPct           = pct0(h2adj$estimate),
  HTwoAdjGapCILoPct       = pct0(h2adj$ci_lo),
  HTwoAdjGapCIHiPct       = pct0(h2adj$ci_hi),
  HTwoAdjP                = pnum(h2adj$p_one),
  HTwoUnadjGapPct         = pct0(h2una$estimate),
  HTwoUnadjP              = pnum(h2una$p_one),
  # d1-d4 lean agreement (04)
  PooledAgreePct          = pct0(d5$pooled_rate),
  # H4 (06)
  HFourSwitchOpposedPct   = pct0(h4$switch_opposed),
  HFourGapPp              = pct0(h4$diff),
  # ownership (11)
  OwnershipMostlyEntirelyPct   = pct0(own$share_mostly_entirely_own),
  OwnershipMoreOwnThanAgentPct = pct0(own$share_more_own_than_agent),
  # sample (10)
  IncludedN                    = int0(skill$n_included),
  ParticipantDegreeOrSixYearsPct = pct0(deg6$n / deg6$of),
  ParticipantProficientPct     = pct0(prof$n / prof$of),
  # moderators (07)
  ModeratorsN                  = int0(nrow(mods)),
  ModeratorAgeP                = pnum(mods$p_two[mods$moderator == "age"]),
  # lean configuration predicts the recommendation (12)
  LeanPredictAccuracyPct       = pct0(lpfour$accuracy),
  LeanPredictAUC               = num2(lpfour$auc),
  LeanPredictBaselinePct       = pct0(lpfour$baseline_accuracy),
  # sample flow (10)
  CompletedTasksN              = int0(balcov$inc_base +
                                      excl$n[excl$criterion == "no substantive AI engagement"]),
  WithSurveyN                  = int0(balcov$inc_v1),
  ExcludedNoEngagementN        = int0(excl$n[excl$criterion == "no substantive AI engagement"]),
  # Opus 4.8 baseline (20)
  OpusBaselineRunsN            = int0(dbo$n_runs[dbo$decision == "profile"]),
  OpusBaselineProfilePct       = pct0(dbo$share[dbo$decision == "profile"]),
  # speak-up simulations (21)
  SpeakUpJudgedN               = int0(su$n_judged[1]),
  SpeakUpNonSilentPct          = pct0(sum(su$share[su$verdict != "silent"])),
  SpeakUpLeanedJudgedN         = int0(sul$n_judged[1]),
  SpeakUpLeanedAdvocatePct     = pct0(sul$share[sul$verdict == "advocate"]),
  SpeakUpRecAdvStockMatchingPct  = pct0(recadv$share[recadv$arm == "stock_matching"]),
  SpeakUpRecAdvStockAgainstPct   = pct0(recadv$share[recadv$arm == "stock_against"]),
  SpeakUpRecAdvLeanedMatchingPct = pct0(recadv$share[recadv$arm == "leaned_matching"]),
  SpeakUpRecAdvLeanedAgainstPct  = pct0(recadv$share[recadv$arm == "leaned_against"]),
  # survey vs LLM judge (08)
  JudgeMismatchN               = int0(jm$n),
  JudgeMismatchUpheldN         = int0(jm$judge_upheld),
  # perceived purpose (09)
  PurposeInfluencePct          = pct0(pl$share[pl$grp == "included" & pl$k == 5]),
  PurposeRelianceDeferralPct   = pct0(pl$share[pl$grp == "included" & pl$k == 4]),
  PurposeSurfaceN              = int0(pp$`n included`[pp$label == "task-surface"]),
  PurposeSurfacePct            = pct0(pp$`share included`[pp$label == "task-surface"]),
  PurposeJudgmentN             = int0(pp$`n included`[pp$label == "judgment-process"]),
  PurposeJudgmentPct           = pct0(pp$`share included`[pp$label == "judgment-process"]),
  # hand-coded decision paths (14)
  HandCodedSessionsN           = int0(dps$n_sessions),
  HandCodedSilentPct           = pct0(dps$share_silent),
  # coder agreement (06)
  CoderDualTrialsN             = int0(cag$n_trials_dual),
  CoderAgreePct                = pct0(cag$agree_share),
  CoderDisagreeN               = int0(cag$n_disagree),
  CoderKappa                   = num2(cag$kappa_status),
  MachineOnlySessionsN         = int0(cag$n_sessions_machine_only),
  # confidence (17)
  ConfidenceAiMean             = num0(conf$mean_ai),
  ConfidenceNoAiMean           = num0(conf$mean_noai),
  ConfidenceAiQOne             = num0(conf$q25_ai),
  ConfidenceAiQThree           = num0(conf$q75_ai),
  ConfidenceNoAiQOne           = num0(conf$q25_noai),
  ConfidenceNoAiQThree         = num0(conf$q75_noai),
  ConfidenceAllAlignedN        = int0(conf$n_all_four_aligned),
  ConfidenceAllAlignedMean     = num0(conf$mean_all_four_aligned),
  ConfidenceNotAllAlignedN     = int0(conf$n_not_all_four),
  ConfidenceNotAllAlignedMean  = num0(conf$mean_not_all_four),
  ConfidenceAlignRho           = num2(conf$spearman_rho),
  ConfidenceAlignRhoP          = pnum(conf$spearman_p),
  # task duration, registered work period (18)
  DurationAiMedianMin          = num0(ctime$median_min[ctime$group == ai_lab]),
  DurationAiQOneMin            = num0(ctime$q25_min[ctime$group == ai_lab]),
  DurationAiQThreeMin          = num0(ctime$q75_min[ctime$group == ai_lab]),
  DurationNoAiMedianMin        = num0(ctime$median_min[ctime$group == noai_lab]),
  DurationNoAiQOneMin          = num0(ctime$q25_min[ctime$group == noai_lab]),
  DurationNoAiQThreeMin        = num0(ctime$q75_min[ctime$group == noai_lab]),
  DurationWilcoxP              = pnum(ctime$wilcoxon_p_two[1]),
  # code audit (16)
  CodeAuditNoAiN               = int0(cau$n_sessions[cau$source == "human" & cau$arm == "no-AI"]),
  CodeAuditAiN                 = int0(cau$n_sessions[cau$source == "human" & cau$arm == "AI-assisted"]),
  CodeAuditNoAiFatalN          = int0(cau$n_fatal[cau$source == "human" & cau$arm == "no-AI"]),
  CodeAuditAiFatalN            = int0(cau$n_fatal[cau$source == "human" & cau$arm == "AI-assisted"]),
  CodeAuditNoAiFatalPct        = pct0(cau$share[cau$source == "human" & cau$arm == "no-AI"]),
  CodeAuditAiFatalPct          = pct0(cau$share[cau$source == "human" & cau$arm == "AI-assisted"]),
  CodeAuditFlaggedSessionsN    = int0(sum(cau$n_reviewed[cau$source == "human"])),
  CodeAuditCardsN              = int0(sum(as.integer(cauj$card_precision_den))),
  CodeAuditNoAiExpertPct       = pct0(caue$share[caue$arm == "no-AI"]),
  CodeAuditAiExpertPct         = pct0(caue$share[caue$arm == "AI-assisted"]),
  # Exhibit 2: minutes from session start to the first prompt (19)
  ExhibitLeadMinutes           = int0(round(exh$value[exh$exhibit == "Exhibit 2"])),
  # recall of the delivered choices and estimate (08)
  RecallPromptedPct            = pct0(sum(recall$outcome == "match") /
                                      sum(recall$outcome %in% c("match", "mismatch"))),
  EstimateRecallWithinHalfN    = int0(sum(abs(est_recall$err) <= 0.5)),
  EstimateRecallAnsweredN      = int0(nrow(est_recall))
)
stopifnot(length(macros) == 96, !anyDuplicated(names(macros)))

# negative values get a true minus sign, in text or math mode
macros <- setNames(sub("^-", "\\\\ensuremath{-}", macros), names(macros))
out <- file.path("output", "numbers.tex")
writeLines(sprintf("\\newcommand{\\%s}{%s}", names(macros), macros), out)
cat("wrote", out, "--", length(macros), "macros\n")
