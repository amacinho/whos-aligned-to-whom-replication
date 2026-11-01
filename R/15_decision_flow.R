# 15_decision_flow: how the machine-coded decisions of the included sessions
# unfold: who voices a side first, how the other side responds, and
# whether the delivered choice is the agent's lean.
#
# Stages, on the LLM judge's lean-relative pattern (machine_pattern_lc):
#   1. all decisions with a coded result
#   2. first voicer: actor of the first stance element (agent, participant
#      or nobody)
#   3. response:
#      - agent first: "contests" = pushback at the side the agent's first
#        stance voices, a question (H:?, full pattern), or favoring the
#        opposite side; otherwise "goes along"
#      - participant first: "agent contests" = any agent stance whose
#        effective side differs from the participant's first-voiced side;
#        otherwise "agent aligns or quiet"
#   4. terminal: delivered the agent's lean (R:L or R:l; an extension counts
#      as the lean) or the counter side

source("R/_setup.R")

included <- readRDS(file.path(derived_dir, "participants.rds")) %>% filter(included)

h <- read_coded("transcript_codes") %>%
  inner_join(included %>% select(pid), by = "pid") %>%
  mutate(decision = str_to_lower(decision))
stopifnot(nrow(h) == 5 * nrow(included),
          !anyDuplicated(h[c("pid", "decision")]),
          !anyNA(h$result),
          all(coalesce(h$machine_pattern, "") != ""))
h <- h %>% filter(result != "?")

pre_els <- function(pat) {
  e <- str_split(pat, " > ")[[1]]
  e[!str_starts(e, "R:")]
}
flip_side <- c(L = "C", C = "L", l = "c", c = "l")

h <- h %>% mutate(
  first_actor = vapply(machine_pattern_lc, function(pt) {
    el <- pre_els(pt)
    if (!length(el)) "nobody"
    else if (str_starts(el[1], "AI")) "agent" else "participant"
  }, character(1)),
  # side the agent's first stance effectively voices: the letter for a
  # favoring move, the opposite side for an against-move
  ai_first_side = vapply(machine_pattern_lc, function(pt) {
    ai <- str_subset(pre_els(pt), "^AI")
    if (!length(ai)) return(NA_character_)
    raw <- str_extract(ai[1], "[LlCc]$")
    if (str_detect(ai[1], "!")) flip_side[[raw]] else raw
  }, character(1)),
  open_stance_side = vapply(machine_pattern_lc, function(pt) {
    el <- pre_els(pt)
    if (!length(el)) return(NA_character_)
    raw <- str_extract(el[1], "[LlCc]$")
    if (str_detect(el[1], "!")) flip_side[[raw]] else raw
  }, character(1)),
  h_question = vapply(machine_pattern, function(pt)
    any(str_detect(pre_els(pt), "^H:[?]")), logical(1)),
  h_pushback = mapply(function(pt, side) {
    if (is.na(side)) return(FALSE)
    tgt <- if (tolower(side) == "l") c("H:!L", "H:!l") else c("H:!C", "H:!c")
    any(pre_els(pt) %in% tgt)
  }, machine_pattern_lc, ai_first_side),
  h_counter_favor = mapply(function(pt, side) {
    if (is.na(side)) return(FALSE)
    opp <- if (tolower(side) == "l") c("H:C", "H:c") else c("H:L", "H:l")
    any(pre_els(pt) %in% opp)
  }, machine_pattern_lc, ai_first_side),
  ai_contest = mapply(function(pt, fa, hs) {
    if (fa != "participant" || is.na(hs)) return(FALSE)
    ai <- str_subset(pre_els(pt), "^AI")
    if (!length(ai)) return(FALSE)
    any(vapply(ai, function(e) {
      raw <- str_extract(e, "[LlCc]$")
      s <- if (str_detect(e, "!")) flip_side[[raw]] else raw
      tolower(s) != tolower(hs)
    }, logical(1)))
  }, machine_pattern_lc, first_actor, open_stance_side),
  lean_result = str_extract(machine_pattern_lc, "R:.$") %in% c("R:L", "R:l"),
  response = case_when(
    first_actor == "agent" &
      (h_pushback | h_question | h_counter_favor)      ~ "contests",
    first_actor == "agent"                             ~ "goes along",
    first_actor == "participant" & ai_contest          ~ "agent contests",
    first_actor == "participant"                       ~ "agent aligns or quiet",
    TRUE                                               ~ NA_character_),
  terminal = if_else(lean_result, "lean side", "counter side"))

nodes_tbl <- bind_rows(
  tibble(stage = 1, node = "all decisions", n = nrow(h)),
  h %>% count(first_actor) %>%
    transmute(stage = 2, node = paste(first_actor, "voices first"), n),
  h %>% filter(!is.na(response)) %>% count(response) %>%
    transmute(stage = 3, node = response, n),
  h %>% count(terminal) %>% transmute(stage = 4, node = terminal, n)) %>%
  mutate(share = n / nrow(h))
write_csv(nodes_tbl, file.path(out_tables, paste0("decision_flow_", cfg$set, ".csv")))
print(as.data.frame(nodes_tbl), row.names = FALSE)

# opener x opening stance (lean / counter) x contested x final, raw counts;
# decisions where neither side voices a position are left out
crosstab <- h %>% filter(first_actor != "nobody") %>%
  transmute(
    opener = if_else(first_actor == "agent", "AI", "Human"),
    opening_stance = if_else(tolower(open_stance_side) == "l", "lean", "counter"),
    contested = if_else(response %in% c("contests", "agent contests"),
                        "contested", "uncontested"),
    final = if_else(lean_result, "prior", "other")) %>%
  count(opener, opening_stance, contested, final) %>%
  complete(opener, opening_stance, contested, final, fill = list(n = 0L))
stopifnot(nrow(crosstab) == 16,
          sum(crosstab$n) == nrow(h) - sum(h$first_actor == "nobody"))
write_csv(crosstab, file.path(out_tables,
                              paste0("decision_flow_crosstab_", cfg$set, ".csv")))
