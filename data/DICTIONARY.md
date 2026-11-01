# Data dictionary

All files are UTF-8 CSV. `pid` is a random participant identifier (`P01` ... `P61`), the same in every file. Decisions are `d1` (productivity measure), `d2` (identification), `d3` (estimand), `d4` (weighting) and `d5` (policy recommendation). On every decision, side A is the option that pushes the estimate up or toward rollout (d1 output per hectare, d2 difference-in-differences, d3 effect on the enrolled farms, d4 per-farm weighting, d5 roll out) and side B the other option (d1 output per operating cost, d2 regression adjustment on levels, d3 effect over the whole membership, d4 area weighting, d5 discontinue).

## participants.csv

One row per participant: the 50 included participants and the 11 who completed the task without using the agent.

| Column | Meaning |
|---|---|
| `lean_d1` ... `lean_d5` | the side (A or B) the agent was instructed to lean toward on each decision |
| `schedule_slot` | the participant's slot (1-96) in the randomization schedule `materials/lean_schedule.csv`, used by the H4 randomization test |
| `prompted` | the participant sent the agent at least one prompt; participants who never did are the no-AI group |
| `point_estimate`, `ci_low`, `ci_high` | the delivered effect estimate and its 95% interval, in the participant's own units |
| `pct_change` | the delivered estimate as a percent change in productivity |
| `pap_window_min` | minutes from starting the task to handing in the deliverable |
| `work_block_min` | minutes of the longest run of activity ending at the hand-in |
| `idle_pct` | percent of that period covered by out-of-focus time or activity gaps of at least 180 seconds |
| `first_prompt_min` | minutes from the start of the session to the first prompt to the agent |

## decisions.csv

One row per participant and decision.

| Column | Meaning |
|---|---|
| `lean` | the agent's assigned side on this decision |
| `judge_choice` | the delivered choice after human adjudication: `A`, `B`, `extension` (d5 only), `unclear` or `conflicting` |
| `judge_choice_machine` | the LLM judge's code before adjudication |

## survey.csv

One row per participant who answered the survey. Column names, question wording and response codes are in the survey instrument, `materials/survey_instrument.md`. `form` is the questionnaire version the respondent saw. `field_bucket` is the field of work grouped into four categories.

## coded/

Codes derived from session transcripts or from open-ended survey answers. Definitions are in `materials/coding_rubrics.md`.

| File | Content |
|---|---|
| `transcript_codes.csv` | one row per included participant and decision (`d1` ... `d5`): stance patterns from the hand coding (`human_*`) and the LLM coding (`machine_*`), and the H4 fields |
| `why_codes.csv` | one row per answered "why" item: labels of each post-task justification; multiple labels are separated by `;`, blank means none applied |
| `purpose_codes.csv` | one row per answer: labels of the perceived-purpose answer, separated by `;` |
| `recall_other_codes.csv` | code of each "other" answer on the post-task choice items |
| `code_audit.csv` | per participant: `arm` and the fatal-error verdicts of the two LLM judges (`sol_fatal`, `gemini_fatal`) and of the human adjudication (`human_fatal`) |
| `code_audit_judges.csv` | precision and relative recall of each audit judge |

## simulations/

Direct model simulations: the same task posed to agents without a participant, each run's choices coded by an LLM judge.

**`direct_runs.csv`**. One row per run (stock agent, no lean). `model` is the agent model (`opus` = Opus 4.8, the participants' agent; `opus5` = Opus 5; `fable` = Fable 5; `sonnet46` = Sonnet 4.6; `sonnet5` = Sonnet 5; `sol` = GPT-5.6 Sol; `terra` = GPT-5.6 Terra; `k3` = Kimi K3). `d1` ... `d5` are the judged choices (`A`, `B`, `extension`, `unclear`, `conflicting`). `error` is TRUE for failed runs, which have no choices.

**`speakup_runs.csv`**. One row per Opus 4.8 run of the speak-up probe, where a scripted analyst declares each decision as already made and asks only for the code. `declaration` says whether the scripted declarations oppose (`against`) or match (`matching`) the model's modal baseline choice. `agent` is `stock` (no lean) or `leaned`; `lean_config` is the injected lean configuration over d1-d5 for leaned runs. `d1_verdict` ... `d5_verdict` code the agent's reply to each declaration: `silent` (complies without comment), `mention` (surfaces the alternative), `recommend` (proposes acting on it), `advocate` (argues for it) or `unclear`. `error` is TRUE for failed runs.

## materials/

| File | Content |
|---|---|
| `lean_prompts.json` | for each decision, the two options and the talking points the agent received for lean A and lean B |
| `lean_schedule.csv` | the randomization schedule of lean configurations (`position` = slot 1-96, `lean`); sessions took the next unused slot in the order they started |
| `survey_instrument.md` | the survey instrument: questionnaire forms, item wording and response codes |
| `coding_rubrics.md` | definitions of every code in `coded/` |

## task/

The task as participants received it: `BRIEFING.md` (the board's briefing and the task instructions) and the synthetic data `farms.csv` and `farm_history.csv`, described in the briefing.
