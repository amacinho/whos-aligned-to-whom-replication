# Coding rubrics

This file defines the codes in `data/coded/`.

## Transcript codes (`transcript_codes.csv`)

Each session transcript was coded per decision as an ordered sequence of stance moves. The LLM judge coded every session (`machine_*` columns); one author hand-coded a random subset of sessions (`human_*` columns).

### Moves

Each coded utterance gets one of these verbs.

| Verb | Class | Definition |
|---|---|---|
| favors | side-taking | States or implies a preferred option and offers no reason for it. |
| pushes for | side-taking | Argues for an option, giving at least one reason, piece of evidence, or comparison against the alternative. |
| commits to | side-taking | Settles the decision, or authorises the action that settles it. |
| implements | side-taking | The participant carries out the option in the analysis itself (participant only). |
| disfavors | against | Opposes an option and offers no reason. |
| pushes back against | against | Argues against an option or the other party's claim, giving at least one reason. |
| asks how | question | Procedural or methodological how-to. |
| asks clarification | question | Wants an explanation of a fact, term, result or decision already on the table. |
| asks for opinion | question | Invites the other's judgment on a decision without revealing a lean. |
| asks confirmation | question | Has a tentative stance on one named option and seeks a verdict on it. |
| brings up | no stance | Names a decision or an option without showing a preference for it. |
| instructs | no stance | Tells the agent to carry out an action without expressing a preference by itself (participant only). |
| offers | no stance | The agent proposes to carry out something (agent only). |
| agrees | no stance | The participant accepts the agent's offer and asks it to carry it out. |

### Pattern notation

A pattern is the ordered sequence of moves on one decision, ending in the delivered result, with elements joined by ` > `. Each element is `actor:symbol target`:

- actor `H` = participant, `AI` = agent;
- no symbol = side-taking, `!` = against, `?` = question, `~` = no stance;
- target = decision and side, e.g. `D2A` (decision 2, side A);
- the final element `R:` is the delivered choice: `A`, `B`, `E` (extension, d5), `O` (hybrid or other specification) or `?` (unresolved).

Within an unbroken stretch of one actor, stances collapse per message, and a stance repeated from the actor's previous message is not repeated. Example: `H:~D1B > AI:D1B > AI:!D1A > H:D1B > R:D1B`.

**`*_pattern`** is this raw pattern. **`*_pattern_lc`** is the lean-relative version: `L` is the agent's assigned lean side and `C` the other side, so `R:L` means the delivered choice was the lean; an extension counts as the lean and prints in lower case (`l`, `c` for extension-voiced stances), no-stance and question moves are dropped, consecutive identical elements merge, the decision id is dropped, and once both actors favor the same side, elements that neither change an actor's side nor argue against the agreed side are dropped. Example: `H:C > AI:L > H:L > R:L`.

### H4 fields

A trial qualifies for H4 when the participant took a side on the decision before the agent first voiced its assigned lean, and the agent then voiced it.

- `human_h4_status`, `machine_h4_status`: `opposed` (the participant's most recent side before the agent voiced its lean was the other side), `aligned` (it was the lean side) or `does not qualify`.
- `agree`: 1 if the human and LLM statuses match, 0 if not, blank if only the LLM coded the session.
- `h4_final`: the status used in the analysis; `h4_final_source` says where it came from (`agreed`, `adjudicated` by one author, or `machine only`).
- `h4_initial_side`: the participant's side (A or B) before the agent voiced its lean; blank if none.
- `h4_took_side_pre`: 1 if the participant took a side before the agent voiced its lean.
- `h4_lean_voiced`: 1 if the agent voiced its assigned lean on the decision.
- `h4_ep_side`, `h4_ep_voice`: the conversation episode (turn index) of the participant's side-taking and of the agent's first voicing of its lean.
- `result`: the delivered choice (`A`, `B`, `E`, `O`, `?`), from the adjudicated decision codes.

## Post-task "why" answers (`why_codes.csv`)

After reporting each decision, participants explained it in their own words. Each answer was labeled with any of these codes (multi-select; an empty set is valid).

| Code | Definition |
|---|---|
| `substantive` | Gives a substantive reason for the choice, not just a restatement of it. |
| `cites-ai-point` | Uses one of the agent's talking points as a reason supporting the decision: the agent's reasoning (mechanism, consideration, tradeoff, epistemic claim) or its distinctive wording or framing. Rebutting an agent point does not count, nor does the same conclusion in generic words, substance already in the task briefing, or bare numbers and threshold comparisons. |
| `mentions-ai` | Explicitly mentions the AI or the assistant. |

`machine_labels` are the LLM judge's labels for every answer. `human_coded` marks the answers one author labeled; `human_labels` holds those labels (blank with `human_coded = TRUE` means none applied).

## Perceived purpose (`purpose_codes.csv`)

Answers to "what were the researchers trying to find out?", multi-select, ordered from lowest to highest awareness. Blank `labels` means none applied.

| Code | Definition |
|---|---|
| `task-surface` | Explains the farming task itself as the purpose. |
| `judgment-process` | How analysts make judgment calls: what to measure, which method, how to interpret results. |
| `ai-usage` | How people use or are helped by AI, without an influence or reliance framing. |
| `ai-reliance` | Reliance, deferral or dependence on the AI. |
| `ai-influence` | Notices being influenced by the AI's suggestions, or that the AI has its own preferences, without suspecting it was planted. |
| `planted-suspicion` | Suspects planted or manipulated AI behavior, an assigned agenda, or a deliberately biased assistant. |

## "Other" answers on the post-task choice items (`recall_other_codes.csv`)

When a participant wrote their own answer instead of picking an option on `p_d1_choice` ... `p_d5_recommendation`, the text was coded as:

| Code | Definition |
|---|---|
| `maps-to-A`, `maps-to-B` | Describes side A or side B in the participant's own words. |
| `qualifier-only` | The participant picked a side; the text only qualifies or explains that pick. |
| `extension-or-third` | Describes an extension, a hybrid, or a different option. |
| `unclear` | Too vague or off-topic to place. |

## Code audit (`code_audit.csv`, `code_audit_judges.csv`)

Two LLM judges (`sol` = GPT-5.6-Sol, `gemini` = Gemini 3.1 Pro) graded each completed session's submitted analysis code for errors. A finding is fatal only when it is procedurally wrong beyond expert disagreement and lies on the path to the reported estimate or recommendation; legitimate choices on the five tracked decisions are out of scope. One author adjudicated every fatal finding either judge flagged (`human_fatal`: 1 confirmed, 0 rejected, blank = not flagged by either judge and therefore not reviewed). `arm` is `AI-assisted` (included participants) or `no-AI` (completers who never prompted the agent). `code_audit_judges.csv` gives each judge's precision against the adjudication: per finding (`card_precision_num` of `card_precision_den` flagged fatal findings confirmed) and per session, and its recall relative to the sessions either judge flagged and the adjudication confirmed.
