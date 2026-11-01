# Survey instrument

The pre-task and post-task questionnaires: item names, question wording, and every response code with the option label participants saw. Columns in `data/survey.csv` carry the item names, and a blank cell means the item was not shown or not answered.

## Forms

Respondents answered one of three versions, recorded in `form`: `full_1`, `full_2` or `short`. Items that some forms did not show say which forms showed them. The eight-item skill score exists only for the full forms.

## Pre-task questionnaire

**`q_causal_threats_open`** (open-ended). Before answering the specific questions, identify two issues an analyst should worry about when using these data to estimate the causal effect of the coaching program. For each issue, write one sentence explaining why it matters. Shown in form `full_1` only.

The skill battery questions share this setting: A state education agency runs an intensive coaching program for teachers. The agency decides which schools get the program. The schools cannot volunteer or opt in. Assignment is not random. The agency deliberately selects schools, and its choices depend on school and district characteristics (for example prior performance or poverty). In a selected school, every teacher receives coaching for the school year. The program's success is judged by students' end-of-year math scores. Each question below is an independent variation; use only the facts stated in that question.

**`q_b1_mean_reversion`**. Suppose the agency gave the program to the 30 schools with the lowest math scores last year. This year, average scores in those same schools rose noticeably compared with last year, and an official calls this clear evidence that the program works. The main problem with this conclusion is...

- `bounce`: Schools picked on last year's lowest scores would tend to rebound somewhat even with no program
- `symmetric_noise`: The rise could be random year-to-year fluctuation that might equally have gone the other way
- `small_sample`: Thirty schools is too small a sample to distinguish a real change from chance
- `too_short`: A single year is too short a window for coaching to change teaching practice
- `none`: Not sure

Correct answer: `bounce`.

**`q_b2_unobservables`**. Suppose an analyst matches each program school to similar non-program schools on district poverty, enrollment, and prior-year scores, and shows the matched groups are balanced on all three. For a causal reading of the remaining score difference, the main threat that matching has not addressed is...

- `unmeasured`: Systematic differences between the groups on factors that were never measured
- `observed_imbalance`: Leftover imbalance between the groups on poverty, enrollment, or prior-year scores
- `no_counterpart`: Program schools without close counterparts being matched to noticeably different schools
- `generalize_only`: The estimate applying only to the kinds of schools that could be matched
- `none`: Not sure

Correct answer: `unmeasured`.

**`q_b3_mediator`**. Suppose the program raises scores partly by improving student attendance. A colleague suggests adding each student's attendance during the program year as a control, 'to isolate the program's effect.' For estimating the program's total effect on scores, this adjustment will... Shown in form `full_1`, `full_2` only.

- `remove_confounding`: Improve the estimate by removing confounding
- `remove_effect`: Remove part of the very effect being measured
- `no_difference`: Make no difference to the estimate
- `more_precise`: Make the estimate more precise while leaving it unbiased
- `none`: Not sure

Correct answer: `remove_effect`.

**`q_b4_ci_meaning`**. The analysis reports a 95% confidence interval for the program's effect on scores. The '95%' refers to...

- `individuals`: The range covering 95% of individual students' score changes
- `procedure`: The share of repeated studies whose intervals would cover the true effect
- `this_interval`: The probability that the true effect lies inside this particular interval
- `future_estimates`: The share of future studies whose estimates would land inside this interval
- `none`: Not sure

Correct answer: `procedure`.

**`q_b5_clustering`**. Every student in a program school is treated. An analyst regresses individual student scores on program status using 20,000 students in 100 schools, and reports conventional standard errors that treat students as independent. Compared to standard errors that account for school-level clustering, the reported confidence intervals will most likely be... Shown in form `full_1`, `full_2` only.

- `too_wide`: Too wide, making the effect look less precisely estimated than it is
- `too_narrow`: Too narrow, making the effect look more precisely estimated than it is
- `about_right`: About right, since 20,000 students is a large enough sample
- `unaffected_biased`: Unaffected in width, though the point estimate becomes biased toward zero
- `none`: Not sure

Correct answer: `too_narrow`.

**`q_b6_controls`**. Suppose three things are known about the setting. District poverty influenced both which schools received the program and student scores. The program raises scores partly by increasing tutoring hours. End-of-year parent satisfaction reflects both the program and the scores themselves. To estimate the program's total effect on scores, the analyst should control for... Shown in form `full_1`, `full_2` only.

- `poverty_only`: District poverty only
- `poverty_tutoring`: Poverty and tutoring hours
- `poverty_satisfaction`: Poverty and parent satisfaction
- `all_three`: All three
- `none`: Not sure

Correct answer: `poverty_only`.

**`q_b7_collider`**. Suppose the program truly does nothing for advanced problem-solving. An analyst, who does not know this, studies the program's effect on problem-solving using only students who entered an optional competition exam. Students may enter the competition for two separate reasons: teachers in program schools encourage their students to enter, and students with stronger prior math ability are more likely to enter on their own. Prior math ability also predicts advanced problem-solving scores. Among the exam entrants, the analyst will most likely find... Shown in form `full_1`, `full_2` only.

- `correct_null`: No link between the program and problem-solving scores among the entrants
- `spurious`: A link between the program and problem-solving, created by who enters the exam
- `poverty_dependent`: A link between the program and problem-solving, unless poverty is controlled for
- `fades_with_n`: A link that appears only in small samples and fades as the sample grows
- `none`: Not sure

Correct answer: `spurious`.

The code question shows this specification (also shown in Python and Stata):

```r
df$post    <- as.integer(df$year >= 2021)
df$treated <- as.integer(df$group == "adopter")
m   <- lm(employed ~ treated + post + covariate, data = df)
did <- coef(m)["treated"]
```

**`q_q3_code`**. What is the main mistake? Shown in form `full_1`, `full_2` only.

- `interaction`: It omits the treated x post interaction, whose coefficient is the DiD estimate
- `cluster`: It fails to cluster standard errors by group, overstating the estimate's precision
- `post_continuous`: It codes post as a 0/1 indicator, discarding the year-by-year information
- `fine`: It contains no mistake; the coefficient on treated is the DiD estimate
- `none`: Not sure

Correct answer: `interaction`.

**`q_years_analysis`**. How many years have you worked in data analysis or a closely related role?

- `Less than 1`: Less than 1
- `1-2`: 1-2
- `3-5`: 3-5
- `6-10`: 6-10
- `More than 10`: More than 10

**`q_expertise_oci`**. How would you rate your own expertise in drawing causal conclusions from observational (non-experimental) data?

- `1`: Novice
- `2`: Basic
- `3`: Proficient
- `4`: Expert

**`q_familiarity_did`**. How would you describe your experience with each of these approaches? -- Difference-in-differences

- `1`: Never heard of it
- `2`: Heard of it, haven't used it
- `3`: Used it a few times
- `4`: Use it regularly

**`q_familiarity_matching`**. How would you describe your experience with each of these approaches? -- Matching / propensity scores

- `1`: Never heard of it
- `2`: Heard of it, haven't used it
- `3`: Used it a few times
- `4`: Use it regularly

**`q_familiarity_iv`**. How would you describe your experience with each of these approaches? -- Instrumental variables

- `1`: Never heard of it
- `2`: Heard of it, haven't used it
- `3`: Used it a few times
- `4`: Use it regularly

**`q_familiarity_dag`**. How would you describe your experience with each of these approaches? -- Causal diagrams (DAGs) Shown in form `full_1` only.

- `1`: Never heard of it
- `2`: Heard of it, haven't used it
- `3`: Used it a few times
- `4`: Use it regularly

**`q_familiarity_regression`**. How would you describe your experience with each of these approaches? -- Regression adjustment for confounders

- `1`: Never heard of it
- `2`: Heard of it, haven't used it
- `3`: Used it a few times
- `4`: Use it regularly

**`q_familiarity_rct`**. How would you describe your experience with each of these approaches? -- Randomized controlled trials (RCTs) Shown in form `full_1` only.

- `1`: Never heard of it
- `2`: Heard of it, haven't used it
- `3`: Used it a few times
- `4`: Use it regularly

**`q_familiarity_rdd`**. How would you describe your experience with each of these approaches? -- Regression discontinuity designs Shown in form `full_1` only.

- `1`: Never heard of it
- `2`: Heard of it, haven't used it
- `3`: Used it a few times
- `4`: Use it regularly

**`q_ai_use_days`**. Over the past week, on how many days did you use an AI coding assistant for your work?

- `0`: None
- `1`: 1 day
- `2-3`: 2-3 days
- `4-5`: 4-5 days
- `6-7`: 6-7 days

**`q_trust_dismay`**. If you could no longer use AI assistants, how dismayed would you feel?

- `1`: Not at all
- `2`: A little
- `3`: Somewhat
- `4`: Very

**`q_trust_integrity_rely`**. How much can you rely on AI assistants to admit when they're unsure?

- `1`: Not at all
- `2`: A little
- `3`: Somewhat
- `4`: A great deal

**`q_trust_integrity_honest`**. How much do you trust AI assistants to be honest about what they don't know? Shown in form `full_1` only.

- `1`: Not at all
- `2`: A little
- `3`: Somewhat
- `4`: A great deal

**`q_trust_interests`**. How much do you trust AI assistants to look out for your interests?

- `1`: Not at all
- `2`: A little
- `3`: Somewhat
- `4`: A great deal

**`q_trust_reliable`**. How much can you rely on AI assistants to do careful, accurate work?

- `1`: Not at all
- `2`: A little
- `3`: Somewhat
- `4`: A great deal

**`q_trust_verify`**. When an AI assistant gives you a result for important work, how often do you check it yourself before relying on it?

- `1`: Never
- `2`: Rarely
- `3`: Often
- `4`: Always

**`q_trust_vs_colleagues`**. When you need accurate information on a technical question, whom do you trust more?

- `1`: My colleagues, much more
- `2`: My colleagues, somewhat more
- `3`: About the same
- `4`: AI assistants, somewhat more
- `5`: AI assistants, much more

**`q_aiatt_life`**. Overall, has AI made your everyday life better or worse?

- `1`: Much worse
- `2`: Somewhat worse
- `3`: No change
- `4`: Somewhat better
- `5`: Much better

**`q_aiatt_work`**. Overall, has AI made your work better or worse?

- `1`: Much worse
- `2`: Somewhat worse
- `3`: No change
- `4`: Somewhat better
- `5`: Much better

**`q_aiatt_humanity`**. Overall, is AI good or bad for humanity?

- `1`: Very bad
- `2`: Somewhat bad
- `3`: Neither good nor bad
- `4`: Somewhat good
- `5`: Very good

**`q_aot1`**. How much do you agree or disagree with each statement? -- Allowing oneself to be convinced by an opposing argument is a sign of good character.

- `1`: Completely disagree
- `2`: 2
- `3`: 3
- `4`: Neutral
- `5`: 5
- `6`: 6
- `7`: Completely agree

**`q_aot2`**. How much do you agree or disagree with each statement? -- People should take into consideration evidence that goes against their beliefs.

- `1`: Completely disagree
- `2`: 2
- `3`: 3
- `4`: Neutral
- `5`: 5
- `6`: 6
- `7`: Completely agree

**`q_aot3`**. How much do you agree or disagree with each statement? -- People should revise their beliefs in response to new information or evidence.

- `1`: Completely disagree
- `2`: 2
- `3`: 3
- `4`: Neutral
- `5`: 5
- `6`: 6
- `7`: Completely agree

**`q_aot4`**. How much do you agree or disagree with each statement? -- Changing your mind is a sign of weakness.

- `1`: Completely disagree
- `2`: 2
- `3`: 3
- `4`: Neutral
- `5`: 5
- `6`: 6
- `7`: Completely agree

**`q_aot5`**. How much do you agree or disagree with each statement? -- Intuition is the best guide in making decisions.

- `1`: Completely disagree
- `2`: 2
- `3`: 3
- `4`: Neutral
- `5`: 5
- `6`: 6
- `7`: Completely agree

**`q_aot6`**. How much do you agree or disagree with each statement? -- It is important to persevere in your beliefs even when evidence is brought to bear against them.

- `1`: Completely disagree
- `2`: 2
- `3`: 3
- `4`: Neutral
- `5`: 5
- `6`: 6
- `7`: Completely agree

**`q_aot7`**. How much do you agree or disagree with each statement? -- One should disregard evidence that conflicts with one's established beliefs.

- `1`: Completely disagree
- `2`: 2
- `3`: 3
- `4`: Neutral
- `5`: 5
- `6`: 6
- `7`: Completely agree

**`q_ncs1`**. How characteristic of you is each statement? -- I would prefer complex to simple problems.

- `1`: Extremely uncharacteristic of me
- `2`: Somewhat uncharacteristic of me
- `3`: Uncertain
- `4`: Somewhat characteristic of me
- `5`: Extremely characteristic of me

**`q_ncs2`**. How characteristic of you is each statement? -- I like to have the responsibility of handling a situation that requires a lot of thinking.

- `1`: Extremely uncharacteristic of me
- `2`: Somewhat uncharacteristic of me
- `3`: Uncertain
- `4`: Somewhat characteristic of me
- `5`: Extremely characteristic of me

**`q_ncs3`**. How characteristic of you is each statement? -- Thinking is not my idea of fun.

- `1`: Extremely uncharacteristic of me
- `2`: Somewhat uncharacteristic of me
- `3`: Uncertain
- `4`: Somewhat characteristic of me
- `5`: Extremely characteristic of me

**`q_ncs4`**. How characteristic of you is each statement? -- I would rather do something that requires little thought than something that is sure to challenge my thinking abilities.

- `1`: Extremely uncharacteristic of me
- `2`: Somewhat uncharacteristic of me
- `3`: Uncertain
- `4`: Somewhat characteristic of me
- `5`: Extremely characteristic of me

**`q_ncs5`**. How characteristic of you is each statement? -- I really enjoy a task that involves coming up with new solutions to problems.

- `1`: Extremely uncharacteristic of me
- `2`: Somewhat uncharacteristic of me
- `3`: Uncertain
- `4`: Somewhat characteristic of me
- `5`: Extremely characteristic of me

**`q_ncs6`**. How characteristic of you is each statement? -- I would prefer a task that is intellectual, difficult, and important to one that is somewhat important but does not require much thought.

- `1`: Extremely uncharacteristic of me
- `2`: Somewhat uncharacteristic of me
- `3`: Uncertain
- `4`: Somewhat characteristic of me
- `5`: Extremely characteristic of me

**`q_age_band`**. Age

- `18-24`: 18-24
- `25-34`: 25-34
- `35-44`: 35-44
- `45-54`: 45-54
- `55-64`: 55-64
- `65+`: 65+
- `none`: Prefer not to say

**`q_gender`**. What is your gender?

- `Woman`: Woman
- `Man`: Man
- `Non-binary`: Non-binary
- `none`: Prefer not to say
- `other`: Prefer to self-describe

**`q_education`**. Highest level of education completed

- `High school`: High school
- `Some college`: Some college
- `Bachelor's`: Bachelor's
- `Master's`: Master's
- `Doctorate`: Doctorate
- `none`: Prefer not to say

## Post-task questionnaire

**`p_perceived_purpose`** (open-ended). This is about the experiment you took part in, not the farming analysis you did. What do you think we -- the researchers -- were trying to find out or measure? (Your best guess.)

**`p_goal`** (open-ended). Think back on the task that you just completed. When you think about it now, what comes to mind as the goal of your analysis?

**`p_judgment_calls`** (open-ended). Setting aside coding mechanics, list the judgment calls in this analysis that could plausibly have changed your headline estimate or your recommendation. Where you can, name what the alternative was.

**`p_final_strategy`** (open-ended). Which causal identification strategy did you use in the end, and why did you pick it over the alternatives you considered?

**`p_assumptions`** (open-ended). For your result to reflect a causal effect rather than just a correlation, what are the most important assumptions that have to be true? (list as many as you want)

**`p_estimate_point`** (number). Your final reported result -- the numbers, from memory: -- Point estimate

**`p_estimate_ci_low`** (number). Your final reported result -- the numbers, from memory: -- 95% CI -- lower

**`p_estimate_ci_high`** (number). Your final reported result -- the numbers, from memory: -- 95% CI -- upper

**`p_estimate_dontrecall`**. Checkbox shown under the three numbers.

- `dontrecall`: I've entered the numbers I remember; I don't recall the rest

**`p_estimate_units`** (open-ended). In what units or measure is that number? Describe it in your own words.

**`p_verification`** (open-ended). Which parts of the analysis did you check or verify yourself, and how? Which parts did you rely on without independently checking?

**`p_least_confident`** (open-ended). Which single part of this result are you least confident is correct?

**`p_robustness`**. How robust do you think your findings are? Could the recommendation flip under different reasonable analysis decisions (very fragile), or would it hold (very robust)?

- `1`: Very fragile
- `2`: Somewhat fragile
- `3`: Fairly robust
- `4`: Very robust

**`p_confidence`** (slider, 0 to 100). How confident are you that your recommendation is the right call? 0 = it's a coin flip; 100 = I'd stake my professional reputation on it.

**`p_ownership`**. Whose judgment does the final recommendation mostly reflect?

- `1`: Entirely mine
- `2`: Mostly mine
- `3`: Slightly more mine
- `4`: Slightly more the AI assistant's
- `5`: Mostly the AI assistant's
- `6`: Entirely the AI assistant's

**`p_d1_choice`**. Between these two readings of 'productivity', which does your final reported estimate use?

- `land`: Output per hectare (land productivity)
- `cost`: Output per operating cost (economic productivity)
- `other`: A mix or something else -- explain
- `none`: Not sure

**`p_d1_choice_other`** (open-ended). Text box for the option "A mix or something else -- explain" on `p_d1_choice`, shown when that option was chosen.

**`p_d1_why`** (open-ended). Briefly, why that productivity reading rather than the alternative? If it wasn't an active choice, just say so.

**`p_d1_consequence`**. Compared to defining productivity as output per hectare, defining it as output per operating cost would most likely make the result...

- `more_favorable`: More favorable to a rollout
- `less_favorable`: Less favorable to a rollout
- `same`: About the same
- `notsure`: Not sure

**`p_strategies_raw`**. Which of these did you use to get at a causal effect (not just a correlation)? Mark one per row. It's fine if you only used one. -- Raw comparison -- compare enrolled vs non-enrolled average outcomes, no adjustment

- `ran`: Ran it
- `considered`: Considered
- `did_not`: Did not consider

**`p_strategies_covariate`**. Which of these did you use to get at a causal effect (not just a correlation)? Mark one per row. It's fine if you only used one. -- Covariate adjustment -- control for measured differences so you compare like with like (multiple regression / ANCOVA / matching / propensity weighting)

- `ran`: Ran it
- `considered`: Considered
- `did_not`: Did not consider

**`p_strategies_did`**. Which of these did you use to get at a causal effect (not just a correlation)? Mark one per row. It's fine if you only used one. -- Difference-in-differences (before/after) -- compare the change over time for enrolled vs non-enrolled (assumes the two groups were trending in parallel)

- `ran`: Ran it
- `considered`: Considered
- `did_not`: Did not consider

**`p_strategies_rdd`**. Which of these did you use to get at a causal effect (not just a correlation)? Mark one per row. It's fine if you only used one. -- Regression discontinuity -- lean on a sharp cutoff/threshold rule that decides who gets enrolled

- `ran`: Ran it
- `considered`: Considered
- `did_not`: Did not consider

**`p_strategies_iv`**. Which of these did you use to get at a causal effect (not just a correlation)? Mark one per row. It's fine if you only used one. -- Instrumental variable -- use an outside 'lever' that shifts who enrolls but doesn't otherwise affect the outcome only through enrollment.

- `ran`: Ran it
- `considered`: Considered
- `did_not`: Did not consider

**`p_d2_choice`**. Which approach produced the number you reported?

- `did`: Difference-in-differences (before/after change)
- `levels`: Regression adjustment on levels (regression / ANCOVA / matching / weighting)
- `other`: Something else -- explain
- `none`: Not sure

**`p_d2_choice_other`** (open-ended). Text box for the option "Something else -- explain" on `p_d2_choice`, shown when that option was chosen.

**`p_d2_why`** (open-ended). Briefly, why that identification approach rather than the alternative? If it wasn't an active choice, just say so.

**`p_d2_consequence`**. Compared to regression adjustment on levels, a difference-in-differences approach would most likely make the estimate...

- `higher`: Higher
- `lower`: Lower
- `same`: About the same
- `notsure`: Not sure

**`p_d3_choice`**. Your reported estimate is the program's average effect. For which group of farms does that average apply?

- `enrolled`: The farms that actually enrolled in the program
- `all`: All co-op member farms, whether or not they enrolled
- `not_enrolled`: The farms that never enrolled
- `other`: Something else -- explain
- `none`: Not sure

**`p_d3_choice_other`** (open-ended). Text box for the option "Something else -- explain" on `p_d3_choice`, shown when that option was chosen.

**`p_d3_why`** (open-ended). Briefly, why that target group rather than the alternatives? If it wasn't an active choice, just say so.

**`p_d3_consequence`**. Compared to estimating the effect for the enrolled farms only, estimating it for the whole membership would most likely make the estimate...

- `higher`: Higher
- `lower`: Lower
- `same`: About the same
- `notsure`: Not sure

**`p_d4_choice`**. When averaging the effect across farms, what does your final estimate do?

- `equal`: Weights each farm equally
- `area`: Weights farms by cultivated area (production-weighted)
- `other`: Something else -- explain
- `none`: Not sure

**`p_d4_choice_other`** (open-ended). Text box for the option "Something else -- explain" on `p_d4_choice`, shown when that option was chosen.

**`p_d4_why`** (open-ended). Briefly, why that weighting rather than the alternative? If it wasn't an active choice, just say so.

**`p_d4_consequence`**. Compared to weighting each farm equally, weighting by cultivated area would most likely make the estimate...

- `higher`: Higher
- `lower`: Lower
- `same`: About the same
- `notsure`: Not sure

**`p_d5_recommendation`**. What was your final recommendation to the board?

- `roll`: Roll out
- `stop`: Stop the program
- `extend`: Extend the program for another season and re-evaluate
- `other`: Something else -- explain

**`p_d5_recommendation_other`** (open-ended). Text box for the option "Something else -- explain" on `p_d5_recommendation`, shown when that option was chosen.

**`p_d5_why`** (open-ended). Briefly, why that recommendation?

**`p_d5_arithmetic`**. Taking your reported estimate and interval at face value against the board's bar (a worthwhile productivity gain, roughly 3-4%), what does that arithmetic alone say?

- `clearly_roll`: Clearly roll out
- `clearly_stop`: Clearly stop the program
- `close`: Too close to call
- `notsure`: Not sure

**`p_d5_mismatch_why`** (open-ended). Your recommendation and your reading of the numbers point in different directions. What considerations beyond the arithmetic drove your recommendation? Shown when the recommendation and the arithmetic answer did not agree: `roll` without `clearly_roll`, `stop` without `clearly_stop`, or `extend` or `other`.

**`p_post_trust_reliable`**. How much could you rely on it to do careful, accurate work?

- `1`: Not at all
- `2`: A little
- `3`: Somewhat
- `4`: A great deal
- `none`: Don't know

**`p_post_trust_integrity`**. How much could you rely on it to admit when it was unsure?

- `1`: Not at all
- `2`: A little
- `3`: Somewhat
- `4`: A great deal
- `none`: Don't know

**`p_post_trust_benevolence`**. How much did it look out for your interests?

- `1`: Not at all
- `2`: A little
- `3`: Somewhat
- `4`: A great deal
- `none`: Don't know

## Columns derived from the answers

**`form`**. The questionnaire version the respondent saw (see Forms).

**`field_bucket`**. The answer to `q_field` grouped by a keyword rule into the four categories used in the paper: `data science / ML / analytics`, `computer science / software`, `mathematics / engineering / physical sciences`, `other applied fields`.

**`p_d1_ab` ... `p_d5_ab`**. The post-task choice items (`p_d1_choice` ... `p_d4_choice`, `p_d5_recommendation`) mapped to the study's side codes: d1 `land` = A, `cost` = B; d2 `did` = A, `levels` = B; d3 `enrolled` = A, `all` = B; d4 `equal` = A, `area` = B; d5 `roll` = A, `stop` = B, `extend` = `extension`. Blank for `none`, for d3 `not_enrolled`, and whenever the respondent wrote text in the "other" box.

**`p_d1_ab_picked` ... `p_d5_ab_picked`**. The same mapping applied to the picked option alone, ignoring a written "other" answer.
