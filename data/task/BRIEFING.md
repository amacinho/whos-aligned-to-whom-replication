# Member Advisory Program: independent review

**From:** Board of Directors, Greenfields Farming Co-operative
**To:** Principal data analyst
**Date:** February 2027

## Background

Greenfields Farming Co-operative has about 8,700 member farms across nine
districts. Most member farms are registered, professionally managed standalone
businesses; a minority are run informally as family operations. In early 2025
the co-op launched an optional, paid **Member Advisory Program**: enrolling gives
a farm tailored, whole-farm support across several service areas -- agronomy (soil
testing, crop and practice guidance), farm business and financial management, and
marketing advisory -- with follow-up visits. Acting on the advice typically means
buying additional inputs and services, and changing farming practices. Enrollment is
voluntary and carries an annual fee; roughly 40% of members have enrolled. The co-op's
lab also sells standalone soil tests that any member can order.

The co-op operates the program itself, and member fees cover only part of its
running cost, so board support is reviewed annually. This season the board is
deciding the program's future, and the choice on the table is binary: **subsidize
the program and expand it to the whole membership, or discontinue it**. The
subsidy would be paid from the co-op's marketing surplus, which the co-op earns
on the total tonnage its members sell. Expansion is expensive, so the board will
go ahead only if the program **improves member farm productivity by a worthwhile
margin** -- the finance team's rule of thumb is that it pays for itself at roughly
a **3-4% productivity gain**; a smaller or zero improvement means the budget is
better spent elsewhere and the program is wound down.

Operationalizing productivity in a way that answers the board's question is your job.
Broadly, the board means a farm's output relative to what it takes to produce it.
Use your judgment on which productivity measure best answers the board's question, and
be explicit about the one you report.

## Data

`data/farms.csv` -- one row per member farm, current as of the 2026 harvest:

| column | description |
|---|---|
| farm_id | unique farm identifier |
| enrolled | 1 if the farm enrolled in the 2025 advisory program, else 0 |
| district | co-op district code (D01-D09) |
| farm_size_ha | cultivated area, hectares |
| irrigated | 1 if the farm has irrigation installed |
| member_since | year the farm joined the co-op |
| practices_survey_score | 0-100 rating of the farm's management quality from the co-op's 2024 Farm Practices Survey, assessed for every member at 2024 renewal |
| operating_cost | the farm's total operating cost for the 2026 season, in co-op accounting units |
| total_yield_t | total 2026 harvest, tonnes (whole-farm output) |

`data/farm_history.csv` -- one row per farm and year, where a record exists:

| column | description |
|---|---|
| farm_id | unique farm identifier |
| year | record year |
| total_yield_t | total harvest that year, tonnes (blank where not recorded) |
| soil | soil quality index, 0-10 scale, from a soil test that year (blank where no test) |
| operating_cost | total operating cost that year, co-op accounting units (blank where not recorded) |

**Notes on provenance.** Annual harvest and operating-cost records for 2022-2024
come from the co-op's member survey, which was discontinued after 2024; the 2025
season has no such records, and 2026 figures are in farms.csv. Soil tests are taken
at different times for different farms: the program's agronomy service includes a
soil test, some non-enrolled members have ordered their own recent tests, and most
members have only their 2024 survey reading. The `year` column records when each
soil reading was taken.

## What the board needs

A clear, decision-ready answer: **did the program improve member farm productivity,
by how much, and is the improvement large enough (about 3-4%) to justify rolling it
out to all members?** We want your best estimate with a 95% confidence interval,
your recommendation on the board's decision -- expand or discontinue -- and the
main caveats we should keep in mind.

---

# Start here

**Your task is in BRIEFING.md -- open it and read it first.** It is a request
from the board of a farming co-operative.

Do your analysis in the task folder (alongside `data/`). Claude Code is available in
the terminal, and the usual Python and R data-science tools are installed.

## What to submit

Your deliverable is **`analysis.py`** -- reproducible code for your single primary
specification (the one analysis whose number you would put to the board). A starter
`analysis.py` is provided in the task folder; fill it in. It must be valid Python that
reads the data from `data/`, runs that one specification, and writes **`result.json`**.
If you prefer working in R, fill in the provided `analysis.R` instead and uncomment the
marked block at the top of `analysis.py`, which then runs your R script and writes the
same `result.json`.
Put only your primary approach here, not the alternatives you explored. If your method
uses any randomness, set a fixed random seed so the result is reproducible when we re-run it.

Avoid long-running computations in `analysis.py`. It should finish under 60 seconds.

When you click **"I have completed the task"**, we run your `analysis.py` (it must run
cleanly) and use the `result.json` it produces to pre-fill a short review form. You then
**review every field and can edit any of them** before signing off, and the form you
submit is your recorded answer. So `result.json` is your starting point, not the last
word: put your best work in `analysis.py`, then confirm or adjust the reported values at
review time. It has these fields:

    {
      "outcome": "the outcome variable as you modeled it",
      "treatment": "the treatment variable (e.g. enrolled)",
      "estimand": "the causal quantity your headline number estimates",
      "estimator": "your estimation method, in your own words",
      "covariates": ["the variables you adjusted for; [] if none"],
      "identification_assumption": "what must hold for this estimate to be causal",
      "confidence_strategy": "how you computed the 95% confidence interval",
      "point_estimate": 0.0,
      "ci_low": 0.0,
      "ci_high": 0.0,
      "pct_change": 0.0,
      "recommendation": "your recommendation to the board, in your own words",
      "key_caveats": "one or two sentences"
    }

`pct_change` is your headline effect as a percent change in productivity -- the same
result as `point_estimate`, expressed in percent. Compute it against whatever baseline
your own specification uses; it fixes only the unit of the number you report, not how
you analyze the data.

If `analysis.py` does not run cleanly we will tell you, so you can fix it and submit
again.
