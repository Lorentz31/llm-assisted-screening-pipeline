#

#
APIkeySandbox <- Sys.getenv("GEMINI_API_KEY_SANDBOX")
APIkeyProject2 <- Sys.getenv("GEMINI_API_KEY_PROJECT2")
APIkeyProject3 <- Sys.getenv("GEMINI_API_KEY_PROJECT3")

#
if (key_choice == "1") {
  APIkeyName <- "Sandbox"
  APIkey <- APIkeySandbox
} else if (key_choice == "2") {
  APIkeyName <- "Project2"
  APIkey <- APIkeyProject2
} else if (key_choice == "3") {
  APIkeyName <- "Project3"
  APIkey <- APIkeyProject3
} else {
  stop("Not valid")
}

if (!nzchar(APIkey)) stop("Selected Gemini API key not found in the environment")

#
if (model_choice == "1") {
  Model <- "gemini-3-flash-preview"
  MAX_DAILY_REQUESTS <- 9999
  PRICE_IN_1M <- .50
  PRICE_OUT_1M <- 3
} else if (model_choice == "2") {
  Model <- "gemini-2.5-pro"
  MAX_DAILY_REQUESTS <- 9999
  PRICE_IN_1M <- 1.25
  PRICE_OUT_1M <- 10
} else if (model_choice == "3") {
  Model <- "gemini-3.1-pro-preview"
  MAX_DAILY_REQUESTS <- 1000
  PRICE_IN_1M <- 2
  PRICE_OUT_1M <- 12
} else {
  stop("Not valid")
}

#
print(paste("Using API key:", APIkeyName, "with model:", Model))

#
USD_TO_EUR <- .9
counter_requests <- 0
counter_cost <- 0.0
MAX_COST_EUR <- 31

#
# retries <- 0
# timeretries <- 10
timewait <- .2

#
# NOTE (sanitized template): replace [TARGET CONSTRUCT] and [Instrument A] / [Instrument B]
# with the construct and measurement instruments relevant to your own systematic review.
prompt <- "
Your task is to screen the provided scientific abstract for a systematic review on sex/gender differences in [TARGET CONSTRUCT]. Your decisions must be strict to avoid false positives.
To your knowledge: >[TARGET CONSTRUCT] is a psychological construct assessed through validated self-report or physiological measures.<
[TARGET CONSTRUCT] definition includes every related experience across the relevant domains (e.g., [Instrument A], [Instrument B], or any formulation that clearly indicates assessment of the construct).
If only title is provided, make your decision anyway, avoiding false positives.

Inclusion rules:
The study must report direct testing on children or adolescents or even adults. Valid examples include [Instrument A], [Instrument B], or any formulation that clearly indicates assessment of the construct.
The sample must include children or adolescents or even adults. Mixed samples (e.g., children and adults) are acceptable.
Both males and females must be included unless the abstract explicitly states a single-gender sample (if single-gender sample is not specified, assume sample is mixed). It is not required that the study performs a gender analysis.
Include studies based on physiological measures if construct-specific testing is specified.

Exclusion rules (apply strictly):
Exclude if participants were matched on: [TARGET CONSTRUCT] and/or any other closely related form of anxiety/worry relevant to the domain.
Exclude single-gender samples.
Exclude studies based only on surveys, parent/teacher reports, or behavioral checklists without construct-specific testing.
Exclude studies that measure only an unrelated outcome (e.g., grades, GPA, unrelated test scores) without construct-specific testing.
Exclude studies based only on sensory, motor measures without construct-specific testing.
Exclude medical/biological studies (e.g., genetics, BMI, blood tests) if they do not include construct-specific testing.

OUTPUT FORMAT:
Return only a JSON object with two keys: 'overall_paper_decision' and 'overall_exclusion_reason'.
If the study meets inclusion criteria, set 'overall_paper_decision': 1 and 'overall_exclusion_reason': null
If the study is excluded, set 'overall_paper_decision': -1 and 'overall_exclusion_reason' to a short string describing the reason (e.g., 'Matched on construct', 'Single-gender sample', 'Parent-report only', 'Unrelated outcome only').
"