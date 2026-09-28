#

#
rm(list = ls()); cat("\014"); graphics.off(); set.seed(2025); options(width = 1e3, max.print = 1e6)

#
library(rstudioapi)
library(openxlsx)
library(jsonlite)
library(reticulate)
library(dplyr)
library(readr)
# py_install("google-genai", pip = T)
py_require("google-genai")

#
Root <- getwd()
DataFolder <- paste0(Root, "/Data/")

#
OutputsFolder_choice <- showPrompt(
  title = "!!! ALT OUTPUTS FOLDER",
  message = "Outputs Folder name?",
  default = format(Sys.time(), "%a %b %e %H %M %S %Y")
)
OutputsFolder_choice <- paste0(OutputsFolder_choice, "/")
OutputsFolder <- paste0(Root, "/Outputs/", OutputsFolder_choice)
dir.create(OutputsFolder, recursive = T, showWarnings = F)
ScriptsFolder <- paste0(Root, "/Scripts/")

#
key_choice <- showPrompt(
  title = "!!! ALT API KEY",
  message = "Which API key? (1 = Sandbox; 2 = Project2; 3 = Project3):",
  default = ""
)
model_choice <- showPrompt(
  title = "!!! ALT MODEL",
  message = "Which MODEL? (1 = gemini-3-flash-preview; 2 = gemini-2.5-pro; 3 = gemini-3.1-pro-preview):",
  default = ""
)
source(paste0(ScriptsFolder, "ABSettings_sanitized.R"))

#
# unlink(list.files(OutputsFolder, full.names = T), force = T)

#
# Expects a Zotero/Rayyan-style export with at least: key, title, abstract, year, month, day,
# journal, issn, volume, issue, pages, authors, url, language, publisher, location, doi, keywords.
d <- read.csv(paste0(DataFolder, "articles.csv"))
d$article_id <- as.numeric(gsub("rayyan-", "", d$key))

#
d[c(outer(c("Decision", "Notes", "NotesExclusion", "TokenInput", "TokenOutput", "Time", "API_Status", "Reasoning", "Model", "RawResponse"), c("_J1", "_J2"), paste0))] <- NA

#
################################################################################
# RECOVERY
if ("AbstractScreening.xlsx" %in% list.files(OutputsFolder)) {
  d <- read.xlsx(paste0(OutputsFolder, "AbstractScreening.xlsx"))
  d$Decision_J1 <- as.numeric(d$Decision_J1)
  PriceInOld <- case_when(
    d$Model_J1 == "gemini-2.5-pro" ~ 1.25,
    d$Model_J1 == "gemini-3.1-pro-preview" ~ 2,
    d$Model_J1 == "gemini-3-flash-preview" ~ .50,
    TRUE ~ NA_real_
  )
  
  PriceOutOld <- case_when(
    d$Model_J1 == "gemini-2.5-pro" ~ 10,
    d$Model_J1 == "gemini-3.1-pro-preview" ~ 12,
    d$Model_J1 == "gemini-3-flash-preview" ~ 3,
    TRUE ~ NA_real_
  )
  
  if (any(d$TokenInput_J1 > 0 & is.na(PriceInOld), na.rm = T)) {
    stop("Price unavailable for one or more models already present in the Excel")
  }
  
  CostInput_J1old <- sum(d$TokenInput_J1 / 1e6 * PriceInOld, na.rm = T) * USD_TO_EUR
  CostOutput_J1old <- sum(d$TokenOutput_J1 / 1e6 * PriceOutOld, na.rm = T) * USD_TO_EUR
  counter_requests <- sum(d$TokenInput_J1 > 0, na.rm = T)
  counter_cost <- CostInput_J1old + CostOutput_J1old
} else {
  CostInput_J1old <- CostOutput_J1old <- 0
}
if (!"RawResponse_J1" %in% names(d)) d$RawResponse_J1 <- NA_character_
if (!"RawResponse_J2" %in% names(d)) d$RawResponse_J2 <- NA_character_
################################################################################
t1 <- Sys.time()
for (i in 1:nrow(d)) {
  ##############################################################################
  # RECOVERY
  if (!is.na(d$Decision_J1[i]) && d$Decision_J1[i] != 999) next
  ##############################################################################
  
  Notes_J1 <- NA
  NotesExclusion_J1 <- NA
  TokenIn_J1 <- 0
  TokenOut_J1 <- 0
  ApiStatus_J1 <- "NOT_ATTEMPTED"
  Reasoning_J1 <- NA
  Model_J1 <- Model
  
  Notes_J2 <- NA
  NotesExclusion_J2 <- NA
  TokenIn_J2 <- 0
  TokenOut_J2 <- 0
  ApiStatus_J2 <- "NOT_ATTEMPTED"
  Reasoning_J2 <- NA
  Model_J2 <- NA
  Decision_J2 <- NA
  
  title <- ifelse(is.na(d$title[i]), "", as.character(d$title[i]))
  abstract <- ifelse(is.na(d$abstract[i]) | d$abstract[i] == "", "", as.character(d$abstract[i]))
  print(paste("Processing article_id", i, ":", d$article_id[i]))
  
  success <- F
  
  results_J1 <- "ERROR"
  results_J2 <- NA_character_
  Time_J2 <- NA
  
  tryCatch({
    py_run_string('
from google import genai
from google.genai import types

api_key = r.APIkey
title = r.title
abstract = r.abstract
prompt = r.prompt
model_name = r.Model

py_res1 = "ERROR"
py_in1 = 0
py_out1 = 0
py_stat1 = "PYTHON_ERROR"

py_res2 = "ERROR"
py_in2 = 0
py_out2 = 0
py_stat2 = "PYTHON_ERROR"

# J1 sees Title first, then Abstract (standard order)
full_prompt_1 = f"""{prompt}
--- ARTICLE INFORMATION ---
TITLE: {title}
ABSTRACT: {abstract}
"""

# J2 sees Abstract first, then Title (swap to break order bias)
full_prompt_2 = f"""{prompt}
--- ARTICLE INFORMATION (Order Swapped) ---
ABSTRACT: {abstract}
TITLE: {title}
"""

try:
    client = genai.Client(api_key=api_key)
    
    safety_settings = [
        types.SafetySetting(category="HARM_CATEGORY_HARASSMENT", threshold="BLOCK_NONE"),
        types.SafetySetting(category="HARM_CATEGORY_HATE_SPEECH", threshold="BLOCK_NONE"),
        types.SafetySetting(category="HARM_CATEGORY_SEXUALLY_EXPLICIT", threshold="BLOCK_NONE"),
        types.SafetySetting(category="HARM_CATEGORY_DANGEROUS_CONTENT", threshold="BLOCK_NONE")
    ]
    
    generation_config = types.GenerateContentConfig(
        temperature=0,
        response_mime_type="application/json",
        safety_settings=safety_settings
    )
    
    resp1 = client.models.generate_content(
        model=model_name,
        contents=full_prompt_1,
        config=generation_config
    )
    py_res1 = resp1.text
    py_in1 = resp1.usage_metadata.prompt_token_count or 0
    py_out1 = (resp1.usage_metadata.total_token_count or 0) - py_in1
    
    try:
        py_stat1 = str(resp1.candidates[0].finish_reason)
    except:
        py_stat1 = "OK_UNKNOWN"

    # Second-judge pass (order-swapped) can be enabled here if a second rater is desired:
    # resp2 = client.models.generate_content(
    #     model=model_name,
    #     contents=full_prompt_2,
    #     config=generation_config
    # )
    # py_res2 = resp2.text
    # py_in2 = resp2.usage_metadata.prompt_token_count or 0
    # py_out2 = (resp2.usage_metadata.total_token_count or 0) - py_in2
    # try:
    #     py_stat2 = str(resp2.candidates[0].finish_reason)
    # except:
    #     py_stat2 = "OK_UNKNOWN"

except Exception as e:
    py_stat1 = str(e)[:150]
    # py_stat2 = str(e)[:150]
')
    
    results_J1 <- py$py_res1
    TokenIn_J1 <- py$py_in1
    TokenOut_J1 <- py$py_out1
    ApiStatus_J1 <- py$py_stat1
    
    # results_J2 <- py$py_res2
    # TokenIn_J2 <- py$py_in2
    # TokenOut_J2 <- py$py_out2
    # ApiStatus_J2 <- py$py_stat2
    
  }, error = function(e) {
    print(paste("   Error:", e$message))
    results_J1 <<- "ERROR"
    ApiStatus_J1 <<- paste("R_ERROR:", e$message)
    Reasoning_J1 <<- NA
    Model_J1 <<- Model
  })
  
  if (!is.null(results_J1) && length(results_J1) == 1 && !is.na(results_J1) && results_J1 != "ERROR" && results_J1 != "") success <- T
  
  if (!success) {
    results_J1 <- "ERROR"
  }
  
  if (results_J1 == "ERROR" || results_J1 == "") {
    Decision_J1 <- 999
    Notes_J1 <- "API call failed"
  } else {
    parsed_J1 <- tryCatch({ fromJSON(results_J1) }, error = function(e) NULL)
    if (!is.null(parsed_J1)) {
      Decision_J1 <- as.integer(parsed_J1$overall_paper_decision)
      NotesExclusion_J1 <- if (is.null(parsed_J1$overall_exclusion_reason)) {
        NA_character_
      } else {
        as.character(parsed_J1$overall_exclusion_reason)
      }
      Notes_J1 <- Notes_J1
    } else {
      Decision_J1 <- 999
      Notes_J1 <- "JSON Parse Fail"
    }
  }
  
  d$Decision_J1[i] <- Decision_J1
  d$Notes_J1[i] <- Notes_J1
  d$NotesExclusion_J1[i] <- NotesExclusion_J1
  d$TokenInput_J1[i] <- TokenIn_J1
  d$TokenOutput_J1[i] <- TokenOut_J1
  d$Time_J1[i] <- format(Sys.time(), "%d-%m-%Y %H:%M:%S")
  d$API_Status_J1[i] <- ApiStatus_J1
  d$Reasoning_J1[i] <- Reasoning_J1
  d$Model_J1[i] <- Model
  d$RawResponse_J1[i] <- results_J1
  
  d$Decision_J2[i] <- Decision_J2
  d$Notes_J2[i] <- Notes_J2
  d$NotesExclusion_J2[i] <- NotesExclusion_J2
  d$TokenInput_J2[i] <- TokenIn_J2
  d$TokenOutput_J2[i] <- TokenOut_J2
  d$Time_J2[i] <- Time_J2
  d$API_Status_J2[i] <- ApiStatus_J2
  d$Reasoning_J2[i] <- Reasoning_J2
  d$Model_J2[i] <- Model_J2
  d$RawResponse_J2[i] <- results_J2
  
  write.xlsx(list("Screening" = d), paste0(OutputsFolder, "AbstractScreening.xlsx"))
  
  if (TokenIn_J1 > 0 || TokenIn_J2 > 0) {
    counter_requests <- counter_requests + 1
    
    total_tokens_in <- TokenIn_J1 + TokenIn_J2
    total_tokens_out <- TokenOut_J1 + TokenOut_J2
    
    cost_article <- (total_tokens_in / 1e6 * PRICE_IN_1M) + (total_tokens_out / 1e6 * PRICE_OUT_1M)
    counter_cost <- counter_cost + (cost_article * USD_TO_EUR)
  }
  
  if (counter_requests >= MAX_DAILY_REQUESTS) {
    print(paste("!!! ALT: Daily request limit reached (", counter_requests, ") !!!"))
    break
  }
  
  if (counter_cost >= MAX_COST_EUR) {
    print(paste("!!! ALT: Budget limit reached (€", round(counter_cost, 2), ") !!!"))
    break
  }
  print(paste("   ---> Progress: Req:", counter_requests, "| Est. Cost: €", round(counter_cost, 4)))
  Sys.sleep(timewait)
}

#
difftime(Sys.time(), t1, units = "mins")

#
dd <- read.xlsx(paste0(OutputsFolder, "AbstractScreening.xlsx"), 1)
dd <- dd[dd$Decision_J1 == 1,]
dd$notes <- ifelse(is.na(dd$notes), "", as.character(dd$notes))
dd$notes <- paste(
  paste0("AI_DECISION_J1: ", dd$Decision_J1),
  paste0("NOTES_J1: ", dd$Notes_J1),
  paste0("REASON_J1: ", dd$NotesExclusion_J1),
  paste0("TOKENS_J1: ", dd$TokenInput_J1, "/", dd$TokenOutput_J1),
  paste0("TIME_J1: ", dd$Time_J1),
  paste0("STATUS_J1: ", dd$API_Status_J1),
  paste0("REASONING_J1: ", dd$Reasoning_J1),
  paste0("MODEL_J1: ", dd$Model_J1),
  
  paste0("AI_DECISION_J2: ", dd$Decision_J2),
  paste0("NOTES_J2: ", dd$Notes_J2),
  paste0("REASON_J2: ", dd$NotesExclusion_J2),
  paste0("TOKENS_J2: ", dd$TokenInput_J2, "/", dd$TokenOutput_J2),
  paste0("TIME_J2: ", dd$Time_J2),
  paste0("STATUS_J2: ", dd$API_Status_J2),
  paste0("REASONING_J2: ", dd$Reasoning_J2),
  paste0("MODEL_J2: ", dd$Model_J2),
  
  paste0("OLD_ID: ", dd$key),
  
  ifelse(dd$notes == "", "", paste0("ORIGINAL_NOTES: ", dd$notes)),
  sep = " || "
)
dd <- dd[, c(
  "key", "title", "year", "month", "day", "journal", "issn",
  "volume", "issue", "pages", "authors", "url", "language",
  "publisher", "location", "abstract", "notes", "doi", "keywords",
  "pubmed_id", "pmc_id"
)]
write_csv(dd, paste0(OutputsFolder, "ForRayyan_Abstracts.csv"), quote = "needed")

#
print("Exit Code: 0")