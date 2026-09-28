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
library(readxl)
# py_install("google-genai", pip = T)
py_require("google-genai")

#
Root <- getwd()
DataFolder <- paste0(Root, "/Data/")

ScriptsFolder <- paste0(Root, "/Scripts/")
PDFFolder <- paste0(DataFolder, "/storage/")

#
OutputsFolder_choice <- showPrompt(
  title = "!!! ALT OUTPUTS FOLDER",
  message = "Outputs Folder name?",
  default = format(Sys.time(), "%a %b %e %H %M %S %Y")
)
OutputsFolder_choice <- paste0(OutputsFolder_choice, "/")
OutputsFolder <- paste0(Root, "/Outputs/", OutputsFolder_choice)
dir.create(OutputsFolder)

#
key_choice <- showPrompt(
  title = "!!! ALT API KEY",
  message = "Which API key? (1 = Sandbox; 2 = Project2; 3 = Project3):",
  default = ""
)
model_choice <- showPrompt(
  title = "!!! ALT MODEL",
  message = "Which MODEL? (1 = gemini-2.5-pro; 2 = gemini-3.1-pro-preview):",
  default = ""
)
source(paste0(ScriptsFolder, "FSSCOSettings_sanitized.R")) 

#
# Expects a screening export (e.g. from Rayyan) with at least: File.Attachments (path to PDF,
# relative to a local reference-manager storage folder), Publication.Year, Item.Type.
d1 <- read.csv(paste0(DataFolder, "Elementi esportati.csv"), 1)
d1[c("ID", 
     "Included",
     "Data",
     "Note.about.data/further.reason.for.exclusion",
     "e-mail",
     "Request",
     "PDF",
     "TokenInput",
     "TokenOutput",
     "Time",
     "API_Status",
     "Model",
     "RawResponse",
     "AddNotes")] <- NA

#
cols <- c(
  "ID", "ID_SAMPLE", "Notes",
  "PublicationType", "StudyDesign",
  "Group", "GroupType",         
  "RecruitmentSource",          
  "MaleN", "FemaleN",
  "MaleM", "MaleSD",
  "FemaleM", "FemaleSD",
  "d",
  "Tool", "StandardizedTool", "AdministrationLanguage",
  "GenderInvariance", "Index", "ScoreType", "ParameterType", "MeasurementType",
  "Country", "Ethnicity", "Year",
  "AgeRange", "AgeRangeMonths",
  "AgeM", "AgeSD",
  "AgeMmonths", "AgeSDmonths",
  "MaleAgeM", "MaleAgeSD",
  "FemaleAgeM", "FemaleAgeSD",
  "MaleAgeMmonths", "MaleAgeSDmonths",
  "FemaleAgeMmonths", "FemaleAgeSDmonths",
  
  "Grade"
)
d2 <- data.frame(matrix(ncol = length(cols), nrow = 0))
colnames(d2) <- cols

################################################################################
# RECOVERY
if (file.exists(paste0(OutputsFolder, "ScreeningData.xlsx"))) {
  
  d1 <- read.xlsx(paste0(OutputsFolder, "ScreeningData.xlsx"), 1)
  d1$ID <- as.numeric(d1$ID)
  
  if (!"Model" %in% names(d1)) d1$Model <- NA_character_
  if (!"RawResponse" %in% names(d1)) d1$RawResponse <- NA_character_
  
  PriceIn_old <- case_when(
    d1$Model == "gemini-2.5-flash" ~ .30,
    d1$Model == "gemini-3-flash-preview" ~ .50,
    d1$Model == "gemini-2.5-pro" & d1$TokenInput <= 200000 ~ 1.25,
    d1$Model == "gemini-2.5-pro" & d1$TokenInput > 200000 ~ 2.50,
    d1$Model == "gemini-3.1-pro-preview" & d1$TokenInput <= 200000 ~ 2,
    d1$Model == "gemini-3.1-pro-preview" & d1$TokenInput > 200000 ~ 4,
    TRUE ~ NA_real_
  )
  
  PriceOut_old <- case_when(
    d1$Model == "gemini-2.5-flash" ~ 2.5,
    d1$Model == "gemini-3-flash-preview" ~ 3,
    d1$Model == "gemini-2.5-pro" & d1$TokenInput <= 200000 ~ 10,
    d1$Model == "gemini-2.5-pro" & d1$TokenInput > 200000 ~ 15,
    d1$Model == "gemini-3.1-pro-preview" & d1$TokenInput <= 200000 ~ 12,
    d1$Model == "gemini-3.1-pro-preview" & d1$TokenInput > 200000 ~ 18,
    TRUE ~ NA_real_
  )
  
  if (any(d1$TokenInput > 0 & is.na(PriceIn_old), na.rm = T)) {
    stop("Price unavailable for one or more models already present in the Excel")
  }
  
  CostInput_old <- sum(
    d1$TokenInput / 1e6 * PriceIn_old,
    na.rm = T
  ) * USD_TO_EUR
  
  CostOutput_old <- sum(
    d1$TokenOutput / 1e6 * PriceOut_old,
    na.rm = T
  ) * USD_TO_EUR
  
  counter_requests <- sum(d1$TokenInput > 0, na.rm = T)
  counter_cost <- CostInput_old + CostOutput_old
  
  d2 <- read.xlsx(paste0(OutputsFolder, "ScreeningData.xlsx"), 2)
  
} else {
  CostInput_old <- CostOutput_old <- 0
}
################################################################################
t1 <- Sys.time()
for (i in 1:nrow(d1)) {
  ##############################################################################
  # RECOVERY
  if (
    !is.na(d1$ID[i]) &&
    (
      d1$API_Status[i] %in% "NO_PDF" ||
      (is.na(d1$AddNotes[i]) && !is.na(d1$Included[i]))
    )
  ) next
  ##############################################################################
  
  #
  ID <- i
  PDF <- 1
  Included <- NA
  Data <- NA
  DataAll <- NA
  `Note about data/further reason for exclusion` <- NA
  `e-mail` <- NA
  Request <- NA
  
  #
  TokenIn <- 0
  TokenOut <- 0
  Time <- format(Sys.time(), "%d-%m-%Y %H:%M:%S") 
  ApiStatus <- "NOT_ATTEMPTED"
  AddNotes <- NA
  
  #
  # Path to each PDF as stored by your local reference manager (e.g. Zotero); adjust the prefix
  # stripped below to match your own storage folder.
  pdffile <- gsub("~/Zotero/storage/", "", d1$File.Attachments[i], fixed = T)
  pdffile <- paste0(PDFFolder, gsub("\\", "/", pdffile, fixed = T))
  if (!file.exists(pdffile) || d1$File.Attachments[i] == "") {
    print(paste("SKIPPING ID", ID, ": PDF missing."))
    d1$ID[i] <- i
    d1$PDF[i] <- 0
    d1$Time[i] <- format(Sys.time(), "%d-%m-%Y %H:%M:%S") 
    d1$API_Status[i] <- "NO_PDF"
    write.xlsx(list("Screening" = d1, "Data" = d2), paste0(OutputsFolder, "ScreeningData.xlsx"))
    next
  }
  
  print(paste("Processing ID", ID, ":", d1$File[i]))
  
  #
  success <- F
  results <- "ERROR"
  
  for (attempt in seq_len(retries + 1)) {
    tryCatch({
      py_run_string('
from google import genai
from google.genai import types
import warnings
import os
import time
import tempfile
import shutil

warnings.filterwarnings("ignore")
os.environ["GRPC_VERBOSITY"] = "ERROR"

api_key = r.APIkey
pdffile = r.pdffile
prompt = r.prompt
model_name = r.Model

py_in1 = 0
py_out1 = 0
py_res1 = "ERROR"
py_stat1 = "PYTHON_ERROR"

client = None
pdf_file_uploaded = None

tmp_dir = None

try:
    client = genai.Client(api_key=api_key)

    tmp_dir = tempfile.mkdtemp()
    safe_pdf = os.path.join(tmp_dir, "article.pdf")
    shutil.copy2(pdffile, safe_pdf)
    
    pdf_file_uploaded = client.files.upload(file=safe_pdf)


    timeout = 0
    while pdf_file_uploaded.state.name == "PROCESSING":
        time.sleep(5)
        timeout += 5
        pdf_file_uploaded = client.files.get(
            name=pdf_file_uploaded.name
        )

        if timeout > 60:
            raise TimeoutError("PDF processing exceeded 60 seconds")

    if pdf_file_uploaded.state.name == "FAILED":
        raise RuntimeError("PDF processing failed")

    generation_config = types.GenerateContentConfig(
        temperature=0,
        response_mime_type="application/json"
    )

    resp1 = client.models.generate_content(
        model=model_name,
        contents=[prompt, pdf_file_uploaded],
        config=generation_config
    )

    py_res1 = resp1.text or "ERROR"

    if resp1.usage_metadata is not None:
        py_in1 = resp1.usage_metadata.prompt_token_count or 0
        py_out1 = (
            (resp1.usage_metadata.total_token_count or 0) - py_in1
        )

    try:
        py_stat1 = str(resp1.candidates[0].finish_reason)
    except:
        py_stat1 = "OK_UNKNOWN"

except Exception as e:
    py_stat1 = str(e)[:150]

    try:
        if "resp1" in locals() and resp1.usage_metadata is not None:
            py_in1 = resp1.usage_metadata.prompt_token_count or 0
            py_out1 = (
                (resp1.usage_metadata.total_token_count or 0) - py_in1
            )
    except:
        pass

finally:
    if client is not None and pdf_file_uploaded is not None:
        try:
            client.files.delete(name=pdf_file_uploaded.name)
            
            if tmp_dir is not None:
              shutil.rmtree(tmp_dir, ignore_errors=True)
        except:
            pass
')
      
      results <- py$py_res1
      
      #
      current_in <- ifelse(is.null(py$py_in1), 0, py$py_in1)
      current_out <- ifelse(is.null(py$py_out1), 0, py$py_out1)
      
      TokenIn <- TokenIn + current_in
      TokenOut <- TokenOut + current_out
      
      ApiStatus <- py$py_stat1
      
    }, error = function(e) {
      print(paste("   Attempt", attempt, "- R Error:", e$message))
      results <<- "ERROR"
      ApiStatus <<- paste("R_ERROR:", e$message)
    })
    
    if (results != "ERROR" && results != "") {
      success <- T
      break
    }
    
    if (attempt <= retries) {
      print(paste("   Waiting ", timeretries, " seconds before retrying..."))
      Sys.sleep(timeretries)
    }
  }
  
  if (!success) {
    results <- "ERROR"
  }
  
  if (results == "ERROR" || results == "") {
    ID <- i
    AddNotes <- "API call failed"
  } else {
    parsed <- tryCatch({ fromJSON(results) }, error = function(e) NULL)
    
    if (!is.null(parsed)) {
      
      #
      s1 <- parsed$step_1_screening
      s2 <- parsed$step_2_metadata
      s3 <- parsed$step_3_data_extraction
      
      # Included
      Included <- 0
      if (!is.null(s1$overall_paper_decision) && isTRUE(as.logical(s1$overall_paper_decision))) {
        Included <- 1
      }
      
      # Exclusion Reason
      `Note about data/further reason for exclusion` <- s1$overall_exclusion_reason
      
      # E-mail
      `e-mail` <- NA
      if (!is.null(s2) && !is.null(s2$contact_email)) {
        `e-mail` <- s2$contact_email
      }
      
      # DataAll
      DataAll <- 0
      if (!is.null(s1$participant_groups_screening) && is.data.frame(s1$participant_groups_screening)) {
        groups_df <- s1$participant_groups_screening
        if ("group_data_availability" %in% names(groups_df)) {
          DataAll <- ifelse(sum(as.integer(as.logical(groups_df$group_data_availability)), na.rm = TRUE) > 0, 1, 0)
        }
      }
      
      # Request Text
      Request <- NA
      if (!is.null(s2) && !is.null(s2$data_request_text)) {
        Request <- s2$data_request_text
      }
      
      AddNotes <- NA
      
      #
      ma_raw <- s3$ma_data_extraction
      
      #
      if (Included == 1 && !is.null(ma_raw) && is.data.frame(ma_raw) && nrow(ma_raw) > 0) {
        
        batch <- data.frame(matrix(NA, nrow = nrow(ma_raw), ncol = length(cols)))
        colnames(batch) <- cols
        
        #
        batch$ID <- ID
        
        #
        batch$Year <- d1$Publication.Year[i]
        batch$PublicationType <- d1$Item.Type[i]
        
        if (!is.null(s2$StudyDesign))       batch$StudyDesign <- s2$StudyDesign
        if (!is.null(s2$RecruitmentSource)) batch$RecruitmentSource <- s2$RecruitmentSource
        
        #
        batch$Group <- ma_raw$group_name
        
        #
        if (!is.null(s1$participant_groups_screening)) {
          
          #
          match_idx <- match(batch$Group, s1$participant_groups_screening$group_name)
          
          #
          batch$GroupType <- s1$participant_groups_screening$group_type[match_idx]
          batch$ID_SAMPLE <- s1$participant_groups_screening$ID_SAMPLE[match_idx] 
          
          #
          if ("Country" %in% names(s1$participant_groups_screening)) {
            batch$Country <- s1$participant_groups_screening$Country[match_idx]
          }
          
          if ("Ethnicity" %in% names(s1$participant_groups_screening)) {
            batch$Ethnicity <- s1$participant_groups_screening$Ethnicity[match_idx]
          }
          
        }
        batch$Tool   <- ma_raw$Tool
        
        if ("StandardizedTool" %in% names(ma_raw)) {
          batch$StandardizedTool <- ma_raw$StandardizedTool
        }
        
        if ("AdministrationLanguage" %in% names(ma_raw)) {
          batch$AdministrationLanguage <- ma_raw$AdministrationLanguage
        }
        
        if ("GenderInvariance" %in% names(ma_raw)) {
          batch$GenderInvariance <- ma_raw$GenderInvariance
        }
        
        batch$Index <- ma_raw$Index
        
        batch$ScoreType <- ma_raw$ScoreType
        batch$ParameterType <- ma_raw$ParameterType
        
        batch$MeasurementType <- ma_raw$MeasurementType
        
        batch$Notes     <- ma_raw$Notes
        
        #
        batch$MaleN     <- ma_raw$MaleN
        batch$FemaleN   <- ma_raw$FemaleN
        batch$MaleM     <- ma_raw$MaleM
        batch$MaleSD    <- ma_raw$MaleSD
        batch$FemaleM   <- ma_raw$FemaleM
        batch$FemaleSD  <- ma_raw$FemaleSD
        
        #
        if ("Cohens_d" %in% names(ma_raw)) batch$d <- ma_raw$Cohens_d
        
        #
        if ("Grade" %in% names(ma_raw)) batch$Grade <- ma_raw$Grade
        
        if ("AgeM" %in% names(ma_raw)) batch$AgeM <- ma_raw$AgeM
        if ("AgeRange" %in% names(ma_raw)) batch$AgeRange <- ma_raw$AgeRange
        if ("AgeSD" %in% names(ma_raw)) batch$AgeSD <- ma_raw$AgeSD
        
        if ("MaleAgeM" %in% names(ma_raw)) batch$MaleAgeM <- ma_raw$MaleAgeM
        if ("MaleAgeSD" %in% names(ma_raw)) batch$MaleAgeSD <- ma_raw$MaleAgeSD
        if ("FemaleAgeM" %in% names(ma_raw)) batch$FemaleAgeM <- ma_raw$FemaleAgeM
        if ("FemaleAgeSD" %in% names(ma_raw)) batch$FemaleAgeSD <- ma_raw$FemaleAgeSD
        
        if ("AgeRangeMonths" %in% names(ma_raw)) batch$AgeRangeMonths <- ma_raw$AgeRangeMonths
        if ("AgeMmonths" %in% names(ma_raw)) batch$AgeMmonths <- ma_raw$AgeMmonths
        if ("AgeSDmonths" %in% names(ma_raw)) batch$AgeSDmonths <- ma_raw$AgeSDmonths
        
        if ("MaleAgeMmonths" %in% names(ma_raw)) batch$MaleAgeMmonths <- ma_raw$MaleAgeMmonths
        if ("MaleAgeSDmonths" %in% names(ma_raw)) batch$MaleAgeSDmonths <- ma_raw$MaleAgeSDmonths
        if ("FemaleAgeMmonths" %in% names(ma_raw)) batch$FemaleAgeMmonths <- ma_raw$FemaleAgeMmonths
        if ("FemaleAgeSDmonths" %in% names(ma_raw)) batch$FemaleAgeSDmonths <- ma_raw$FemaleAgeSDmonths
        
        d2 <- rbind(d2, batch)
        print(paste("   SUCCESS: Extracted", nrow(batch), "row(s)"))
      }
      
    } else {
      #
      Included <- NA
      `Note about data/further reason for exclusion` <- "JSON Parse Error"
      `e-mail` <- NA
      DataAll <- NA
      Request <- NA
      AddNotes <- "JSON Parse Fail"
    }
  }
  
  d1$ID[i] <- ID
  d1$Included[i] <- Included
  d1$Data[i] <- DataAll
  d1$`Note.about.data/further.reason.for.exclusion`[i] <- `Note about data/further reason for exclusion`
  d1$`e-mail`[i] <- `e-mail`
  d1$Request[i] <- Request
  d1$PDF[i] <- PDF
  
  #
  d1$TokenInput[i] <- TokenIn
  d1$TokenOutput[i] <- TokenOut
  d1$Time[i] <- format(Sys.time(), "%d-%m-%Y %H:%M:%S")
  d1$API_Status[i] <- ApiStatus
  d1$AddNotes[i] <- AddNotes
  d1$Model[i] <- Model
  d1$RawResponse[i] <- if (
    is.null(results) || results == "ERROR" || results == ""
  ) {
    NA_character_
  } else {
    as.character(results)
  }
  
  #
  write.xlsx(list("Screening" = d1, "Data" = d2), paste0(OutputsFolder, "ScreeningData.xlsx"))
  
  if (TokenIn > 0) {
    counter_requests <- counter_requests + 1
    
    total_tokens_in <- TokenIn
    total_tokens_out <- TokenOut
    
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
  print(paste(
    "   ---> Progress: Req:",
    counter_requests,
    "| Est. Cost: €",
    round(counter_cost, 4)
  ))
  Sys.sleep(timewait)
}

#
difftime(Sys.time(), t1, units = "mins")

#
IDs <- unique(read.xlsx(paste0(OutputsFolder, "ScreeningData.xlsx"), 2)$ID)
if (length(IDs) > 0){
  dd <- read.xlsx(paste0(OutputsFolder, "ScreeningData.xlsx"), 1)
  dd <- dd[which(IDs %in% dd$ID), ]
  dd$Notes <- ifelse(is.na(dd$Notes), "", as.character(dd$Notes))
  dd$Notes <- paste(
    paste0("ID: ", dd$ID),
    paste0("Included: ", dd$Included),
    paste0("Data: ", dd$Data),
    paste0("Note about data/further reason for exclusion: ", dd$`Note.about.data/further.reason.for.exclusion`),
    paste0("e-mail: ", dd$`e-mail`),
    paste0("Request: ", dd$Request),
    paste0("PDF: ", dd$PDF),
    paste0("TokenInput_pt2: ", dd$TokenInput),
    paste0("TokenOutput_pt2: ", dd$TokenOutput),
    paste0("Time_pt2: ", dd$Time),
    paste0("API_Status: ", dd$API_Status),
    paste0("AddNotes: ", dd$AddNotes),
    paste0("ZoteroID: ", dd$Key),
    paste0("File.Attachments: ", dd$File.Attachments),
    paste0("Archive.Location: ", dd$Archive.Location),
    
    ifelse(dd$Notes == "", "", paste0("ORIGINAL_Notes: ", dd$Notes)),
    sep = " || "
  )
  
  #
  newCols <- c(
    "Key"               = "key",
    "Title"             = "title",
    "Publication.Year"  = "year",
    "Publication.Title" = "journal",
    "ISSN"              = "issn",
    "Volume"            = "volume",
    "Issue"             = "issue",
    "Pages"             = "pages",
    "Author"            = "authors",
    "Url"               = "url",
    "Language"          = "language",
    "Publisher"         = "publisher",
    "Place"             = "location",
    "Abstract.Note"     = "abstract",
    "Notes"             = "notes",
    "DOI"               = "doi",
    "Manual.Tags"       = "keywords"
  )
  names(dd)[names(dd) %in% names(newCols)] <- newCols[names(dd)[names(dd) %in% names(newCols)]]
  
  #
  dd <- dd[, names(dd) %in% c(
    "key", "title", "year", "month", "day", "journal", "issn",
    "volume", "issue", "pages", "authors", "url", "language",
    "publisher", "location", "abstract", "notes", "doi", "keywords",
    "pubmed_id", "pmc_id"
  )]
  write_csv(dd, paste0(OutputsFolder, "ForRayyan_Data.csv"), quote = "needed")  
}

#
print("Exit Code: 0")