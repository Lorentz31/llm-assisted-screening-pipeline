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
  Model <- "gemini-2.5-pro"
  MAX_DAILY_REQUESTS <- 9999
  PRICE_IN_1M <- 1.25
  PRICE_OUT_1M <- 10
} else if (model_choice == "2") {
  Model <- "gemini-3.1-pro-preview"
  MAX_DAILY_REQUESTS <- 9999
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
retries <- 0
timeretries <- 10
timewait <- .2

#
# NOTE (sanitized template): replace [TARGET CONSTRUCT] with the construct of interest
# for your own meta-analysis (e.g., a specific psychological or clinical measure).
prompt <- "
Your task is to perform a surgically precise analysis of the scientific article.
Adhere strictly to logical gates to minimize output and cost.

Return your analysis EXCLUSIVELY in a single, valid JSON object.

The JSON object must contain exactly the following 3 top-level keys: 'step_1_screening', 'step_2_metadata', 'step_3_data_extraction'.

### STEP 1: SCREENING (ALWAYS REQUIRED)
'step_1_screening': {
   
   'participant_groups_screening': [
       # Create ONE object for EACH distinct participant group.
       {
          - 'ID_SAMPLE': integer. (Progressive ID: 1, 2, 3...).
          - 'group_name': string. (Exact author label).
          - 'group_type': string. (Extract the specific clinical diagnosis or group label used by authors. Examples: 'TD', 'ADHD', 'SLD', 'Autism', 'Gifted', 'Preterm', 'Top-performers', etc. Do NOT force into examples if different).
          - 'Country': string or null.
          - 'Ethnicity': string or null.
             # Use the fixed format: 'Label|N=value|Pct=value; Label|N=value|Pct=value'.
             # Create one segment for each ethnicity reported in the sample.
             # Use NR when N or percentage is not reported.
             # Preserve the authors' exact ethnicity labels.
             # Do NOT infer ethnicity from country, location, names, or article language.
             # Example: 'White|N=120|Pct=60; Black|N=50|Pct=25; Asian|N=30|Pct=15'.
          
          - 'inclusion_metrics': {
              'is_empirical_study': boolean,
              
              'meets_construct_criteria': boolean,
                 # ABSTRACT RULE: 'Exclude studies based only on surveys, parent/teacher reports, academic achievement, or biological measures without any form of construct-specific testing.'
                 # STRICT CHECK: Set to TRUE only if the paper reports actual [TARGET CONSTRUCT] data. Data comprises questionnaire scores as well as physiological measures.
                 # CRITICAL EXCLUSION: If the paper uses ONLY a general, unrelated measure, set this to FALSE. A general/unrelated measure is not the specific target construct.
              
              'includes_both_genders_with_n_gt_2': boolean,
                 # ABSTRACT RULE: 'Exclude single-gender samples.'
                 # STRICT CHECK: The group must explicitly include BOTH Males and Females (N > 2 for each) in the demographics.
              
              # MATCHING CHECK (CRITICAL)
              'is_not_matched_on_construct': boolean.
                 # ABSTRACT RULE: 'Exclude if participants were matched on the target construct or any closely related measure.'
                 # STRICT CHECK: Set to FALSE if the group was explicitly matched to another group based on ANY related measure (Final score or specific subscales).
                 # Examples of exclusion: 'matched for [construct]', 'paired based on [construct] scores'.
          },

          - 'group_is_eligible': boolean.
              (TRUE only if ALL 4 inclusion_metrics are true).

          - 'group_data_availability': boolean.
              STRICT DEFINITION: Set to TRUE if AND ONLY IF the document contains:
              
              1) SUFFICIENT STATS REPORTED SEPARATELY BY GENDER:
                 A) THE FULL TRIAD for BOTH genders: (Male N, Mean, SD) AND (Female N, Mean, SD). 
                    *CRITICAL: If SD is missing but SE or CI is present, set to TRUE.*
                 OR
                 B) Explicit inference stats comparing Males vs Females (d, t, F).
              
              *EXCLUSION: Regression coefficients, Odds Ratios, or Correlations DO NOT count as data. Set FALSE.*
       }
   ],
   
   'overall_paper_decision': boolean.
   # Set to TRUE if at least one group has 'group_is_eligible' = TRUE.
      
   'overall_exclusion_reason': string.
   # MANDATORY: If 'overall_paper_decision' is TRUE, return an empty string ''.
   # If FALSE, provide the reason (e.g., 'Construct not measured', 'Only parent-reports used', 'Single gender', 'review only', 'construct-matched sample'). NEVER return null.
}

### STEP 2: METADATA & CONTACT (LOGICAL GATE)
# CRITICAL: If 'overall_paper_decision' is FALSE, 'step_2_metadata' MUST be null.

'step_2_metadata': {
   
   # --- CONTACT INFO ---
   - 'contact_email': string or null.
      # Generate ONLY if eligible groups exist but have 'group_data_availability' = FALSE.
   
   - 'data_request_text': string or null.
      # Generate ONLY if eligible groups exist but have 'group_data_availability' = FALSE.
      # TEMPLATE: 'Means and SDs (or SEs) of the [TARGET CONSTRUCT] data, reported separately by gender for the [INSERT MISSING GROUP NAME HERE] group.'

   # --- DESCRIPTIVE METADATA ---
   # LOGIC: Extract these 3 fields ONLY IF at least one group has 'group_data_availability' = TRUE.
   # If NO group has data available, set these to null (save tokens).
   
   - 'StudyDesign': string or null.
   
   - 'RecruitmentSource': string or null.
      # PURPOSE: Identify specific cohorts to prevent duplicates (e.g., 'ALSPAC', 'ABCD Study', 'Hospital Database').
      # Do not describe the method (e.g. 'flyers'), just the source name if available.
}

### STEP 3: DATA EXTRACTION (LOGICAL GATE)
# CRITICAL: If 'overall_paper_decision' is FALSE, 'step_3_data_extraction' MUST be null.

'step_3_data_extraction': {
   
   'ma_data_extraction': [
       # Extract ONLY for groups where 'group_data_availability' is TRUE.
       # STRUCTURAL RULE: Create ONE object per measurement occasion containing BOTH Male and Female data. Measurement occasions comprise different waves in longitudinal design, multiple tools assessing the construct, pre/post phases of experimental and intervention studies. DO NOT split genders into separate objects.
       # CRITICAL RULE: Ensure Age and Grade match the EXACT measurement occasion/wave of this object. Do not generalize across the whole study.
       {
          - 'group_name': string.
          
          # --- AGE & GRADE FOR THIS SPECIFIC OCCASION ---
          - 'AgeM': number or null.
          - 'AgeRange': string.
          - 'AgeSD': number or null.
          - 'MaleAgeM': number or null.
          - 'MaleAgeSD': number or null.
          - 'FemaleAgeM': number or null.
          - 'FemaleAgeSD': number or null.
          - 'AgeRangeMonths': string.
          - 'AgeSDmonths': number or null.
          - 'MaleAgeMmonths': number or null.
          - 'MaleAgeSDmonths': number or null.
          - 'FemaleAgeMmonths': number or null.
          - 'FemaleAgeSDmonths': number or null.
          - 'Grade': string or null.
             # PRIORITY: Always extract the EXACT numerical grade or school year (e.g., 'Grade 2', '3rd to 5th grade') if reported in the text. 
             # ONLY use broad categories (e.g., 'Middle School', 'High School') if the specific numerical grades are NOT mentioned. 
             # DO NOT copy-paste raw text or long lists of percentages.
          
          - 'Tool': string.
            # STRICT FORMAT: Return ONLY the full name of the battery/task used. If full name is not available, return the acronym.
            # EXAMPLE: If text says '[Instrument A full name]', return '[Instrument A full name]'.
            # EXAMPLE: If text says 'Heart Rate', return 'Heart Rate'. If text says 'fMRI', return 'fMRI'.

        - 'StandardizedTool': boolean or null.
           # Set to true if the measure is an established standardized or validated instrument.
           # Set to false only if it is explicitly described as ad hoc, researcher-created, or non-standardized.
           # Set to null if the available information is insufficient. Do not infer.
        
        - 'AdministrationLanguage': string or null.
           # Language in which this specific measure was administered.
           # Do NOT report the publication language and do NOT infer it from Country.
        
        - 'GenderInvariance': string or null.
           # Report any analysis concerning measurement equivalence or measurement differences across sex/gender for this measure.
           # This includes measurement invariance at any level (e.g., configural, metric, scalar, or strict invariance), DIF analyses, MIMIC models, LSEM, moderated factor analysis, or equivalent approaches.
           # Briefly report both the method/type of analysis and its main conclusion.
           # Examples: 'Metric invariance supported'; 'Partial scalar invariance'; 'MIMIC model detected gender-related DIF'; 'LSEM indicated gender-related loading differences'.
           # Return null if no such analysis was conducted or reported.
           # Do NOT infer invariance from non-significant mean differences between males and females.
           
          - 'Index': string. 
             # STRICT MAPPING RULE: 
              - If the measure is a psychological measure (e.g., questionnaires) set to null.
              - If the measure is a physiological measure return the index used. For example, if assessed through electroencephalogram, return the index assessed, e.g., 'P400'; if cortisol levels were assessed, return the index assessed, e.g., mg/L.
             
          - 'ScoreType': string.
             # EXAMPLE: 'Accuracy', 'Errors', 'RT' (or others). 
             # EXAMPLE: 'Mean level' and 'Latency' relate mainly to non-psychological measures.
             
          - 'ParameterType': string.
             # EXAMPLE: 'Raw', 'z-score', 't-score' (or others).
             
          - 'MeasurementType': string.
             # STRICT RULE: return only 'State' or 'Trait'. 
             # EXAMPLE: if the trait form of the construct was assessed, return 'Trait'; if assessed as a state (e.g., with experimental manipulation), return 'State'.
             
          - 'MaleN': number or null.
          - 'FemaleN': number or null.
          - 'MaleM': number or null.
          - 'MaleSD': number or null.
          - 'FemaleM': number or null.
          - 'FemaleSD': number or null.

          # RULE: If MaleM/SD and FemaleM/SD are extracted, set these inferential stats to null.
          - 'Cohens_d': number or null.
          # DIRECTION: Standard is d > 0 (Male > Female). If paper uses d > 0 for Female > Male, specify clearly in Notes.
          
          - 'Notes': string. (e.g., 'SE used instead of SD', 'CI used instead of SD').
       }
   ]
}

DO NOT add any text outside the JSON object.
"