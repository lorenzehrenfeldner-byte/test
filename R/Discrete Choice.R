
#packages
library(apollo)
library(dplyr)
library(tidyr)
library(readxl)

# Initialize Apollo
apollo_initialise()

# Set core controls
apollo_control = list(
  modelName       = "Mobility_Preference_Model",
  modelDescr      = "MNL model on Austria Mobility Behavior",
  indivID         = "CASE", 
  outputDirectory = "output",
  panelData       = TRUE
)

# ---- DATA PREPARATION ----
database_raw <- read_excel("data/data_mobilitaetsverhalten.xlsx") 

### DATA CLEANING
database <- database_raw %>%
  filter(!is.na(A303) & A303 %in% c(1, 2)) %>%
  filter(!is.na(A304) & A304 %in% c(1, 2)) %>%
  filter(!is.na(A305) & A305 %in% c(1, 2)) %>%
  filter(!is.na(A306) & A306 %in% c(1, 2))

# ---- RESHAPE DATA TO LONG FORMAT ----
# 1. Reshape DESIGN to wide format (attributes per scenario)
# We want one row per scenario (303, 304, etc.) with columns price_1, price_2, etc. Beneficiary (1=Car, 0=Öffi)
design_long = data.frame(
  scenario = c(303, 303, 304, 304, 305, 305, 306, 306),
  alt      = c(1, 2, 1, 2, 1, 2, 1, 2),
  price    = c(100, 50, 50, 100, 100, 50, 50, 100), 
  co2      = c(25, 2, 2, 25, 2, 25, 25, 2),    
  benefit  = c(0, 1, 0, 1, 0, 1, 0, 1)          
)

design_wide <- design_long %>%
  pivot_wider(
    id_cols     = scenario,
    names_from  = alt,
    values_from = c(price, co2, benefit),
    names_sep   = "_"
  )

# 2. Reshape DATABASE to long format (one row per choice task)
database_long <- database %>%
  pivot_longer(
    cols      = c(A303, A304, A305, A306),
    names_to  = "scenario_str",
    values_to = "choice"
  ) %>%
  mutate(
    scenario = as.numeric(gsub("A", "", scenario_str))
  )

# 3. Join Design into Database
database <- left_join(database_long, design_wide, by = "scenario")

# ---- MODEL PARAMETERS ----
apollo_beta = c(
  b_price   = 0,
  b_co2     = 0,
  b_benefit = 0
)

apollo_fixed = c() 

# ---- MODEL DEFINITION ----
apollo_inputs = apollo_validateInputs()

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  # Define Utilities (vectorized for all rows)
  V = list()
  
  # Alternative 1
  V[['alt1']] = b_price * price_1 + 
                b_co2   * co2_1 +
                b_benefit * benefit_1
  
  # Alternative 2
  V[['alt2']] = b_price * price_2 + 
                b_co2   * co2_2 +
                b_benefit * benefit_2
  
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2),
    choiceVar     = choice, # 'choice' column from long database
    V             = V
  )
  
  # Calculate probabilities for all rows
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  # Multiply probabilities across within-individual observations (Panel)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ---- ESTIMATION ----
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ---- OUTPUT ----
apollo_modelOutput(model)

