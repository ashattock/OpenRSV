###########################################################
# PROGNOSIS
#
# Calculate base probilities of each prognosis based on age.
# Also calculate severe disease multiplicate factors based 
# on variant infected with, number of past exposures, and 
# number of vaccine doses.
#
# These factors are used within model.R to generate a prognosis
# for each newly infected individual, at which stage waning 
# immunity (for vaccine-induced and acquired immunity) as well 
# as factors such as comorbidities and PrEP are considered. 
#
###########################################################

# ---------------------------------------------------------
# Prognosis probabilities by age, variant, doses, and exposures
# ---------------------------------------------------------
prognosis_probabilities = function(y) {
  
  # ---- Age-related disease probabilities ----
  
  # Parse age-related probability functions defined by user
  asymptomatic   = fn_age_probabilities(y, "asymptomatic_age")    # Asymptomatic probability
  severe_symptom = fn_age_probabilities(y, "severe_symptom_age")  # Severe given symptoms
  death_severe   = fn_age_probabilities(y, "death_severe_age")    # Death given severe
  
  # Remove redundant variables
  y[c("asymptomatic_age", "severe_symptom_age", "death_severe_age")] = NULL
  
  # ---- Age-related cumulative probability matrix ----
  
  # Proportion of all cases that are asymptomatic
  p_asym = asymptomatic
  
  # Proportion of all cases that are mild - the rest are severe
  p_mild   = (1 - p_asym) * (1 - severe_symptom)
  p_severe = (1 - p_asym) * severe_symptom
  
  # Of those severe cases, only some will recover after hospitalisation
  p_hosp = p_severe * (1 - death_severe)
  p_dead = p_severe * death_severe

  # Sanity check: Check that these sum to one and nothing weird is going on
  if (any(abs(p_asym + p_mild + p_hosp + p_dead - 1) > 1e-6))
    stop("Prognosis probabilities must sum to 1 for all age groups")
  
  # Combine probabilities into dataframe (age group x prognosis state)
  y$prognosis$age = data.table(age  = y$ages,
                               asym = p_asym,
                               mild = p_mild,
                               hosp = p_hosp, 
                               dead = p_dead) %>%
    pivot_longer(cols = -age, names_to = "state") %>%
    as.data.table()
  
  # ---- Vaccine severity and dose/exposure-response relationships ----
  
  # # Extract variant severity factor for all circulating variants
  # variant_df = y$variants %>%
  #   select(variant = id, 
  #          severe  = severity) %>%
  #   mutate(crit = severe, 
  #          dead = severe)
  # 
  # # INTERPRETATION: Reductions in ICU and death for those hospitalised...
  # 
  # # Vaccine dose - response relationship
  # dose_df = fn_response(x = y$dose_response, 
  #                       count = 0 : y$max_dose_count, 
  #                       name  = "vaccine_doses")
  # 
  # # Expsoure - response relationship
  # exposure_df = fn_response(x = y$exposure_response, 
  #                           count = 0 : y$max_infection_count, 
  #                           name  = "num_infections")
  # 
  # # Severity risk factors considering variant, exposures, and vaccine doses
  # severity_df = 
  #   expand_grid(variant_df, dose_df, exposure_df, 
  #               .name_repair = make.names) %>%
  #   pivot_longer(cols = c(-variant, -vaccine_doses, -num_infections), 
  #                names_to = "state") %>%
  #   group_by(variant, vaccine_doses, num_infections, state) %>%
  #   summarise(value = prod(value)) %>%
  #   ungroup() %>%
  #   as.data.table()
  # 
  # # Repeat the dataframe for care-seeking and non-care-seeking severe states
  # y$prognosis$severity = 
  #   rbind(severity_df %>% mutate(state = paste0(state, "_care")), 
  #         severity_df %>% mutate(state = paste0(state, "_home"))) %>%
  #   arrange(variant, vaccine_doses, num_infections, state)
  # 
  # # Remove redundant variables
  # y[c("dose_response", "exposure_response")] = NULL
  
  return(y)
}

# ---------------------------------------------------------
# Wrapper for parsing age-related probability functions
# ---------------------------------------------------------
fn_age_probabilities = function(y, fn_str) {
  
  # Parse age-related probability function and bound below
  age_vector = parse_fn(y[[fn_str]], along = list(x = y$ages)) %>% pmax(1e-12)
  
  return(age_vector)
}

# ---------------------------------------------------------
# Format response dose/exposure-response relationships
# ---------------------------------------------------------
fn_response = function(x, count, name) {
  
  # Construct state-response dictionary
  responses = qc(icu_risk_factor, death_risk_factor)
  response_dict = setNames(qc(crit, dead), responses)
  
  # States referred to in response list
  states = response_dict[names(x)]
  
  # Construct datatable of response for each count
  count_df = as_named_dt(x, states) %>% 
    cbind(count) %>% 
    mutate(across(.cols = -count, 
                  .fns  = function(x) x ^ count)) %>% 
    select(!!name := count, all_of(unname(states)))
  
  return(count_df)
}

