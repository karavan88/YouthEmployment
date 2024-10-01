#-------------------------------------------------------------------
# Project: Do Non-Cognitive Skills Affect Employment Outcomes of Youth During the School-to-Work Transition?
# Organization: SFedU Future Skills Research Lab
# Objective: Prepare a dataset for the analysis
# Author:  Garen Avanesian, Valery Egorova
# Date: 24 June 2024
#-------------------------------------------------------------------

wave25 <- readRDS(file.path(outData, "rlms_youth25.rds"))
wave28 <- readRDS(file.path(outData, "rlms_youth28.rds"))

master <- 
  wave25 %>%
  full_join(wave28) %>%
  filter(age >= 15 & age <= 34) %>%
  mutate(hh_id_fin = case_when(!is.na(hhid28) ~ hhid28,
                           TRUE ~ as.numeric(paste0("25", hhid25)))) %>%
  #group_by(hh_id_fin) %>%
  group_by(idind) %>%
  mutate(n = 1) %>%
  mutate(n_total = sum(n)) %>%
  # SELECT ONLY VARIABLES NEEDED FOR ESTIMATION
  select(idind, family_id, wave, region, hhid25, hhid28, hh_id_fin, # id vars
         sex, age, area, ses, ses5, # socio-demographic and hh characteristics
         working,  in_education, h_edu, # exp,
         O, C, E, A, ES, G, # big five traits
         n, n_total) %>%
  mutate(in_education = case_when(in_education == 1 ~ in_education,
                                  TRUE ~ 0)) %>%
  # get rid of those above 29 who were not in the prev panel
  mutate(check = case_when(wave == "28" & age >29 & n_total == 1 ~ "drop",
                           TRUE ~ "keep")) %>%
  filter(check == "keep")

# dim(master)

# View(master)

saveRDS(master, file.path(outData, "model_data.rds"))