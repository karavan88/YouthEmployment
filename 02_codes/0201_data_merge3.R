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
  group_by(idind) %>%
  mutate(n = 1) %>%
  mutate(n_total = sum(n))

dim(wave25)
dim(wave28)
dim(master)

length(unique(master$idind))
length(unique(master$idind[master$n_total==2]))

both <- 
  master %>%
  filter(n_total == 2) %>%
  arrange(idind) %>%
  group_by(idind) 
  
  