#-------------------------------------------------------------------
# Project: Do Non-Cognitive Skills Affect Employment Outcomes of Youth During the School-to-Work Transition?
# Organization: SFedU Future Skills Research Lab
# Objective: Prepare a dataset for the analysis
# Author:  Garen Avanesian, Valery Egorova
# Date: 24 June 2024
#-------------------------------------------------------------------

source(file.path(rcodes, "0200_load_packages.R"))

non_cogn28  <- read_sav("01_input_data/r28i_open_2023.sav")
hi28        <- read_sav("01_input_data/r28iall_61.sav")
hh28        <- read_sav("01_input_data/r28hall_61.sav")

data_hh28 <-
  hh28 %>%
  select(xb1.o, xf14, xa3, xredid_h, xid_h, region) %>%
  replace_with_na(replace = list(xf14 = c(99999997, 99999998, 99999999))) %>%
  mutate(x = xf14/xb1.o,
         number_of_fmem = xb1.o,
         windex = percent_rank(x),
         Income = xf14,
         family = xa3,
         windex5 = case_when(windex <= 0.20 ~ 1,
                             windex > 0.20 & windex <= 0.40 ~ 2,
                             windex > 0.4 & windex <= 0.60 ~ 3,
                             windex >= 0.61 & windex <= 0.80 ~ 4,
                             windex > 0.8  ~ 5,
                             T ~ as.numeric(NA))) %>%
  select(windex5, family, Income, windex, number_of_fmem, xredid_h, xa3, xid_h, windex, x, region)

# View(data_hh28)

data_hi28 <- 
  hi28 %>%
  select(idind, xredid_i, status, xh5, xj1, x_age, xid_h,  xj161.3y, xj161.3m,
         xj70.2, xj72.1a, xj72.2a, xj72.3a, xj72.4a, xj72.5a, xj72.6a, xj81, 
         x_inwgt, x_diplom, xh3, xj65, xj62, xj322, xj72.171, xm3, xj13.2) %>%
  replace_with_na_all(condition ~.x %in% c(99999996, 99999997, 99999998, 99999999)) %>%
  mutate(area = case_when(status %in% c(1,2) ~ "urban",
                         status %in% c(3,4) ~ "rural",
                         T ~ as.character(NA)), 
         gender = ifelse(xh5 == 1, "male", "female"),
         age = x_age,
         in_school = ifelse(xj70.2 == 1, 2, 0),
         in_courses = ifelse(xj72.1a == 2, 1, 0),
         in_ptu_non_usec = ifelse(xj72.2a == 2, 1, 0),
         in_ptu_usec = ifelse(xj72.3a == 2, 1, 0),
         in_tech = ifelse(xj72.4a == 2, 1, 0),
         in_uni = ifelse(xj72.5a == 2, 1, 0),
         in_postgrad = ifelse(xj72.6a == 2, 1, 0),
         h_edu = case_when(x_diplom %in% c(1,2,3) ~ "1. No school",
                           x_diplom == 4 ~"2. School",
                           x_diplom == 5 ~"3. Professional school",
                           x_diplom == 6 ~ "4. Higher education"),
         working  = ifelse(xj1 %in% c(1:4), 1, 0), # dep var
         life_satis = xj65,
         self_p_SES = case_when(xj62 %in% c(1:3) ~ 1,
                                xj62 %in% c(4:6) ~ 2,
                                xj62 %in% c(7:9) ~ 3,
                                T ~ as.numeric(NA)), 
         in_education = ifelse(in_school == 1 | in_courses == 1 | in_ptu_non_usec == 1  |
                               in_ptu_usec == 1  | in_tech == 1  | in_uni == 1  | in_postgrad == 1, 1, 0),
         months = case_when(!is.na(xj161.3m)  ~ xj161.3m/12, T ~0),
         exp = xj161.3y + months,
         family_id = xh3, 
         seeking_empl = ifelse(xj81 == 1, 1, 0),
         salary = xj13.2) %>%
  select(idind, xredid_i, xid_h,  area, gender, age, h_edu, working, seeking_empl, exp, life_satis, 
         self_p_SES, in_education, family_id, salary, x_inwgt, xj62) %>%
  filter(age >= 15 & age <= 34)

ses_data28 <-  
  data_hh28 %>%
  select(xid_h, Income, number_of_fmem , windex, windex5,  x, region) %>%
  mutate(Income1 = case_when(is.na(Income) ~ median(Income, na.rm = T), 
                             TRUE ~ Income),
         hh_pers = case_when(is.na(number_of_fmem) ~ mean(number_of_fmem, na.rm = T), 
                             TRUE ~ number_of_fmem)) %>%
  mutate(Income_pc = Income1/number_of_fmem) %>%
  mutate(ses5 = ntile(Income_pc, 5),
         ses10 = ntile(Income_pc, 10),
         ses = case_when(ses10 %in% (1:4) ~ "1. Bottom 40%",
                         ses10 %in% (5:9) ~ "2. Middle 50%",
                         ses10 == 10 ~ "3. Top 10%")) %>%
  select(xid_h, ses5, region, ses10, ses)

openness <- c("o1", "o2", "o3") 
con <- c("c1", "c2", "c3")
ex <- c("e1", "e2", "e3")
ag <- c("a1", "a2", "a3")
em_st <- c("es1", "es2", "es3") 
grit <- c("g1", "g2", "g3")

data_nc28 <- 
  non_cogn28 %>%
  select(xj445.3, xj445.11, xj445.14,
         xj445.2, xj445.12, xj445.17,
         xj445.1, xj445.4, xj445.20,
         xj445.9, xj445.16, xj445.19,
         xj445.5, xj445.10, xj445.18,
         xj445.6, xj445.8, xj445.13, idind) %>%
  replace_with_na_all(condition = ~.x == 88888888) %>%
  mutate(o1 = 5 - xj445.3,
         o2 = 5 - xj445.11,
         o3 = 5 - xj445.14,
         c1 = 5 - xj445.2,
         c2 = xj445.12,
         c3 = 5 - xj445.17,
         e1 = 5 - xj445.1,
         e2 = xj445.4, 
         e3 = 5 - xj445.20,
         a1 = 5 - xj445.9,
         a2 = 5 - xj445.16,
         a3 = 5 - xj445.19,
         es1 = 5 - xj445.5,
         es2 = xj445.10,
         es3 = xj445.18,
         g1 = 5 - xj445.6,
         g2 = 5 - xj445.8,
         g3 = 5 - xj445.13) 

data_nc28 <- 
  data_nc28 %>%
  mutate(O = scale(rowMeans(data_nc28[openness], na.rm = T),center = TRUE, scale = TRUE),
         C = scale(rowMeans(data_nc28[con], na.rm = T),center = TRUE, scale = TRUE),
         E = scale(rowMeans(data_nc28[ex], na.rm = T),center = TRUE, scale = TRUE),
         A = scale(rowMeans(data_nc28[ag], na.rm = T),center = TRUE, scale = TRUE),
         ES = scale(rowMeans(data_nc28[em_st], na.rm = T),center = TRUE, scale = TRUE),
         G = scale(rowMeans(data_nc28[grit], na.rm = T),center = TRUE, scale = TRUE)) %>%
  select(idind,  O, C, E, A, ES, G)

# View(data_nc28)

data_master28 <- 
  data_hi28 %>%
  left_join(data_nc28) %>%
  left_join(ses_data28) %>%
  mutate(ses = factor(ses),
         area = factor(area),
         sex = factor(gender),
         h_edu = factor(h_edu),
         wave = "28",
         region = as_factor(region)) %>% 
  select(O, C, E, A, ES, G, ses5, area, sex, age, h_edu, working, seeking_empl, exp, life_satis, 
         self_p_SES, in_education, family_id, salary, region, ses, wave, idind)

saveRDS(data_master28, file.path(outData, "rlms_youth28.rds"))
     