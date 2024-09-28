#-------------------------------------------------------------------
# Project: Do Non-Cognitive Skills Affect Employment Outcomes of Youth During the School-to-Work Transition?
# Organization: SFedU Future Skills Research Lab
# Objective: Prepare a dataset for the analysis
# Author:  Garen Avanesian, Valery Egorova
# Date: 24 June 2024
#-------------------------------------------------------------------

non_cogn25  <- read_sav(file.path(inputData, "ind_25_noncognitive_for_all.sav"))
hi25        <- read_sav(file.path(inputData, "r25i_os_32.sav"))
hh25        <- read_sav(file.path(inputData,"r25h_os41.sav"))

data_hh25 <-
  hh25 %>%
  select(ub1.o, uf14, ua3, UREDID_H, UID_H, region) %>%
  replace_with_na(replace = list(uf14 = c(99999997, 99999998, 99999999))) %>%
  mutate(x = uf14/ub1.o,
         number_of_fmem = ub1.o,
         windex = percent_rank(x),
         Income = uf14,
         family = ua3,
         windex5 = case_when(windex <= 0.20 ~ 1,
                             windex > 0.20 & windex <= 0.40 ~ 2,
                             windex > 0.4 & windex <= 0.60 ~ 3,
                             windex >= 0.61 & windex <= 0.80 ~ 4,
                             windex > 0.8  ~ 5,
                             T ~ as.numeric(NA)),
         uredid_h = UREDID_H,
         uid_h = UID_H) %>%
  select(windex5, family, Income, windex, number_of_fmem, uredid_h, ua3, uid_h, windex, x, region)

data_hi25 <- 
  hi25 %>%
  select(idind, uredid_i, status, uh5, uj1, u_age, uid_h,  uj161.3y, uj161.3m,
         uj70.2, uj72.1a, uj72.2a, uj72.3a, uj72.4a, uj72.5a, uj72.6a, uj81, 
         u_inwgt, u_diplom, uh3, uj65, uj62, uj322, uj72.171, um3, uj13.2) %>%
  replace_with_na_all(condition ~.x %in% c(99999996, 99999997, 99999998, 99999999)) %>%
  mutate(area = case_when(status %in% c(1,2) ~ "urban",
                          status %in% c(3,4) ~ "rural",
                          T ~ as.character(NA)), 
         gender = ifelse(uh5 == 1, "male", "female"),
         age = u_age,
         in_school = ifelse(uj70.2 == 1, 2, 0),
         in_courses = ifelse(uj72.1a == 2, 1, 0),
         in_ptu_non_usec = ifelse(uj72.2a == 2, 1, 0),
         in_ptu_usec = ifelse(uj72.3a == 2, 1, 0),
         in_tech = ifelse(uj72.4a == 2, 1, 0),
         in_uni = ifelse(uj72.5a == 2, 1, 0),
         in_postgrad = ifelse(uj72.6a == 2, 1, 0),
         h_edu = case_when(u_diplom %in% c(1,2,3) ~ "1. No school",
                           u_diplom == 4 ~"2. School",
                           u_diplom == 5 ~"3. Professional school",
                           u_diplom == 6 ~ "4. Higher education"),
         working  = ifelse(uj1 %in% c(1:4), 1, 0), # dep var
         life_satis = uj65,
         self_p_SES = case_when(uj62 %in% c(1:3) ~ 1,
                                uj62 %in% c(4:6) ~ 2,
                                uj62 %in% c(7:9) ~ 3,
                                T ~ as.numeric(NA)), 
         in_education = ifelse(in_school == 1 | in_courses == 1 | in_ptu_non_usec == 1  |
                                 in_ptu_usec == 1  | in_tech == 1  | in_uni == 1  | in_postgrad == 1, 1, 0),
         months = case_when(!is.na(uj161.3m)  ~ uj161.3m/12, T ~0),
         exp = uj161.3y + months,
         family_id = uh3, 
         seeking_empl = ifelse(uj81 == 1, 1, 0),
         salary = uj13.2) %>%
  select(idind, uredid_i, uid_h,  area, gender, age, h_edu, working, seeking_empl, exp, life_satis, 
         self_p_SES, in_education, family_id, salary, u_inwgt, uj62) %>%
  filter(age >= 15 & age <= 29)

ses_data25 <-  
  data_hh25 %>%
  select(uid_h, Income, number_of_fmem , windex, windex5,  x, region) %>%
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
  select(uid_h, ses5, region, ses10, ses, Income_pc, Income, Income1)

View(ses_data25)

openness <- c("o1", "o2", "o3") 
con <- c("c1", "c2", "c3")
ex <- c("e1", "e2", "e3")
ag <- c("a1", "a2", "a3")
em_st <- c("es1", "es2", "es3") 
grit <- c("g1", "g2", "g3")

data_nc25 <- 
  non_cogn25 %>%
  select(j445.3, j445.11, j445.14,
         j445.2, j445.12, j445.17,
         j445.1, j445.4, j445.20,
         j445.9, j445.16, j445.19,
         j445.5, j445.10, j445.18,
         j445.6, j445.8, j445.13, idind) %>%
  replace_with_na_all(condition = ~.x == 88888888) %>%
  mutate(o1 = 5 - j445.3,
         o2 = 5 - j445.11,
         o3 = 5 - j445.14,
         c1 = 5 - j445.2,
         c2 = j445.12,
         c3 = 5 - j445.17,
         e1 = 5 - j445.1,
         e2 = j445.4, 
         e3 = 5 - j445.20,
         a1 = 5 - j445.9,
         a2 = 5 - j445.16,
         a3 = 5 - j445.19,
         es1 = 5 - j445.5,
         es2 = j445.10,
         es3 = j445.18,
         g1 = 5 - j445.6,
         g2 = 5 - j445.8,
         g3 = 5 - j445.13) %>%
  mutate(O = scale(rowMeans(select(., all_of(openness)), na.rm = T),center = TRUE, scale = TRUE),
         C = scale(rowMeans(select(., all_of(con)), na.rm = T),center = TRUE, scale = TRUE),
         E = scale(rowMeans(select(., all_of(ex)), na.rm = T),center = TRUE, scale = TRUE),
         A = scale(rowMeans(select(., all_of(ag)), na.rm = T),center = TRUE, scale = TRUE),
         ES = scale(rowMeans(select(., all_of(em_st)), na.rm = T),center = TRUE, scale = TRUE),
         G = scale(rowMeans(select(., all_of(grit)), na.rm = T),center = TRUE, scale = TRUE)) %>%
  select(idind,  O, C, E, A, ES, G)
  
data_master25 <- 
  data_hi25 %>%
  left_join(data_nc25) %>%
  left_join(ses_data25) %>%
  mutate(ses = factor(ses),
         area = factor(area),
         sex = factor(gender),
         h_edu = factor(h_edu),
         wave = "25",
         region = as_factor(region)) %>% 
  select(O, C, E, A, ES, G, ses5, area, sex, age, h_edu, working, seeking_empl, exp, life_satis, 
         self_p_SES, in_education, family_id, salary, region, ses, wave, idind)

# dim(data_hi25)
# dim(data_master25)

saveRDS(data_master25, file.path(outData, "rlms_youth25.rds"))
