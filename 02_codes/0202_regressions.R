#-------------------------------------------------------------------
# Project: Do Non-Cognitive Skills Affect Employment Outcomes of Youth During the School-to-Work Transition?
# Organization: SFedU Future Skills Research Lab
# Objective: Run the regressions
# Author:  Garen Avanesian
# Date: 28 Sept 2024
#-------------------------------------------------------------------

# m1 <- working ~ age, sex, in_education, ses5, h_edu, area, region, O, C, E, A, ES, 
# m1 - random intercept by region 
# m2 - random intercept and random slopes of NCS by ses5
master <- readRDS(file.path(outData, "model_data.rds"))

m1 <- lmer(working ~ sex + in_education + ses + h_edu + area +  
               O + C + E + A + ES + 
               (1|region) + (1|age) + (1|wave) + (1|idind),
             REML = F, data = master)

summary(m1)

m2 <- lmer(working ~ sex + in_education  + h_edu + area +  
               O + C + E + A + ES + 
               (1|region) + (1|age) + (1|wave) + (1|idind) +
               (1 + O + C + E + A + ES | ses),
             REML = F, data = master)

mlm_ha_by_ses =
  coef(m2)$`ses` %>%
  as.data.frame() %>%
  rownames_to_column(var = "ses") %>%
  select(ses, O, C, E, A, ES) %>%
  gather(Skill, Estimate, -ses) %>%
  mutate(Estimate = as.numeric(Estimate)*100)

plot_mlm_ha_by_ses =
  ggplot(mlm_ha_by_ses, aes(Estimate, ses))+
  geom_point(size = 7, color = "lightblue")+
  geom_text(aes(label = round(Estimate, 2)), size = 2, color = "black")+
  #geom_segment( aes(x=0, xend=Estimate, y=SES_Quintile, yend=SES_Quintile), color="skyblue")+
  geom_vline(xintercept=0, linetype="solid",  color = "black")+
  theme_bw()+
  scale_y_discrete(limits=rev)+
  facet_wrap(Skill~.,)+
  ylab("")+
  xlab("")


m3 <- lmer(working ~ sex + in_education + ses + area +  
             O + C + E + A + ES + 
             (1|region) + (1|age) + (1|wave) + (1|idind) +
             (1 + O + C + E + A + ES | h_edu),
           REML = F, data = master)

mlm_ha_by_edu =
  coef(m3)$`h_edu` %>%
  as.data.frame() %>%
  rownames_to_column(var = "h_edu") %>%
  select(h_edu, O, C, E, A, ES) %>%
  gather(Skill, Estimate, -h_edu) %>%
  mutate(Estimate = as.numeric(Estimate)*100)