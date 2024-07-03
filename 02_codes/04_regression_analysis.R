# m1 <- working ~ age, sex, in_education, ses5, h_edu, area, region, O, C, E, A, ES, 
# m1 - random intercept by region 
# m2 - random intercept and random slopes of NCS by ses5


mlm1 <- lmer(working ~ sex + in_education + ses5 + h_edu + area +  
                       O + C + E + A + ES + 
               + (1|region) + (1|age),
             REML = F, #control = lmerControl(optimizer = "Nelder_Mead"), 
             data = data_master)

mlm2 <- lmer(working ~ sex + in_education + ses5 + h_edu + area +  
               O + C + E + A + ES + self_p_SES  +
               (1|region) + (1|age) + 
               (1 + O + C + E + A + ES  | ses5),
             REML = F, #control = lmerControl(optimizer = "Nelder_Mead"), 
             data = data_master)

mlm_ha_by_ses =
  coef(mlm2)$`ses` %>%
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

mlm3 <- lmer(working ~ sex + in_education + ses5 + h_edu + area +  
               O + C + E + A + ES + self_p_SES + I(O^2) +
               (1|region) + (1|age) + 
               (1 + O + C + E + A + ES + I(O^2) | ses5),
             REML = F, #control = lmerControl(optimizer = "Nelder_Mead"), 
             data = data_master)

mlm3_scatter =
  ggpredict(mlm3, terms = c("O","ses5"),
            type="re") %>% 
  plot()+
  ylim(0,0.6)
  

ggpredict(mlm2,
          type="re") 