sink("basic_statistics.txt")
hope_tibble<-df_video_intervention %>% 
  group_by(vaccination_status) %>%
  summarise(weighted.mean(hope, weight)) 
hope_tibble
confidence_tibble<-df_video_intervention %>%
  group_by(vaccination_status) %>%
  summarise(weighted.mean(vaccine_confidence, weight))
confidence_tibble
sink()

mod1 <- "# a path
         vaccine_confidence ~ a * hope

         # b path
         vaccination_status ~ b * vaccine_confidence

         # c prime path 
         vaccination_status ~ cp * hope

         # indirect and total effects
         ab := a * b
         total := cp + ab"

fsem1 <- sem(mod1, data = df_video_intervention, se = "bootstrap", bootstrap = 10000) # fit structural equation models

sink("fsem.txt")
summary(fsem1)
sink()

model1 <- lm(data = df_video_intervention, vaccine_confidence ~ hope + age + gender 
             + residence + education + occupation + income)
model2 <- glm(data = df_video_intervention, vaccination_status~hope+vaccine_confidence + age + gender 
              + residence + education + occupation + income, family = "binomial")
sem <- mediate(model1, model2, treat = "hope", mediator = "vaccine_confidence", boot = T)

sink("sem.txt")
summary(sem)
sink()