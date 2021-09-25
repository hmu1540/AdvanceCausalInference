library(tidyverse)
library(haven)
library(glue)

git_path <- "https://raw.github.com/hmu1540/AdvanceCausalInference/master/"

data <- "nsw_mixtape.dta"
nsw_dw <- read_dta(glue("{git_path}{data}"))

data <- "cps_mixtape.dta"
nsw_dw_cpscontrol <- read_dta(glue("{git_path}{data}")) %>% 
  bind_rows(nsw_dw) %>% 
  mutate(agesq = age^2,
         agecube = age^3,
         educsq = educ*educ,
         u74 = case_when(re74 == 0 ~ 1, TRUE ~ 0),
         u75 = case_when(re75 == 0 ~ 1, TRUE ~ 0),
         interaction1 = educ*re74,
         re74sq = re74^2,
         re75sq = re75^2,
         interaction2 = u74*hisp)

# estimating
logit_nsw <- glm(treat ~ age + agesq + agecube + educ + educsq + 
                   marr + nodegree + black + hisp + re74 + re75 + u74 +
                   u75 + interaction1, family = binomial(link = "logit"), 
                 data = nsw_dw_cpscontrol)

nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(pscore = logit_nsw$fitted.values)

# analysis: pscore distribution, 

# mean pscore 
pscore_control <- nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  pull(pscore) %>% 
  mean()

pscore_treated <- nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  pull(pscore) %>% 
  mean()

# histogram
nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore)) +
  theme_light()

nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore)) +
  theme_light()

nsw_dw_cpscontrol %>% 
  mutate(treat = as_factor(treat)) %>% 
  ggplot(aes(pscore, fill = treat)) +
  geom_histogram(alpha = 0.2) 

  

