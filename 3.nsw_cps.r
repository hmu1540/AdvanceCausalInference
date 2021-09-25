library(tidyverse)
library(haven) #Import foreign statistical formats into R via the embedded 'ReadStat' C library
library(glue)
library(estimatr)
library(stargazer)
library(texreg)

git_path <- "https://raw.github.com/hmu1540/AdvanceCausalInference/master/"

data <- "nsw_mixtape.dta"
nsw_dw <- read_dta(glue("{git_path}{data}"))

data <- "cps_mixtape.dta"
nsw_dw_cpscontrol <- read_dta(glue("{git_path}{data}")) %>% 
  bind_rows(nsw_dw)


data_structured <- nsw_dw_cpscontrol %>% 
  mutate(agesq = age^2,
         agecube = age^3,
         educsq = educ*educ,
         u74 = case_when(re74 == 0 ~ 1, TRUE ~ 0),
         u75 = case_when(re75 == 0 ~ 1, TRUE ~ 0),
         interaction1 = educ*re74,
         re74sq = re74^2,
         re75sq = re75^2,
         interaction2 = u74*hisp) %>% 
  pivot_longer(c(re75, re78), names_to = "year", values_to = "re") %>% 
  separate(year, into = c("temp", "year"), sep = 2) %>% 
  select(-temp) %>% 
  mutate(post = if_else(year == 78, 1, 0))

lm_did <- data_structured %>% 
  lm_robust(re~post*treat, data = ., se_type = "stata")



lm_did_hascov <- data_structured %>% 
  lm_robust(re~post*treat + age + agesq + agecube + educ + educsq + 
              marr + nodegree + black + hisp + u74 +
              u75
              , data = ., se_type = "stata")


texreg(list(lm_did, lm_did_hascov), include.ci = FALSE)
