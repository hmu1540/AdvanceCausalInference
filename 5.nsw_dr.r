library(DRDID)
library(tidyverse)
library(haven)
library(glue)
library(texreg)

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

nsw_dw_cpscontrol_structured <- nsw_dw_cpscontrol %>% 
  mutate(unit_id = c(1:nrow(nsw_dw_cpscontrol))) %>% 
  pivot_longer(c(re75, re78), names_to = "year", values_to = "re") %>% 
  separate(year, into = c("temp", "year"), sep = 2) %>% 
  select(-temp) %>% 
  mutate(post = if_else(year == 78, 1, 0))
   
dr_est1 <- nsw_dw_cpscontrol_structured %>% 
  drdid(yname = "re",
        tname = "post",
        idname = "unit_id",
        dname = "treat",
        xformla = ~1,
        data = .,
        panel = TRUE,
        estMethod = "imp"
        )
dr_est1_b <- nsw_dw_cpscontrol_structured %>% 
  drdid(yname = "re",
        tname = "post",
        idname = "unit_id",
        dname = "treat",
        xformla = ~1,
        data = .,
        panel = TRUE,
        estMethod = "trad"
        )
texreg(dr_est1)

dr_est1 <- nsw_dw_cpscontrol_structured %>% 
  drdid(yname = "re",
        tname = "post",
        idname = "unit_id",
        dname = "treat",
        xformla = ~ re74 + black,
        data = .,
        panel = TRUE,
        estMethod = "imp")

dr_est1_b <- nsw_dw_cpscontrol_structured %>% 
  drdid(yname = "re",
        tname = "post",
        idname = "unit_id",
        dname = "treat",
        xformla = ~ re74 + black,
        data = .,
        panel = TRUE,
        estMethod = "trad"
  )
