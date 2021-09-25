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
# Columns: 22
# $ data_id      <chr> "CPS1", "CPS1", "CPS1", "CPS1", "CPS1", "CPS1", "CPS1", ~
#   $ treat        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
#   $ age          <dbl> 45, 45, 21, 21, 38, 38, 48, 48, 18, 18, 22, 22, 48, 48, ~
#   $ educ         <dbl> 11, 11, 14, 14, 12, 12, 6, 6, 8, 8, 11, 11, 10, 10, 11, ~
#   $ black        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
#   $ hisp         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
#   $ marr         <dbl> 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1,~
#   $ nodegree     <dbl> 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,~
#   $ re74         <dbl> 21516.670, 21516.670, 3175.971, 3175.971, 23039.020, 230~
#   $ agesq        <dbl> 2025, 2025, 441, 441, 1444, 1444, 2304, 2304, 324, 324, ~
#   $ agecube      <dbl> 91125, 91125, 9261, 9261, 54872, 54872, 110592, 110592, ~
#   $ educsq       <dbl> 121, 121, 196, 196, 144, 144, 36, 36, 64, 64, 121, 121, ~
#   $ u74          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
#   $ u75          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,~
#   $ interaction1 <dbl> 236683.37, 236683.37, 44463.59, 44463.59, 276468.23, 276~
#   $ re74sq       <dbl> 462967085, 462967085, 10086791, 10086791, 530796421, 530~
#   $ re75sq       <dbl> 637236856, 637236856, 34252516, 34252516, 631555086, 631~
#   $ interaction2 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
#   $ pscore       <dbl> 2.019543e-05, 2.019543e-05, 1.190358e-03, 1.190358e-03, ~
#   $ year         <chr> "75", "78", "75", "78", "75", "78", "75", "78", "75", "7~
# $ re           <dbl> 25243.551, 25564.670, 5852.565, 13496.080, 25130.760, 25~
# $ post      
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
