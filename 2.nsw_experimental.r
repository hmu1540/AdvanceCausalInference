library(tidyverse)
library(haven) #Import foreign statistical formats into R via the embedded 'ReadStat' C library
library(glue)

git_path <- "https://raw.github.com/hmu1540/AdvanceCausalInference/master/"
data <- "nsw_mixtape.dta"

nsw_dw <- read_dta(glue("{git_path}{data}"))

nsw_dw %>% 
  filter(treat == 1) %>% 
  summary(re78)

mean1 <- nsw_dw %>% 
  filter(treat == 1) %>% 
  pull(re78) %>% 
  mean()

nsw_dw$y1 <- mean1

nsw_dw %>% 
  filter(treat == 0) %>% 
  summary(re78)

mean0 <- nsw_dw %>% 
  filter(treat == 0) %>% 
  pull(re78) %>% 
  mean()

nsw_dw$y0 <- mean0

ate <- unique(nsw_dw$y1 - nsw_dw$y0)

nsw_dw <- nsw_dw %>% 
  filter(treat == 1) %>% 
  select(-y1, -y0)

