library(tidyverse)
library(tidytext)
library(RColorBrewer)
library(ggthemes)

fm_resp <- readRDS(here("Data/fm_resp.RDS"))

thomas_cap <- fm_resp %>% 
  filter(!is.na(product_capacity))

thomas_cap %>% 
  filter(useful == "Affirmatively", str_detect(broad_loc, "Man")) %>% 
  select(specific_product, desc, product_capacity) %>% 
  arrange(specific_product) %>% 
  view()