library(tidyverse)
library(tidytext)
library(RColorBrewer)
library(ggthemes)
library(here)
library(readxl)

fm_resp <- readRDS(here("Data/Cleaned/fm_resp.RDS"))

fm_resp %>% 
  filter

thomas_cap <- fm_resp %>% 
  filter(!is.na(product_capacity))

thomas_cap %>% 
  select(company, sales, employees, location, specific_product, desc, product_capacity) %>% 
  arrange(specific_product) %>% 
  view()

cap_det <- read_xlsx(here("Data/thom_cap.xlsx"), sheet = "Sheet2")

cap_det1 <- left_join(cap_det, fm_resp, by = "company")


cap_det %>% 
  group_by(Product) %>% 
  mutate(count = n(), 
         count_lab = case_when(
           `Monthly Capacity` == max(`Monthly Capacity`) ~ paste(count, " Companies")
           )) %>% 
  filter(!Product %in% c("Cloth Masks","Face Shields"), str_detect(company, "Lining", negate = TRUE)) %>% 
  ggplot(aes(Product, `Monthly Capacity`, fill = company)) + 
  geom_col(position = "stack", color = "Black", width = .3) + 
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Maximum Monthly Capacity", subtitle = "Data from Thomasnet, 5/29/20. \n9 Unique Suppliers", y = "Maximum Units per Month", fill = "") + 
  theme_bw()

cap_det %>% 
  group_by(Product) %>% 
  mutate(count = n(), 
         count_lab = case_when(
           `Monthly Capacity` == max(`Monthly Capacity`) ~ paste(count, " Companies")
         )) %>% 
  filter(Product == "Respirators", str_detect(company, "Lining", negate = TRUE)) %>% 
  ggplot(aes(reorder(company, `Monthly Capacity`), `Monthly Capacity`, group = company)) + 
  geom_col(position = "stack", fill = "light grey", color = "Black") + 
  geom_label(aes(label = str_wrap(desc2)), size = 3, y = 1000000) + 
  scale_y_continuous(labels = scales::comma) + 
  coord_flip() + 
  labs(title = "Maximum Monthly Capacity, Selected Suppliers", subtitle = "Data from Thomasnet, 5/29/20. \n7 Companies Total (~6.7% of Total Self-Identifying as FDA Approved Manufacturing Universe)", y = "Maximum Respirators per Month", x = "") + 
  theme_bw()

cap_det %>% 
  group_by(Product) %>% 
  mutate(count = n(), 
         count_lab = case_when(
           `Monthly Capacity` == max(`Monthly Capacity`) ~ paste(count, " Companies")
         )) %>% 
  filter(Product == "Respirators", str_detect(company, "Lining")) %>% 
  ggplot(aes(reorder(company, `Monthly Capacity`), `Monthly Capacity`, group = company)) + 
  geom_col(position = "stack", fill = "light grey", color = "Black", width = .3) + 
  geom_label(aes(label = str_wrap(desc)), size = 3, y = 10000000) + 
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Maximum Monthly Capacity, USA Lining Inc", subtitle = "Data from Thomasnet, 5/29/20. \nCompany Located in Norman, Oklahoma ", y = "Maximum Units per Month", x = "") + 
  theme_bw()

