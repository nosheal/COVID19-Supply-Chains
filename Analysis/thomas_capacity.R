library(tidyverse)
library(tidytext)
library(RColorBrewer)
library(ggthemes)
library(here)
library(readxl)

### WORK

dir1 <- "C:/Users/surface/Box/COVID 19 Master Folder/Data/Masks/"

conf_cap <- read_xlsx(paste(dir1, "manf_comps.xlsx", sep = ""), sheet = "Capacity") %>% 
  filter(dom_useful == "1")

conf_cap %>% 
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
  labs(title = "Maximum Monthly Capacity", subtitle = "Data from Thomasnet, 07/13/2020. \n8 Unique Suppliers", y = "Maximum Units per Month", fill = "") + 
  theme_bw()

cap_col <- c(brewer.pal(9, "Set3")[5], brewer.pal(9, "Set3")[8], brewer.pal(9, "Set3")[6], brewer.pal(9, "Set3")[2], brewer.pal(9, "Set3")[3], brewer.pal(9, "Set3")[4], brewer.pal(9, "Set3")[7])

conf_cap %>% 
  group_by(Product) %>% 
  mutate(count = n(), 
         count_lab = case_when(
           `Monthly Capacity` == max(`Monthly Capacity`) ~ paste(count, " Companies")
         )) %>% 
  filter(Product == "Respirators", str_detect(company, "Lining", negate = TRUE)) %>% 
  ggplot(aes(reorder(company, `Monthly Capacity`), `Monthly Capacity`, group = company)) + 
  geom_col(aes(fill = reorder(company, `Monthly Capacity`)), position = "stack", color = "Black", show.legend = FALSE) + 
  geom_label(aes(label = str_wrap(desc2)), size = 3, y = 1700000, show.legend = FALSE, alpha = 0.88) + 
  scale_y_continuous(labels = scales::comma) + 
  #scale_fill_brewer(palette = "Set3") +
  scale_fill_manual(values = cap_col) +
  coord_flip() + 
  labs(title = "Maximum Monthly Capacity, Selected Suppliers", subtitle = "Data from Thomasnet, 07/13/2020. \n7 Companies Total \n30.4% of Confirmed Domestic Thomasnet Manufacturers of Standard FDA Approved Hospital Grade Respirators \n3.3% of All Potential Thomasnet Manufacturers", y = "Maximum Respirators per Month", x = "") + 
  theme_bw()

cap_col2 <- c(brewer.pal(9, "Set3")[6], brewer.pal(9, "Set3")[2], brewer.pal(9, "Set3")[3], brewer.pal(9, "Set3")[1])
  
conf_cap %>% 
  group_by(Product) %>% 
  mutate(count = n(), 
         count_lab = case_when(
           `Monthly Capacity` == max(`Monthly Capacity`) ~ paste(count, " Companies")
         )) %>% 
  filter(Product == "Surgical Masks", str_detect(company, "Lining", negate = TRUE)) %>% 
  ggplot(aes(reorder(company, `Monthly Capacity`), `Monthly Capacity`, group = company)) + 
  geom_col(aes(fill = reorder(company, `Monthly Capacity`)), position = "stack", color = "Black", show.legend = FALSE) + 
  geom_label(aes(label = str_wrap(desc2)), size = 3, y = 3000000, show.legend = FALSE, alpha = 0.88) + 
  scale_y_continuous(labels = scales::comma) + 
  #scale_fill_brewer(palette = "Set3") +
  scale_fill_manual(values = cap_col2) +
  coord_flip() + 
  labs(title = "Maximum Monthly Capacity, Selected Suppliers", subtitle = "Data from Thomasnet, 07/13/2020. \n4 Companies Total \n15.4% of Confirmed Domestic Thomasnet Manufacturers of Standard FDA Approved Hospital Grade Masks \n1.4% of All Potential Thomasnet Manufacturers", y = "Maximum Respirators per Month", x = "") + 
  theme_bw()

#### OLD
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

my_pal <- c(brewer.pal(9, "Set3")[2], brewer.pal(9, "Set3")[3], brewer.pal(9, "Set3")[4], brewer.pal(9, "Set3")[6], brewer.pal(9, "Set3")[7], brewer.pal(9, "Set3")[8], brewer.pal(9, "Set3")[9])

cap_det %>% 
  group_by(Product) %>% 
  mutate(count = n(), 
         count_lab = case_when(
           `Monthly Capacity` == max(`Monthly Capacity`) ~ paste(count, " Companies")
         )) %>% 
  filter(Product == "Respirators", str_detect(company, "Lining", negate = TRUE)) %>% 
  ggplot(aes(reorder(company, `Monthly Capacity`), `Monthly Capacity`, group = company)) + 
  geom_col(aes(fill = company), position = "stack", color = "Black", show.legend = FALSE) + 
  geom_label(aes(label = str_wrap(desc2)), size = 3, y = 1000000, show.legend = FALSE, alpha = 0.88) + 
  scale_y_continuous(labels = scales::comma) + 
  scale_fill_manual(values = my_pal) +
  coord_flip() + 
  labs(title = "Maximum Monthly Capacity, Selected Suppliers", subtitle = "Data from Thomasnet, 5/29/20. \n7 Companies Total (~3.4% of ThomasNet Manufacturers)", y = "Maximum Respirators per Month", x = "") + 
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

