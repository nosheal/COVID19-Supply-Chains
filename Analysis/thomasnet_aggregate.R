library(readxl)
library(tidyverse)
library(lubridate)
library(ggrepel)

box_dir <- "C:/Users/Nikhil Kalathil/Box/COVID 19 Master Folder/Data/Masks/"

box_here <- function(file) {
  paste(box_dir, file, sep = "")
}

agg %>% 
  select(specific_product, total, date) %>% 
  filter(specific_product != "Spunbonded Olefins") %>% 
  group_by(specific_product) %>% 
  arrange(specific_product, date) %>% 
  four_week('total') %>% 
  view()


agg <- read_xlsx(box_here("thomasnet_aggregate.xlsx")) %>% 
  select(specific_product, total, date) %>% 
  filter(specific_product != "Spunbonded Olefins") %>% 
  group_by(specific_product) %>% 
  arrange(specific_product, date) %>% 
  mutate(week_1 = lag(total, n= 1, order_by = date), 
         week_2 = lag(total, n = 2, order_by = date),
         week_3 = lag(total, n = 3, order_by = date),
         week_4 = lag(total, n = 4, order_by = date),
         week_4_3 = (week_3 - week_4)/week_4,
         week_3_2 = (week_2 - week_3)/week_3,
         week_2_1 = (week_1 - week_2)/week_2,
         week_1_0 = (total - week_1)/week_1,
         mvg_avg = (week_4_3 + week_3_2 + week_2_1 + week_1_0) / 4,
         percent_change = 100*round(mvg_avg, 3))

agg %>% 
  mutate(prod_lab = case_when(
    date == ymd("2020-06-05") ~ specific_product
  ), 
  change_lab = case_when(
    date == ymd("2020-08-25") ~ paste(percent_change, " %", sep = ""))) %>% 
  ggplot(aes(date, total, group = specific_product, color = specific_product, fill = specific_product, label = prod_lab)) +
  geom_line(alpha = 0.5, show.legend = FALSE) + 
  geom_label_repel(color = "Black", show.legend = FALSE, alpha = 0.7, hjust = -0.2, vjust = 1) + 
  geom_label_repel(aes(label = change_lab), color = "Black", show.legend = FALSE, alpha = 0.7, hjust = -0.2, vjust = 1) +
  geom_point(shape = 21, alpha = 0.7, size = 3, color = "Black", show.legend = FALSE) + 
  labs(title = "Thomasnet Supplier Search Results Over Time", subtitle = "For Surgical Grade Mask/Respirator Supply Chain Search Terms \n05/30/20 - 08/31/20", y = "Unique Thomasnet Suppliers", fill = "", x = "Date") + 
  theme_bw()