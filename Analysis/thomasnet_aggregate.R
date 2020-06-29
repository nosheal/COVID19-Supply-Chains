library(readxl)
library(tidyverse)
library(lubridate)
library(ggrepel)


agg <- read_xlsx(here("Data/thomasnet_aggregate.xlsx")) %>% 
  select(specific_product, total, date)

agg %>% 
  mutate(prod_lab = case_when(
    date == ymd("2020-06-05") ~ specific_product
  )) %>% 
  ggplot(aes(date, total, group = specific_product, color = specific_product, fill = specific_product, label = prod_lab)) +
  geom_line(alpha = 0.5, show.legend = FALSE) + 
  geom_label_repel(color = "Black", show.legend = FALSE, alpha = 0.7, hjust = -0.2, vjust = 1) + 
  geom_point(shape = 21, alpha = 0.7, size = 3, color = "Black", show.legend = FALSE) + 
  labs(title = "Thomasnet Supplier Search Results Over Time", subtitle = "For Key Mask/Respirator Supply Chain Search Terms", y = "Unique Suppliers", fill = "") + 
  theme_bw()