library(here)
library(tidyverse)
library(readxl)
library(patchwork)
library(RColorBrewer)

box_dir <- "C:/Users/Nikhil Kalathil/Box/COVID 19 Master Folder/Interviews/NK Interview Recordings"

box_here <- function(file) {
  paste(box_dir, file, sep = "")
}

log <- read_excel(box_here("/log.xlsx"), sheet = "Sheet1")

log %>% 
  mutate(category = case_when(
    `broad category` %in% c("End product manufacturer", "Intermediary product manufacturer") ~ `pivot category`, 
    `broad category`%in% c("Federal Government", "State Government") ~ `broad category`, 
    TRUE ~ "Other"
  ), 
  category2 = case_when(
    `broad category`%in% c("Federal Government", "State Government") ~ `pivot category`, 
    category %in% c("Other") ~ `pivot category`, 
    `broad category` %in% c("End product manufacturer", "Intermediary product manufacturer") ~ `broad category`
  )) %>% 
  group_by(category, category2) %>% 
  count() %>% 
  group_by(category) %>% 
  mutate(total = n()) %>% 
  ggplot() + 
  geom_col(aes(x = reorder(category, total), y = n, group = category2, fill = category2), position = "stack") + 
  guides(fill = FALSE) + 
  coord_flip()

log %>% 
  mutate(category = case_when(
    `broad category` %in% c("End product manufacturer", "Intermediary product manufacturer") ~ `pivot category`, 
    `broad category`%in% c("Federal Government", "State Government") ~ `broad category`, 
    TRUE ~ "Other"
  )) %>% 
  mutate(category3 = case_when(
    category %in% c("Other", "Federal Government", "State Government") ~ category, 
    str_detect(category, "Pivot") & str_detect(category, "Non-Pivot", negate = TRUE) ~ "Pivot", 
    str_detect(category, "New Entrant") ~ "New Entrant" ,
    str_detect(category, "Scale Up") ~ "Scale Up", 
    str_detect(category, "Non-Pivot") ~ "Non-Pivot"
  )) %>%  
  mutate(category4 = case_when(
    category %in% c("Other", "Federal Government", "State Government") ~ `pivot category`, 
    str_detect(`pivot category`, "Small") ~ "Small Firm", 
    str_detect(`pivot category`, "Medium") ~ "Medium Firm", 
    str_detect(`pivot category`, "Large") ~ "Large Firm", 
  )) %>% 
  filter(!category %in% c("Other", "Federal Government", "State Government")) %>% 
  view()


int_graph <- log %>% 
  mutate(category = case_when(
    `broad category` %in% c("End product manufacturer", "Intermediary product manufacturer") ~ `pivot category`, 
    `broad category`%in% c("Federal Government", "State Government") ~ `broad category`, 
    TRUE ~ "Other"
  )) %>% 
  mutate(category3 = case_when(
    category %in% c("Other", "Federal Government", "State Government") ~ category, 
    str_detect(category, "Pivot") & str_detect(category, "Non-Pivot", negate = TRUE) ~ "Pivot", 
    str_detect(category, "New Entrant") ~ "New Entrant" ,
    str_detect(category, "Scale Up") ~ "Scale Up", 
    str_detect(category, "Non-Pivot") ~ "Non-Pivot"
  )) %>%  
  mutate(category4 = case_when(
    category %in% c("Other", "Federal Government", "State Government") ~ `pivot category`, 
    str_detect(`pivot category`, "Small") ~ "Small Firm", 
    str_detect(`pivot category`, "Medium") ~ "Medium Firm", 
    str_detect(`pivot category`, "Large") ~ "Large Firm", 
  )) %>% 
  group_by(category3, category4) %>% 
  count() %>% 
  mutate(category_pos = case_when(
    category3 == "New Entrant" ~ 1, 
    category3 == "Pivot" ~ 2, 
    category3 == "Scale Up" ~ 3, 
    category3 == "Non-Pivot" ~ 4, 
    category3 == "Federal Government" ~ 5, 
    category3 == "State Government" ~ 6, 
    category3 == "Other" ~ 7
  ))

manf_ent = int_graph %>% 
  filter(!category3 %in% c("Other", "Federal Government", "State Government")) %>%
  ungroup() %>% 
  count() %>% 
  unlist()

non_manf = int_graph %>% 
  filter(category3 %in% c("Other", "Federal Government", "State Government")) %>%
  ungroup() %>% 
  count() %>% 
  unlist()

int1 <- int_graph %>% 
  filter(!category3 %in% c("Other", "Federal Government", "State Government")) %>% 
  ggplot() + 
  geom_col(aes(x = reorder(category3, -category_pos), y = n, group = category4, fill = category4), position = "stack", color = "Black") + 
  guides(fill = FALSE) + 
  coord_flip() +
  scale_fill_brewer(palette = "Blues") + 
  labs(title = paste("Manufacturing Entities, N:", manf_ent, sep = " "), x = "", y = "Interviews") +
  theme_bw() +
  theme(axis.text = element_text(size = 15), 
        title = element_text(size = 18))

non_manf_pal <- c(brewer.pal(9, "Purples"), brewer.pal(6, "Oranges"))
int2 <- int_graph %>% 
  filter(category3 %in% c("Other", "Federal Government", "State Government")) %>%
  mutate(nonmanf_pos = case_when(
    category4 == "Response Coordination" ~ 1, 
    category4 == "Regulation: FDA" ~ 2, 
    category4 == "Regulation: NIOSH" ~ 3, 
    category4 == "Federal Research Lab" ~ 4, 
    str_detect(category4, "Missouri") ~ 5, 
    str_detect(category4, "Washington") ~ 6, 
    str_detect(category4, "Alabama") ~ 7, 
    str_detect(category4, "North Carolina") ~ 8,
    str_detect(category4, "Regional Economic Development Organization") ~ 9, 
    category4 == "Industrial Association" ~ 10, 
    category4 == "Testing/Support" ~ 11, 
    category4 == "Regulatory Consultant" ~ 12, 
    str_detect(category4, "Industry Non-Profit") ~ 13, 
    category4 == "Regional Economic Development Consultant" ~ 14, 
    category4 == "Supplier Sourcing Data Platform" ~ 15
  )) %>%  
  ggplot() + 
  geom_col(alpha = 0.7, aes(x = reorder(category3, -category_pos), y = n, group = category4, fill = reorder(category4, nonmanf_pos)), position = "stack", color = "Black") + 
  guides(fill = FALSE) + 
  coord_flip() +
  scale_fill_manual(values = non_manf_pal) + 
  labs(title = paste("Non-Manufacturing Entities, N:", non_manf, sep = " "), x = "", y = "Interviews") +
  theme_bw() +
  theme(axis.text = element_text(size = 15), 
        title = element_text(size = 18))

int_all <- int1 / int2  

int_all

int_all_lab <- int1 / (int2  + geom_label_repel(aes(x = reorder(category3, -category_pos), y = n, group = category4, fill = reorder(category4, nonmanf_pos), label = category4), position = "stack"))