#!/usr/bin/env Rscript
# Author: Ryan
# Date: 2024-01-31
# Purpose: choose deck
library(parallel)
library(logger)
suppressMessages(library(tidyverse))
# 检测系统的CPU数
cores <- detectCores()

cards <- readxl::read_excel("clashmini.xlsx")

# check for integrity

cards_by_class <- cards %>%
#  filter(is_available) %>%
  separate_rows(classes) %>%
  mutate(name = factor(name))

table(cards_by_class$classes)

level_score <- 1:6 / 10 + 1
names(level_score) = c("common", "uncommon", "rare", "epic", "fabled", "legendary")

mini_ids <- cards_by_class %>%
  filter(type == "mini") %>%
  distinct(id) %>%
  pull(id)
hero_ids <- cards_by_class %>%
  filter(type == "hero") %>%
  distinct(id) %>%
  pull(id)

mini_cb <- combn(mini_ids, 5)

passed_lst <- mclapply(seq.int(ncol(mini_cb)), function(i){
# for (i in seq.int(ncol(mini_cb))) {
  mini_tbl <- cards_by_class %>%
    filter(id %in% mini_cb[,i])
  hero_candidate_ids <- cards_by_class %>%
    filter(type == "hero",
           classes %in% mini_tbl$classes) %>%
    distinct(id) %>%
    pull(id)
  pass_tbl_i <- tibble()
  if (i %% 1000 == 0)
    log_info(paste(i,ncol(mini_cb)))
  for (j in hero_candidate_ids) {
    deck_tbl <- cards_by_class %>%
      filter(id %in% c(mini_cb[,i], j))
    deck_classes <- deck_tbl %>%
      group_by(classes) %>%
      summarise(n = n()) %>%
      filter(n > 1) %>%
      rowwise() %>%
      mutate(nc = min(4, floor(n/2) * 2))
    if (sum(deck_classes$nc) < 10)
      next()
    deck_classes_str <- deck_classes %>%
      mutate(str = paste0(classes, nc)) %>%
      pull(str) %>%
      paste(collapse = ",")
    deck_cost = deck_tbl %>%
      distinct(name, .keep_all = T) %>%
      pull(cost) %>%
      sum()
    pass_tbl_i <- bind_rows(pass_tbl_i, 
      deck_tbl %>%
        distinct(type, name) %>%
        arrange(type) %>%
        mutate(type = paste0(type, c('', 1:5))) %>%
        spread(type, name) %>%
        mutate(classes = deck_classes_str,
               sum     = sum(deck_classes$nc),
               deck_cost = deck_cost,
               is_available = all(deck_tbl$is_available),
               score = sum(level_score[deck_tbl$level]))
    )
  }
  pass_tbl_i
} , mc.cores = cores)
passed_tbl <- bind_rows(passed_lst)
saveRDS(passed_tbl, "all_passed_tbl.rds")
readr::write_excel_csv(passed_tbl, "all_passed_tbl.csv")
