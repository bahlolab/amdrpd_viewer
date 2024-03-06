
rm(list=ls()); gc()

load('data/amd_rpd_shiny_dat.Rda')

library(tidyverse)
library(tidysteps)

library(patchwork)

theme_set(ggplot2::theme_minimal())

library(shiny)
library(shinyauthr)
library(bslib)
library(ggtext)

library(thematic) # theming for ggplot



library(here)


# custom fns --------------------------------------------------------------

source('functions/fuzzy_lookup.R')



# security: read pwd hash -----------------------------------------------------------

user_base_hash <- read_rds('pswd/pwd_hash_tbl.Rds')


# rename ------------------------------------------------------------------

assay_long <- assay_long %>% rename(cera_id=mru_id)

col_na_count <- col_na_count %>% rename(cera_id=mru_id)


# shorten assay descript --------------------------------------------------

too_long <- sort_prop(assay_long, key) %>% slice(17:26) %>% pull(key)

shorter <- quote_this('
                      rpe_immunostain drusen_quant oct_flicker mito_membrane pbmc_rpe_culture 
                      pr_phagocytosis monolayer_form seahorse_assay stress_drusen_count sod_assay')


lookup_tbl <- tibble(too_long,shorter)


# overwrite with shorter rownames --------------------------------------------

assay_long <- fuzzy_lookup(lookup_tbl, search_term = too_long, replace_term = shorter,
                           .df = assay_long, 
                           search_col = 'key', new_col = 'key', #NB this overwrites the column
                           .default = key) # %>% sort_prop(key)

row_na_count <- fuzzy_lookup(lookup_tbl, search_term = too_long, replace_term = shorter,
                           .df = row_na_count, 
                           search_col = 'key', new_col = 'key', #NB this overwrites the column
                           .default = key) # %>% 





# set up selectors --------------------------------------------------------

assay_vec <- row_na_count %>% ungroup() %>% 
  #filter(n<957) %>% 
  pull(key)

px_vec <- unique(col_na_count$mru_id)


# demog distinct ----------------------------------------------------------

demog_cat <- distinct(demog_cat)
demog_cont <- distinct(demog_cont)


# shorten values ----------------------------------------------------------

demog_cat <- demog_cat %>%
  mutate(across(c(smoking_status,snp_amd,snp_rpd),
                .fns = ~ str_remove(.,"RPD "))) %>%
  mutate(across(c(smoking_status,snp_amd,snp_rpd),
                .fns = ~ str_remove(.,"AMD"))) %>%
  mutate(across(c(smoking_status,snp_amd,snp_rpd),
                .fns = ~ str_remove(.,"Advanced "))) %>%
  mutate(across(c(smoking_status,snp_amd,snp_rpd),
                .fns = ~ str_remove(.,"_Smoked"))) %>%
  mutate(across(c(smoking_status,snp_amd,snp_rpd),
                .fns = ~ str_remove(.,"_Smoker"))) %>%
  mutate(across(c(smoking_status,snp_amd,snp_rpd),
                .fns = ~ trimws(.)))



# full demog --------------------------------------------------------------


demog_full <- full_join(demog_cat,demog_cont,by=c('source','cera_id')) %>% 
  mutate(snp_amd=factor(snp_amd,levels=quote_this("No Early Intermediate GA CNV")))



# demog categories --------------------------------------------------------

#data for stacked barchart of counts

demog_plt_dat <- demog_full %>% 
  select(-c(1,weight_kg:age_at_med_hx)) %>% 
  gather(key,value,-1) %>% 
  mutate(value= str_remove(value,"RPD")) %>% 
  mutate(value= str_remove(value," AMD")) %>%
  mutate(value= str_remove(value,"Advanced ")) %>%
  mutate(value= str_remove(value,"_Smoked")) %>%
  mutate(value= str_remove(value,"_Smoker")) %>% 
  mutate(value= ifelse(is.na(value),"NA",value)) %>%
  mutate(value=fct_inorder(value)) %>% 
  #mutate(value=fct_relevel(value,'NA', after = Inf))
  mutate(value=fct_relevel(value,'NA'))



