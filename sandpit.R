## R v4.3.2 

## Aim:

## Notes:

#rm(list=ls()); gc()
#renv::init()

library(tidyverse); library(readxl); library(janitor)
library(glue);      library(scales); library(here) 

if(!require(tidysteps)){
  devtools::install_github('bansell/tidysteps'); 
library(tidysteps)}

theme_set(theme_minimal())
fix_tidyverse_conflicts()

default_grey <- "#595959"

here()

library(ggtext)
# source ------------------------------------------------------------------

source('global.R')

# plot cont ---------------------------------------------------------------



mysplit <- 'sex'
mysplit <- 'snp_rpd'

demog_full %>% 
  gather(key,value,c(weight_kg:age_at_med_hx)) %>% 
  ggplot(aes(y=key,x=value,
             col=!!sym(mysplit))) + 
  geom_violin(alpha=0) + geom_boxplot(alpha=0,width=0.5,
                                      position=position_dodge(width=0.85)) + 
  geom_point(position=position_jitterdodge(dodge.width = 0.8,jitter.width = 0.15),
             cex=0.2,alpha=0.5) +
  facet_wrap(~key,scales='free',ncol=1) + ylab('') + xlab('') 
  


# plot cat ---------------------------------------------------------------

#in global:
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
  mutate(value=fct_relevel(value,'NA', after = Inf))


demog_plt_dat %>% 
  ggplot(aes(y=key )) + # , col=!!sym(mysplit))) + 
  geom_bar(aes(fill=value),col='white') 


#in server:

# subset here << #

demog_plt_counts <- demog_plt_dat %>% 
  # demog_ss <- demog_full %>% filter()
  group_by(key) %>% 
  sort_prop(key,value) %>% 
  arrange(key,value) 



demog_plt_counts %>% #str()
  ggplot(aes(x = key, y = n,fill=value)) +
  geom_col(#aes(fill = value), 
           col = 'white', position = 'stack') +
  geom_richtext(aes(x = key, y = n, label = paste0('<b>',value,'</b>')),
                #fill = NA, 
                label.color = NA, text.color = 'white',
                position = position_stack(vjust = 0.5)) +
  geom_richtext(aes(x = key, y = n, label = paste0('<br><br>','<b>',n,'</b>')),
            #fill = NA,  wreaks havoc with order
            label.color = NA, text.color = 'white',
            position = position_stack(vjust = 0.5)) +
    labs(y = 'Count',x='') +
  scale_fill_manual(values=c(gg_col_default(12),
                             #"dark grey")) +
                             "#818181")) +
  theme_minimal() + coord_flip() + legend_hide()
  
#tbc update colours

demog_plt_counts %>%
ggplot(aes(x = key, y = n, fill = value)) +
  geom_col(col = 'white', position = 'stack') +
  geom_richtext(aes(x = key, y = n, 
                    label = paste0(value,
                                   '<br><br>',
                                   "<b>",n,"</b>")), #tbc: make bold
                #label = paste0('\n\n',n)), 
                text.color='white', #fill=NA,
                label.color=NA,
                position = position_stack(vjust = 0.5)) +
  labs(y = 'Count',x='') +
  scale_fill_manual(values=c(gg_col_default(12),
                             #"dark grey")) +
                             "#818181")) +
  theme_minimal() + coord_flip() + legend_hide() 

