library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(ggtext)  # for geom_richtext

# Assuming 'global.R' contains necessary objects like user_base_hash, user, password, assay_vec, demog_plt_dat, etc.

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  source('global.R')
  
 
  # user pwd authentication code --------------------------------------------
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base_hash,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  # session duration log ----------------------------------------------------
  
  # record start time
  start_time <- as.character(Sys.time())
  
  # write tsv when session ends (shiny tab closed by user)
  session$onSessionEnded(function() {
    end_time <- as.character(Sys.time())
    
    session_tbl <- tibble(start_time, end_time) %>%
      mutate(event='tab_close', session=session$token )
    
   # if (length(list.files('session_logs/', pattern='sessions.tsv')) == 0) {
    #  write_tsv(session_tbl, file = "session_logs/sessions.tsv")
    #} else {
    #  write_tsv(session_tbl, file = "session_logs/sessions.tsv", append = TRUE)
    #}
  })
  
  # write tsv when session ends (logout button clicked by user)
  observeEvent(logout_init(), {
    end_time <- as.character(Sys.time())
    
    session_tbl <- tibble(start_time, end_time) %>%
      mutate(event='log_out', session=session$token )
    
    if (length(list.files('session_logs/', pattern='sessions.tsv')) == 0) {
      write_tsv(session_tbl, file = "session_logs/sessions.tsv")
    } else {
      write_tsv(session_tbl, file = "session_logs/sessions.tsv", append = TRUE)
    }
  })
  

# thematic ----------------------------------------------------------------

  #thematic_shiny()
  #not really working?  cf # https://rstudio.github.io/thematic/articles/auto.html?q=shiny#shiny
  
# UI sidePanel --------------------------------------------
  
  # requires authentication to view
  output$sidebarlayout <- renderUI({
    
    req(credentials()$user_auth)
    
    page_sidebar(
    
      
      # Application title
      #title = "AMD RPD Consortium Overview",
      
      
        sidebar = sidebar(
            br(),
            selectInput(inputId = 'assay_or'
                    #,label = "Filter assays (a OR b):"
                    ,label = tooltip(span("Filter assays",
                                         bsicons::bs_icon("info-circle-fill")), 
                                    'Multiple selection displays data for donors in assay A and/or B.')
                    ,multiple = TRUE
                    ,choices = assay_vec
                    ,selected = 'rpe_line'
                    ),
          br(),
          radioButtons(inputId = 'boxplot_lvl', 
                     label = 'Segment data by:',
                     choiceNames = c('amd','rpd','sex','smoker'),
                     choiceValues = c('snp_amd','snp_rpd','sex','smoking_status')
        )
      ),
      
# main panel --------------------------------------------------------------

## layout cards ------------------------------------------------------------

      
      layout_column_wrap(
      
          width = 1, height = 600, 
        heights_equal = 'row', 
        
        # first plot card
        card(full_screen = TRUE,
             #card_header( 
            #  tooltip(span("Assays",
            #               bsicons::bs_icon("info-circle-fill")), 'The assays we did')),
             plotOutput("big_plot") 
        ),
        
        # second row with 2 cards 
        layout_column_wrap(
          width = 1/2, height = 200, heights_equal = 'row',
          card(plotOutput('demog_cat_plot')), 
          card(plotOutput('demog_cont_boxplt'))
        )
      )
    )
  })
  

# UI end ------------------------------------------------------------------


# ~~~ -------------------------------------------------------------------------

  
# server start ------------------------------------------------------------
  
  # subset data -------------------------------------------------------------
  
  subset_ids <- reactive({
    if (!is.null(input$assay_or)) {
      assay_long %>% 
        filter(key %in% input$assay_or) %>%
        drop_na(value) %>%
        select(cera_id) %>% 
        distinct() %>% 
        pull(cera_id)
    } else if (is.null(input$assay_or)) {
      px_vec
    }
  })
  
  # set up plot objects -----------------------------------------------------
  
  ## plt cat -----------------------------------------------------------------
  
  demog_plt_counts <- reactive({
 
       demog_plt_dat %>% 
      filter(cera_id %in% subset_ids()) %>% 
      group_by(key) %>% 
      sort_prop(key, value) %>% 
      arrange(key, value) 
  })
  
  plt_cat <- reactive({
    
    total <- demog_plt_counts() %>% filter(key=='sex') %>% ungroup() %>% summarize(sum=sum(n))
    
    demog_plt_counts() %>% 
      ggplot(aes(x = key, y = n, fill = value)) +
      geom_col(col = 'white', position = 'stack') +
      geom_richtext(aes(x = key, y = n, 
                        label = paste0("<b>", value, '<br>', n, "</b>")),
                    text.color = 'white',
                    label.color = NA,
                    size = 4,
                    position = position_stack(vjust = 0.5)) +
      #add sum
      ylab(paste0('\nTotal = ',total)) + xlab( '') +
      #scale_fill_manual overrides any thematic::thematic_shiny() effects ?
      #tbc could update gg_col_default to count unique keys in demog_plt_counts()
      scale_fill_manual(values = c("#818181", gg_col_default(12))) +
      theme_minimal() + 
      theme(axis.text.y = element_text(size = 10),
            #axis.text.x = element_text(size = 8),
            axis.text.x = element_blank(),
            axis.title = element_text(size = 14, face = 'bold')) +
          coord_flip() + 
          legend_hide() 
  })
  
  output$demog_cat_plot <- renderPlot({ 
    print(plt_cat())
  })
  
  ## plt cont box ------------------------------------------------------------
  
  boxlevels <- reactive({input$boxplot_lvl})
  
  plt_cont <- reactive({
    demog_full %>% 
      filter(cera_id %in% subset_ids()) %>% 
      gather(key, value, c(weight_kg:age_at_med_hx)) %>% 
      drop_na() %>% 
      ggplot(aes(y = key, x = value,
                 col = !!sym(boxlevels()),
                 fill = !!sym(boxlevels()))) +
      geom_violin(alpha = 0.2) + 
      geom_boxplot(alpha = 0.2, width = 0.5,
                   position = position_dodge(width = 0.85)) + 
      geom_point(position = position_jitterdodge(dodge.width = 0.9,
                                                 jitter.width = 0.1, 
                                                 jitter.height = 0),
                 cex = 0.75, alpha = 0.5) +
      #tidyExt::geom_boxdodge() +
      ylab('') + 
      xlab('') +
      theme(axis.text.y = element_text(size = 10),
            axis.text.x = element_blank())
             
  })
  
  output$demog_cont_boxplt <- renderPlot({
    print(plt_cont() +
            coord_flip() +
            facet_wrap(~key, scales = 'free', ncol = 3) +
            theme(strip.text = element_text(size = 12))) 
  })
  
  ## plt_assay -------------------------------------------------------------------------
  
  plt_ass <- reactive({
    assay_long %>% 
      mutate(source = factor(source, levels = c('CERA', 'Pebay', 'Fletcher'))) %>% 
      filter(cera_id %in% subset_ids()) %>% 
      mutate(
        cera_id = factor(cera_id, levels = col_na_count$cera_id),
        key = factor(key, levels = rev(row_na_count$key))
      ) %>%
      # join categorical metadata to enable faceting
      left_join(demog_full %>% select(cera_id, snp_amd,snp_rpd,smoking_status,sex),
                by = 'cera_id') %>% 
      filter(value == '+') %>%
      ggplot(aes(x = cera_id, y = key, fill = value)) + 
      geom_tile() + 
      geom_text(aes(x = length(subset_ids), y = key, label = ''), nudge_x = -20) +
      x_angle() +
      scale_fill_manual(values = c('#00B4F0')) +
      # tbc: reactive split on radio button. requires joining demog_cat factors to assay_long   
      # facet_wrap(~boxlevels(),nrow=1) + 
      # y_angle() + 
      ylab('') + 
      xlab('AMD RPD Cohort Patients') + 
      legend_hide() +
      theme(axis.text.x.bottom = element_blank(),
            axis.text.y = element_text(size = 10),
            strip.text = element_text(size = 12))
  })
  
  # big_plot -----------------------------------------------------------------
  
  pbig <- reactive({
    plt_ass() + 
      facet_grid(~get(input$boxplot_lvl), scales = "free_x", space = 'free')
  })
  
  output$big_plot <- renderPlot({
    print(pbig())
  })
  
})
