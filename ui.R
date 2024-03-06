
source('global.R')

# UI ----------------------------------------------------------------------


ui <-  page_fluid(

# custom tags -------------------------------------------------------------

tags$head(
  tags$style(
    HTML("
           .my-title-panel {
             /* background-color: #00B4F0; /* Set your desired background color */ 
             /*color: #818181; /* Set the text color */
             color: #00B4F0; /* Set the text color */
             font-size: 12 px; /* Set the text size */
             padding: 10px; /* Adjust padding as needed */
           }
           ")
  )
),


## theme -------------------------------------------------------------------


theme = bs_theme(
  bootswatch = "yeti",
  #bootswatch = "flatly",
  #base_font = font_google("Crimson Text"), #google font is auto-downloaded
  #"font-size-base" = "0.75rem", "enable-rounded" = TRUE, 
  spacer = "0.9rem",
  navbar_bg = "#00BFC4"
)
,



# title & logo ------------------------------------------------------------



titlePanel(windowTitle = "",
           title =
             div(
               #img(
              #   src = "pic.png",
              #   height = 100,
              #   width = 40,
              #   style = "margin:5px 10px"
              # ),
               "AMD RPD Consortium Overview",
               class = "my-title-panel"
             )),





# login section -----------------------------------------------------------

#br(),br(),br(),br(),

shinyauthr::loginUI(id = "login",
                    title="Log in",
                    user_title = "User name",
                    pass_title = "Password"),


div(shinyauthr::logoutUI(id = "logout",
                         #logout button features:
                         class = 'btn-dark',
                         style = "position: absolute; top: 10px; right: 10px; ")),

#remaining bulk of ui code now sits within server.R ; and requires authentication
uiOutput("sidebarlayout")

 )

