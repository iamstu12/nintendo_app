

# Load in Libraries ----

library(tidyverse)
library(CodeClanData)
library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(DT)
library(shinythemes)
library(beepr)

# Load in Data ----

game_sales <- game_sales


# Clean Data ----

# Rename columns and change to 'title' case

game_sales <- game_sales %>% 
  
  rename('year of release' = year_of_release) %>%
  rename('critic score' = critic_score) %>%
  rename('user score' = user_score) %>%
  rename(console = platform)

colnames(game_sales) <- str_to_title(colnames(game_sales))

# Create a new column called 'platform', based on the type of console

game_sales <- game_sales %>% 
  
  mutate(Platform = case_when(
    Console == "PS4" ~ "Playstation",
    Console == "PS3" ~ "Playstation",
    Console == "PS2" ~ "Playstation",
    Console == "PS" ~ "Playstation",
    Console == "PSP" ~ "Playstation",
    Console == "PSV" ~ "Playstation",
    Console == "X360" ~ "Xbox",
    Console == "XOne" ~ "Xbox",
    Console == "XB" ~ "Xbox",
    Console == "PC" ~ "PC",
    TRUE ~ "Nintendo"
  ))

# Rename some of the consoles

game_sales <- game_sales %>%
  
   mutate(Console = recode(Console,
                          "X360" = "Xbox 360",
                          "PS" = "PS One",
                          "XOne" = "Xbox One",
                          "XB" = "Xbox",
                          "GC" = "GameCube",
                          "GBA" = "Gameboy Advance",
                          "WiiU" = "Wii U")) 


# Arrange results in descending order

game_sales <- game_sales %>%
  mutate(`Overall Score` = `Critic Score` + `User Score`) %>% 
  arrange(desc(`Overall Score`))




# Game App ----

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
  
  theme = shinytheme("yeti"),
  
  br(),
  br(),

  img(src = "600px-Nintendo.svg.png",
      width = "400", height = "100"),
  
  titlePanel(h1("Top Rated Games")),
  
 
  pickerInput("console",
              (h3("Choose your Console")),
              choices = c("Gameboy Advance", "GameCube", "DS", "Wii", "3DS", "Wii U"), # <---- order of release date 
              options = list(`actions-box` = TRUE),
              multiple = T,
              width = NULL
  ),
  
  br(),
  
  
  actionButton("update", (h2("Here We GO!"))), # <---- mario catch phrase
  
  br(),
  br(),
  
  ), # < ---- closes sidebar panel
  
 
  mainPanel(
    
    br(),
    br(),
    br(),
    
    img(src = "marionsmb23kxtk.png",
                width = "300", height = "300")
    
    
  ) # < ---- closes main panel
  
  ), # < ---- closes sidebar layout
  
  
  hr(),
  

  DT::dataTableOutput("table_output"), 
  
  br(),
  
  tags$p(h5(strong("Please note:"))),
  tags$p(h5("Games are rated by the highest critic scores AND user scores.")),
  tags$p(h5("Critic scores are out of 100 and user scores are out of 10.")),
  
  br(),
  hr()
  
  
)  # < ---- closes fluid page



server <- function(input, output) {
  
  game_data <- eventReactive(input$update, {
    
    observeEvent(input$update, {
      beep(8 )
    })
    
    game_sales %>%
      select("Name", "Console", "Year Of Release", "Critic Score", "User Score") %>%
      filter(Console == input$console)
  
      
  })
  
  output$table_output <- DT::renderDataTable({
    game_data() 
    
  })
}


shinyApp(ui = ui, server = server)


# this particular app is aimed towards nintendo fans
# the purpose of the app is to be able to view the top rated games
# users can either select all consoles and view the top games OR
# select their favourite console(s) individually















