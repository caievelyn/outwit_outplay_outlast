# Load the shiny library

library(shiny)
library(tidyverse)
library(gganimate)
library(ggplot2)
library(janitor)
library(rebus)
library(DT)
library(shinythemes)

# Read in the CSV file

survivor_data <- read_csv("survivor_data.csv")

# colnames(survivor_data) <- c("Contestant",
#                              "Age",
#                              "City",
#                              "State",
#                              "Season",
#                              "Finish",
#                              "Tribal Challenge Wins", 
#                              "Individual Challenge Wins",
#                              "Total Wins",
#                              "Days Lasted",
#                              "Votes Against",
#                              "Gender",
#                              "Occupation",
#                              "Season Number",
#                              "Idols Found",
#                              "Idols Played")

survivor_data$season.x <- as.factor(survivor_data$season.x)

survivor_data$idols_found[is.na(survivor_data$idols_found)] <- 0

survivor_data$idols_played[is.na(survivor_data$idols_played)] <- 0

# Define UI for application that draws a histogram based on the gender selected

ui <- fluidPage(
  
  theme = shinytheme("simplex"),
  
  # Application title
  
  h1("Survivor: Outwit, Outplay, Outlast"),
  
  navbarPage("Survivor!",
    tabPanel("Explore the Dataset",
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "select_season",
                      label = "Choose a season to display:",
                      choices = c("All", levels(survivor_data$season.x)),
                      multiple = TRUE),
          sliderInput(inputId = "age_slider",
                      label = "Age",
                      min = min(survivor_data$age),
                      max = max(survivor_data$age),
                      step = 1,
                      value = c(min(survivor_data$age), max(survivor_data$age))),
          br(),
          sliderInput(inputId = "days_slider",
                    label = "Days Lasted",
                    min = min(survivor_data$daysLasted),
                    max = max(survivor_data$daysLasted),
                    step = 1,
                    value = c(min(survivor_data$daysLasted), max(survivor_data$daysLasted))),
          br(),
          sliderInput(inputId = "idols_slider",
                      label = "Idols Played",
                      min = 0,
                      max = max(survivor_data$idols_played),
                      step = 1,
                      value = c(0, max(survivor_data$idols_played))),
          br(),
          checkboxGroupInput(inputId = "gender_check",
                             label = "Gender",
                             choices = c("Male", "Female"),
                             selected = c("Male", "Female")),
          br(),
          radioButtons(inputId = "winner",
                     label = "Finish Place",
                     choices = c("All", "Sole Survivor")),
          br(),
          submitButton(text = "Display")
        ),
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("About", textOutput("blurb")),
                      tabPanel("Data Explorer", dataTableOutput("data_explorer"))
          )
        )
      )
    ),
    tabPanel("Outwit: Idols",
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("About", textOutput("blurb")),
                    tabPanel("Data Explorer", dataTableOutput("data_explorer"))))),
    tabPanel("Outplay: Immunity and Challenges"),
    tabPanel("Outlast: High Level Trends",
      sidebarLayout(
        sidebarPanel(
          
          # Sidebar with a radio button input for selected gender
          
          radioButtons(inputId = "gender",
                       choices = c("Female", "Male"),
                       label = "Gender"),
          br(),
          submitButton(text = "Display")
        ),
        
        mainPanel(plotOutput("outlastPlot"))
      )
    )
  )
)

# Define server logic required to draw a histogram

server <- function(input, output) {
  
  output$blurb <- renderText({
    "hello this is Survivor!"
  })
  
  output$data_explorer <- renderDataTable({
    req(input$select_season)
    data <- survivor_data
    data <- subset(data,
                   age >= input$age_slider[1] &
                   age <= input$age_slider[2] &
                   daysLasted >= input$days_slider[1] &
                   daysLasted <= input$days_slider[2] &
                   idols_played >= input$idols_slider[1] &
                   idols_played <= input$idols_slider[2])
    if (input$select_season != "All") {
      data <- subset(data, season.x == input$select_season)
    }
    
    if (input$winner == "Sole Survivor") {
      data <- subset(data, finish == 1)
    }
    
    if (length(input$gender_check) == 1) {
      data <- subset(data, gender == input$gender_check)
    }
    
      data
  })
  
  
  output$outlastPlot <- renderPlot ({
    
    data <- survivor_data
    
    data <- subset(data,
                   
                   # Filter out the N/A values for individual challenge wins,
                   # which indicates that the contestant was voted out before
                   # the merge
                   
                   individualChallengeWins != "N/A" &
                     
                     # Filter out those who lasted less than two weeks, or 14
                     # days. Since the merge is usually the mid-way point and
                     # contain around a dozen remaining contestants, it varies
                     # season to season, but the merge has never occurred prior
                     # to the two week mark.
                     
                     daysLasted > 14 &
                     
                     # Filter only for the selected gender
                     
                     gender == input$gender)
    
    # Create the ggplot object that will hold the actual histogram
    
    p <-  ggplot(data, mapping = aes(x = individualChallengeWins)) +
      geom_histogram(show.legend =FALSE) +
      
      # Change the scaling for the x-axis to be easier to read
      
      scale_x_continuous(breaks = seq(0, 10, by = 1)) +
      labs(title = "Individual Challenge Wins By Post-Merge Constestants In 37 Seasons of Survivor",
           subtitle = "Skewed right plots for number of individual wins show that they are rare for both genders,\n although more on the high extreme for men",
           caption = "Source: https://raw.githubusercontent.com/davekwiatkowski/survivor-data/master/player-data.json")
    
    # Call the ggplot object
    p
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)