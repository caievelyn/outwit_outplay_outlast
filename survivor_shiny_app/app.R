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

survivor_data$season.x <- as.factor(survivor_data$season.x)

# Define UI for application that draws a histogram based on the gender selected

ui <- fluidPage(
  
  theme = shinytheme("simplex"),
  
  # Application title
  
  h1("Individual Challenge Wins By Post-Merge Constestants In 37 Seasons of Survivor"),
  
  navbarPage("Survivor!",
    tabPanel("Explore the Dataset",
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "select_season",
                      label = "Choose a season to display:",
                      choices = levels(survivor_data$season.x),
                      multiple = TRUE
          )
        ),
        mainPanel(
          dataTableOutput("data_explorer")
        )
      )
    ),
    tabPanel("Outwit"
    ),
    tabPanel("Outplay"
    ),
    tabPanel("Outlast",
      sidebarLayout(
        sidebarPanel(
          
          # Sidebar with a radio button input for selected gender
          
          radioButtons(inputId = "gender",
                       choices = c("Female", "Male"),
                       label = "Gender")
        ),
        
        mainPanel(plotOutput("outlastPlot"))
      )
    )
  )
)

# Define server logic required to draw a histogram

server <- function(input, output) {
  
  # Render a plot for the output
  
  output$outlastPlot <- renderPlot ({
    
    data <- subset(survivor_data,
                   
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
  
  output$data_explorer <- renderDataTable({
    
    data <- subset(survivor_data,
                   season.x == input$select_season)
    
    data
  })
}


# Run the application 
shinyApp(ui = ui, server = server)