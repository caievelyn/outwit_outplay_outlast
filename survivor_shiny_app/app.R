# Load the appropriate libraries

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

# Change the season column to a factor

survivor_data$season.x <- as.factor(survivor_data$season.x)

# Change the NA values created through the join in the idols_found column to 0

survivor_data$idols_found[is.na(survivor_data$idols_found)] <- 0

# Change the NA values created through the join in the idols_played column to 0

survivor_data$idols_played[is.na(survivor_data$idols_played)] <- 0

# Define UI for application

ui <- fluidPage(
  
  # Change theme using shinytheme library to 'simplex'
  
  theme = shinytheme("simplex"),
  
  # Application title
  
  h1(" Survivor: Outwit, Outplay, Outlast"),
  
  # Add a caption detailing what the project is for
  
  h4("by Evelyn Cai, for Gov 1005, a data course at Harvard University", style = "color:red"),
  
  # Create the navigation bar
  
  navbarPage("Survivor!",
             
             # Create the dataset explorer tab. This wil render a data table, so
             # add a sidebar layout with various different functionalities to
             # filter the dataset.
             
             tabPanel("Explore the Dataset",
                      sidebarLayout(
                        sidebarPanel(
                          
                          # Create a selectInput for the user to select a
                          # specific season(s), or all the seasons. Multiple =
                          # TRUE so users can select more than one season.
                          
                          selectInput(inputId = "select_season",
                          label = "Choose a season to display:",
                          choices = c("All", levels(survivor_data$season.x)),
                          multiple = TRUE),
                          
                          # Create a two-sided range slider to adjust for age,
                          # stepping by 1. Set the default to the whole range,
                          # using the min() and max() function
                          
                      sliderInput(inputId = "age_slider",
                                  label = "Age",
                                  min = min(survivor_data$age),
                                  max = max(survivor_data$age),
                                  step = 1,
                                  value = c(min(survivor_data$age), max(survivor_data$age))),
                      br(),
                      
                      # Create a two-sided range slider for numbers of days
                      # lasted, stepping by 1 and setting the default value to
                      # the whole range as well.
                      
                      sliderInput(inputId = "days_slider",
                                label = "Days Lasted",
                                min = min(survivor_data$daysLasted),
                                max = max(survivor_data$daysLasted),
                                step = 1,
                                value = c(min(survivor_data$daysLasted), max(survivor_data$daysLasted))),
                      br(),
                      
                      # Create a two-ranged slider for number of idols played,
                      # with the minimum at 0 and the max at the max() of
                      # idols_played, which is 3. Step by 1 because the range is
                      # quite small, and set the default value to the whole
                      # range.
                      
                      sliderInput(inputId = "idols_slider",
                                  label = "Idols Played",
                                  min = 0,
                                  max = max(survivor_data$idols_played),
                                  step = 1,
                                  value = c(0, max(survivor_data$idols_played))),
                      br(),
                      
                      # Create a checkboxgroup input so more than one box can be
                      # checked off for filtering by gender. The default is that
                      # both are selected (which represents the whole dataset)
                      
                      checkboxGroupInput(inputId = "gender_check",
                                         label = "Gender",
                                         choices = c("Male", "Female"),
                                         selected = c("Male", "Female")),
                      br(),
                      
                      # Add two radio buttons that allow the user to select for
                      # either the Sole Survivor of each season, or all the
                      # contestants
                      
                      radioButtons(inputId = "winner",
                                 label = "Finish Place",
                                 choices = c("All", "Sole Survivor")),
                      br(),
                      
                      # Add a submit button, so that the data will not display
                      # unless clicked. This way we can avoid reptitive req()
                      # functions in the server end.
                      
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
    
    # Create a tab in the navbar for the Outwit portion
    
    tabPanel("Outwit: Idols",
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("About", textOutput("blurb")),
                    tabPanel("Data Explorer", dataTableOutput("data_explorer"))))),
    
    # Create a tab in the navbar for the outplay portion
    
    tabPanel("Outplay: Immunity and Challenges",
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("About", textOutput("blurb")),
                    tabPanel("Data Explorer", dataTableOutput("data_explorer"))))),
    
    # Create a tab in the navbar for the outlast portion
    
    tabPanel("Outlast: High Level Trends",
             mainPanel(
               tabsetPanel(type = "tabs",
                           tabPanel("About", textOutput("blurb")),
                           tabPanel("Data Explorer", dataTableOutput("data_explorer")))),
      sidebarLayout(
        sidebarPanel(
          
          # Create radio buttons in the side bar that allow the user to select
          # for female or male contestants
          
          radioButtons(inputId = "gender",
                       choices = c("Female", "Male"),
                       label = "Gender"),
          br(),
          
          # Create a submit button
          
          submitButton(text = "Display")
        ),
        
        # Output a plot
        
        mainPanel(plotOutput("outlastPlot"))
      )
    )
  )
)

# Define server logic required to draw a histogram

server <- function(input, output) {
  
  # Create the blurb that introduces what Survivor is, why it's so interesting
  # to analyze, and the two datasets that I utilized, as well as a brief intro
  # to the gov 1005 course.
  
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