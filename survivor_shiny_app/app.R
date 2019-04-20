# Load the shiny library

library(shiny)

# Define UI for application that draws a histogram based on the gender selected

ui <- fluidPage(
  
  # Application title
  
  titlePanel("Individual Challenge Wins By Post-Merge Constestants In 37 Seasons of Survivor"),
  
  # Sidebar with a radio button input for selected gender
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "gender",
                   choices = c("Female", "Male"),
                   label = "Gender")
    ),
    
    # Show a plot of the generated distribution
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic required to draw a histogram

server <- function(input, output) {
  
  # Render a plot for the output
  
  output$plot <- renderPlot ({
    
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
}


# Run the application 
shinyApp(ui = ui, server = server)

