
# Load the appropriate libraries

library(shiny)
library(tidyverse)
library(gganimate)
library(ggplot2)
library(janitor)
library(rebus)
library(DT)
library(shinythemes)
library(shinyWidgets)
library(gt)
library(ggthemes)
library(fivethirtyeight)
library(ggmap)

# Read in the rds file for the data on survivor contestants that includes the
# geocoded location data and assign to survivor_data

survivor_data <- read_rds("www/geo_join.rds")

# Define UI for application

ui <- fluidPage(
  
  # Change theme using shinytheme library to 'united'
  
  theme = shinytheme("united"),
  
  # Application title
  
  h1(" Survivor: Outwit, Outplay, Outlast"),
  
  # Add a caption detailing what the project is for
  
  h4("by Evelyn Cai, for Gov 1005, a data course at Harvard University"),
  
  # Create the navigation bar
  
  navbarPage("Survivor!",
             
      # Create the dataset explorer tab. This wil render a data table, so add a
      # sidebar layout with various different functionalities to filter the
      # dataset.
             
      tabPanel("Explore the Dataset",
          sidebarLayout(
              sidebarPanel(
      
                  # Create a selectInput for the user to select a specific
                  # season(s), or all the seasons. Multiple = TRUE so users can
                  # select more than one season.
                          
                  selectInput(inputId = "select_season",
                              label = "Choose a season to display:",
                              choices = c("All", levels(survivor_data$season.x)),
                              multiple = TRUE),
                          
                  # Create a two-sided range slider to adjust for age, stepping
                  # by 1. Set the default to the whole range, using the min()
                  # and max() function
                          
                  sliderInput(inputId = "age_slider",
                              label = "Age",
                              min = min(survivor_data$age),
                              max = max(survivor_data$age),
                              step = 1,
                              value = c(min(survivor_data$age), max(survivor_data$age))),
                  br(),
                      
                  # Create a two-sided range slider for numbers of days lasted,
                  # stepping by 1 and setting the default value to the whole
                  # range as well.
                  
                  sliderInput(inputId = "days_slider",
                              label = "Days Lasted",
                              min = min(survivor_data$daysLasted),
                              max = max(survivor_data$daysLasted),
                              step = 1,
                              value = c(min(survivor_data$daysLasted), max(survivor_data$daysLasted))),
                  br(),
                  
                  # Create a two-ranged slider for number of idols played, with
                  # the minimum at 0 and the max at the max() of idols_played,
                  # which is 3. Step by 1 because the range is quite small, and
                  # set the default value to the whole range.
                  
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
              
          # Define the main panel UI    
              
          mainPanel(
              tabsetPanel(type = "tabs",
                          
                  # Add a panel that explains what Survivor is and where the
                  # data comes from
                          
                  tabPanel("About",
                           h2("The Hit CBS Reality TV Show"),
                           h4("Survivor is a hit reality TV show produced by
                           CBS. Since its first episode aired in May 2000,
                           Survivor has enjoyed consistently high rates of
                           viewership. It is touted as an exciting,
                           adventurous, clever, and family-friendly program.
                           Indeed, the 38 seasons of Survivor since have seen
                           many fans of the show go on to become participants
                           themselves. It's not just the allure of the $1
                           million prize, but also the coveted title of Sole
                           Survivor. Survivor follows a system in which around
                           20 participants are divided into two or more
                           tribes. They compete against the other tribe(s) in
                           physical and mental challenges to avoid tribal
                           council, which occurs every other day. At tribal
                           council, one person is voted off the island, so
                           strategizing and forming alliances are crucial to
                           gameplay. Once around half of the participants are
                           left, the tribes merge into one, a process dubbed
                           as 'The Merge'."),
                           br(),
                           h2("The Merge: Voting and Strategy"),
                          h4("The Merge typically signifies truly individual
                          gameplay; sometimes players vote with their voting
                          bloc to ensure that they are able to stick around,
                          but players must also be conscious of building a
                          resume. The Sole Survivor, or ultimate winner, is
                          voted for by a jury of around 8-10 people who were
                          voted off post-merge. Contestants are expected to
                          balance a fine line by making flashy and bold moves
                          to impress the jury, while also making sure not to
                          offend the jury or create personal tension."),
                          br(),
                          h2("Outwit, Outplay, Outlast"),
                          h3("Outwit"),
                          h4("The multifaceted nature of this complex game can
                          be captured in the three areas that a successful
                          contestant excels in: Outwit, Outplay, and Outlast.
                          Outwitting involves using advantages and twists to
                          one's advantage, as host Jeff Probst boasts of the
                          unpredictability of the game. A consistent trademark
                          advantage on Survivor is the hidden immunity idol,
                          which is typically located somewhere on the island
                          for a crafty conestant to find. Once in their
                          possession, it can be given away but not stolen, and
                          nullifies all votes to kick them off the island when
                          played at one tribal council. Many players have
                          potentially lost $1 million or kept themselves in
                          the game depending on their usage of their immunity
                          idol."),
                          h3("Outplay"),
                          h4("The second area is Outplay, which involves the
                          large physical portion of the game. Survivor sees
                          contestants lose dozens of pounds due to
                          malnourishment, and frequent physical challenges are
                          staged to determine which tribe gets sent to tribal
                          council. Outplaying also entails the proper usages
                          of legacies and advantages gifted to players, as
                          well as maintaining a healthy social game and being
                          able to 'rally the troops' or so to speak."),
                          h3("Outlast"),
                          h4("Lastly, of course, you must outlast the other
                          players. With the exception of one season, the last
                          day is the 39th day. Outlasting entails all of the
                          above: ensuring you get enough food to eat to stay
                          physically well, ensuring your social relationships
                          maintain your social spot in the game, and doing
                          well in challenges to accomplish both tasks."),
                          br(),
                          h2("Guiding Questions"),
                          br(),
                          h4("I am curious about high-level trends that
                          distinguish those who win from those who do not.
                          Does age play a factor? Gender? What are the
                          occupations of those who win? Where do Survivor
                          contestants call home - is it a uniform spread
                          across the map, as CBS may be looking for diversity,
                          or are there clusters around metropolitan areas? How
                          do people play immunity idols, and does their usage
                          have an effect on how many days someone lasts? Do
                          winners excel more in the Outwit, Outplay, or
                          Outlast area? What distinguishes the winner from the
                          other two runner-ups who survive all 39 days as
                          well? Are less votes against someone a good
                          indicator that they ran below the radar and
                          therefore correlated with a higher finish place, or
                          are less votes a sign that someone wasn't really a
                          contender for the game and corresponds to a lower
                          finish place?"),
                          h1("Data source: @davekwiatkowski (Github)")),
                  br(),
                  h2("Data Explorer: Survivor Contestants' Information"),
                  tabPanel("Data Explorer", dataTableOutput("data_explorer"))
                  )
              )
          )
          ),
    
      # Create a tab in the navbar for the Outwit portion
    
      tabPanel("Outwit: Idol Play",
          mainPanel(width = 8,
              tabsetPanel(type = "tabs",
                  tabPanel("Outwit", tableOutput("idolfindingPlot"),
                                     br(),
                                     gt_output("idolfinding2Plot"))
                  )
              )
          ),
    
      # Create a tab in the navbar for the outplay portion
    
      tabPanel("Outplay: Immunity & Challenges",
          mainPanel(width = 8,
              tabsetPanel(type = "tabs",
                  tabPanel("Outplay",
                           plotOutput("winsComparisonPlot"),
                           h4("As shown above, there is a negative correlation
                           between the finish number and the mean number of
                           wins, indicating that those who place higher
                           generally have more wins. Of course, we cannot
                           determine a causal relationship, as those who place
                           higher have more opportunities to win since they
                           stay in the game longer. Therefore, we can take a
                           look at the Final Three contestants who lasted
                           until day 39 for every season, as pictured below."),
                           br(),
                           plotOutput("finalThreeComparisonPlot"),
                           h4("Sole Survivors tend to win more in tribal and
                           individual challenges, increasing their total wins,
                           compared to the first- and second- runners up.
                           While we cannot determine whether their increased
                           number of wins influenced their ultimate win, or
                           whether a 'Sole Survivor'-type contestant just
                           naturally tends to win more, there is a clear
                           indicator here that Sole Survivors tend to win."))
                  )
              )
          ),
    
      # Create a tab in the navbar for the outlast portion
    
      tabPanel("Outlast: Sole Survivor & Trends",
          # Output a plot
        
          mainPanel(
              tabsetPanel(type = "tabs",
                  tabPanel("Winner Analysis"),
                  tabPanel("High-Level Trends",
                           leafletOutput("outlastPlot")))
              )
          )
      )
  )

# Define server logic required to draw a histogram

server <- function(input, output) {
  
    # Create the blurb that introduces what Survivor is, why it's so interesting
    # to analyze, and the two datasets that I utilized, as well as a brief intro
    # to the gov 1005 course.
    
    
  
    # Define the data_explorer output as a DataTable
    
    output$data_explorer <- DT::renderDataTable({
    
        # Require that a season is selected or else an error message will pop up
        
        req(input$select_season)
    
        # Define data as survivor_data
        
        data <- survivor_data
        
        # Subset the data based on the slider inputs for the age_slider,
        # days_slider, and idols_slider, subsetting to the first and second values,
        # which represent the min and max values
    
        data <- data %>%
                       filter(
                       age >= input$age_slider[1] &
                       age <= input$age_slider[2] &
                       daysLasted >= input$days_slider[1] &
                       daysLasted <= input$days_slider[2] &
                       idols_played >= input$idols_slider[1] &
                       idols_played <= input$idols_slider[2])
    
        # Use an if loop to subset the data if All if not selected
        # %in%
        
        if (input$select_season != "All") {
          data <- filter(data, season.x %in% input$select_season)
        }
    
        # Use an if loop to subset the data if Sole Survivor is selected so that
        # only those who finished first are selected
        
        if (input$winner == "Sole Survivor") {
          data <- filter(data, finish == 1)
        }
    
        # Use an if loop to subset the data if the length of gender_check is only
        # one (indicating that only one gender is selected) to the gender checked
        
        if (length(input$gender_check) == 1) {
          data <- filter(data, gender == input$gender_check)
        }
    
        # Call data again to actually display the data
        
        data
        
  })
    
    output$winsComparisonPlot <- renderPlot({
      
      data <- survivor_data
      
      p <- data %>%
        group_by(finish) %>%
        summarize(wins_total = mean(totalWins),
                  wins_tribal = mean(tribalChallengeWins),
                  wins_ind = mean(individualChallengeWins)) %>%
        ungroup() %>%
        mutate(wins_tribal = replace_na(wins_tribal, 0),
               wins_ind = replace_na(wins_ind, 0)) %>%
        gather(key = "win_type", value = "mean", wins_total, wins_tribal, wins_ind) %>%
        arrange(finish) %>%
        group_by(finish, win_type) %>%
        ggplot(aes(x = finish, y = mean, color = win_type)) +
        geom_line(size = 1.1, show.legend = FALSE) +
        scale_color_brewer(type = 'seq', palette = 'Dark2') +
        annotate("text", x = 3.5, y = 1, size = 5, color = "#289E80", label = "Individual") +
        annotate("text", x = 4, y = 8.5, size = 5, color = "#E35934", label = "Total") +
        annotate("text", x = 6, y = 5, size = 5, color = "#5639A6", label = "Tribal") +
        scale_x_continuous(breaks = seq(1, 20, by = 1)) +
        scale_y_continuous(breaks = seq(0, 10, by = 2)) +
        theme_fivethirtyeight()
      
      p
    })
    
    output$finalThreeComparisonPlot <- renderPlot({
      data <- survivor_data
      p <- data %>%
        select(season_number, totalWins, tribalChallengeWins, individualChallengeWins, finish, daysLasted) %>%
        group_by(season_number) %>%
        filter(daysLasted == max(daysLasted)) %>%
        ungroup() %>%
        group_by(finish) %>%
        summarize(wins_total = mean(totalWins),
                  wins_tribal = mean(tribalChallengeWins),
                  wins_ind = mean(individualChallengeWins)) %>%
        ungroup() %>%
        mutate(wins_tribal = replace_na(wins_tribal, 0),
               wins_ind = replace_na(wins_ind, 0)) %>%
        gather(key = "win_type", value = "mean", wins_total, wins_tribal, wins_ind) %>%
        arrange(finish) %>%
        ggplot(aes(x = finish, y = mean, fill = finish)) +
        geom_col() +
        scale_color_brewer(type = "div", palette = "Dark2") +
        scale_y_continuous(breaks = seq(0, 10, by = 2)) +
        facet_wrap(~win_type) +
        theme_fivethirtyeight()
      
      p
      
    })
    
    
    
    
    
    output$idolfindingPlot <- renderTable({
      
      data <- survivor_data
      
      g <- data %>%
           filter(finish == 1) %>%
           select(idols_found) %>%
           group_by(idols_found) %>%
           count() %>%
           ungroup() %>%
           mutate(all = sum(n),
                  proportion = n / all) %>%
           select(idols_found, proportion) %>%
           gt() %>%
           cols_label(idols_found = "Idols Found",
                     proportion = "Proportion") %>%
           tab_header(title = "Immunity Idol Hunting: Sole Survivors",
                      subtitle = "Over 40% found at least one") %>%
          cols_align("center")
          
      g
    })
    
    
    output$idolfinding2Plot <- render_gt({
      data <- survivor_data
      
      g <- data %>%
        select(idols_found) %>%
        group_by(idols_found) %>%
        count() %>%
        ungroup() %>%
        mutate(all = sum(n),
               proportion = n / all) %>%
        select(idols_found, proportion) %>%
        gt() %>%
        cols_label(idols_found = "Idols Found",
                   proportion = "Proportion") %>%
        tab_header(title = "Immunity Idol Hunting: All Contestants",
                   subtitle = "Only about 10% found one or more idols") %>%
        cols_align("center")
      
      g
    })
    
  
    output$outlastPlot <- renderLeaflet({
    
      data <- survivor_data
      
      m <- data %>%
        leaflet() %>%
        addProviderTiles("Stamen.Terrain") %>%
        setView(lng = -92.5, lat = 40, zoom = 3.25) %>%
        addCircleMarkers(radius = 5, popup = ~contestant)
      
      m
    
    })
}


# Run the application 
shinyApp(ui = ui, server = server)