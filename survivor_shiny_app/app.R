
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
library(leaflet)
library(gganimate)

# Read in the rds file for the data on survivor contestants that includes the
# geocoded location data and assign to survivor_data

survivor_data <- read_rds("www/geo_join.rds")

# Define UI for application

ui <- fluidPage(
  
  # Change theme using shinytheme library to 'cosmo'
  
  theme = shinytheme("cosmo"),
  
  # Application title
  
  h1(" Survivor: What Does it Take to Win?"),
  
  # Add a caption detailing what the project is for
  
  h3("by Evelyn Cai for GOV1005"),
  
  # Create the navigation bar, making the title blank
  
  navbarPage("",
             
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
              
          # Define the main panel UI that will output a datatable. Use DT's data
          # tables, which are interactive
              
          mainPanel(
            h2("Data Explorer: Survivor Contestants' Information"),
            DT::dataTableOutput("data_explorer"))
          )
      ),
    
      # Create a tab in the navbar for the Outwit portion
    
      tabPanel("Outwit",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            img(src = "https://vignette.wikia.nocookie.net/survivor/images/4/41/Thailand_Immunity_Idol.jpg/revision/latest/zoom-crop/width/240/height/240?cb=20120427222241", width = 225, height = 200),
            h5("Immunity Idols are a trademark of Survivor. When played correctly,
               they can guarantee immunity from being voted off for one tribal council."),
            br(),
            h5("Due to their power, they are well-hidden and searching for hidden immunity
               idols has become a staple of good players.")),
          mainPanel(width = 6,
            h2("Outwit: Idol Play"),
            gt_output("idolfindingPlot"),
            br(),
            gt_output("idolfinding2Plot"))
        )
      ),
    
      # Create a tab in the navbar for the outplay portion
    
      tabPanel("Outplay",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            img(src = "https://imagesvc.timeincapp.com/v3/fan/image?url=https://survivingtribal.com/wp-content/blogs.dir/399/files/2017/11/Survivor-season-35-episode-9-immunity-challenge.jpg&c=sc&w=1200&h=675", width = 225, height = 200),
            h5("Tribal challenges are the mostly physical, sometimes mental challenges before the Merge
               that determine which tribe gets sent to tribal council to vote off a member. They can
               also award winners with food or comfort items."),
            br(),
            h5("Later, individual immunity challenges become the only way to guarantee safety in the game,
               and both help a player by providing security, but also hinder their game by publicizing
               their physical strength.")),
          mainPanel(width = 6,
            h2("Outplay: Immunity & Challenges"),
                           
                           # Output the plot comparing three types of wins to
                           # the finish place of a contestant
                           
                           plotOutput("winsComparisonPlot"),
                           br(),
                           
                           # Describe the negative correlation
                           
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
                           
                           # Output the plot comparing the finish place of the
                           # three contestants who made it to the end to their
                           # number of wins
                           
                           plotOutput("finalThreeComparisonPlot"),
                           br(),
                           
                           # Describe the trend
                           
                           h4("Sole Survivors tend to win more in tribal and
                           individual challenges, increasing their total wins,
                           compared to the first- and second- runners up.
                           While we cannot determine whether their increased
                           number of wins influenced their ultimate win, or
                           whether a 'Sole Survivor'-type contestant just
                           naturally tends to win more, there is a clear
                           indicator here that Sole Survivors tend to win."),
                           br())
              )
          ),
    
      # Create a tab in the navbar for the outlast portion
    
      tabPanel("Outlast",
        sidebarLayout(
          sidebarPanel(
          img(src = "https://cdn.freebiesupply.com/logos/large/2x/survivor-logo-png-transparent.png", width = 225, height = 250),
          width = 3,
          h5("Getting voted off is an automatic disqualifier for the 1 million dollar prize,
             so outlasting is one of the most important components to the game."),
          br(),
          h5("Typically, three contestants last until the last day - they are dubbed the Final
             Three and are voted upon to determine the ultimate Sole Survivor.")),
          mainPanel(
            width = 6,
            h2("Outlast: What Makes a Survivor Contestant and Winner?"),
              tabsetPanel(type = "tabs",
                  
                  # Create one tab for winner analysis                
                          
                  tabPanel("Trends",
                           h3("Hometowns of Survivor Contestants"),
                           h5("Sole Survivors are in red"),
                           leafletOutput("outlastPlot"),
                           br(),
                  
                           # Describe the trends seen in the map
                  
                           h4("As seen above, there is already a lack of representation
                              of the Midwest. This is especially apparent for Sole Survivors, who
                              hail heavily from the Northeast, Texas, and California. There is also
                              a significant clumping effect around urban areas, and the plot is rather 
                              dense around the Northeast compared to all other areas in the United States,
                              besides Los Angeles."),
                          br(),
                          imageOutput("animatedorderPlot")
                          
                  ),
                  
                  # Create another tab for high-level trends and output a leaflet plot
                  
                  tabPanel("Winner Analysis"))
          )
              )
          ),
      
      # Add an panel that explains what Survivor is and where the data comes
      # from
        
        tabPanel("About",
                 
                 # Add a sidebar that talks about the project and the data
                 # sources
                 
                 sidebarLayout(
                   sidebarPanel(
                     
                     # Make the width half of the remaining width from mainPanel
                     # so the mainPanel is centered
                     
                     width = 3,
                     
                     # Add hyperlinks to the data sources and Github repo within the headers
                     
                     h5("This project was created for Gov 1005: Data, a course taught by David Kane at Harvard University."),
                     br(),
                     h5("Major thanks are owed to Dave Kwiatkowski for compiling ", a("the contestant data.", href="https://github.com/davekwiatkowski/survivor-data"),
                        "The", a("immunity idol data", href = "https://docs.google.com/spreadsheets/d/1jTtpv3pdivUWo3oF3nGBWcDnG69cTw63QHfXgsfXgTI/edit#gid=0"), "was obtained from Jeff Pitman as of March 2019."),
                     h5("The ggmap package was also instrumental in geocoding the data: D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R Journal, 5(1), 144-161, URL http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf"),
                     br(),
                     h5("A link to the Github repository can be found", a("here.", href = "https://github.com/caievelyn/outwit_outplay_outlast"))
                   ),
                   
                 # Define the mainPanel, which will contain information about
                 # Survivor and gameplay for those who are unfamiliar with the
                 # TV show
                   
                 mainPanel(
                   
                 # Make the width 6 so that it is centered with a side panel of
                 # width 3
                   
                 width = 6,
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
                    finish place?"))
                 )
                 )
      )
)

# Define server logic required to output the various plots

server <- function(input, output) {
  
    # Define the data_explorer output as a DataTable
    
    output$data_explorer <- DT::renderDataTable ({
    
        # Require that a season is selected or else an error message will pop up
        
        req(input$select_season)
    
        # Define data as survivor_data
        
        data <- survivor_data
        
        # Subset the data based on the slider inputs for the age_slider,
        # days_slider, and idols_slider, subsetting to the first and second
        # values, which represent the min and max values of each variable. Use
        # the greater than or equal to and less than or equal to signs to be
        # inclusive of each value selected at the endpoints
    
        data <- data %>%
                       filter(
                       age >= input$age_slider[1] &
                       age <= input$age_slider[2] &
                       daysLasted >= input$days_slider[1] &
                       daysLasted <= input$days_slider[2] &
                       idols_played >= input$idols_slider[1] &
                       idols_played <= input$idols_slider[2])
    
        # Use an if loop to filter the data to the seasons selected if 'All' is
        # not selected
        
        if (input$select_season != "All") {
          data <- filter(data, season.x %in% input$select_season)
        }
    
        # Use an if loop to filter the data if Sole Survivor is selected so that
        # only those who finished first are selected
        
        if (input$winner == "Sole Survivor") {
          data <- filter(data, finish == 1)
        }
    
        # Use an if loop to filter the data if the length of gender_check is
        # only one (indicating that only one gender is selected). Then filter
        # for the gender checked
        
        if (length(input$gender_check) == 1) {
          data <- filter(data, gender == input$gender_check)
        }
    
        # Call data again to actually display the transformed data; if the data
        # was not altered, then the original data will be displayed
        
        data
        
  })
    
    # Render a plot that compares the number of individual, tribal, and total
    # wins to the finishing place of a contestant
    
    output$winsComparisonPlot <- renderPlot({
      
      # Define data
      
      data <- survivor_data
      
      # Create an object p that will store the plot
      
      p <- data %>%
        
        # Group by the finish place so our calculations for the mean wins apply
        # to each finishing group. Use summarize to easily calculate the mean
        # and ungroup afterwards to prevent messing with the gather() function
        
        group_by(finish) %>%
        summarize(wins_total = mean(totalWins),
                  wins_tribal = mean(tribalChallengeWins),
                  wins_ind = mean(individualChallengeWins)) %>%
        ungroup() %>%
        
        # Replace NA values with 0 so that we can plot those points later. Call
        # this after the calculation of the means, as recoding NA values to 0
        # prior would falsely indicate that those who did not make it far enough
        # in the show to participate in individual immunity challenges actually
        # did get to participate, but did not win.
        
        mutate(wins_tribal = replace_na(wins_tribal, 0),
               wins_ind = replace_na(wins_ind, 0)) %>%
        
        # Use the gather function to group key values into one column for ease
        # of plotting, with their current values placed into a new column titled
        # "value".
        
        gather(key = "win_type", value = "mean", wins_total, wins_tribal, wins_ind) %>%
        
        # Arrange by the finish place from first to twentieth
        
        arrange(finish) %>%
        
        # Group by the finish and win_type
        
        group_by(finish, win_type) %>%
        
        # Call a ggplot and pass the finish variable to the x-axis, and the mean
        # values to the y-axis. Specify win_type as color so that three
        # differently colored lines will be generated.
        
        ggplot(aes(x = finish, y = mean, color = win_type)) +
        
        # Add a line, making it slightly thicker for ease of reading. Turn off
        # the legend, since we will be manually annotating the graph.
        
        geom_line(size = 1.1, show.legend = FALSE) +
        
        # Use the R Color Brewer palettes for color customization
        
        scale_color_brewer(type = 'seq', palette = 'Dark2') +
        
        # Use the annotate() function to manually add labels to each line, using
        # the hex color code to match the line color.
        
        annotate("text", x = 3.5, y = 1, size = 5, color = "#289E80", label = "Individual") +
        annotate("text", x = 4, y = 8.5, size = 5, color = "#E35934", label = "Total") +
        annotate("text", x = 6, y = 5, size = 5, color = "#5639A6", label = "Tribal") +
        
        # Change the scaling and labelling of the x- and y- axes for ease of
        # reading
        
        scale_x_continuous(breaks = seq(1, 20, by = 1)) +
        scale_y_continuous(breaks = seq(0, 10, by = 2)) +
        
        # Add title and subtitle
        
        labs(title = "Average Wins per Place",
             subtitle = "The higher placing contestants tend to win more on average") +
        
        # Change the theme to fivethirtyeight's theme, which is crisper and more
        # minimal
        
        theme_fivethirtyeight() +
        
        # Add back the axis titles and change them for the x- and y-axes
        
        theme(axis.title = element_text()) +
        xlab("Finish Place") +
        ylab("Average Number of Wins")
      
      # Call the plot so that it will show
      
      p
      
    })
    
    # Render a faceted bar plot displaying the difference in average wins for
    # those who lasted the same number of days based on finish place
    
    output$finalThreeComparisonPlot <- renderPlot({
      
      # Define data
      
      data <- survivor_data
      
      # Store the plot in an object to be called later
      
      p <- data %>%
        select(season_number, totalWins, tribalChallengeWins, individualChallengeWins, finish, daysLasted) %>%
        
        # Group by season number and filter for those who lasted the maximum
        # number of days for their season (indicating that they were in the
        # Final Two or Final Three). Ungroup afterwards for ease of using the
        # gather() function later.
        
        group_by(season_number) %>%
        filter(daysLasted == max(daysLasted)) %>%
        ungroup() %>%
        
        # Group by finish place and perform the mean function to find the mean
        # for each type of win: tribal, individual, and total. Ungroup
        # afterwards.
        
        group_by(finish) %>%
        summarize(wins_total = mean(totalWins),
                  wins_tribal = mean(tribalChallengeWins),
                  wins_ind = mean(individualChallengeWins)) %>%
        ungroup() %>%
        
        # Replace NA values with 0 so that we can plot those points later. Call
        # this after the calculation of the means, as recoding NA values to 0
        # prior would falsely indicate that those who did not make it far enough
        # in the show to participate in individual immunity challenges actually
        # did get to participate, but did not win.
        
        mutate(wins_tribal = replace_na(wins_tribal, 0),
               wins_ind = replace_na(wins_ind, 0)) %>%
        
        # Use the gather function to group key values into one column for ease
        # of plotting, with their current values placed into a new column titled
        # "value".
        
        gather(key = "win_type", value = "mean", wins_total, wins_tribal, wins_ind) %>%
        
        # Relevel the factors before recoding since the variable names are clean and
        # easy to work with still. Change them so that the faceted plot later will
        # display from shortest bars to highest bars.
        
        mutate(win_type = fct_relevel(win_type, "wins_ind", "wins_tribal", "wins_total"),
               
               # Recode the factor levels for ease of display later in the ggplot
               
               win_type = fct_recode(win_type,
                                     "Total Wins" = "wins_total",
                                     "Tribal Wins" = "wins_tribal",
                                     "Individual Wins" =  "wins_ind")) %>%
        
        # Call a ggplot, passing finish place to the x- variable and mean to the
        # y- variable. Fill the bar with a color that corresponds to the finish
        # place, so that we can get rid of the legend.
        
        ggplot(aes(x = finish, y = mean, fill = finish)) +
        
        # Call a bar graph, getting rid of the legend
        
        geom_col(show.legend = FALSE) +
        
        # Use R Color Brewer palettes for color customization
        
        scale_color_brewer(type = "div", palette = "Dark2") +
        
        # Change the tick mark values and labels for the y-axis
        
        scale_y_continuous(breaks = seq(0, 10, by = 2)) +
        
        # Facet by the type of win (individual, total, and tribal)
        
        facet_wrap(~win_type) +
        
        # Change the labels of the x- and y- axes, add a title and subtitle, and add
        # an appropriate title and subtitle
        
        labs(title = "Average Wins for the Final Three",
             subtitle = "With the number of days lasted held constant, it appears that Sole\nSurvivors won more on average than their runner-ups") +
        
        # Alter theme to be more minimal via 538's theme (it has a great font and panel
        # grid system!)
        
        theme_fivethirtyeight() +
        
        # Add back the axis titles
        
        theme(axis.title = element_text()) +
        
        xlab("Finishing Place") +
        ylab("Average Number of Wins")
      
      # Call the plot so that it shows on the app
      
      p
      
    })
    
    # Create a GT graphic displaying the percentage of Sole Survivors who found
    # a certain number of idols
    
    output$idolfindingPlot <- render_gt({
      
      data <- survivor_data
      
      # Store graphic in an object called g
      
      g <- data %>%
        
        # Filter only for Sole Survivors and select idols_found
        
        filter(finish == 1) %>%
        select(idols_found) %>%
        
        # Count the number contestants who found a certain number of idols
        
        group_by(idols_found) %>%
        count() %>%
        ungroup() %>%
        
        # Find the total number of contestants by adding up n
        
        mutate(all = sum(n),
               
               # Calculate the proportion by dividing the number of contestants
               # in one group by the total number of contestants
               
               proportion = n / all) %>%
        select(idols_found, proportion) %>%
        
        # Call gt
        
        gt() %>%
        
        # Ensure that the proportions are displayed as percentages
        
        fmt_percent(columns = vars(proportion)) %>%
        
        # Add appropriate labels and headers, and center the columns
        
        cols_label(idols_found = "Idols Found",
                   proportion = "Percentage") %>%
        tab_header(title = "Immunity Idol Hunting: Sole Survivors",
                   subtitle = "Over 40% found one or more idols") %>%
        cols_align("center")
      
      # Call the object name so it will show up
      
      g
      
    })
      
      # Create a GT graphic displaying the percentage of Sole Survivors who found
      # a certain number of idols
      
      output$idolfinding2Plot <- render_gt({
        
        data <- survivor_data
        
        # Store graphic in an object called g
        
        g <- data %>%
          
          # Filter out Sole Survivors and select idols_found
          
          filter(finish != 1) %>%
          select(idols_found) %>%
          
          # Count the number contestants who found a certain number of idols
          
          group_by(idols_found) %>%
          count() %>%
          ungroup() %>%
          
          # Find the total number of contestants by adding up n
          
          mutate(all = sum(n),
                 
                 # Calculate the proportion by dividing the number of contestants
                 # in one group by the total number of contestants
                 
                 proportion = n / all) %>%
          select(idols_found, proportion) %>%
          
          # Call gt
          
          gt() %>%
          
          # Ensure that the proportions are displayed as percentages
          
          fmt_percent(columns = vars(proportion)) %>%
          
          # Add appropriate labels and align the columns to center
          
          cols_label(idols_found = "Idols Found",
                     proportion = "Percentage") %>%
          tab_header(title = "Immunity Idol Hunting: non-Winners",
                     subtitle = "Only about 10% found one or more idols") %>%
          cols_align("center")
      
      # Call the object to display it
        
      g
      
    })
    
    # Create a leaflet plot showing the home cities of contestants  
  
    output$outlastPlot <- renderLeaflet({
    
      # Define data as all the points for contestants who were not winners
      
      data <- survivor_data %>%
        filter(finish != 1)
      
      # Define data2 as the information for Sole Survivors
      
      data2 <- survivor_data %>%
        filter(finish == 1)
      
      # Store object to call later
      
      m <- data %>%
        
        # Call leaflet
        
        leaflet() %>%
        
        # Add pre-styled tiles; this one clearly delineates the state lines
        # while maintaining visual appeal
        
        addProviderTiles("Stamen.Terrain") %>%
        
        # Set the automatic view so that the United States are fully in view
        
        setView(lng = -95, lat = 37.5, zoom = 3.5) %>%
        
        # Add circle markers that are blue for non-winners
        
        addCircleMarkers(radius = 6.5,
                         
                         # Remove the stroke, which adds to the radius
                         
                         stroke = FALSE,
                         
                         # Specify the fill color as blue and opacity as a
                         # number between 0 and 1. I chose 0.45 as it allows you
                         # to see the higher concentration of contestants
                         # easily.
                         
                         fill = TRUE,
                         fillColor = "blue",
                         fillOpacity = 0.45,
                         
                         # Create popups that contain the contestant's name,
                         # season number, and finish place.
                         
                         popup = paste(data$contestant, "<br>",
                                       "Season ", data$season_number, ":",
                                       data$season.x, "<br>",
                                       "Finish: ", data$finish))
      
      # Create a second leaflet object based off the first one, this time with a
      # different layer of colored circle markers for Sole Survivors
      
      m2 <- m %>%
        
        # Add circle markers that are red for winners
        
        addCircleMarkers(data = data2,
                         radius = 6.5,
                         
                         # Remove the stroke, which adds to the radius
                         
                         stroke = FALSE,
                         
                         # Specify the fill color as red and increase the
                         # opacity so it's easier to spot on a map of cool toned
                         # colors.
                         
                         fill = TRUE,
                         fillColor = "red",
                         fillOpacity = 0.8,
                         
                         # Create popups that contain the contestant's name,
                         # season number, and the fact that they were Sole
                         # Survivor.
                         
                         popup = paste(data2$contestant, "<br>",
                                       "Season ", data2$season_number, ":",
                                       data2$season.x, "<br>",
                                       "Sole Survivor"))
      
      # Call leaflet object for display
      
      m2
    
    })
    
    output$animatedorderPlot <- renderImage ({
      
      # Return a list showing the file name
      
      list(src = "www/outfile.gif",
           contentType = 'image/gif',
           width = 675,
           height = 600
      )}, deleteFile = FALSE)
    
}

# Run the application 

shinyApp(ui = ui, server = server)