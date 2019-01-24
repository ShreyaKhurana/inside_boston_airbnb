neighborhoods = c('East Boston', 'Dorchester', 'Beacon Hill', 'Fenway', 'Downtown',
                  'South End', 'North End', 'Jamaica Plain', 'Chinatown', 'Allston',
                  'Back Bay', 'West End', 'Charlestown', 'South Boston', 'Roxbury',
                  'Brighton', 'West Roxbury', 'Roslindale', 'South Boston Waterfront',
                  'Mattapan', 'Bay Village', 'Mission Hill', 'Longwood Medical Area',
                  'Hyde Park', 'Leather District')

variable_names =  c("host_response_time","host_neighbourhood","zipcode" ,"host_is_superhost" ,"host_verifications",
                    "host_has_profile_pic","host_identity_verified", "neighbourhood_cleansed","is_location_exact",
                    "property_type" , "room_type" ,"bed_type", "calendar_updated","instant_bookable" ,
                    "cancellation_policy" ,"require_guest_profile_picture","require_guest_phone_verification")

ui <- dashboardPage(
  dashboardHeader(title = 'Inside Airbnb Boston', titleWidth = 250),
  dashboardSidebar(width = 250,
    sidebarMenu(
      id="tabs",
            # menuItem("EDA", 
      #          # menuSubItem('Correlation Plot', icon=NULL, tabName = 'corrplot'),
      #          # menuSubItem('Median Price', icon=NULL, tabName = 'median_price'),
      #          # menuSubItem('Log Transformation of Price', icon = NULL, tabName = 'logprice'),
      #          # menuSubItem('Variable Spread vs Log(Price)', icon = NULL, tabName = 'variable'),
      #          tabName = "EDA", icon = icon("database")),
      menuItem("Wecome to Boston!", tabName = "map", icon = icon("map-marker")),
      menuItem("Just Scatter It!", tabName = "scatter", icon = icon("area-chart")),
      menuItem("Let the exploring begin!",  icon = icon("glyphicon glyphicon-signal"), selected = TRUE,
           menuSubItem("Correlation Plot", tabName = "corrplot", icon = icon("angle-right")),
           menuSubItem("Median Price", tabName = "med", icon = icon("angle-right")),
           menuSubItem("Log Transformation of Price", tabName = "logprice", icon = icon("angle-right")),
           menuSubItem("Variable Spread vs Log(Price)", tabName = "variable", icon = icon("angle-right"))
      ),
      menuItem("Word Clouds", tabName = "cloud", icon = icon("comments-o")),
      menuItem("Sentiment Analysis", tabName = "sentiment", icon = icon("balance-scale"))
      
    )
  ),
 
  dashboardBody(
    tabItems(
      tabItem(tabName = "scatter",
              fluidRow(
                sidebarPanel(
                  selectInput('scat1', 'Select variable on x-axis', namec),
                  selectInput('scat2', 'Select variable on y-axis', namec)
                ),
                p(
                  paste("*Note: Colours depict Neighbourhood. ")
                ),
                mainPanel(
                  plotOutput('scat')
                
                )
              )
      ),
      tabItem(tabName = "corrplot",
              fluidRow(
                column(12,
                       align="center",
                       box(width = NULL, height=1,
                       plotOutput('plot3')
                  )
                )
              
              )
              ),
      tabItem(tabName = "med",
               fluidRow(
                 sidebarPanel(
                   selectInput('var2', 'Select variable to show relationship with Median Price', c('Bedrooms', 'Guests'))
                 ),
                 
                 mainPanel(
                   imageOutput('showpic')
                 )
               )
               ),
      tabItem(tabName = "variable",
              fluidRow(
                sidebarPanel(
                  selectInput('var', 'Select Variable to show its boxplot', variable_names)
                ),
                mainPanel(
                  plotOutput('plot4')
                )
              )
              
      ),
      tabItem(tabName = "logprice",
              fluidRow(
                splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot7"), plotOutput("plot8"))
              )
      ),
      tabItem(tabName = "sentiment",
              fluidRow(
                column(
                  width = 9,
                  box(width = NULL, height=1,
                    plotOutput('plot9')
                  )
                ),
                column(width = 3,
                  
                  box(width=NULL,
                      htmlOutput("text1")
                    # p(
                      # class = "text-muted",
                      # paste("We took a subset of 10 listings and tried to categorize the review",
                      #       "comments into various categories as shown on the left using ",
                      #       "sentiment analysis. Difference can be seen neighbourhood wise ",
                      #       "as well as sentiment wise. Neighbourhoods like Fenway, Jamaica ",
                      #       "Plain and Back Bay tend to have higher proportion of positive words- ",
                      #       "trust, joy and anticipation sentiments are highlighted in these ",
                      #       "neighbourhoods.\n\nIn the general bag of words however we see that",
                      #       "these sentiments tend to have a higher proportion."
                      # )
                    # )
                  )
                
                
                )
             
              )
              ),
    
      tabItem(tabName = "cloud",
              fluidRow(
                sidebarPanel(
                  selectInput('nbr', 'Neighbourhood', neighborhoods, multiple=TRUE, selected = 'East Boston'),
                  
                  sliderInput("max",
                              "Maximum Number of Words:",
                              min = 1,  max = 250,  value = 100),
                  sliderInput("rating",
                              "Reviewers Rating:",
                              min = 20,  max = 100,  value = c(30, 100)),
                  radioButtons('host', 'Is host a Superhost?', c( 'No', 'Yes')),
                  hr(),
                  actionButton("update", "Cloud it for me!")
                 ),
                mainPanel(
                  plotOutput('plot1'),
                  fluidRow(
                    column(10, align="center",
                           plotOutput('plot2')
                    )
                  )
                )
              )
      ),
      tabItem(tabName = "map",
              fluidPage(
                sidebarPanel(
                  selectInput('nbr2', 'Neighborhood', neighborhoods, multiple=TRUE, selected = c("East Boston", "Dorchester")),
                  actionButton("listing_update", "Find Listings"),
                  hr(),
                  sliderInput("minPrice",
                              "Minimum Price per Night (In $):",
                              min = 10,  max = 4000,  value = 10),
                  sliderInput("maxPrice",
                              "Maximum Price per Night (In $)",
                              min = 10,  max = 4000,  value = 500),
                  selectInput('property', 'Property Type', c('House', 'Apartment', 'Condominium', 'Villa', 'Bed & Breakfast','Townhouse', 'Entire Floor', 'Loft', 'Guesthouse', 'Dorm', 'Camper/RV', 'Boat', 'Other'), 
                              multiple=TRUE,
                              selected = 'Apartment'),
                  selectInput('room', 'Room Type', c('Entire home/apt',  'Private room', 'Shared room'), 
                              multiple=TRUE,
                              selected = 'Entire home/apt'),
                  selectInput('superhost', 'Superhost', c('No', 'Yes')),
                  selectInput('hostverified', 'Host identity Verified', c('No', 'Yes')),
                  selectInput('accomodates', 'Number of Guests', c(1:16)),
                  selectInput('bathroom', 'Number of bathrooms', seq(0,6,0.5)),
                  selectInput('instant', 'Instant Bookable', c('No', 'Yes')),
                  selectInput('cancel', 'Cancellation Policy', c('Moderate', 'Flexible', 'Strict', 'Super Strict'), 
                              multiple = TRUE,
                              selected = 'Flexible'),
                  selectInput('internet', 'Wi-fi', c('Yes', 'No')),
                  selectInput('checkin', '24-hour check in', c('No', 'Yes')),
                  selectInput('tv', 'TV', c('Yes', 'No')),
                  selectInput('ac', 'AC', c('Yes', 'No')),
                  selectInput('family', 'Family/Kid friendly', c('No', 'Yes')),
                  selectInput('pets', 'Pet-friendly', c('Yes', 'No')),
                  selectInput('parking', 'Free Parking', c('No', 'Yes')),
                  selectInput('breakfast', 'Breakfast', c('Yes', 'No'))
                ),
                mainPanel(
                  tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                  leafletOutput('mymap',width = "1000", height = "900")
                )
              )
      )
    )
  )
)
