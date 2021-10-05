##########################UI.R#################################################

ui <- navbarPage("Kickstarter Projects ", theme = shinytheme("darkly"),   #flatly
                 tabPanel("Analysis",
                          setBackgroundColor("GhostWhite"),
                          sidebarLayout(
                            sidebarPanel(width=3, 
                                         img(src = "kickstart1.jpeg", height = 80, width = 300),
                                         br(),
                                         br(),
                                         selectInput(inputId = "category",
                                                     label  = "Choose a category:",
                                                     choices = c(unique(KickStarter$main_category)),
                                                     selected = character(0)),
                                         br(),
                                         br(),
                                         selectInput(inputId="Visual_types",
                                                     label="Choose a Graph to explore:",
                                                     choices = c( "Existing Sub Categories"= "sub_categ",
                                                                  "Count of Succesful Sub Categories"= "no_succ_sub_categ",
                                                                  "Success Rate of Sub Categories"= "succ_sub_categ_rate",
                                                                  #"Project Name WordCloud "="title",
                                                                  "Goal Vs Pledge amount"= "amount"
                                                                  # "Best day to launch"="day"
                                                     ),
                                                     selected = character(0)),
                                         HTML(paste0(
                                           "<br><br><br><br><br><br><br><br><br>",
                                           "<br><br><br><br><br><br><br>"
                                         ),
                                         )
                            ),
                            mainPanel(
                              img(src = "Kickstarter_logo.png", height = 40, width = 900),
                              br(),
                              tags$hr(),
                              p(span("In this section you will select a category and graph ",
                                     "type to analyse the choosen main category and details regarding ",
                                     "its sub category and projects launced on the Kickstarter platform.",
                                     style = "color:black")),
                              tags$hr(),
                              plotlyOutput("obs2",  height = 450)
                              #plotOutput("Proj_title")
                            )
                          )
                 ) ,
                 tabPanel("Visualization",
                          sidebarLayout(
                            sidebarPanel( width=3,
                                          img(src = "kickstart1.jpeg", height = 80, width = 300),
                                          br(),
                                          br(),
                                          radioButtons(inputId="graph_id", 
                                                       label="Choose a Graph to analyse ",
                                                       choices= c("Popular Main Categories"="A", 
                                                                  "Successful Vs Total launch project" = "B",
                                                                  "Average No. of Backers"="C",
                                                                  "Successful launch project" = "D",
                                                                  "Campaingn days" = "E"
                                                       ),
                                                       selected = "A"), 
                                          HTML(paste0(
                                            "<br><br><br><br><br><br><br><br><br>",
                                            "<br><br><br><br><br><br><br>"
                                          )
                                          )
                            ),
                            
                            mainPanel(
                              img(src = "Kickstarter_logo.png", height = 40, width = 900),
                              br(),
                              tags$hr(),
                              p(span("In this section you can view different visualization ",
                                     "type to analyse the details regarding projects launched at Kickstarter platform",
                                     style = "color:black")),
                              tags$hr(),
                              tags$style(HTML("
                  .tabbable > .nav > li > a[data-value='Visualization'] {background-color: steelblue}
                  .tabbable > .nav > li > a[data-value='Categorization'] {background-color: steelblue}
                  ")),
                              tabsetPanel(
                                tabPanel("Visualization", 
                                         fluidRow( plotlyOutput("obs1", height = 450))),
                                tabPanel("Categorization", 
                                         fluidRow( sankeyNetworkOutput('sn', width = 700, height = 700)))
                                #verbatimTextOutput("value1"),
                                #uiOutput("obs1"),
                                #verbatimTextOutput("value2"),
                                #plotlyOutput("obs1")
                              )
                            )
                          )
                 ),
                 ############# tab - map and data
                 tabPanel("World Map & Data", 
                          fluidPage(
                            p(span("In this section you can view total number of projects launched in different",
                                   "countries around the world and data is also provided to search any information ",
                                   "you want regarding projects launched at Kickstarter platform",
                                   style = "color:black")),
                            tags$hr(),
                            leafletOutput("mymap"),
                            DT::dataTableOutput("tableID") 
                            )
                          ),
                 tabPanel("WordCloud",
                          setBackgroundColor("GhostWhite"),
                          sidebarLayout(
                            sidebarPanel(width=3, 
                                         img(src = "kickstart1.jpeg", height = 80, width = 300),
                                         br(),
                                         br(),
                                         selectInput(inputId = "category1",
                                                     label  = "Choose a category:",
                                                     choices = c(unique(KickStarter$main_category)),
                                                     selected = character(0)),
                                         br(),
                                         uiOutput("obs4"),
                                         sliderInput("max",
                                                     "Maximum Number of Words:",
                                                     min = 1,  max = 200,  value = 100),
                                         actionButton("update", "Project Title",color = "success"),
                                         HTML(paste0(
                                           "<br><br><br><br><br><br><br><br><br>",
                                           "<br><br><br><br><br><br><br>"
                                         )
                                         )
                                         
                            ),
                            mainPanel(
                              img(src = "Kickstarter_logo.png", height = 40, width = 900),
                              br(),
                              tags$hr(),
                              p(span("In this section you will select a main category, subcategory and ",
                                     "maximum no. of words to find famous and most repetatively word used in title of the ",
                                     "succesful project which was launced on the Kickstarter platform.",
                                     style = "color:black")),
                              tags$hr(),
                              #  verbatimTextOutput("val1"),
                              #  verbatimTextOutput("val2"),
                              plotOutput("Proj_title", width= 600, height = 450)
                            )
                          )
                 ) 
                 
)
