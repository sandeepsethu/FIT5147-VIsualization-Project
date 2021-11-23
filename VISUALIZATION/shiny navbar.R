# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#importing required libraries
require(base)
require(utils)
require(stats)
require(methods)
require(graphics)
require(grDevices)
require(datasets)
require(shiny)
require(plyr)
require(ggplot2)
require(dplyr)
require(plyr)
require(httpuv)
require(RColorBrewer)
require(ggmap)
require(maps)
require(rgdal)
require(scales)
require(maptools)
require(gridExtra)
require(rgeos)
require(leaflet)
require(plotly)
require(sf)
require(htmlwidgets)
require(hrbrthemes)
require(packcircles)
require(shinyWidgets)
require(shinydashboard)
require(gotop)

#reading the required csv file
State_population2 <- read.csv("./STATE POPULATION.csv", header=T)
profession <- read.csv("./profession.csv",header=T)
causes <- read.csv("./causes.csv",header=T)
age_suicide <- read.csv("./lolipop_age_suicide_rate.csv", header=T)
education<-read.csv("./education.csv",header=T)
gdp<- read.csv("./gdp.csv",header=T)

#reading the shape file
states_shape = readOGR(".","IND_adm1")

#importing shiny html tags
shiny::tags


#setting reactive values
values <- reactiveValues()
values$flag<-0
values$p<-37

#modify the dataframes for plotting putposes
State_population <- State_population2[State_population2$STATE.UT != "TOTAL (ALL INDIA)", ]   

#modify the dataframes for plotting putposes
fortest3 <- State_population2[State_population2$STATE.UT == "TOTAL (ALL INDIA)", ]

#modify the dataframes for plotting putposes
fortest3 <- fortest3%>%
  arrange(Year) %>%
  mutate(pct.chg = (total_suicide_rate - lag(total_suicide_rate))*100/lag(total_suicide_rate))

#modify the dataframes for plotting putposes
fortest3 <- fortest3%>%
  arrange(Year) %>%
  mutate(pct.chg.total = (Total_suicide - lag(Total_suicide))*100/lag(Total_suicide))

#modify the dataframes for plotting putposes
gdp <- gdp%>%
  arrange(Year) %>%
  mutate(pct.chg.total = (Suicide.as...of.total.deaths - lag(Suicide.as...of.total.deaths))*100/lag(Suicide.as...of.total.deaths))

#modify the dataframes for plotting putposes
fortest4 <- age_suicide[which(age_suicide$Year=='2001'),]

#modify the dataframes for plotting putposes
fortest2 <- State_population[which(State_population$Year=='2001'),]

#setting up the shape file with the data for chloropleth map
df = data.frame(states_shape)
states_shape1 <- gSimplify(states_shape, tol=0.1, topologyPreserve=TRUE)
df.polygon2 = SpatialPolygonsDataFrame(states_shape1, df)
df.polygon2@data$ID_1<-as.character(df.polygon2@data$ID_1)
State_population$id <- as.character(State_population$id)
df.polygon2@data <- left_join(df.polygon2@data, State_population, by=c("ID_1"="id"))
fortest<-df.polygon2
fortest@data <- df.polygon2@data[which(df.polygon2@data$Year=='2001'),]

#setting up the choices for user inputs
all_years <- unique(State_population$Year)
all_sex <- c("Total","Male","Female")
all_age <- c("Total","upto 14 years","15-29 years","30-44 Years","45-59 Years","60 Years and above")



#ui
ui <- navbarPage("The sea of hoplessness", id = "inTabset",
                 
                 #first tab
                 tabPanel("Introduction",
                          fluidRow(
                            #text descriptions
                            column(6, offset=1,
                                   p(h1("The sea of hopelessness")),
                                   p(h2("An exploration of suicide statistics in India.")),
                                   p(h3("FIT5147 Narrative Visualisation Project")),
                                   tags$br(),
                                   tags$br(),
                                   p(tags$b("! Trigger warning !")),
                                   p(tags$b("This project contains content about suicide")),
                                   p("Are you thinking about suicide? Are you in emotional distress? Do you know someone who is thinking about suicide or is in emotional distress? 
                                     Please contact your national suicide prevention organisation."),
                                   p(tags$ul(tags$li("AU ", a(href="https://www.lifeline.org.au/", "Lifeline Australia"), " 13 11 14"),
                                             tags$li("IND ", a(href="http://www.jeevanaastha.com/"," Jeevan Aastha Helpline"), " 1800 233 3330"),
                                             tags$li("US ", a(href="https://suicidepreventionlifeline.org/"," National Suicide Prevention Lifeline"), " 1-800-273-8255"),
                                             tags$li("UK ", a(href="https://www.samaritans.org/"," Samaritans"), " 116 123"),
                                             tags$li(a(href="https://suicidepreventionlifeline.org/","List of suicide crisis lines by country")),
                                        )),
                                   tags$br(),
                                   p(h6("Navigate through the project using the tabs above")),
                                   p(em("or continue reading about the project ↓")),
                                   tags$br(),
                                   p(h3("About the Project")),
                                   tags$br(),
                                   p("Whether it be due to pressure, hopelessness or a grave lack of will to survive, for many people suicide is the last straw. 
                                     It is estimated that suicide is the tenth leading cause of death worldwide and it has rightfully led to a global effort to set up efficient 
                                     suicide prevention hotlines (Hawton & van Heeringen, 2009). While it is a morbid yet personal decision, it is still possible to explore existing 
                                     data regarding suicide to help identify possible patterns and trends in order to help prevent, or at least reduce these occurrences in the future."),
                                   p("Being from India and having had my own experiences with depression and having close acquaintances take their own lives, understanding the data behind 
                                     suicides to help the conversation is a personal mission for me. Through this project, I explore publicly available suicide data for India and visualize my findings."),
                                   p("For the purpose of this project, I have used annual data regarding suicides from the years 2001-2014 in India. The data for this project was obtained from the",
                                     a(href="https://data.gov.in/","open government data platform")," of India."),
                                   tags$br(),
                                   p(h5("References")),
                                   p(tags$ul(tags$li("Hawton, K., & van Heeringen, K. (2009). Suicide. The Lancet, 373(9672), 1372-1381. doi: 10.1016/s0140-6736(09)60372-x")))
                                   ),
                            
                            #importing picture for tab one
                            column(3,
                                             br(),br(),br(),br(),br(),br(),br(),
                                             img(src="https://thebookofman.com/wp-content/uploads/2018/09/GettyImages-838492794-1920x2133.jpg",height = 400, width = 400)
                                          )
                                      ),
                          
                          #action button to move to next tab
                          actionButton('jumpToP2', 'Jump to Second Tab')
                          ),
                  
                 #htmlk tags        
                 tags$head(
                   tags$style(HTML("
            code {
                display:block;
                padding:9.5px;
                margin:0 0 10px;
                margin-top:10px;
                font-size:13px;
                line-height:20px;
                word-break:break-all;
                word-wrap:break-word;
                white-space:pre-wrap;
                background-color:#F5F5F5;
                border:1px solid rgba(0,0,0,0.15);
                border-radius:4px; 
                font-family:monospace;
            }"))),      
                 
                 #tab2
                 tabPanel("National Suicide Trends",
                          #text descriptions
                          fluidRow(
                            column(10, offset=1,h3("National Trend of Suicide in India"))
                          ),
                          br(),
                          fluidRow(
                            column(10, offset=1,"Displayed below are the trends of absolute number of suicides and the relative suicide rate (per 100000 of the population).")
                          ),
                          br(),
                          fluidRow(
                            column(10,offset=1,"Looking at the national suicide data, it is easy to observe that there was an increasing trend in both absolute and relative terms until 2010. 
                              2012 stands out as an outlier with a sharp decrease in the suicide rate, 
                              but the decrease was shortlived as the numbers bumped up again the following year.")
                          ),
                          br(),
                          
                          
                          fluidRow(
                            #plotting the charts for trend
                            column(4, plotlyOutput("plot3")),
                            column(4, plotlyOutput("plot4")),
                            column(4, plotlyOutput("percentagesuicide"))
                          ),
                          br(),
                          
                          #box texts with icons
                          fluidRow(column(width=12, offset=1, box(width = 4,background="light-blue",
                                   infoBox(334,"Average daily suicides", icon = icon("ambulance"), fill = TRUE)),
                                   box(width = 4, background="light-blue",
                                       infoBox(11.878,"% increase in suicide rate in 2013 ", icon = icon("angle-double-up"), fill = TRUE)),
                                   box(width = 4, background="light-blue",
                                       infoBox(135585,"Suicides in 2011 ", icon = icon("skull-crossbones"), fill = TRUE)))),
                          
                          br(),
                          hr(),
                          
                          #text description
                          fluidRow(column(10, offset=1, h4(textOutput("ecocrisisheading"))),
                                   br(),
                                   column(10, offset=1, textOutput("ecocrisistext"))),
                                   br(),
                                   br(),
                                   hr(),
                                   br(),
                                   br(),
                          fluidRow(column(10, offset=1, h4(textOutput("sexdiffheading"))),
                                   br(),
                                   column(10,offset=1, textOutput("sexdifftext"))),
                                   br(),
                                   br(),
                          
                          #plotting  male-female rates
                          fluidRow(column(10, offset=1, plotlyOutput("sexdiff"))),
                          br(),
                          br(),
                          hr(),
                          
                          #text descriptions
                          fluidRow(column(10, offset=1, h5("Interactive visualization of the gap"))),
                          br(),
                          fluidRow(column(10, offset=1, "The difference in the suicide rates for male and female can be seen irrespective of the age groups or the region.
                                          The gap between suicide rates of men and women across all age groups and states widen over time."), 
                                    column(5, offset=1,"There has been an increase in the suicide rate of youngsters in recent years.
                                           The 15-29 age group has the smallest difference in the suicide rates(female suicide rate is slightly higher in 2001)."), 
                                    column(5, offset=1,"States having higher HDI rank tends to have higher rates of suicide.
                                           This can be because of inefficient civil registration systems, incomplete reporting of deaths, 
                                           variable standards in certifying death and the legal and social consequences of suicide in states which rank lower on HDI")),
                          br(),br(),
                          fluidRow(column(10, offset=1, em("Use the slider to change the views for the lollipop charts"))),
                          br(),
                          fluidRow(
                            column(5, offset=1,
                                   #user input for year
                                   sliderInput("Year2",
                                               "Select the Year to display data for:",
                                               value = 2001,
                                               min = 2001,
                                               max = 2014),
                                   fluidRow(
                                     column(5,
                                            #plotting lollipop for age groups
                                            plotOutput("plot5"))
                                   )
                            ),
                            column(width = 6,
                                   #plotting lollipop for states
                                   plotOutput("plot2"))
                          ),
                          br(),
                          br(),
                          br(),br(),br(),br(),
                          br(),br(),br(),br(),
                          br(),br(),
                          
                          br(),hr(),br(),
                          fluidRow(column(10,offset=1,"Continue story on the next tab")),
                          #jump to next tab
                          actionButton('jumpToP3', 'Jump to Third Tab')
                          ),
                 
                 #third tab
                 tabPanel("Interactive Map and Key Insights",
                          fluidRow(column(10, offset=1, h3("Interactive visualizations of suicide statistics"))),
                          br(),
                          #text descriptions
                          fluidRow(column(3, offset=1, em("Use the slider input to display the corresponding chloropleth map of suicide rates and modify the bubble charts. By Default the option is set to 2001.")),
                                   column(3, em("Use the sex user input to display the corresponding chloropleth map of suicide rates and modify the line and bubble charts. By Default the option is set to Total(male and female)")),
                                   column(3, em("Use the age group user input to display the corresponding bubble charts for profession and causes. By default the option is set to Total(all age groups)"))),
                          br(),
                          
                          #go to top button 
                          use_gotop(
                            src = "fas fa-chevron-up",
                            width = 45,
                            opacity = 0.5,
                            place = "right",
                            color = "",
                            appear = 200,
                            scrolltime = 800,
                            fadein = 500,
                            fadeout = 500,
                            marginX = 2,
                            marginY = 2,
                            container = "",
                            zIndex = 9
                          ),
                          
                          
                          #user inputs
                          fluidRow(column(3, offset=1,sliderInput("Year",
                                                         "Select the Year to display data for:",
                                                         value = 2001,
                                                         min = 2001,
                                                         max = 2014)),
                                   column(3, selectInput(inputId = "Sex",
                                                         label = "Select the sex to display",
                                                         choices = all_sex,
                                                         selected = TRUE,
                                                         multiple = FALSE,
                                                         selectize = TRUE)),
                                   column(3, selectInput(inputId = "Age",
                                                                     label = "Select the age group to display",
                                                                     choices = all_age,
                                                                     selected = TRUE,
                                                                     multiple = FALSE,
                                                                     selectize = TRUE))),
                          
                          fluidRow(
                            #leaflet
                                   column(4, offset=1, leafletOutput("map")),
                                   #user guide
                                   column(2, em("Hover over the state and bubbles to display name and relevant suicide rate. 
                          Click on each state to show the line and bubble charts. The line and bubble charts can be reset 
                          to national view by clicking on the same state twice.")),
                                   #line chart
                                   column(4, plotOutput("smallplot"))),
                          #bubble charts
                          fluidRow(column(4, plotlyOutput("profbubble")),
                                   column(4, plotlyOutput("causesbubble")),
                                   column(4, plotlyOutput("edububble"))),
                          
                          br(),hr(),
                          
                          #text description
                          fluidRow(column(10, offset=1,h5("Top 5 and Bottom 5 States"))),
                          br(),
                          fluidRow(column(10,offset=1, "Here is a comprehensive look at the top and bottom 5 states according to the suicide rates.")),
                          br(),
                          fluidRow(column(10,offset=1, "The suicide rates of the 5 highest states are 22 times that of the 5 lowest.
                                          Implying that suicide prevention efforts could be focused here.")),
                          br(),
                          
                          #top5 and bottom5 plots
                          fluidRow(column(5, offset=1, plotOutput("top5")),
                                   column(5, plotOutput("bottom5"))),
                          br(),
                          
                          #user guide
                          fluidRow(column(10, offset=1, em("Use the buttons below to quickly jump to the regions. 
                                          Click on the arrow on the right to go to the top of the page."))),
                          br(),
                          
                          #quick navigations
                          fluidRow(column(2, offset=1,actionButton('jumpToPondi', 'Jump to plots for Puducherry')),
                                  column(2, actionButton('jumpToKerala', 'Jump to plots for Kerala')),
                                  column(2, actionButton('jumpToBihar', 'Jump to plots for Bihar')),
                                  column(2, actionButton('jumpToUP', 'Jump to plots for Uttar Pradesh'))
                                  ),
                          br(),
                          
                          #text description
                          fluidRow(column(10, offset=1, "The line chart shows that the states with higher suicide rates show decreasing trends and 
                                          those with lower rates have increasing trends.")),
                          br(),br(),
                          hr(),
                          
                          #text descriptions
                          fluidRow(column(10, offset=1, h4("Suicides in the farming belt"))),
                          br(),
                          #image
                          fluidRow(column(6, offset=1, img(src="farmers.jpg", height = 500, width = 500)),
                                   #text descriptions
                                   column(4, p("The epidemic of farmer suicide in India is well documented. 
                                          And this is evident in how most people who commit suicide work in the 
                                          agriculture sector and that economic issues is one of the most cited causes."),
                                          p(em("Use the below buttons to easily view the visualizations for major regions in India's farming belt.
                                               Click the arrow on the bottom right to go to the top of the page to see the effects."))),
                                   #quick navigation buttons
                                   column(4,offset=1,p("         "),p("  ")),
                                   column(4,offset=1,actionButton('jumpToUP2', 'Jump to plots for Uttar Pradesh')),
                                   column(4,offset=1,p("         "),p("  ")),
                                   column(4,offset=1,actionButton('jumpToPunjab', 'Jump to plots for Punjab')),
                                   column(4,offset=1,p("         "),p("  ")),
                                   column(4,offset=1,actionButton('jumpToGujarat', 'Jump to plots for Gujarat')),
                                   column(4,offset=1,p("         "),p("  ")),
                                   column(4,offset=1,actionButton('jumpToHaryana', 'Jump to plots for Haryana')),
                                   column(4,offset=1,p("         "),p("  "))),
                          br(),
                          br(),
                          hr(),
                          #text descriptions
                          fluidRow(column(10, offset=1, h4("Student Suicides"))),
                          br(),
                          #image
                          fluidRow(column(6, offset=1, img(src="students.jpg", height = 500, width = 500)),
                                   #text descriptions
                                   column(4, p("There has been a new epidemic of student suicide in India.
                                          Intense pressure and huge workloads combined with the lack of mental support resources
                                               has seen students opting to take their own lives in major schools and colleges 
                                               all over the country"),
                                          p(em("Use the below buttons to easily view the visualizations for the regions with the highest literacy rate.
                                               Click the arrow on the bottom right to go to the top of the page to see the effects."))),
                                   #quick navigation buttons
                                   column(4,offset=1,p("         "),p("  ")),
                                   column(4,offset=1,actionButton('jumpToKerala2', 'Jump to plots for Kerala')),
                                   column(4,offset=1,p("         "),p("  ")),
                                   column(4,offset=1,actionButton('jumpToMaharashtra', 'Jump to plots for Maharashtra')),
                                   column(4,offset=1,p("         "),p("  ")),
                                   column(4,offset=1,actionButton('jumpToDelhi', 'Jump to plots for Delhi')),
                                   column(4,offset=1,p("         "),p("  ")),
                                   column(4,offset=1,actionButton('jumpToTamilNadu', 'Jump to plots for Tamil Nadu')),
                                   column(4,offset=1,p("         "),p("  "))),
                          br(),
                          fluidRow(column(10,offset=1,"Conclude story on the next tab")),
                          #jump to next page
                          actionButton('jumpToP4', 'Jump to Fourth Tab')

                         
                 ),
            #fourth tab
                tabPanel("Concluding Points",
                         fluidRow(column(10, offset=1, h3("Key Inferences")),
                                  column(10,offset=1,p("         "),p("  "))
                                  ),
                         br(),
                         
                         #quick points with icons
                         fluidRow(column(width=12, offset=1, box(width = 3,background="light-blue",
                                                                 infoBox(10.25,": The mean Suicide Rate of India", icon = icon("ambulance"), fill = TRUE)),
                                         box(width = 3, background="light-blue",
                                             infoBox("Female suicide rate is",1.7,"times smaller than male", icon = icon("restroom"), fill = TRUE)),
                                         box(width = 3, background="light-blue",
                                             infoBox(21.3,"% increase in sucides from 2001 to 2014", icon = icon("skull-crossbones"), fill = TRUE)))),
                         br(),hr(),
                         fluidRow(column(width=12, offset=1, box(width = 3,background="light-blue",
                                                                infoBox(73,": % increase in the gap between male and female suicide rates from 2001-14", icon = icon("angle-double-up"), fill = TRUE)),
                                         box(width = 3, background="light-blue",
                                             infoBox("Only the 15-29 age group shows increasing trend in suicide rate", icon = icon("angle-double-up"), fill = TRUE)),
                                         box(width = 3, background="light-blue",
                                             infoBox("States with higher HDI have higher suicide rate and vice versa", icon = icon("skull-crossbones"), fill = TRUE)))),
                         br(),hr(),
                         fluidRow(column(width=12, offset=1, box(width = 3,background="light-blue",
                                                                 infoBox("People with higher education are less likely to commit suicide", icon = icon("user-graduate"), fill = TRUE)),
                                         box(width = 3, background="light-blue",
                                             infoBox(24.7,": % decrease in the number of people working in agriculture committing suicide", icon = icon("tractor"), fill = TRUE)),
                                         box(width = 3, background="light-blue",
                                             infoBox("Southern and North Eastern states have the highest suicide rates", icon = icon("skull-crossbones"), fill = TRUE)))),
                         br(),hr(),
                         fluidRow(column(width=12, offset=1, box(width = 3,background="light-blue",
                                                                 infoBox(47.38,": % increase in the number of students committing suicide", icon = icon("user-graduate"), fill = TRUE)),
                                         box(width = 3, background="light-blue",
                                             infoBox(121,": % increase in the number of people committing suicide due to health reasons", icon = icon("ambulance"), fill = TRUE)),
                                         box(width = 3, background="light-blue",
                                             infoBox(21,": % increase in the number of people committing suicide due to mental health reasons", icon = icon("brain"), fill = TRUE)))),
                         br(),hr(),
                         #text descriptions
                         fluidRow(column(width=10, offset=1, p(h4("Conclusion"))),
                                  column(width=10, offset=1, p("Suicide statistics in India provide some troubling insights. The data shows a steadily increasing trend in the 
                                                               number of people who take their own life. It is clear that men are more likely to commit suicide and hence more
                                                               effort is needed to ensure that there are proper avenues of support available to the vulnerable in all regions of 
                                                               India and especially for the poorer and less literate section of the country."),
                                         p("What is even more frightening is that these numbers might be significantly underreported due to inefficient civil registration systems, 
                                           incomplete reporting of deaths, variable standards in certifying death and the legal and social consequences of suicide. Another troubling 
                                           insight is the increase in the number of people in the age group 15-29 committing suicide."),
                                         p("There needs to be more research conducted wrt the matter of suicide in India and better and more comprehensive
                                           data collection. In the meantime, everyone should try to reach out for help if they need it and make sure that we are more empathetic to the plight of others.")))
                )
                                
  
)

#server
server<- (function(input, output, session) { 
  
  #action button to jump to next tab
  observeEvent(input$jumpToP2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "National Suicide Trends")
  })
  
  #action button to jump to next tab
  observeEvent(input$jumpToP3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Interactive Map and Key Insights")
  })
  
  #action button to jump to next tab
  observeEvent(input$jumpToP4, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Concluding Points")
  })
  
  #action button to change filter and view relevant plot
  observeEvent(input$jumpToPondi, {
    values$p=27
    updateTabsetPanel(session, "inTabset",
                      selected = "Interactive Map and Key Insights")
  })

  #action button to change filter and view relevant plot
  observeEvent(input$jumpToKerala, {
    values$p=18
    updateTabsetPanel(session, "inTabset",
                      selected = "Interactive Map and Key Insights")
  })

  #action button to change filter and view relevant plot
  observeEvent(input$jumpToUP, {
    values$p=34
    updateTabsetPanel(session, "inTabset",
                      selected = "Interactive Map and Key Insights")
  })
  
  #action button to change filter and view relevant plot
  observeEvent(input$jumpToBihar, {
    values$p=5
    updateTabsetPanel(session, "inTabset",
                      selected = "Interactive Map and Key Insights")
  })
  
  #action button to change filter and view relevant plot
  observeEvent(input$jumpToUP2, {
    values$p=34
    updateTabsetPanel(session, "inTabset",
                      selected = "Interactive Map and Key Insights")
  })
  
  #action button to change filter and view relevant plot
  observeEvent(input$jumpToPunjab, {
    values$p=28
    updateTabsetPanel(session, "inTabset",
                      selected = "Interactive Map and Key Insights")
  })
  
  #action button to change filter and view relevant plot
  observeEvent(input$jumpToGujarat, {
    values$p=12
    updateTabsetPanel(session, "inTabset",
                      selected = "Interactive Map and Key Insights")
  })
  
  #action button to change filter and view relevant plot
  observeEvent(input$jumpToHaryana, {
    values$p=13
    updateTabsetPanel(session, "inTabset",
                      selected = "Interactive Map and Key Insights")
  })
  
  #action button to change filter and view relevant plot
  observeEvent(input$jumpToKerala2, {
    values$p=18
    updateTabsetPanel(session, "inTabset",
                      selected = "Interactive Map and Key Insights")
  })
  
  #action button to change filter and view relevant plot
  observeEvent(input$jumpToMaharashtra, {
    values$p=21
    updateTabsetPanel(session, "inTabset",
                      selected = "Interactive Map and Key Insights")
  })
  
  #action button to change filter and view relevant plot
  observeEvent(input$jumpToTamilNadu, {
    values$p=31
    updateTabsetPanel(session, "inTabset",
                      selected = "Interactive Map and Key Insights")
  })
  
  #action button to change filter and view relevant plot
  observeEvent(input$jumpToDelhi, {
    values$p=10
    updateTabsetPanel(session, "inTabset",
                      selected = "Interactive Map and Key Insights")
  })
  
  #render required texts
  output$ecocrisisheading <- renderText("Economic Crisis?")
  
  #render required texts
  output$ecocrisistext <- renderText("2007-08 saw a global economic crisis which led to companies around the world going bankrupt. 
                                     Since 2008, the number of suicides in several countries increased. From international research it appears suicide can correlate with an economical crisis. 
                                     It’s unknown if this crisis had an effect on India's suicide rate because the data shows an increasing trend prior to this period as well.")
  
  #render required texts
  output$sexdiffheading <- renderText("How big a difference does your sex make?")
  
  #render required texts
  output$sexdifftext <- renderText("Quite a huge difference when we look at the data. Male suicide rates are 1.7 times higher than female suicide rates from the period 2001-14.
                                   On the bright side, female suicide rates have shown a decreasing trend, however male suicide rates show a dangerously increasing trend.")
  
  #render required texts
  output$caption1 <- renderText("Interactive Maps and Bubble Charts")
  
  #render required texts
  output$description <- renderText("Use the Year and Sex user inputs to display the corresponding chloropleth map of suicide rates and modify the bubble charts.
                                   By Default the options are set to 2001 and Total(male and female).")
  
  #render required texts
  output$description2 <- renderText(("The chloropleth map shows that the southern states of India as well as the smaller Eastern states have the highest suicide rates.
                                     The reasons for this might be that they have smaller populations and hence relative suicide rates might be distorted.
                                     
                                     The bubble charts show that men who are unemployed and working in the agricultural sector are most vulnerable.
                                     Vast majority of women who commit suicide are housewives."))
  
  #render required texts
  output$description4 <- renderText("The views for the lollipop charts can be changed using the Year Input below.")
  
  #render required texts
  output$description3 <- renderText("The gap between suicide rates of men and women across all age groups widen over time.
                                    There has been an increase in the suicide rate of youngsters in recent years.
                                    States having higher HDI rank tends to have higher rates of suicide. 
                                    ")
  
  #render required texts
  output$caption2 <- renderText("Lollipop charts based on user selection of Year.")
  
  
  #plot for relative suicide rate
  output$plot3<- renderPlotly({
    scatterPlot1 <- fortest3 %>% 
      ggplot(aes(x = Year, y = total_suicide_rate, 
                 text = paste(
                   "Year: ", Year, "\n",
                   "Total suicide rate: ", round(total_suicide_rate,3), "\n",
                   "Percentage change in suicide rate: ", round(pct.chg,3), "\n",
                   sep = ""
                 ))) + 
      geom_line(aes(group=1))+
      geom_point(alpha=0.7, colour = "#51A0D5")+
      labs(x = "Years from 2001-2014", 
           y = "Total suicide rate",
           title = "Relative number of suicides(per 100000)") +
      theme_classic()
    ggplotly(scatterPlot1, tooltip = "text")})
  
  #plot absolute number of suicides
  output$plot4<- renderPlotly({
    scatterPlot2 <- fortest3 %>% 
      ggplot(aes(x = Year, y = Total_suicide, 
                 text = paste(
                   "Year: ", Year, "\n",
                   "Total number of suicides: ", Total_suicide, "\n",
                   "Percentage change in number of suicides: ", round(pct.chg.total,3), "\n",
                   sep = ""
                 ))) + 
      geom_line(aes(group=1))+
      geom_point(alpha=0.7, colour = "#51A0D5")+
      scale_y_continuous(labels = scales::comma)+
      labs(x = "Years from 2001-2014", 
           y = "Number of suicides",
           title = "Absolute number of suicides") +
      theme_classic()
    ggplotly(scatterPlot2, tooltip = "text")})
  
  #plot suicide as % of total deaths 
  output$percentagesuicide<- renderPlotly({
    scatterPlot3 <- gdp %>% 
      ggplot(aes(x = Year, y = Suicide.as...of.total.deaths, 
                 text = paste(
                   "Year: ", Year, "\n",
                   "Suicides as % of total deaths: ", Suicide.as...of.total.deaths, "\n",
                   "Percentage change: ", round(pct.chg.total,3), "\n",
                   sep = ""
                 ))) + 
      geom_line(aes(group=1))+
      geom_point(alpha=0.7, colour = "#51A0D5")+
      scale_y_continuous(labels = scales::comma)+
      labs(x = "Years from 2001-2014", 
           y = "Percentage of suicides",
           title = "Suicides as % of total deaths") +
      theme_classic()
    ggplotly(scatterPlot3, tooltip = "text")})
  
  #plot male and female suicide rates
  output$sexdiff <- renderPlotly({
    Indian_population<-fortest3
    sexdiff<- ggplot(Indian_population, aes(Year)) + 
      geom_line(aes(y = male_suicide_rate, colour = "male suicide rate")) + 
      #geom_point(y = Indian_population$male_suicide_rate) +
      geom_line(linetype="dashed",aes(y = mean(male_suicide_rate), colour = "mean male suicide rate"))+
      geom_line(aes(y = total_suicide_rate, colour = "total suicide rate"))+
      #geom_point(y = Indian_population$total_suicide_rate) +
      geom_line(linetype="dashed",aes(y = mean(total_suicide_rate), colour = "mean total suicide rate"))+
      geom_line(aes(y = female_suicide_rate, colour = "female suicide rate"))+
      #geom_point(y = Indian_population$female_suicide_rate) +
      geom_line(linetype="dashed",aes(y = mean(female_suicide_rate), colour = "mean female suicide rate"))+
      ggtitle("Suicide rate over time")+
      labs(x = "Year", y = "Suicide rate")+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
    ggplotly(sexdiff)
  })
  
  #filter age groups by year
  age_groups <- reactive({if(is.null(input$Year2))
  {fortest4} else{
    age_suicide%>%
      dplyr::filter(Year %in% input$Year2)
  }
    
  })
  
  #filter state suicide rates by year
  Loli_plot<-reactive({if(is.null(input$Year2))
  {fortest2} else{
    State_population%>%
      dplyr::filter(Year %in% input$Year2)
  }
  })
  
  #lollipop chart for suicide rates by sex for states
  output$plot2<-renderPlot({
    #filter data and arrange by HDI rank
    fortest2<-Loli_plot()
    fortest2 <- fortest2 %>%
      rowwise() %>%
      arrange(HDI) %>%
      mutate(STATE.UT=factor(STATE.UT, STATE.UT))
    # Plot
    ggplot(fortest2) +
      geom_segment( aes(x=STATE.UT, xend=STATE.UT, y=male_suicide_rate, yend=female_suicide_rate), color="grey") +
      geom_point( aes(x=STATE.UT, y=male_suicide_rate, color="Male suicide rate"), size=3 ) +
      geom_point( aes(x=STATE.UT, y=female_suicide_rate, color="Female suicide rate"), size=3 ) +
      coord_flip()+
      scale_color_discrete(name = "Legend")+
      theme_ipsum() +theme(legend.position="bottom",plot.title = element_text(hjust = 1, vjust=0))+
      labs(title = "Suicide rates by sex for states(ranked by HDI)", x = "States ranked by HDI(Best to Worst)", y = "Suicide rate")
  },height=800,width = 600)
  
  
  #lollipop for agre groups
  output$plot5<-renderPlot({
    fortest4<-age_groups()
    # Plot
    ggplot(fortest4) +
      geom_segment( aes(x=Age.Group, xend=Age.Group, y=Male.Suicide.Rate, yend=Female.Suicide.Rate), color="grey") +
      geom_point( aes(x=Age.Group, y=Male.Suicide.Rate, color="Male suicide rate"), size=3 ) +
      geom_point( aes(x=Age.Group, y=Female.Suicide.Rate, color="Female suicide rate"), size=3 ) +
      coord_flip()+
      scale_color_discrete(name = "Legend")+
      theme_ipsum() +theme(legend.position="bottom")+
      labs(title = "Suicide rates by age groups and sex", x = "Age Groups", y = "Suicide rate")
  },width = 400)
  
  
  #reactive filtering of map data after year input
  Final_map <- reactive({if(is.null(input$Year))
  {fortest} else{
    lasttest <- df.polygon2
    lasttest@data <- df.polygon2@data[which(df.polygon2@data$Year==input$Year),]
    lasttest
  }
  })
  
  #reactive filtering of map data after sex input
  Final_map2<-reactive({
    lasttest<- Final_map()
    if(is.null(input$Sex))
    {lasttest$suicide_display<- lasttest$total_suicide_rate
    } else{
      if(input$Sex=="Total")
      {lasttest$suicide_display<- lasttest$total_suicide_rate} else{
        if(input$Sex=="Male")
        {lasttest$suicide_display<- lasttest$male_suicide_rate} else{
          if(input$Sex=="Female")
          {lasttest$suicide_display<- lasttest$female_suicide_rate}
        }
      }
    }
    lasttest
  })
  
  #filtering of education data after year and sex inputs
  edu_bubble <- reactive({if(is.null(input$Year))
    {bubbleedu<-education%>%
      dplyr::filter(Year %in% 2001)}else{
        bubbleedu<-education%>%
          dplyr::filter(Year %in% input$Year)
      }
    if(is.null(input$Sex))
    {bubbleedu$suicide_display<- bubbleedu$Total
    } else{
      if(input$Sex=="Total")
      {bubbleedu$suicide_display<- bubbleedu$Total} else{
        if(input$Sex=="Male")
        {bubbleedu$suicide_display<- bubbleedu$Male} else{
          if(input$Sex=="Female")
          {bubbleedu$suicide_display<- bubbleedu$Female}
        }
      }
    }
    bubbleedu
    })
  
  #filtering of profession data after year, sex and age group input
  prof_bubble <- reactive({if(is.null(input$Year))
    {bubbleprof<-profession%>%
      dplyr::filter(Year %in% 2001)}else{
        bubbleprof<-profession%>%
          dplyr::filter(Year %in% input$Year)
      }
    if(is.null(input$Age))
    {if(is.null(input$Sex))
    {bubbleprof$suicide_display<- bubbleprof$Total
    } else{
      if(input$Sex=="Total")
      {bubbleprof$suicide_display<- bubbleprof$Total} else{
        if(input$Sex=="Male")
        {bubbleprof$suicide_display<- bubbleprof$Total.Male} else{
          if(input$Sex=="Female")
          {bubbleprof$suicide_display<- bubbleprof$Total.Female}
        }
      }
    }}else{
      if(input$Age=="Total")
      {if(is.null(input$Sex))
      {bubbleprof$suicide_display<- bubbleprof$Total
      } else{
        if(input$Sex=="Total")
        {bubbleprof$suicide_display<- bubbleprof$Total} else{
          if(input$Sex=="Male")
          {bubbleprof$suicide_display<- bubbleprof$Total.Male} else{
            if(input$Sex=="Female")
            {bubbleprof$suicide_display<- bubbleprof$Total.Female}
          }
        }
      }}else{
        if(input$Age=="upto 14 years"){
          if(is.null(input$Sex))
          {bubbleprof$suicide_display<- bubbleprof$Total.upto.14.years
          } else{
            if(input$Sex=="Total")
            {bubbleprof$suicide_display<- bubbleprof$Total.upto.14.years } else{
              if(input$Sex=="Male")
              {bubbleprof$suicide_display<- bubbleprof$Male.upto.14.years} else{
                if(input$Sex=="Female")
                {bubbleprof$suicide_display<- bubbleprof$Female.upto.14.years}
              }
            }
          }
        }else{
          if(input$Age=="15-29 years"){
            if(is.null(input$Sex))
            {bubbleprof$suicide_display<- bubbleprof$Total.15.29.years
            } else{
              if(input$Sex=="Total")
              {bubbleprof$suicide_display<- bubbleprof$Total.15.29.years } else{
                if(input$Sex=="Male")
                {bubbleprof$suicide_display<- bubbleprof$Male.15.29.years } else{
                  if(input$Sex=="Female")
                  {bubbleprof$suicide_display<- bubbleprof$Female.15.29.years }
                }
              }
            }
          }else{
            if(input$Age=="30-44 Years"){
              if(is.null(input$Sex))
              {bubbleprof$suicide_display<- bubbleprof$Total.30.44.years 
              } else{
                if(input$Sex=="Total")
                {bubbleprof$suicide_display<- bubbleprof$Total.30.44.years } else{
                  if(input$Sex=="Male")
                  {bubbleprof$suicide_display<- bubbleprof$Male.30.44.years} else{
                    if(input$Sex=="Female")
                    {bubbleprof$suicide_display<- bubbleprof$Female.30.44.years}
                  }
                }
              }
            }else{
              if(input$Age=="45-59 Years"){
                if(is.null(input$Sex))
                {bubbleprof$suicide_display<- bubbleprof$Total.45.59.years
                } else{
                  if(input$Sex=="Total")
                  {bubbleprof$suicide_display<- bubbleprof$Total.45.59.years} else{
                    if(input$Sex=="Male")
                    {bubbleprof$suicide_display<- bubbleprof$Male.45.59.years} else{
                      if(input$Sex=="Female")
                      {bubbleprof$suicide_display<- bubbleprof$Female.45.59.years}
                    }
                  }
                }
              }else{
                if(input$Age=="60 Years and above"){
                  if(is.null(input$Sex))
                  {bubbleprof$suicide_display<- bubbleprof$Total.60.years.and.above
                  } else{
                    if(input$Sex=="Total")
                    {bubbleprof$suicide_display<- bubbleprof$Total.60.years.and.above} else{
                      if(input$Sex=="Male")
                      {bubbleprof$suicide_display<- bubbleprof$Male.60.years.and.above} else{
                        if(input$Sex=="Female")
                        {bubbleprof$suicide_display<- bubbleprof$Female.60.years.and.above}
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    bubbleprof})
  
  #filtering of causes data after year, sex and age group input
  causes_bubble <- reactive({if(is.null(input$Year))
  {bubblecauses<-causes%>%
    dplyr::filter(Year %in% 2001)}else{
      bubblecauses<-causes%>%
        dplyr::filter(Year %in% input$Year)
    }
    if(is.null(input$Age))
    {if(is.null(input$Sex))
    {bubblecauses$suicide_display<- bubblecauses$Grand.Total
    } else{
      if(input$Sex=="Total")
      {bubblecauses$suicide_display<- bubblecauses$Grand.Total} else{
        if(input$Sex=="Male")
        {bubblecauses$suicide_display<- bubblecauses$Total.Male} else{
          if(input$Sex=="Female")
          {bubblecauses$suicide_display<- bubblecauses$Total.Female}
        }
      }
    }}else{
      if(input$Age=="Total")
      {if(is.null(input$Sex))
      {bubblecauses$suicide_display<- bubblecauses$Grand.Total
      } else{
        if(input$Sex=="Total")
        {bubblecauses$suicide_display<- bubblecauses$Grand.Total} else{
          if(input$Sex=="Male")
          {bubblecauses$suicide_display<- bubblecauses$Total.Male} else{
            if(input$Sex=="Female")
            {bubblecauses$suicide_display<- bubblecauses$Total.Female}
          }
        }
      }}else{
        if(input$Age=="upto 14 years"){
          if(is.null(input$Sex))
          {bubblecauses$suicide_display<- bubblecauses$Male.upto.14.years+bubblecauses$Female.upto.14.years
          } else{
            if(input$Sex=="Total")
            {bubblecauses$suicide_display<- bubblecauses$Male.upto.14.years+bubblecauses$Female.upto.14.years } else{
              if(input$Sex=="Male")
              {bubblecauses$suicide_display<- bubblecauses$Male.upto.14.years} else{
                if(input$Sex=="Female")
                {bubblecauses$suicide_display<- bubblecauses$Female.upto.14.years}
              }
            }
          }
        }else{
          if(input$Age=="15-29 years"){
            if(is.null(input$Sex))
            {bubblecauses$suicide_display<- bubblecauses$Male.15.29.years+bubblecauses$Female.15.29.years
            } else{
              if(input$Sex=="Total")
              {bubblecauses$suicide_display<- bubblecauses$Male.15.29.years+bubblecauses$Female.15.29.years } else{
                if(input$Sex=="Male")
                {bubblecauses$suicide_display<- bubblecauses$Male.15.29.years } else{
                  if(input$Sex=="Female")
                  {bubblecauses$suicide_display<- bubblecauses$Female.15.29.years }
                }
              }
            }
          }else{
            if(input$Age=="30-44 Years"){
              if(is.null(input$Sex))
              {bubblecauses$suicide_display<- bubblecauses$Male.30.44.years+bubblecauses$Female.30.44.years 
              } else{
                if(input$Sex=="Total")
                {bubblecauses$suicide_display<- bubblecauses$Male.30.44.years+bubblecauses$Female.30.44.years } else{
                  if(input$Sex=="Male")
                  {bubblecauses$suicide_display<- bubblecauses$Male.30.44.years} else{
                    if(input$Sex=="Female")
                    {bubblecauses$suicide_display<- bubblecauses$Female.30.44.years}
                  }
                }
              }
            }else{
              if(input$Age=="45-59 Years"){
                if(is.null(input$Sex))
                {bubblecauses$suicide_display<- bubblecauses$Male.45.59.years+bubblecauses$Female.45.59.years
                } else{
                  if(input$Sex=="Total")
                  {bubblecauses$suicide_display<- bubblecauses$Male.45.59.years+bubblecauses$Female.45.59.years} else{
                    if(input$Sex=="Male")
                    {bubblecauses$suicide_display<- bubblecauses$Male.45.59.years} else{
                      if(input$Sex=="Female")
                      {bubblecauses$suicide_display<- bubblecauses$Female.45.59.years}
                    }
                  }
                }
              }else{
                if(input$Age=="60 Years and above"){
                  if(is.null(input$Sex))
                  {bubblecauses$suicide_display<- bubblecauses$Male.60.years.and.above+bubblecauses$Female.60.years.and.above
                  } else{
                    if(input$Sex=="Total")
                    {bubblecauses$suicide_display<- bubblecauses$Male.60.years.and.above+bubblecauses$Female.60.years.and.above} else{
                      if(input$Sex=="Male")
                      {bubblecauses$suicide_display<- bubblecauses$Male.60.years.and.above} else{
                        if(input$Sex=="Female")
                        {bubblecauses$suicide_display<- bubblecauses$Female.60.years.and.above}
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    bubblecauses})
  
  #setting colour palette on receiving year and sex inouts
  pal <- reactive({
    lasttest<-Final_map2()
    colorNumeric(
      palette = "YlGnBu",
      domain = lasttest@data$suicide_display)
  })
  
  
  #rendering leaflet 
  output$map<-renderLeaflet({
    pal <- pal()
    lasttest<-Final_map2()
    df.polygon<- Final_map2()
    leaflet(options = leafletOptions(minZoom = 4, maxZoom = 12)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = Final_map2(), 
                  fillColor = ~pal(suicide_display), 
                  layerId = ~ID_1,
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    stroke = 4, weight=6),
                  label = paste0("State: ", lasttest$STATE.UT, "   " , "Suicide Rate: ", round(lasttest$suicide_display,2))) %>%
      
      addLegend(pal = pal, 
                values = lasttest$suicide_display, 
                position = "bottomright", 
                title = "Suicide rate<br>(per 100000)",
                labFormat = labelFormat(suffix = "%")) 
  })
  
  #getting map click position for updating bubbles and line chart
  observeEvent(input$map_shape_click, { # update the location selectInput on map clicks
    values$p <- as.numeric(input$map_shape_click$id)
    if(values$p!=values$flag){values$flag=values$p}else{
      values$p=37
      values$flag=0
    }
    
  })
  
  #updating line chart data on map click
  smallplotdata<- reactive({
    smallplottest<- State_population2
    if(is.null(input$Sex))
    {smallplottest$suicide_display<- smallplottest$total_suicide_rate
    } else{
      if(input$Sex=="Total")
      {smallplottest$suicide_display<- smallplottest$total_suicide_rate} else{
        if(input$Sex=="Male")
        {smallplottest$suicide_display<- smallplottest$male_suicide_rate} else{
          if(input$Sex=="Female")
          {smallplottest$suicide_display<- smallplottest$female_suicide_rate}
        }
      }
    }
    smallplottest
  })
  
  #plotting the line chart
  output$smallplot<-renderPlot({
    smallplottest<-smallplotdata()
    if(is.null(values$p))
    {smallplottest<-smallplottest%>%
      dplyr::filter(id %in% 37)}else{
        smallplottest<-smallplottest%>%
          dplyr::filter(id %in% values$p)}
    region=as.character(smallplottest$STATE.UT[1])
    ggplot(smallplottest, aes(x=Year))+
      geom_line(aes(y = suicide_display))+
      geom_point(y = smallplottest$suicide_display) +
      labs(x = "Year", y = "Suicide rate")+
      ggtitle(paste("Suicide rate (per 100K) in: ",region))  
  })
  
  
  #plotting education bubble
  output$edububble<- renderPlotly({
    #updating data on map click
    fortest7<- edu_bubble()
    if(is.null(values$p))
    {fortest7<-fortest7%>%
      dplyr::filter(id %in% 37)}else{
        fortest7<-fortest7%>%
          dplyr::filter(id %in% values$p)}
    packing <- circleProgressiveLayout(fortest7$suicide_display, sizetype='area')
    packing$radius <- 0.75*packing$radius
    # We can add these packing information to the initial data frame
    fortest7 <- cbind(fortest7, packing)
    # The next step is to go from one center + a radius to the coordinates of a circle that
    # is drawn by a multitude of straight lines.
    dat.gg <- circleLayoutVertices(packing, npoints=50)
    region=as.character(fortest7$STATE.UT[1])
    # Make the plot
    bubblechart3<-ggplot() + 
      # Make the bubbles
      geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
      # Add text in the center of each bubble + control its size
      geom_text(data = fortest7, aes(x, y, size=suicide_display, label = CAUSE, text = paste(
        "Total suicides: ", suicide_display, "\n",
        "Educational level: ", CAUSE, "\n",
        sep = ""))) +
      scale_size_continuous(range = c(0,2)) +
      labs(title = paste("Educational level in: ",region))+
      # General theme:
      theme_void() + 
      theme(legend.position="none") +
      coord_equal()
    ggplotly(bubblechart3, tooltip="text")
  }) 
  
  #plotting causes bubble
  output$causesbubble<- renderPlotly({
    #updating data on map click
    fortest6<- causes_bubble()
    if(is.null(values$p))
    {fortest6<-fortest6%>%
      dplyr::filter(id %in% 37)}else{
        fortest6<-fortest6%>%
          dplyr::filter(id %in% values$p)}
    packing <- circleProgressiveLayout(fortest6$suicide_display, sizetype='area')
    packing$radius <- 0.75*packing$radius
    # We can add these packing information to the initial data frame
    fortest6 <- cbind(fortest6, packing)
    # The next step is to go from one center + a radius to the coordinates of a circle that
    # is drawn by a multitude of straight lines.
    dat.gg <- circleLayoutVertices(packing, npoints=50)
    region=as.character(fortest6$STATE.UT[1])
    # Make the plot
    bubblechart2<-ggplot() + 
      # Make the bubbles
      geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
      # Add text in the center of each bubble + control its size
      geom_text(data = fortest6, aes(x, y, size=suicide_display, label = CAUSE, text = paste(
        "Total suicides: ", suicide_display, "\n",
        "Cause: ", CAUSE, "\n",
        sep = ""))) +
      scale_size_continuous(range = c(0,2)) +
      labs(title = paste("Causes in: ",region))+
      # General theme:
      theme_void() + 
      theme(legend.position="none") +
      coord_equal()
    ggplotly(bubblechart2, tooltip="text")
  })  
  
  #plotting profession bubble
  output$profbubble<- renderPlotly({
    fortest5<- prof_bubble()
    if(is.null(values$p))
    {fortest5<-fortest5%>%
      dplyr::filter(id %in% 37)}else{
        fortest5<-fortest5%>%
          dplyr::filter(id %in% values$p)}
    packing <- circleProgressiveLayout(fortest5$suicide_display, sizetype='area')
    packing$radius <- 0.85*packing$radius
    # We can add these packing information to the initial data frame
    fortest5 <- cbind(fortest5, packing)
    # The next step is to go from one center + a radius to the coordinates of a circle that
    # is drawn by a multitude of straight lines.
    dat.gg <- circleLayoutVertices(packing, npoints=50)
    region=as.character(fortest5$STATE.UT[1])
    # Make the plot
    bubblechart1<-ggplot() + 
      # Make the bubbles
      geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
      # Add text in the center of each bubble + control its size
      geom_text(data = fortest5, aes(x, y, size=suicide_display, label = CAUSE, text = paste(
        "Total suicides: ", suicide_display, "\n",
        "Profession: ", CAUSE, "\n",
        sep = ""))) +
      scale_size_continuous(range = c(0,3)) +
      labs(title = paste("Professions in: ",region))+
      # General theme:
      theme_void() + 
      theme(legend.position="none") +
      coord_equal()
    ggplotly(bubblechart1, tooltip="text")
    
  })
  
  #plotting top 5 states
  output$top5<- renderPlot({
    agg1 <- aggregate(total_suicide_rate~STATE.UT, State_population2, mean)
    agg1 <-agg1[order(-agg1$total_suicide_rate),]
    ggplot(agg1[1:5,], aes(x=reorder(STATE.UT,-total_suicide_rate), y=total_suicide_rate)) +
      geom_bar(stat="identity")+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
      ggtitle("Statewise highest mean suicide rate (per 100K)")+
      labs(x = "State", y = "Mean suicide rate")
  })
  
  #plotting bottom 5 states
  output$bottom5<- renderPlot({
    agg1 <- aggregate(total_suicide_rate~STATE.UT, State_population2, mean)
    agg1 <-agg1[order(-agg1$total_suicide_rate),]
    ggplot(tail(agg1,5), aes(x=reorder(STATE.UT,total_suicide_rate), y=total_suicide_rate)) +
      geom_bar(stat="identity")+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
      ggtitle("Statewise lowest mean suicide rate (per 100K)")+
      labs(x = "State", y = "Mean suicide rate")
  })
})


# Run the application 
shinyApp(ui = ui, server = server)





