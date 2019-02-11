
library(shiny)
library(gtable)
library(grid)
library(ggplot2)
library(dplyr)
library(leaflet)
library(DT)
library(markdown)
library(knitr)
library(shinythemes)
library(plotly)
library(readr)
library(gridExtra)
library(rstan)
library(shinystan)
library(rstanarm)

# Set up the application ui
shinyUI(navbarPage("Forecast Explorer Options",
                       theme = shinytheme("flatly"),
                       
                       # define the tabs to be used in the app ----------------------------------------
                       # introduction splash
                       tabPanel("Intro",
                                h1("Exploring visitation forecasts for U.S. National Parks"),
                                br(),
                                h2("Introduction"),
                                p("This application allows users to compare two different types of predictive models for visitation forecasting in U.S.
                                  National Parks. The first model (purple) uses values from", tags$a(href = "https://trends.google.com/trends/?geo=US","Google Trends"),  ", a free Google service for tracking the search popularity of specific topics, to predict visitation.
The second model (red) uses previous visitation to predict future visitation, this is referred to as autoregressive forecasting. Users can examine the accuracies of both of these models for specific parks and explore how varying values for 
                                  previous visitation and Google Trends affect model projections. This application is designed to complement a peer reviewed publication titled,", tags$em("Bringing forecasting into the future: Using Google to predict visitation in U.S. National Parks."), "Access this paper here (LINK)"),
                                br(),
                                h2("Overview"),
                                p("The plots below show the accuracy of all models for all years. These are designed to display the total accuracy for each model. The X axis shows the real-world visitation to each park, while the Y axis shows the predicted visitation from each model.
Both models were created using a Bayesian framework fit to a negative binomial distribution. Model outputs are informed only by values up to one year before the year being predicted, e.g. the prediction for a given park for 2015 is informed by Google Trends
 values and previous visitation for that park up to 2014 for the Google Trends and autoregressive models respectively.
                                  Hover your cursor over the points to explore for which park and which year you are looking at. X and Y values for each point are also displayed."),
                                br(),
                                fluidRow(
                                  
                                  column(6,plotlyOutput("glR2")),
                                  column(6,plotlyOutput("a5R2"))
                                ),
                                br(),
                                h2("Authorship and contact"),
                                p("This application was created by Matt Clark. Please direct all contact to: matthewclark989@u.boisestate.edu."),
                                br(),
                                p("This project was initiated as part of the Park Break program of the George Wright Society. Support for this project came from the George Wright Society, the National Park Service, and NSF Idaho EPSCoR Award No. IIA-1301792.
                                "),
                                br(),
                                p("Special thank you to Emily Wilkins, Dani Dagan, Robert Powell, Ryan Sharp, and Vicken Hillis for their collaboration on this research project."),
                                hr()),
                       
                       # Model Validation
                       tabPanel("Model validation",
                                # plot the map
                                fluidRow(column(12,
                                                h1("Model validation"),
                                                p("Tool for comparing accuracy of all model types for each park. Credibility intervals (C.I.) represent the inner 25th and 50th percentiles of 2000 visitation predictions for each park. In Bayesian statistics this is referred to as the posterior distribution."),
                                                br(),
                                                strong("Visitation estimates are informed only by data prior to each estimated year."))),
                                
                                hr(),
                                fluidRow(sidebarPanel(width = 3,
                                                      
                                                      helpText("Which park do you want to look at?"),
                                                      selectInput(inputId = "Type", label = "Choose Park",choices=c("ACAD", "ARCH" ,"BADL", "BIBE" ,"BISC" ,"BLCA", "BRCA", "CANY" ,"CARE", "CAVE", "CHIS", "CONG", "CRLA" ,"CUVA", "DEVA", "DENA", "DRTO" ,"EVER" ,"GAAR" ,"GLBA", "GLAC", "GRCA",
                                                                                                                    "GRTE", "GRBA", "GRSA", "GRSM", "GUMO" ,"HALE" ,"HAVO", "HOSP", "ISRO" ,"JOTR", "KATM" ,"KEFJ", "KICA", "KOVA", "LACL", "LAVO" ,"MACA" ,"MEVE", "MORA" , "NOCA", "OLYM",
                                                                                                                    "PEFO", "REDW", "ROMO" ,"SAGU", "SEQU", "SHEN", "THRO", "VIIS", "VOYA", "WICA", "WRST", "YELL" ,"YOSE" ,"ZION"),selected = "CUVA")),

                                         mainPanel(plotOutput("myplot",width="800px",height = "600px")))
                       ),
                       
                       
                       #width="1200px",height = "550px"
                       
                       # forecasting tool
                       tabPanel("Forecast explorer",
                                fluidRow(column(12,
                                                h1("Forecast explorer"),
                                                p("This tool is designed to let users explore how varying inputs affect the visitation predictions produced by each model. We provide this interface as a mechanism to increase understanding of our study only. All visitation projections should be taken with a hefty dose of skepticism. With that being said: start forecasting!"),
                                                br(),
                                                h3("Instructions"),
                                                p("This tool allows you to predict the number of visitors to any specific US National Park as a function of two different predictor variables, either the number of visitors in the previous five years, or the amount of Google search traffic in the previous year. Each model type uses one of those different predictors. First, pick a model type. 
                                                  Then pick a specific national park. Then set the value(s) of the predictor(s) for the model you chose." ))),
                                hr(),
                                fluidRow(sidebarPanel(width = 3,
                                                      h4("Model type"),
                                                      helpText("Chose which type of model you want to inform your predictions."),
                                                      selectInput(inputId = "ModelType",label = "Model Type",choices=c("Autoregressive Forecast", "Google Trends Forecast"),selected="Google Trends Forecast"),
                                                      selectInput(inputId = "Park", label = "Choose Park",choices=c("ACAD", "ARCH" ,"BADL", "BIBE" ,"BISC" ,"BLCA", "BRCA", "CANY" ,"CARE", "CAVE", "CHIS", "CONG", "CRLA" ,"CUVA", "DEVA", "DENA", "DRTO" ,"EVER" ,"GAAR" ,"GLBA", "GLAC", "GRCA",
                                                                                                                    "GRTE", "GRBA", "GRSA", "GRSM", "GUMO" ,"HALE" ,"HAVO", "HOSP", "ISRO" ,"JOTR", "KATM" ,"KEFJ", "KICA", "KOVA", "LACL", "LAVO" ,"MACA" ,"MEVE", "MORA" , "NOCA", "OLYM",
                                                                                                                    "PEFO", "REDW", "ROMO" ,"SAGU", "SEQU", "SHEN", "THRO", "VIIS", "VOYA", "WICA", "WRST", "YELL" ,"YOSE" ,"ZION")), 
                                                      conditionalPanel(condition="input.ModelType == 'Google Trends Forecast'",
                                                                       sliderInput(inputId ="Lag12G", label = "Yearly total Google Trends value (sum of all months) last calendar year", min = 100, step=50,max = 1000, value = 300)),
                                                      conditionalPanel(condition="input.ModelType == 'Autoregressive Forecast'",
                                                                       numericInput(inputId = "a5Vis1", label = "Last year's total visitation", value = 1000000),
                                                                       numericInput(inputId = "a5Vis2", label = "Two years ago total visitation", value = 1000000),
                                                                       numericInput(inputId = "a5Vis3", label = "Three years ago total visitation", value = 1000000),
                                                                       numericInput(inputId = "a5Vis4", label = "Four years ago total visitation", value = 1000000),
                                                                       numericInput(inputId = "a5Vis5", label = "Five years ago total visitation", value = 1000000))),
                                         mainPanel(plotOutput("Forecast", height = 500)))
                       ),
                       
                       
                       # simple data table output
                       tabPanel("Error metrics by park",
                                column(12,
                                       h1("Error Metrics by Park"),
                                       p("This tab shows the R", tags$sup("2"),", mean percent difference, and mean absolute error (MAE) for the median visitation prediction for each park and the observed visitation for each park. The overall error estimates for each model are also displayed"
                                       ),
                                       column(10, dataTableOutput("table", height = "100%")))),
                       
                       
                       #Raw data
                       tabPanel("Data",
                                column(12,
                                       h1("Data"),
                                       p("This is the complete dataframe used to inform these models. All park visitation data was collected from the", tags$a(href="https://irma.nps.gov/Stats/","National Park Visitor Use Statistics Portal"),". The VisLag variables represent the visitation to each park
                                         1:5 years prior to the Year column."),
                                       
                                       column(10, dataTableOutput("data", height = "100%")))),
                   #Key and pop
                   tabPanel("Unit code key & population data",
                            column(12,
                                   h1("Unit Code Key & Population Data"),
                                   p("Key for park unit codes used in this application and the accompanying paper. We also show the population within 50 miles surrounding each park. These data are used in an exploratory analysis in the accompanying paper, but were not used to develop this application. "),
                                   
                                   column(10, dataTableOutput("Codes", height = "100%")))),
                   #Code
                   navbarMenu("Code for this application",
                              tabPanel("User Interface",
                                       h1(tags$a(href = "https://github.com/matthewclark1223/GT-Forecasting-Paper/blob/master/ui.R", "Click here!"))),
                              tabPanel("Server",
                                       h1(tags$a(href = "https://github.com/matthewclark1223/GT-Forecasting-Paper/blob/master/server.R", "Click here!"))))
                       
                       # close the UI definition
                       ))
