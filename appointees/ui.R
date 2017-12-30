# Loading libraries
library(tidyverse)
library(shiny)
library(stringr)
library(shinythemes)
library(shinyWidgets)
library(jsonlite)

library(DT)

json_url <- "https://www.washingtonpost.com/graphics/politics/trump-administration-appointee-tracker/data/appointees_pps.json"

# Convert JSON to dataframe
app <- fromJSON(json_url, simplifyDataFrame=T)

# Extracting a list of organizations
agencies <- sort(unique(app$organization))

# Define UI for application 

# Using the flatly theme
shinyUI(fluidPage(theme=shinytheme("flatly"),

                  # Inserting the Washington Post Investigative logo
                  list(tags$head(HTML('<link rel="icon", href="https://avatars3.githubusercontent.com/u/29076131?s=30&v=4", 
                                   type="image/png" />'))),
                  div(style="padding: 1px 0px; width: '100%'",
                      titlePanel(
                        title="", windowTitle="Political Appointees Explorer"
                      )
                  ),
                  navbarPage(
                    title=div(img(src="https://avatars3.githubusercontent.com/u/29076131?s=30&v=4"), "Political Appointees Explorer"),
                    #title="Politicals",
                             tabPanel("All",
                                      tabsetPanel(
                                        
                                        # Sidebar with a toggle button for total or percent 
                                        sidebarLayout(
                                          sidebarPanel(
                                            h4("Toggle total or percent"),
                                            switchInput(inputId = "total_percent", onLabel="Total", offLabel="Percent"),
                                            p(""),
                                            # Pull-down menu to allow multiple choices for filtering appointment statuses
                                          
                                            h4("Pick status"),
                                            pickerInput(
                                              inputId = "myPicker", 
                                              label = NULL, 
                                              choices = c("Empty", "Awaiting full Senate action", "Confirmed", "Confirmed and Resigned",  "Referred to committee ", "Withdrawn by president", "Pending formal withdrawal by president "), 
                                              selected = "Empty",
                                              options = list(
                                                `actions-box` = TRUE, 
                                                size = 9,
                                                `selected-text-format` = "count > 3"
                                              ), 
                                              multiple = TRUE
                                            )
                                            
                                          ),
                                          mainPanel(
                                            h3(textOutput("type_text")),
                                            tabsetPanel(
                                              tabPanel("Chart", 
                                                       plotOutput("top_chart", height="1100px")),
                                              tabPanel("Table",
                                                       
                                                       dataTableOutput("top_table")
                                                       
                                              )
                                              
                                            )
                                          )
                                          
                                          

                                        )
                                        
                                      )),
                    # Tab to explore specific positions by agency
                               tabPanel("Agency",
                                      tabsetPanel( 
                                        sidebarLayout(
                                          sidebarPanel(
                                            h4("Pick agency"),
                                            # Allow option to choose more than one agency
                                            pickerInput(
                                              inputId = "agency_input", 
                                              label = NULL, 
                                              choices = agencies, 
                                              selected = "Department of State",
                                              options = list(
                                                `actions-box` = TRUE, 
                                                size = 9,
                                                `selected-text-format` = "count > 3"
                                              ), 
                                              multiple = TRUE
                                            )
                                          ),
                                          
                                          # Show a plot of the generated distribution
                                          mainPanel(
                                            dataTableOutput("agency_table")
                                          ))
                                      )),
                            # Tab to focus on appointees who've been waiting longer than 100 days
                             tabPanel("100 Days",
                                      mainPanel(
                                        h4("Nominees waiting more than 100 days for confirmation: "),
                                        h3(textOutput("waiting")),
                                        p(),
                                        dataTableOutput("agency_filtered")
                                      )),
                             # About page tab
                             tabPanel("About",
                                      mainPanel(
                                        h3("What is this?"),
                                        p(),
                                        p("It's a Shiny app to explore different Senate-approved appointees by agency."),
                                        p("Built by Andrew Ba Tran on the Washington Post's Investigative Team."),
                                        p("Data from: "),
                                        a(href="https://www.washingtonpost.com/graphics/politics/trump-administration-appointee-tracker/database/", "Tracking how many key positions Trump has filled so far")))
                  )
                  
))

#https://avatars3.githubusercontent.com/u/29076131?s=200&v=4