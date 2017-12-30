library(shiny)
library(tidyverse)
library(jsonlite)
library(stringr)
library(lubridate)

# JSON file from the Partnership for Public Service
json_url <- "https://www.washingtonpost.com/graphics/politics/trump-administration-appointee-tracker/data/appointees_pps.json"

# Convert JSON to dataframe
app <- fromJSON(json_url, simplifyDataFrame=T)
app$type <- ifelse(app$status=="", "Empty", app$status)
today <- as.character(Sys.Date())
app$final_action_date <- as.character(mdy(app$final_action_date))
app$final_action_date <- ifelse((is.na(app$final_action_date) & app$announcement_date!=""), today, app$final_action_date)
app$announcement_date <- mdy(app$announcement_date)
app$final_action_date <- ymd(app$final_action_date)
app$length <- interval(app$announcement_date, app$final_action_date)/ddays(1)

# Extracting the different appointment types
types <- unique(app$type)

# Setting up a color palette 
type_colors <- c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0','#f0027f','#bf5b17')
names(type_colors) <- types

shinyServer(function(input, output) {

  # Label based on toggle choice  
  output$type_text <- renderText({
    
    ifelse(input$total_percent==T, "Total appointments by type", "Percent of appointments by type")
    
  })
  
  # chart output if/else depending on total or percent toggle
  output$top_chart <- renderPlot({
    
    if (input$total_percent==T) {
      app %>% 
        group_by(organization, type) %>% 
        summarize(Total=n()) %>% 
        mutate(Sum=sum(Total, na.rm=T)) %>% 
        arrange(Sum) %>% 
        ungroup() %>% 
        filter(type %in% input$myPicker) %>% 
        ggplot(aes(x=reorder(organization, Sum), y=Total)) + 
        geom_bar(aes(fill=type), stat="identity", position="stack") +
        coord_flip() + 
        labs(x="Agency", y="Total") +
        theme(axis.text=element_text(size=12)) +
        scale_fill_manual(values = type_colors)
    } else {
      app %>% group_by(organization, type) %>% 
        summarize(Total=n()) %>% 
        mutate(Sum=sum(Total, na.rm=T), Total=round(Total/Sum*100,2)) %>% 
        arrange(Sum) %>% 
        ungroup() %>% 
        filter(type %in% input$myPicker) %>% 
        ggplot(aes(x=reorder(organization, Sum), y=Total)) + 
        geom_bar(aes(fill=type), stat="identity", position="stack") +
        coord_flip() + 
        labs(x="Agency", y="Percent") +
        theme(axis.text=element_text(size=12)) +
        scale_fill_manual(values = type_colors)
      
      
    }
  })
  
  # table output if/else depending on total or percent toggle
  
  output$top_table <- renderDataTable(
    if (input$total_percent==T) {
      
      app %>% group_by(organization, type) %>% 
        summarize(Total=n()) %>% 
        mutate(`Total positions`=sum(Total, na.rm=T)) %>% 
        arrange(`Total positions`) %>% 
        ungroup() %>% filter(type %in% input$myPicker) %>% 
        spread(type, Total) %>% arrange(-`Total positions`)
      
    } else {
      app %>% group_by(organization, type) %>% 
        summarize(Total=n()) %>% 
        mutate(`Total positions`=sum(Total, na.rm=T), Total=round(Total/`Total positions`*100,2)) %>% 
        arrange(`Total positions`) %>% 
        ungroup() %>% filter(type %in% input$myPicker) %>% 
        spread(type, Total) %>% arrange(-`Total positions`)
      
    }
    , 
    extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      pageLength=200,
      
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  
  # tab for specific agency tables
  output$agency_table <- 
    
    renderDataTable(
      app %>% 
        filter(organization %in% input$agency_input) %>% 
        select(organization, position, nominee, announcement_date, days=length, type, prior=nominee_previously_held_senate_confirmed_position, `Position Overview`)
      , 
      extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        pageLength=200,
        
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ))
  
  # tab for specific agency tables (filtered)
  
  output$agency_filtered <- 
    
    renderDataTable(
      app %>% 
        filter(length>=100 & type!="Confirmed") %>% 
        select(organization, position, nominee, announcement_date, days=length, type, prior=nominee_previously_held_senate_confirmed_position, `Position Overview`)
      , 
      extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        pageLength=200,
        
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ))
  
  # determining the number of appointees who've been waiting longer than 100 days
  
  output$waiting <- 
    
    renderText(
      
      nrow(app %>% 
             filter(length>=100 & type!="Confirmed"))
    )
  
})
