#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Shiny App Assignment"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Text for providing a caption ----
      # Note: Changes made to the caption in the textInput control
      # are updated in the output area immediately as you type
      textInput(inputId = "caption",
                label = "Caption:",
                value = "Data Summary"),
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "country",
                  label = "Choose a country:",
                  choices = clean_data2$Country),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10),
      # Button
      downloadButton("downloadData", "Download")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      h3(textOutput("caption", container = span)),
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("view")),
                  tabPanel("Plot", plotOutput("barplot")),
                  tabPanel("Map",plotOutput("map")))
      
      )
    )
)