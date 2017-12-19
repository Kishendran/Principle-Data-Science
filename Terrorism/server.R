#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
#terrorism_db<- read.csv("D://um master//sem 1//principle of data sc - wqd7001//Assignment//Assigment_new//globalterrorism.csv")
#install.packages("ggplot2")
#install.packages("rworldmap")
library(ggplot2)
# Define server logic to summarize and view selected dataset ----

#Cleaning data
#clean_data<- read.csv("D://um master//sem 1//principle of data sc - wqd7001//Assignment//Assigment_new//globalterrorism.csv") 
clean_data1 <- clean_data[clean_data$iyear > "2010",]
clean_data2<- subset(clean_data1, select = -c(10:135))
#clean_data3<- clean_data2[clean_data2$country_txt == "Canada",]
#clean_data4<- clean_data2[clean_data2$country_txt == "Malaysia",]
#clean_data5<- rbind(clean_data3,clean_data4)
clean_data3 <- subset(clean_data2, select = -c(3:7))
library(plyr)
clean_data3 <- rename(clean_data3, c("eventid"= "ID", "iyear"="Year","country"="Country_Code","country_txt"="Country"))
summary(is.na(clean_data3$eventid))
library(formattable)
clean_data3$ID <- formattable(clean_data3$ID, digits = 0, format = "f")  
clean_data2 <- clean_data3

server <- function(input, output) {
  
  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
 
  datasetInput <- reactive({
  input$country
 
  })
  
  # Create caption ----
  # The output$caption is computed based on a reactive expression
  # that returns input$caption. When the user changes the
  # "caption" field:
  #
  # 1. This function is automatically called to recompute the output
  # 2. New caption is pushed back to the browser for re-display
  #
  # Note that because the data-oriented reactive expressions
  # below don't depend on input$caption, those expressions are
  # NOT called when input$caption changes
  output$caption <- renderText({
    input$caption
  })
  
  # Generate a summary of the dataset ----
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated, i.e. whenever the input$dataset changes
  output$summary <- renderPrint({
    counts <- table(clean_data2$Country, clean_data2$Year)
    counts
    mapdf <- as.data.frame(counts) 
    mapdf <- na.omit(mapdf)
    names(mapdf)
    
    mapdf <- rename(mapdf, c("Var1"="Country", "Var2"="Year","Freq"="TotalAttacks"))
    mapdf<-ddply(mapdf,.(Country),summarize,TotalAttacks=sum(TotalAttacks))
   
    dataset <- mapdf
    dataset<-dataset[order(dataset$TotalAttacks,decreasing = TRUE),]
    
    dataset
  })
  
  # Show the first "n" observations ----
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so it will be re-executed whenever
  # input$dataset or input$obs is changed
  output$view <- renderTable({
    clean_data3<-clean_data2[grep(input$country,clean_data2$Country),]
    
    head(clean_data3, n = input$obs)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
  
      paste(input$country,".csv", sep = "")
    },
    content = function(file) {
      
      write.csv(clean_data3, file)
    }
  )
  
  overall_plot <- reactive({
    
   
    clean_data3<-clean_data2[grep(input$country,clean_data2$Country),]
    counts <- table(clean_data3$Country, clean_data3$Year)
    barplot(counts, main="Comparing No of Attacks(2011-2016)",
            xlab="Years", col=sample(colours(),200),
            legend = rownames(clean_data2$Country),ylim = c(0,20000))
    
  })
  
  
  output$barplot <- renderPlot({
    overall_plot()
  })
  
  output$map <- renderPlot({
    counts <- table(clean_data2$Country, clean_data2$Year)
    counts
    mapdf <- as.data.frame(counts) 
    mapdf <- na.omit(mapdf)
    names(mapdf)
    
    mapdf <- rename(mapdf, c("Var1"="Country", "Var2"="Year","Freq"="TotalAttacks"))
    mapdf<-ddply(mapdf,.(Country),summarize,TotalAttacks=sum(TotalAttacks))
    head(mapdf)
    
 
    library(rworldmap)
    n <- joinCountryData2Map(mapdf, joinCode="NAME", nameJoinColumn="Country")
    mapCountryData(n, nameColumnToPlot="TotalAttacks", mapTitle="Global Terrorism Attacks 2011-2016",
                   colourPalette ="rainbow", oceanCol = "sky blue", catMethod = "fixedwidth",missingCountryCol = "white", addLegend = TRUE)
  })
}