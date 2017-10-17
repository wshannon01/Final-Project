#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(reshape2)
library(ggmap)
library(chron)
library(leaflet)
#library(plotly)

# load data
# set directory

setwd("../data")
dat.15 <- read.csv("2015crimelog.csv", stringsAsFactor = FALSE)

dat.15 <- dat.15[!dat.15$Crime == "",]
crime.type <- unique(dat.15$Crime)
crime.type <- c("All Crime", crime.type)
unique.years <- c("2015", "2016", "2017")
unique.years <- as.numeric(unique.years)
pattern.type <- c("Time of Day", "Day of Week", "Month", "Season")

dat.15$time <- sapply(dat.15$Date.Time.Occurred, function(x) strsplit(as.character(x), " ")[[1]][2]) #time without AM, PM
dat.15$ap <- sapply(dat.15$Date.Time.Occurred, function(x) strsplit(as.character(x), " ")[[1]][3]) # AM, PM


dat.15$timeap <- paste(dat.15$time, dat.15$ap) # combine time and AM, PM
# convert to military time
dat.15$mtime <- substr(strptime(dat.15$timeap, "%I:%M %p"), 11,19) 
# remove white space
dat.15$mtime <- trimws(dat.15$mtime)

dat.15$ctime <- hour(hms(dat.15$mtime)) 
#dat.15$ttimes <- times(dat.15$mtime) 
# categorize times into time of day (tod)
tbreaks <- hour(hm("00:00", "5:00", "11:00", "17:00", "23:59"))
tlabels <- c("Night", "Morning", "Afternoon", "Evening")
#dat.15$tod <- cut(x=dat.15$ttime, breaks = tbreaks, labels = tlabels, include.lowest = TRUE)
dat.15$tod <- cut(x=dat.15$ctime, breaks = tbreaks, labels = tlabels, include.lowest = TRUE)

# table crimes by tod
crime.tod.15 <- as.data.frame(table(dat.15$Crime, dat.15$tod))

# date, day, month, quarter
dat.15$date <- sapply(dat.15$Date.Time.Reported, function(x) strsplit(as.character(x), " ")[[1]][1]) %>% mdy()
dat.15$day <- weekdays(dat.15$date)
dat.15$month <- months(dat.15$date)
dat.15$quarter <- quarters(dat.15$date)

dw15 <- as.data.frame(table(dat.15$Crime, dat.15$day))
dwm15 <- melt(dw15, c("Var1", "Var2"))
g.dwm15 <- dwm15 %>% group_by(Var2) %>% summarise(n = sum(value))
dg.dwm15 <- as.data.frame(g.dwm15) 
dg.dwm15$Var2 <- factor(dg.dwm15$Var2, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

dg.dwm15[order(dg.dwm15$Var2),]

dw15m <- as.data.frame(table(dat.15$Crime, dat.15$month))
dwm15m <- melt(dw15m, c("Var1", "Var2"))
g.dwm15m <- dwm15m %>% group_by(Var2) %>% summarise(n = sum(value))

dg.dwm15m <- as.data.frame(g.dwm15m) 
dg.dwm15m$Var2 <- factor(dg.dwm15m$Var2, levels = month.name)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Johns Hopkins East Baltimore Campus: Crime Analysis and Visualization"),
   
   ## Sidebar content
   tabPanel("Crime Trends",
            sidebarLayout(
              sidebarPanel(
                # menuItem("Widgets", tabName = "widgets", icon = icon("th")),
                selectInput("crime.type", "Crime Description", 
                            choices = crime.type),
                selectInput("year", label = "Year",
                                   choices = unique.years),
                selectInput("pattern", label = "Pattern",
                            choices = pattern.type)
                
              ),
              mainPanel( 
                fluidRow(column(plotlyOutput("plot1"), width = 11))
                #fluidRow(column(includeMarkdown('references.Rmd'), width = 11))
              )
            )
   ),
   tabPanel("Map",
            sidebarLayout(
              sidebarPanel(
                selectInput("crime.type.map", "Crime Description", 
                            choices = crime.type),
                selectInput("year.map", "Year",
                            choices = unique.years)
              ),
              mainPanel( 
                fluidRow(column(leafletOutput("mymap"), width = 11))
                #fluidRow(column(includeMarkdown('references.Rmd'), width = 11))
              )
            )
   )
   
)


# Define server logic
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    data <- dg.dwm15
    p <- ggplot(data, 
           aes(x = Var2, y = n)) +
      stat_smooth(se = F) + geom_point() + 
      scale_color_brewer(palette="Set1",
                         name = "Year") + 
      labs(title = paste('Total Crime Occurences by', input$pattern, 'in', input$year),
           x = "", y = "Occurrences") + geom_line(group = 1)
    ggplotly(p)
      #scale_x_continuous(breaks = 1:12, labels = month(1:12, label = T) ) +
      #fte_theme()
  })
  
  
  output$mymap <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      setView(lng=-76.591633, lat=39.2970515, zoom = 12) %>%
      addProviderTiles(providers$CartoDB.Positron)

  })
}

# Run the application 
shinyApp(ui = ui, server = server)

