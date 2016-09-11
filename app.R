# Shiny App project for Developing Data Products Module
# Author: Zoe Lebourgeois
# Date: 05-SEP-2016

# This app is intended to display over time the popularity of baby names in the US.
# It has two tabs:
#  Overview: Plot of the top 5 names from each year plotted over the full time span
#  By Year:  Interactive plots of top boys and girls names for chosen year

library(shiny)
library(babynames)
library(dplyr)
library(tidyr)
library(ggplot2)

data <- babynames %>%
  filter(year > 1915)

top5perYear <-data %>%                          
  rename(nName = n) %>%
  rename(babyName = name) %>%
  arrange(desc(nName)) %>%
  group_by_(.dots=c("year","sex")) %>%
  filter(row_number() <= 5L) %>%
  arrange(year, sex, desc(nName))
#This gives same data as showen in this table https://www.ssa.gov/oact/babynames/top5names.html

rank <- top5perYear %>%
  mutate(ranking = (dense_rank(nName))) %>%
  mutate(babyName = factor(babyName), 
         babyName = factor(babyName, levels = sort(levels(babyName), decreasing = TRUE))) %>%
  mutate(perprop = prop/max(prop))

yrchoice <- ungroup(rank) %>%
              distinct(year)

choices = setNames(nm = yrchoice$year)

yrxlim <- sapply(yrchoice, as.character)

#Server starts here:
server <- function(input, output) {

  
#Capture selected year to display  
  output$year  <- renderText(input$year)
  output$babyName <- renderText(input$babyName)
  
  
  output$stats <- renderPrint({  
    top5sum <-  rank %>%
                 group_by(babyName) %>%
                 filter(babyName == input$babyName) %>%
                 summarise(Total_Years_in_Top_5 = n()) %>%
                 select(Total_Years_in_Top_5)
  
   top5year <- rank %>%
                 group_by(babyName) %>%
                 filter(babyName == input$babyName) %>%
                 slice(which.max(prop)) %>%
                 select(year)
  
  namesSum <- data.frame(Top_Year = top5year, Years_in_Top5 = top5sum)
  namesSum
  
  })

#This plot is a static visualisation of the raw data from R package babynames.
#The result displays the rankings of the top 5 male and female names each year.
#It is possible to see the popularity of each name over the last century
  output$plot1 <- renderPlot({
    
    #Plot rankings
    p <- ggplot(rank, aes(year, babyName, group = 1))
    p <- p + geom_point(aes(size = ranking, colour = sex))
    p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n = 20))
    p <- p + scale_color_manual(breaks = c("F", "M"), values=c("hot pink", "blue"))
    p <- p + labs(x = "Year", y = "Baby Name")
    p

  })

#This plot interacts with the selected year to show the top ten female names of that year    
  output$plot2 <- renderPlot({
    
    yrrankF <-  rank %>%
      filter(year == input$year, sex == "F") %>%
      arrange(desc(ranking))
    
    ypf <- ggplot(yrrankF, aes(x = reorder(babyName, ranking), y = ranking, label=babyName))             
    ypf <- ypf + geom_bar(stat = "identity", fill="hot pink")
    ypf <- ypf + geom_text(hjust= 1.2)
    ypf <- ypf + labs(x = "Baby Name", y = "Ranking")
    ypf <- ypf + theme_classic()
    ypf <- ypf + theme(legend.position = "none", axis.text.y = element_blank())
    ypf <- ypf + coord_flip()
    ypf
  })
  
  #This plot interacts with the selected year to show the top ten male names of that year     
  output$plot3 <- renderPlot({
    
    yrrankM <-  rank %>%
      filter(year == input$year, sex == "M") %>%
      arrange(desc(ranking))

    ypm <- ggplot(yrrankM, aes(x = reorder(babyName, ranking), y = ranking, label=babyName))             
    ypm <- ypm + geom_bar(stat = "identity", fill="blue")
    ypm <- ypm + geom_text(hjust= 1.2, colour = "white")
    ypm <- ypm + labs(x = "Baby Name", y = "Ranking")
    ypm <- ypm + theme_classic()
    ypm <- ypm + theme(legend.position = "none", axis.text.y = element_blank())
    ypm <- ypm + coord_flip()
    ypm
  })
  
  output$plot4 <- renderPlot({
    
    sng <- ungroup(rank) %>%
      filter(babyName == input$babyName)
    
    sngP <- ggplot(sng, aes(factor(year), prop, group = 1)) +
      geom_line(aes(size = 2, colour = "red", linetype = "dotted")) +
      geom_point(aes(size = 2, colour = "red")) +
      expand_limits(x=factor(yrxlim)) +
      #ylim(c(0, 1)) +
      scale_x_discrete(breaks = scales::pretty_breaks(n = 10)) +
      labs(title = "Relative popularity of name over time") +
      theme(legend.position = "none", axis.text.y = element_blank())
    sngP
  })
}


#UI starts here: 
ui <- fluidPage(tabsetPanel(

  tabPanel("By Year",
# Use tag$Style to hide red error messages if user selects unknown name           
           tags$style(type="text/css",
                      ".shiny-output-error { visibility: hidden; }",
                      ".shiny-output-error:before { visibility: hidden; }"
           ),
           fluidRow(
             column(width = 2,
                    h4("Pick a year"),
                    selectInput("year", "Year", choices = choices)
             ),
             column(width = 10,
                    h4("Most popular baby names for ", strong(textOutput("year", inline = TRUE)))
             ),        
             column(width = 5,

                    h3("Girls"),
                    plotOutput("plot2")
             ),
             column(width = 5, 
                    h3("Boys"),
                    plotOutput("plot3")
             ),
          fluidRow(
             column(width = 2,
                    textInput("babyName", "Enter a name", "Mary"),
                    h6("Interesting names: Michael, David, Shirley")
             ),
             column(width = 10,
                    h4("Summary statistics for ", strong(textOutput("babyName", inline = TRUE))), 
                    verbatimTextOutput("stats"),
                    plotOutput("plot4")
             )        
             )
           )
  ),
  tabPanel("Overview",
           fluidRow(
             column(width = 12,
                    h1("Popularity of Baby Names 1914-2015"),
                    h5("Data: ", a(href = "https://www.ssa.gov/oact/babynames/top5names.html", "US Social Security Administration"), 
                       ". For the purposes of this report - the data was loaded using ", a(href = "https://github.com/hadley/babynames", "R package/babynames")),
                    h5("Click on the ", strong("By Year tab"), "above to interact with the data")
             )
           ),
           fluidRow(
             column(width = 12,
                    plotOutput("plot1", height = 850)
             )
           )
  )
))

shinyApp(ui, server)

