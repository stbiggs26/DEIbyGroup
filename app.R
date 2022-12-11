#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(tidyverse)
library(caret)
library(dplyr)
library(ModelMetrics)
library(cluster)
library(factoextra)
library(googlesheets4)
library(rintrojs)
library(shinyjs)
library(bslib)

gs4_deauth()
ss='https://docs.google.com/spreadsheets/d/1_XYP2ObWobVR-9zq4WR1d5aVbcq_PzbdmxcEnRSrknY/edit#gid=615186630'
DEI_dataset = read_sheet(ss)

#change first column to row names 
DEI_dataset2 = DEI_dataset %>% remove_rownames %>% column_to_rownames(var="Division")

#get rid of outliers
Q <- quantile(DEI_dataset2$Percent.Engaged, probs=c(.25, .75))
iqr <- IQR(DEI_dataset2$Percent.Engaged)
eliminated = subset(DEI_dataset2, DEI_dataset2$Percent.Engaged > (Q[1] - 1.5*iqr) & DEI_dataset2$Percent.Engaged < (Q[2]+1.5*iqr))

Q <- quantile(DEI_dataset2$DEI.Programming.Percentage, probs=c(.25, .75))
iqr <- IQR(DEI_dataset2$DEI.Programming.Percentage)
eliminated = subset(eliminated, eliminated$DEI.Programming.Percentage > (Q[1] - 1.5*iqr) & eliminated$DEI.Programming.Percentage < (Q[2]+1.5*iqr))

#Scale data
scale_df = scale(eliminated)

#create data frame
data_frame = as.data.frame(scale_df)


##create UI
ui = navbarPage(title="DEI Programming Effectiveness", fluid=TRUE, collapsible = TRUE, theme = bs_theme(bg = "#F4EAB7", fg = "#051442", primary = "#F3D230", secondary = "#808AAC"),
                tabPanel("HOME", value="home",
                         shinyjs::useShinyjs(),
                         
                         #WHAT IS THIS APP?
                         fluidRow(
                           column(3),
                           column(6,
                                  shiny::HTML("<br><br><center> <h1>What does this app do?</h1> </center><br>"),
                                  shiny::HTML("<h5>This app will demonstrate the relationship, however it exists, between three variables (measurements)
                                              associated with each group on campus. The three variables are the proportion of individuals in each group 
                                              who have attended DEI programming, the Respect and Fairness Index in our most recent campus-wide survey, and 
                                              level of engagement in each group.</h5>")
                           ),
                           column(3)
                         ),
                         fluidRow(
                           
                           style = "height:50px;"),
                         
                         # PAGE BREAK
                         tags$hr(),
                         
                         #TAB DEFINITION?
                         fluidRow(
                           column(3),
                           column(6,
                                  shiny::HTML("<br><br><center> <h1>What can I do under each tab?</h1> </center><br>"),
                                  shiny::HTML("<h5>There are two tabs: 'DEI Data and Correlation' and 'Cluster Analysis.' The Data and Correlation 
                                              tab will allow the user to toggle between two dependent variables (Level of Engagement and Respect
                                               and Fairness Index), compared to a single independent variable (DEI Programming Proportion). You can
                                              visualize this comparison in a scatterplot, draw a regression line, and view the correlation. You can 
                                              also print out a table of the raw numbers. The second tab will allow the user to toggle between different 
                                              k clusers in order to find relationships between each of the groups on campus, based on all three of the 
                                              variables, taken together.</h5>")
                           ),
                           column(3)
                         ),
            
                         fluidRow(
                           
                           style = "height:50px;"),
                         
                         # PAGE BREAK
                         tags$hr(),
                         
                         #AUTHOR?
                         fluidRow(
                           column(3),
                           column(6,
                                  shiny::HTML("<br><br><center> <h1>Who is the author?</h1> </center><br>"),
                                  shiny::HTML("<h5>The author of this application is Sarah Biggs, who works as a Human Resources Consultant. 
                                              This is her first semester in the Data Science program at IU Bloomington. She loves 
                                              data storytelling and is, admittedly, new to the web development experience but really enjoys
                                              the capabilities that Shiny offers.</h5>")
                           ),
                           column(3)
                         ),
                         fluidRow(
                           
                           style = "height:50px;"),
                         ),
                tabPanel("DEI Data and Correlation", value="dataTab",
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("dependent_variable","Choose your dependent variable:",
                                         c("Respect & Fairness Index"="Respect.and.Fairness.Index.Score",
                                           "Level of Engagement" = "Percent.Engaged"), 
                                         selected = "Respect & Fairness Index"),
                              #check  box to add regression line and correlation, 
                              checkboxInput("regression_line", "See regression line and correlation:", FALSE),
      
                             #show data set, check boxes for variables, 
                             checkboxGroupInput("data_table", "Check which variables you'd like to see as a data table:", 
                                               c("Respect & Fairness Index"="Respect.and.Fairness.Index.Score",
                                                 "Level of Engagement" = "Percent.Engaged", 
                                                 "DEI Program Attendance Rate" = "DEI.Programming.Percentage"))),
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Scatterplot and Correlation",
                             plotOutput("scatterplot"),
                             br(),
                             uiOutput("correlation")),
                             tabPanel("Data Table",
                             DT::dataTableOutput("data")))
                           ))),
                tabPanel("Cluster Analysis", value="clusterTab",
                           sidebarLayout(
                             sidebarPanel(
                               #cluster analysis
                               radioButtons("cluster_analysis", "Choose the amount of clusters you'd like to see:", c("2", "3", "4"),
                                         selected = "3")),
                             mainPanel(
                               plotOutput("cluster")
                             ))))


##create Server
server = function(input, output){
  output$scatterplot = renderPlot({
    p1=ggplot(data = data_frame, aes_string(x = data_frame$DEI.Programming.Percentage, y = input$dependent_variable))+geom_point()+xlab("DEI Programming Proportion")+ ggtitle("DEI Programming Scatterplot")
    p2 = {if(input$regression_line)
      p1+geom_smooth(method="lm", se=FALSE)
      else(p1)}
    print(p2)
  }) 
  
  #correlation  
  output$correlation = renderUI({
    req(input$regression_line)
    corr= cor(x=data_frame$DEI.Programming.Percentage, y = data_frame[,input$dependent_variable])
    
    HTML(paste("The correlation between DEI Programming and ", 
               input$dependent_variable, " is ", corr))
  })
  
  #data table  
  DEI_subset = reactive({
    DEI_dataset2[c(input$data_table)]
  })
  output$data = DT::renderDataTable({
    req(input$data_table)
    table = {
      DT::datatable(data = DEI_subset(), options=list(pageLength=10))}
    print(table)
  })
  
  #cluster analysis
  cluster_input = reactive({
    req(input$cluster_analysis)
    strtoi(input$cluster_analysis)
  })
  output$cluster = renderPlot({
    final_k = kmeans(scale_df, cluster_input(), nstart=25)
    fviz_cluster(final_k, data=scale_df)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
