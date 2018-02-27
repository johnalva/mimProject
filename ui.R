library(shiny)
library(readxl)
library(lubridate)
library(plotly)
library(plyr)
library(DT)
library(reshape)
library(caret)

options(shiny.maxRequestSize=90*1024^2) 

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("MIM FTE Calculation"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            fluidRow(
                column(6,
                       fileInput("mimDoc", "Please choose your file", accept = "xlsx")),
                column(6,
                       dateRangeInput("date", start = '2017-07-01', end = Sys.Date(),
                                      "Range of Dates to compare"))),
            fluidRow(
                column(6, selectInput("inputAccount", "Account", choices = "")),
                column(6, selectInput("inputSeverity", "Severity", choices = ""))
            ),
            
            fluidRow(
                column(6, selectInput("inputProblem", "Problem ID", choices = "")),
                column(6, selectInput("inputDM", "DM Assignee", choices = ""))
            ),
            
            fluidRow(
                column(6, selectInput("inputCreated", "Created Time (mm/dd/yy)", choices = ""),
                       helpText("Please use format mm/dd/yyyy hh:mm:ss")),
                column(6, selectInput("inputSolved", "Solved Time", choices = ""),
                       helpText("Please use format mm/dd/yyyy hh:mm:ss"))
            ),
            
            # fluidRow(
            #     column(6, actionButton("do", "Upload File"))
            # ),

            fluidRow(
                column(3,
                       numericInput("base", "Hours per month:",160)),
                column(3,
                       numericInput("percentage", "Utilization % FTE", 80)),
                column(3,
                       selectInput("frequencyDate", "Frequency", 
                                   choices = c("Month", "Week", "Day", "Hour"),
                                   selected = "Week"))),
            fluidRow(
                actionLink("selectall","Select All")), 
            fluidRow(
                column(4,
                       checkboxGroupInput("accountGroups", label = "Select Account(s)")),
                column(4,
                       checkboxGroupInput("impact", "Impact")),
                column(4,
                       checkboxGroupInput("severity", "Severity"))
                
                #choices = c("ALL"))
            )),
        
        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("summaryAccount"),
            fluidRow(
                column(4,
                       tableOutput("impactTable")),
                column(4,
                       tableOutput("severityTable"))),
            tabsetPanel(type = 'tabs',
                        tabPanel("Summary", dataTableOutput("contents")),
                        tabPanel("Tickets", dataTableOutput("tickets")),
                        tabPanel("Head Count", dataTableOutput("headCountTable")),
                        tabPanel("Graphics", plotlyOutput("accountBar"),
                                hr(),
                                plotlyOutput("accountBarTrend"),
                                plotlyOutput("trend")),
                        tabPanel("Volume %",
                                fluidRow(
                                column(12,offset = 4,
                                    plotlyOutput("relativeAccount"))),
                                fluidRow(
                                column(6,
                                       plotlyOutput("relativeImpact")),
                                column(6,
                                    plotlyOutput("relativeSeverity")))))
        )
    )
)
)
