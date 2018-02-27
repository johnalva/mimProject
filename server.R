library(shiny)
library(readxl)
library(lubridate)
library(plotly)
library(ggplot2)
library(plyr)
library(DT)
library(reshape)
library(caret)

#setwd("C:/Users/johnalva/Box Sync/My Documents/Proyectos/R Projects/MIMProjectV2")
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    # Source patch
    #setwd("C:/Users/johnalva/Box Sync/My Documents/Proyectos/R Projects/mim/ProjectMIM.R")
    #setwd("C:/Users/johnalva/Box Sync/My Documents/Proyectos/R Projects/mim")
    source("ProjectMIM.R")
    # Load dataframe from excel file
    reactive({
        mimFile <<- read_excel(paste(input$mimDoc$datapath, ".xlsx", sep=""), 1)
    })
    
    # Validate and load excel file
    mimFile <- reactive({
        validate(
            need(input$mimDoc != "", "Pending Raw Data")
        )
        inFile <- input$mimDoc
        if (is.null(inFile)){
            return(NULL)
        }
        read_excel(paste(inFile$datapath, sep = ""),1)
    })
    
observe({
    
    updateSelectInput(session = session,
                      inputId = "inputAccount",
                      label = "Account",
                      choices = sort(pChar(mimFile())),
                      selected = "ALIAS")
    
    updateSelectInput(session = session,
                      inputId = "inputSeverity",
                      label = "Severity",
                      choices = sort(pNum(mimFile())),
                      selected = "SEVERITY")
    
    updateSelectInput(session = session,
                      inputId = "inputProblem",
                      label = "Problem ID",
                      choices = sort(pChar(mimFile())),
                      selected = "PROBLEM_ID")
    
    updateSelectInput(session = session,
                      inputId = "inputDM",
                      label = "DM Assignee",
                      choices = sort(pChar(mimFile())),
                      selected = "DM_ASSIGNEE")
    
    updateSelectInput(session = session,
                      inputId = "inputCreated",
                      label = "Created Time",
                      choices = sort(pos(mimFile())),
                      selected = "CREATED_TIME")
    
    updateSelectInput(session = session,
                      inputId = "inputSolved",
                      label = "Solved Time",
                      choices = sort(pos(mimFile())),
                      selected = "SOLVED_TIME")
    
})

#observeEvent(input$do,{
    
    # Convert excel file to a data frame
    mimDf <- reactive({
        validate(
            need(input$inputAccount != "", "Account in blank"),
            need(input$inputSeverity != "", "Severity in blank"),
            need(input$inputProblem != "", "Problem ID in blank"),
            need(input$inputDM != "", "DM Assignee in blank"),
            need(input$inputCreated != "", "Created Time in blank"),
            need(input$inputSolved != "", "Solved Time in blank"),
            need(input$inputAccount != input$inputDM, "Account = DM Assignee"),
            need(input$inputAccount != input$inputProblem,"Account == Probem"),
            need(input$inputCreated != input$inputSolved, "Created = Solved"),
            need(input$inputProblem != input$inputDM, "Problem = DM Assignee"),
            need(input$date[1] <= input$date[2], "Range of dates selected invalid")
        )

        nN <- newNames(x = mimFile(),
                 account = input$inputAccount, severity = input$inputSeverity,
                 problemId = input$inputProblem, dmAssignee = input$inputDM,
                 cratedTime = input$inputCreated, SolveTime = input$inputSolved)
        
        #validate(need(as.character(class(nN$CREATED_TIME)) %in% ct, "Invalid Format"))
        as.data.frame(nN)
        # as.data.frame(mimFile())
    })
    
    # Waiting for changes in the check box inputs
    observe({
        if(input$selectall == 0) return(NULL)
        else if (input$selectall%%2 == 0)
        {
            updateCheckboxGroupInput(session, "accountGroups",
                                     label = paste("Select Accounts",
                                                   length(unique(newdf()$ALIAS))),
                                     choices = unique(newdf()$ALIAS))
        }
        else{
            updateCheckboxGroupInput(session, "accountGroups",
                                     label = paste("Select Accounts",
                                                   length(unique(newdf()$ALIAS))),
                                     choices = unique(newdf()$ALIAS),
                                     selected = unique(newdf()$ALIAS))
        }
    })

    observe({
        # New observe to update the impact
        updateCheckboxGroupInput(session, "impact",
                                 label = "Select Impact",
                                 choices = sort(unique(newdf()$Impact),decreasing = FALSE),
                                 selected = unique(newdf()$Impact))

        updateCheckboxGroupInput(session, "severity",
                                 label = "Select Severity",
                                 choices = sort(unique(newdf()$SEVERITY), decreasing = FALSE),
                                 selected = unique(newdf()$SEVERITY))
    })

    # Display summary Contents in a data table
    output$contents <- renderDataTable({
        datatable(dataTest2(), filter = 'top',
                  list(autoWidth = TRUE,pageLenght = 20,
                       pageLength = 15))
    })

    # Display Tickets Table
    output$tickets <- renderDataTable(
        datatable(dataTest(), filter = 'top',
                  list(autoWidth = TRUE,
                       pageLength = 15))
    )

    # Calculation of summary data
    newdf <- reactive({
        mimRun(x = mimDf(),
               day1 = as.character(paste0(input$date[1], " 00:00:00")),
               day2 = as.character(paste0(input$date[2], " 23:59:59")))
    })

    # Calculation of Duration
    newDf1 <- reactive({
        dur(newdf(), input$base, input$percentage, input$impact, input$severity)
    })

    # Calculation with Filter
    newDf2 <- reactive({
        durSummary(newDf1(),newdf(), input$base, input$percentage,
                   input$impact, input$severity)
    })

    # To reactive new dataset for calculation
    dataTest2 <- reactive({
        dfTemp <- subset(newDf2(), ALIAS %in% input$accountGroups)
        dfTemp
    })

    # Chosing of Accounts display to Tickets table and plotly grapsh
    dataTest <- reactive({
        dfTemp <- subset(newdf(), ALIAS %in% input$accountGroups &
                             Impact %in% input$impact &
                             SEVERITY %in% input$severity)
        dfTemp
    })

    # Impact Table
    output$impactTable <- renderTable({
        summaryImpact(dataTest(), input$base, input$percentage)
    })

    # Severity Table
    output$severityTable <- renderTable({
        summarySeverity(dataTest(), input$base, input$percentage)
    })

    output$headCountTable <- renderDataTable({
        headCount(dataTest())
    })

    # Display Summary Table
    output$summaryAccount <- renderTable({
        as.data.frame(sumAccount(dataTest2(), dataTest()))
    })

    output$aveAccount <- renderTable({
        as.data.frame(averageAccounts(dataTest2()))
    })
    
    # Display trend per week
    output$trend <- renderPlotly({
        plot_ly(data = dt <- trendDat(dataTest(), input$frequencyDate),
                x = ~dt[,input$frequencyDate],
                y = ~Duration,
                color = ~ALIAS,
                mode = "markers",
                line = list(shape = "spline")) %>%
            layout(title = paste("Duration in hours per",input$frequencyDate, " ( ",
                                 as.character(input$date[1]),"to",
                                 as.character(input$date[2]), " )"),
                   xaxis = list(title = input$frequencyDate),
                   yaxis = list(title = "Duration in Hours")) %>%
            config(displayModeBar = F)
    })

    # Display Totals per Account accros by week
    output$accountBar <- renderPlotly({
        plot_ly(data = dataTest2(),
                x = ~ALIAS,
                y = ~Total_Incidents,
                type = "bar") %>%
            layout(title = paste("Total Incidents by Account ( ",
                                 as.character(input$date[1]),"to",
                                 as.character(input$date[2]), " )"),
                   xaxis = list(title = "Account"),
                   yaxis = list(title = "Total Incidents")) %>%
            config(displayModeBar = F)
    })


    # Display Totals per Account accros by week
    output$accountBarTrend <- renderPlotly({
        plot_ly(data = dt <- trendVolume(dataTest(), input$frequencyDate),
                x = ~dt[,input$frequencyDate],
                y = ~freq,
                color = ~ALIAS,
                type = "bar") %>%
            layout(title = paste("Total Incidents by Account per",
                                 input$frequencyDate, " ( ",
                                 as.character(input$date[1]),"to",
                                 as.character(input$date[2]), " )"),
                   xaxis = list(title = input$frequencyDate),
                   yaxis = list(title = "Total Incidents"))%>%
            config(displayModeBar = F)
    })
    
    output$relativeAccount <- renderPlotly({
        dt <- dataframeList(dataTest())
        dt <- dt[1]
        dt <- as.data.frame(dt)
        pc <- plot_ly(data = dt,
                      labels = ~Var1,
                      values = ~Freq,
                      type = "pie",
                      hoverinfo = 'text',
                      text = ~paste(input$inputAccount, Var1, Freq, "%")) %>%
            layout(title = paste(input$inputAccount, "% ( ",
                                 as.character(input$date[1]),"to",
                                 as.character(input$date[2]), " )"),
                   autosize = F, width = 400, height = 400) %>%
                   #margin = list(l = 50, r = 50, b = 100, t = 100, pad = 5)) %>%
            config(displayModeBar = F)
    })
    
    output$relativeSeverity <- renderPlotly({
        dt <- dataframeList(dataTest())
        dt <- dt[2]
        dt <- as.data.frame(dt)
        pc <- plot_ly(data = dt,
                      labels = ~Var1,
                      values = ~Freq,
                      type = "pie",
                      hoverinfo = 'text',
                      text = ~paste(input$inputSeverity,Var1))%>%
            layout(title = paste(input$inputSeverity, "% ( ",
                                 as.character(input$date[1]),"to",
                                 as.character(input$date[2]), " )")) %>%
                   # autozise = F, witdth = 400, height = 400,
                   # margin = list(l = 50, r = 50, b = 100, t = 100, pad = 5)) %>%
            config(displayModeBar = F)
    })
    
    output$relativeImpact <- renderPlotly({
        dt <- dataframeList(dataTest())
        dt <- dt[3]
        dt <- as.data.frame(dt)
        pc <- plot_ly(data = dt,
                      labels = ~Var1,
                      values = ~Freq,
                      type = "pie",
                      hoverinfo = 'text',
                      text = ~paste("Impact", Var1))%>%
            layout(title = paste("Impact % ( ",
                                 as.character(input$date[1]),"to",
                                 as.character(input$date[2]), " )"))%>%
            #        autosize = F, width = 400, height = 400,
            # margin = list(l = 50, r = 50, b = 100, t = 100, pad = 5)) %>%
            config(displayModeBar = F)
    })
    
    # Close the R session when Chrome closes
    session$onSessionEnded(function() {
        stopApp()
        q("no")
    })
    
    })
#})