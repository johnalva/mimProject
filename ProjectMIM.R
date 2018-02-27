#####################################
#
    # setwd("C:/Users/johnalva/Downloads")
    # #x <- read_xlsx("MIM Raw Data 2017-2018.xlsx")
    # x <- read_xlsx("Change & Incident Management.xlsx")
    # x <- as.data.frame(x)
    # 
    # day1 <- "2017-07-01 00:00:00"
    # day2 <- "2017-12-31 23:59:59"
# 
######################################

# Rename columns
newNames <- function(x, account, severity, problemId, dmAssignee, cratedTime, SolveTime){
    
    # account <- "Customer"
    # severity <- "Priority"
    # dmAssignee <- "Owner"
    # problemId <- "Work Order Number"
    # cratedTime <- "Actual Start"
    # SolveTime <- "Actual Finish"
    
    newNames1 <- dplyr::select(x, account, severity, dmAssignee, problemId, cratedTime, SolveTime)
    names(newNames1) <- c("ALIAS", "SEVERITY","DM_ASSIGNEE", "PROBLEM_ID",
                         "CREATED_TIME", "SOLVED_TIME")

    # Adding columns
    newNames1$start <- ""
    newNames1$end <- ""
    newNames1$Impact <- ""
    return(newNames1)
}

# Select posixct only
pos <- function(x){
    l <- (sapply(x,is.POSIXct))
    names(x[,l[TRUE]])
}

pChar <- function(x){
    l <- (sapply(x,is.character))
    names(x[,l[TRUE]])
}

pNum <- function(x){
    l <- (sapply(x,is.numeric))
    names(x[,l[TRUE]])
}

# Upload of the first data frame
mimRun <- function(x, day1, day2, fil){
    # x <- newNames1
    d1 <- as.POSIXct(day1, tz = "UTC")
    d2 <- as.POSIXct(day2, tz = "UTC")
    
    # Filter
    f1 <- filter(x, CREATED_TIME <= d2)
    f2 <- filter(f1, SOLVED_TIME >= d1)
    f3 <- select(f2, ALIAS, SEVERITY,PROBLEM_ID, DM_ASSIGNEE, CREATED_TIME, SOLVED_TIME)
    
    # # Adding columns
    # f3$start <- ""
    # f3$end <- ""
    # f3$Impact <- ""
    
    f3$end <- ifelse(f3$SOLVED_TIME > d2, f3$end <- d2, f3$end <- f3$SOLVED_TIME)
    f3$end <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + f3$end
    f3$start <- ifelse(f3$CREATED_TIME < d1, f3$start <- d1, f3$start <- f3$CREATED_TIME)
    f3$start <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + f3$start
    
    # Duration
    f3$Duration <- as.numeric(round(difftime(f3$end,f3$start, units = "hours"),
                                     digits = 2))

    # Adding Quantiles
    df <- (aggregate(data = f3, Duration ~ ALIAS, function(x)max(x)*(.33)))
    names(df) <- c("ALIAS", "Quantile_33")
    df1  <- (aggregate(data = f3, Duration ~ ALIAS, function(x)max(x)*(.66)))
    names(df1) <- c("ALIAS", "Quantile_66")
    # df2  <- (aggregate(data = f3, Duration ~ ALIAS, function(x)quantile(x, c(.50))))
    # names(df2) <- c("ALIAS", "Quantile_50")
    dfQuantile <- merge(df, df1, by = "ALIAS")
    f3 <- merge(f3, dfQuantile, by = "ALIAS")
    
    # Adding Impact
    f3$Impact <- ifelse(f3$Duration < f3$Quantile_33, f3$Impact <- "Low", 
                        ifelse(f3$Duration > f3$Quantile_66, f3$Impact <- "High",
                               f3$Impact <- "Medium"))
    
    f3$Week <- week(f3$end)
    f3$Year <- year(f3$end)
    f3$Month <- month(f3$end)
    f3$Day <- day(f3$end)
    f3$Hour <- hour(f3$end)
    return(f3)
}

# Return split summary table
dur <- function(x, base, percentage, impact, severity){
    # x <- f3
    # impact = c("Low", "Medium","High")
    x <- subset(x, Impact %in% impact)
    sm <- aggregate(data = x, Duration ~ ALIAS + Impact + SEVERITY, FUN = sum)
    #sm$Total_FTE <- round(sm$Duration / 128, digits = 2)
    sm$Total_FTE <- round(sm$Duration / (base*(percentage/100)), digits = 2)
    return(sm)
}
 
# Summary by Impact
summaryImpact <- function(x, base, percentage){
    # x <- f3
    impact_Count <- plyr::count(df = x, vars = "Impact")
    impact_Duration <- aggregate(data = x, Duration ~ Impact, sum)
    impact_Duration_Mean <- aggregate(data = x, Duration ~ Impact, mean)
    impact_Duration_Mean$Duration <- round(impact_Duration_Mean$Duration,2)
    #impact_FTE <- round(impact_Duration$Duration / 128, digits = 2)
    impact_FTE <- round(impact_Duration$Duration / (base*(percentage/100)), digits = 2)
    impactFinal <- merge(impact_Count, impact_Duration_Mean, by = "Impact")
    impactFinal <- cbind(impactFinal, impact_FTE)
    names(impactFinal) <- c("Impact", "Total Inc", "Duration", "FTE")
    Impact <- impactFinal
    return(Impact)
}

# Summary by Severity
summarySeverity <- function(x, base, percentage){
    #x <- f3
    Severity_Count <- plyr::count(df = x, vars = "SEVERITY")
    Severity_Duration <- aggregate(data = x, Duration ~ SEVERITY, sum)
    Severity_Duration_Mean <- aggregate(data = x, Duration ~ SEVERITY, mean)
    Severity_Duration_Mean$Duration <- round(Severity_Duration_Mean$Duration,2)
    #Severity_FTE <- round(Severity_Duration$Duration / 128, digits = 2)
    Severity_FTE <- round(Severity_Duration$Duration / (base*(percentage/100)), digits = 2)
    Severity_Final <- merge(Severity_Count, Severity_Duration_Mean, by = "SEVERITY")
    Severity_Final <- cbind(Severity_Final, Severity_FTE)
    names(Severity_Final) <- c("Severity", "Total Inc", "Duration", "FTE")
    Severity <- Severity_Final
    return(Severity)
}

# Headcount
headCount <- function(x){
    #x <- f3
    head_Count_Users <- plyr::count(df = x, vars = c("DM_ASSIGNEE", "Impact",
                                                     "SEVERITY"))
    
    head_Count_Users <- as.data.frame(head_Count_Users)
    head_Count_Duration <- aggregate(data = x, Duration ~ DM_ASSIGNEE +
                                         Impact + SEVERITY, sum)
    head_Count_Total <- merge(x= head_Count_Users, head_Count_Duration)
    names(head_Count_Total) <- c("MIO", "Impact", "Severity", "Total", "Duration")
    head_Count_Total <- select(head_Count_Total, MIO, Impact, Severity, Duration, Total)
    return(head_Count_Total)
}

# Use dur dataframe to create a summary table
durSummary <- function(x, y, base, percentage, impact, severity){
    # x <- sm
    # y <- f3
    # impact = c("Low", "Medium", "High")
    # severity = c(1,2,3,4)
    y <- subset(y, Impact %in% impact & SEVERITY %in% severity)
    sumX <- aggregate(data = x, Duration ~ ALIAS, FUN = sum)
    
    count_Alias <- plyr::count(y, c("ALIAS"))
    names(count_Alias) <- c("ALIAS", "Total_Incidents")
    x$ALIAS <- as.character(x$ALIAS)    
    sum_mim <- merge(count_Alias, sumX, by = "ALIAS")
    
    # sum_mim$Total_FTE <- round(sum_mim$Duration / 128, digits = 2)
    sum_mim$Total_FTE <- round(sum_mim$Duration / (base*(percentage/100)), digits = 2)
    sum_mim$ALIAS <- factor(sum_mim$ALIAS,
                            levels = unique(sum_mim$ALIAS)[order(sum_mim$Total_Incidents,
                                                                 decreasing = TRUE)])
    return(sum_mim)
}

sumAccount <- function(x,dataTest){
    # x <- sum_mim
    Total_Accounts <- c(length(unique(x$ALIAS)))
    Total_Incidents <- c(sum(x$Total_Incidents))
    Total_Duration <- c(sum(x$Duration))
    Total_FTE_Used <- c(sum(x$Total_FTE))
    Total_MIO <- plyr::count(df = unique(dataTest$DM_ASSIGNEE))
    Total_MIO <- sum(Total_MIO$freq)
    Throughput <- round(Total_Incidents / Total_MIO, digits = 2)
    final <- data.frame(Total_Accounts,Total_Incidents, Total_Duration, 
                        Total_FTE_Used, Total_MIO, Throughput)
    return(final)
}

trendDat <- function(x, frequencyDate){
    # x <- f3
    # frequencyDate <- "Week"
    result <- aggregate(data = x, Duration ~ x[,frequencyDate] + ALIAS, sum)
    names(result)[1] <- frequencyDate
    return(result)
}

trendVolume <- function(x, frequencyDate, calculation){
    # frequencyDate <- "Week"
    # calculation <- "mean"
    resultTrend <- plyr::count(df =  x, vars =  c(frequencyDate, "ALIAS"))
    result1 <- resultTrend
    names(result1)[1] <- frequencyDate
    return(result1)
}

# Data Frame list
dataframeList <- function(x){
    aliasPer <- (round(prop.table(table(x$ALIAS))*100, digits = 2))
    aliasPer <- as.data.frame(aliasPer)
    severityPer <- (round(prop.table(table(x$SEVERITY))*100, digits = 2))
    severityPer <- as.data.frame(severityPer)
    impactPer <- (round(prop.table(table(x$Impact))*100, digits = 2))
    impactPer <- as.data.frame(impactPer)
    list(aliasPer, severityPer, impactPer)
}
