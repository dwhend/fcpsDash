library(XML)
library(RCurl)
library(rlist)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

SCHOOLS<-read.csv("SchoolType.csv")
DASHURL <- "https://apps3.fcps.net/covid-dash/dash-2.asp"

app <- Dash$new()

app$layout(htmlDiv(list(
  htmlDiv(
    list(htmlDiv(
      list(
        htmlLabel('Measure Type'),
        dccRadioItems(
          id = "Radio-Case-Quar",
          options = list(
            list(label = 'Cases', value = 'cases'),
            list(label = 'Quarantining', value = 'quarantining')
          ),
          value = 'cases',
          labelStyle = list('display' = 'block')
        ),
        dccInput(id='Text-Search',value='',type='text',placeholder='Search Schools',debounce=TRUE)
      )
      ,
      style = list('width' = '20%', 'display' = 'inline-block')
    ),
    htmlDiv(
      list(
        htmlLabel('Select measure period'),
        dccDropdown(
          id = 'Dropdown-LastNDay',
          options = list(
            list(label = "Most Recent Day", value = 0),
            list(label = "Last 7 days" , value = 7),
            list(label = "Last 14 days", value = 14),
            list(label = "Last 21 days", value = 21),
            list(label = "Last 28 days", value = 28)
          ),
          value = 7
        )
      ),
      style = list('width' = '60%', 'display' = 'inline-block')
    ))
    ,
    style = list('width' = '100%', 'display' = 'inline-block')
  ),
  htmlDiv(list(
    htmlDiv(dccGraph(id = 'Table-GrandTotal'), style = list('height' = 100)),
    dccGraph(id = 'Boxplot-LastNDay-Total'),
    dccGraph(id = 'Boxplot-LastNDay-Avg'),
    dccInterval(
      id = 'interval-component',
      interval = 3600 * 1000,
      # in milliseconds
      n_intervals = 0
    )
  ))
)))

app$callback(
  output = list(
      output('Table-GrandTotal','figure'),
      output('Boxplot-LastNDay-Total','figure'),
      output('Boxplot-LastNDay-Avg','figure')
  ),
  params = list(
    input('interval-component','n_intervals'),
    input('Dropdown-LastNDay',property='value'),
    input('Radio-Case-Quar',property='value'),
    input('Text-Search',property='value')
    ),

  update_graph <- function(n_intervals,LastNDay,radioCaseQuar,textSearch) {
    
    if(n_intervals==0 || (n_intervals*3600)%%3600==0){
      THEDASH <- getURL(DASHURL,.opts = list(ssl.verifypeer = FALSE))
      TABLES <- readHTMLTable(THEDASH)
      TABLES <- list.clean(TABLES,fun=is.null,recursive=FALSE)
      ROWS <- unlist(lapply(TABLES,function(t) dim(t)[1]))
      DASHDF <- merge(x=TABLES[["NULL"]],y=SCHOOLS,by="School",all.x=TRUE)
      rm(TABLES)
      
      mostRecentDt <- max(as.Date(DASHDF$"Date Reported","%m/%d/%Y"), na.rm=TRUE) %>% as.Date()
      
      DASHC <- data.frame(DateReported=as.Date(DASHDF$`Date Reported`,"%m/%d/%Y")
                          ,School=DASHDF$School
                          ,StudentCaseNum=as.numeric(DASHDF$`Student Cases`)
                          ,StaffCaseNum=as.numeric(DASHDF$`Staff Cases`)
                          ,StudentQuarantineNum=as.numeric(DASHDF$`Student Quarantines`)
                          ,StaffQuarantineNum=as.numeric(DASHDF$`Staff Quarantines`)
                          ,Ndays=(mostRecentDt-as.Date(DASHDF$`Date Reported`,"%m/%d/%Y"))
                          ,SchoolType=DASHDF$`SchoolType`
      )
      
      rm(DASHDF)
    }
    
    if(radioCaseQuar=="cases"){
      
      textSearch <- str_to_lower(str_trim(textSearch))
      
      ifelse(str_trim(textSearch)==''
             ,DASHC_LAST_N_DAY <- DASHC %>% filter(Ndays <= LastNDay)
             ,DASHC_LAST_N_DAY <- DASHC %>% filter((Ndays <= LastNDay) & (str_detect(str_to_lower(str_trim(School)),textSearch))))
      
      DASHC_LAST_N_DAY_Agg <- DASHC_LAST_N_DAY %>% 
        group_by(School,SchoolType) %>% 
        summarise(TotalStudentCases=sum(StudentCaseNum),TotalStaffCases=sum(StaffCaseNum),AvgStudentCases=ave(StudentCaseNum),AvgStaffCases=ave(StaffCaseNum)) %>%
        distinct()
      
      DASHC_LAST_N_DAY_Agg_GrandTotal <- DASHC_LAST_N_DAY_Agg %>%
        group_by() %>%
        summarise(TotalStudentCases=sum(TotalStudentCases),TotalStaffCases=sum(TotalStaffCases)) %>%
        distinct()
      
      DASHC_LAST_N_DAY_Agg <- DASHC_LAST_N_DAY_Agg %>% pivot_longer(
        cols=c("TotalStudentCases","TotalStaffCases","AvgStudentCases","AvgStaffCases")
      )
      
      DASHC_LAST_N_DAY_Agg_Total <- DASHC_LAST_N_DAY_Agg %>% filter(name == c("TotalStudentCases","TotalStaffCases"))
      DASHC_LAST_N_DAY_Agg_Avg <- DASHC_LAST_N_DAY_Agg %>% filter(name == c("AvgStudentCases","AvgStaffCases"))
      
      figGrandTotal <- plot_ly(data=DASHC_LAST_N_DAY_Agg_GrandTotal
                               ,type='table'
                               ,header=list(values=c("Period","Total new student cases","Total new staff cases"),height=30)
                               ,cells= list(values=rbind(paste("Last ",LastNDay," days",sep="")
                                                         ,DASHC_LAST_N_DAY_Agg_GrandTotal$TotalStudentCases
                                                         ,DASHC_LAST_N_DAY_Agg_GrandTotal$TotalStaffCases),height=30)
                               )
    }
    
    if(radioCaseQuar=="quarantining"){
      
      textSearch <- str_to_lower(str_trim(textSearch))
      
      ifelse(str_trim(textSearch)==''
             ,DASHC_LAST_N_DAY <- DASHC %>% filter(Ndays <= LastNDay)
             ,DASHC_LAST_N_DAY <- DASHC %>% filter((Ndays <= LastNDay) & (str_detect(str_to_lower(str_trim(School)),textSearch))))
      
      DASHC_LAST_N_DAY_Agg <- DASHC_LAST_N_DAY %>% 
        group_by(School,SchoolType) %>% 
        summarise(TotalStudentQuar=sum(StudentQuarantineNum),TotalStaffQuar=sum(StaffQuarantineNum),AvgStudentQuar=ave(StudentQuarantineNum),AvgStaffQuar=ave(StaffQuarantineNum)) %>%
        distinct()
      
      DASHC_LAST_N_DAY_Agg_GrandTotal <- DASHC_LAST_N_DAY_Agg %>%
        group_by() %>%
        summarise(TotalStudentQuar=sum(TotalStudentQuar),TotalStaffQuar=sum(TotalStaffQuar)) %>%
        distinct()
      
      DASHC_LAST_N_DAY_Agg <- DASHC_LAST_N_DAY_Agg %>% pivot_longer(
        cols=c("TotalStudentQuar","TotalStaffQuar","AvgStudentQuar","AvgStaffQuar")
      )
      
      DASHC_LAST_N_DAY_Agg_Total <- DASHC_LAST_N_DAY_Agg %>% filter(name == c("TotalStudentQuar","TotalStaffQuar"))
      DASHC_LAST_N_DAY_Agg_Avg <- DASHC_LAST_N_DAY_Agg %>% filter(name == c("AvgStudentQuar","AvgStaffQuar"))
      
      figGrandTotal <- plot_ly(data=DASHC_LAST_N_DAY_Agg_GrandTotal
                               ,type='table'
                               ,header=list(values=c("Period","Total new student quarantines","Total new staff quarantines"),height=30)
                               ,cells=list(values =rbind(paste("Last ",LastNDay," days",sep="")
                                                         ,DASHC_LAST_N_DAY_Agg_GrandTotal$TotalStudentQuar
                                                         ,DASHC_LAST_N_DAY_Agg_GrandTotal$TotalStaffQuar),height=30)
                               )
    }
    
    # print(DASHC_LAST_N_DAY_Agg_Total)
    # print(DASHC_LAST_N_DAY_Agg_Avg)
    
    figTotal <- plot_ly(data=DASHC_LAST_N_DAY_Agg_Total,x=~SchoolType, y=~value, color=~name, type="box", quartilemethod="linear", boxpoints = "all"
                     ,jitter=0.3, pointpos = 0, text=~School, name=~name) %>%
          layout(title = paste("FCPS total new ",radioCaseQuar," for the last ",LastNDay," days as of:",mostRecentDt,sep="")
                  ,boxmode="group"
                  ,xaxis = list(title="",categoryorder="array",categoryarray=c("Elementary","Middle","High School","Other")))

    figAvg <- plot_ly(data=DASHC_LAST_N_DAY_Agg_Avg,x=~SchoolType, y=~value, color=~name, type="box", quartilemethod="linear", boxpoints = "all"
                        ,jitter=0.3, pointpos = 0, text=~School, name=~name) %>%
      layout(title = paste("FCPS Average new ",radioCaseQuar," per day for the last ",LastNDay," days as of:",mostRecentDt,sep="")
             ,boxmode="group"
             ,xaxis = list(title="",categoryorder="array",categoryarray=c("Elementary","Middle","High School","Other")))
    
    return(list(figGrandTotal,figTotal,figAvg))
  }
)

app$run_server(host='0.0.0.0',port='8080')

