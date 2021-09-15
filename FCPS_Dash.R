library(dplyr)
library(tidyr)
library(stringr)
library(plotly)
library(lubridate)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(zoo)

SCHOOLS<-read.csv("SchoolType.csv")
lastUpdateDt <- Sys.time()

app <- Dash$new()

app$title("FCPS Covid-19 Dash")

app$layout(
htmlDiv(list(
  htmlH2('FCPS Covid-19 Dash'),
  htmlDiv(list(
      #radio
      htmlDiv(list(
      htmlLabel('Measure Type'),
      dccRadioItems(
        id = "Radio-Case-Quar",
        options = list(
          list(label = 'Cases', value = 'cases'),
          list(label = 'Quarantining', value = 'quarantining')
        ),
        value = 'cases',
        labelStyle = list('display' = 'block')
      ))
      , style=list('height'='150px','width'='150px','display'='inline-block','verticalAlign'='top')
      ),
      #radio-end
      #search
      htmlDiv(list(
        htmlLabel('Filter by School: '),
        dccInput(
          id = 'Text-Search',
          value = '',
          type = 'text',
          placeholder = 'Search Schools',
          debounce = TRUE
        )
      )
      ,style=list('height'='150px','width'='200px','display'='inline-block','verticalAlign'='top')
      ),
      #search-end
      #disclaimer
      htmlDiv(list(
        htmlP('This dashboard was created by a parent of a FCPS student, and is not directly affiliated with FCPS in anyway.'),
        htmlP('All data herein is publicly available at http://fcps.net/covid19 and https://chfs.ky.gov/Pages/cvdaily.aspx'),
        htmlP('The data presented in these visualizations should not be used to inform any health decisions for you or your family without consulting with your Doctor, or a public health official.'),
        htmlP('Built using Dash for R. Source code available at: https://github.com/dwhend/fcpsDash'),
        htmlP(id='htmlp-lastupdatedt',children="Data last updated ")
      )
      ,style=list('display'='inline-block','verticalAlign'='top')
      ),
      #disclaimer-end
      #measureprd
      htmlDiv(list(
        htmlLabel('Measure Period: '),
        dccDropdown(
          id = 'Dropdown-LastNDay',
          options = list(
            list(label = "Most Recent Day", value = 0),
            list(label = "Last 7 days" , value = 7),
            list(label = "Last 14 days", value = 14),
            list(label = "Last 21 days", value = 21),
            list(label = "Last 28 days", value = 28),
            list(label = "Since first day (Aug 11, 2021)", value = (as.numeric(Sys.Date())-as.numeric(as.Date("2021-08-11"))))
          ),
          value = 7
        )
      ),
      style = list('height'='200px', 'width' = '300px')
      )
      #measureprd-end

  )
  , style=list('height'='10%','width'='100%','display'='inline-block')
  ),
  #charts
  htmlDiv(list(
    dccGraph(id = 'Barchart-Timeseries'),
    htmlDiv(dccGraph(id = 'Table-GrandTotal'), style = list('height' = 200)),
    dccGraph(id = 'Boxplot-LastNDay-Total'),
    dccGraph(id = 'Boxplot-LastNDay-Avg'),
    dccInterval(
      id = 'interval-component',
      interval = 3600 * 1000,
      # in milliseconds
      n_intervals = 0
    )
  )
  )
  #charts-end
)
))

app$callback(
  output = list(
      output('Table-GrandTotal','figure'),
      output('Boxplot-LastNDay-Total','figure'),
      output('Boxplot-LastNDay-Avg','figure'),
      output('Barchart-Timeseries','figure'),
      output('htmlp-lastupdatedt','children')
  ),
  params = list(
    input('interval-component','n_intervals'),
    input('Dropdown-LastNDay',property='value'),
    input('Radio-Case-Quar',property='value'),
    input('Text-Search',property='value')
    ),

  update_graph <- function(n_intervals,LastNDay,radioCaseQuar,textSearch) {
    
    if(n_intervals==0 || (n_intervals*3600)%%3600==0){
      DASHDF <- read.csv(file='FCPS_scrape.csv')
      CountyCovidCases <- read.csv(file="KYDailyCases.csv") %>% filter(County=="Fayette")
      lastUpdateDt <- file.info('FCPS_scrape.csv')$mtime
      mostRecentCountyDt <- max(as.Date(CountyCovidCases$Date,"%Y-%m-%d"))
      
    lastUpdateStr <- paste("Data last updated ",lastUpdateDt," UTC (",format(with_tz(lastUpdateDt, tz = "America/New_York"),'%Y-%m-%d %I:%M %p')," ET) -- Most recent County data available: ",mostRecentCountyDt,sep="")
      
      mostRecentDt <- max(as.Date(DASHDF$Date,"%m/%d/%Y"), na.rm=TRUE)
      
      
      DASHC <- data.frame(DateReported=as.Date(DASHDF$Date,"%m/%d/%Y")
                          ,School=DASHDF$School
                          ,StudentCaseNum=as.numeric(DASHDF$Student.Cases)
                          ,StaffCaseNum=as.numeric(DASHDF$Staff.Cases)
                          ,StudentQuarantineNum=as.numeric(DASHDF$Student.Quarantines)
                          ,StaffQuarantineNum=as.numeric(DASHDF$Staff.Quarantines)
                          ,Ndays=(mostRecentDt-as.Date(DASHDF$Date,"%m/%d/%Y"))
                          ,SchoolType=DASHDF$SchoolType)
      
      rm(DASHDF)
    }
    
    if(radioCaseQuar=="cases"){
      
      CountyCovidCasesLastN <- CountyCovidCases %>%
        arrange(desc(Date)) %>%
        mutate(Roll7D=zoo::rollmean(Cases,k=7,fill=NA,align="left")) %>%
        filter((mostRecentDt-as.Date(Date,"%Y-%m-%d")) <= (LastNDay))
      
      textSearch <- str_to_lower(str_trim(textSearch))
      
      ifelse(str_trim(textSearch)==''
             ,DASHC_LAST_N_DAY <- DASHC %>% filter(Ndays <= LastNDay)
             ,DASHC_LAST_N_DAY <- DASHC %>% filter((Ndays <= LastNDay) & (str_detect(str_to_lower(str_trim(School)),textSearch))))
      
      DASHC_LAST_N_DAY_Agg <- DASHC_LAST_N_DAY %>% 
        group_by(School,SchoolType) %>% 
        summarise(TotalStudentCases=sum(StudentCaseNum),TotalStaffCases=sum(StaffCaseNum),AvgStudentCases=ave(StudentCaseNum),AvgStaffCases=ave(StaffCaseNum)) %>%
        distinct()
      
      DASHC_LAST_N_Timeseries <- DASHC_LAST_N_DAY %>%
        group_by(DateReported) %>%
        summarise(DailyStudent=sum(StudentCaseNum),DailyStaff=sum(StaffCaseNum)) %>%
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
      
      DASHC_LAST_N_Timeseries <- DASHC_LAST_N_DAY %>%
        group_by(DateReported) %>%
        summarise(DailyStudent=sum(StudentQuarantineNum),DailyStaff=sum(StaffQuarantineNum)) %>%
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
    
    figStackedBar <- plot_ly(data=DASHC_LAST_N_Timeseries,x=~DateReported,y=~DailyStudent,type='bar',name='Students') %>%
                      add_trace(y=~DailyStaff,name='Staff') %>%
                      layout(title=paste("Number of new ",radioCaseQuar," by day",sep="")
                              ,xaxis=list(title="Date Reported",tickformat="%b %d %a")
                              ,yaxis=list(title='Count of Student/Staff')
                              ,barmode='stack')
    
    if(radioCaseQuar=="cases") {
      
    figStackedBar <- figStackedBar %>% 
      add_trace(x=CountyCovidCasesLastN$Date, y=CountyCovidCasesLastN$Roll7D, type='scatter', mode="lines+markers"
                 , name="Rolling 7-day average cases (Fayette County)")
    }           

    return(list(figGrandTotal,figTotal,figAvg,figStackedBar,lastUpdateStr))
  }
)

app$run_server(host='0.0.0.0',port='8080')

