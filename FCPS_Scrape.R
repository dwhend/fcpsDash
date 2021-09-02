library(XML)
library(RCurl)
library(rlist)
library(dplyr)

SCHOOLS<-read.csv("SchoolType.csv")
DASHURL <- "https://apps3.fcps.net/covid-dash/v4/dashboard.asp"

THEDASH <- getURL(DASHURL, .opts = list(ssl.verifypeer = FALSE))
TABLES <- list.clean(readHTMLTable(THEDASH),fun=is.null,recursive=FALSE)
DASHDF <- merge(x=TABLES[["NULL"]],y=SCHOOLS,by="School",all.x=TRUE)

mostRecentDt <- max(as.Date(DASHDF$"Date","%m/%d/%Y"), na.rm=TRUE) %>% as.Date()

DASHDF %>% 
  filter(mostRecentDt-as.Date(DASHDF$`Date`,"%m/%d/%Y") <= 28) %>%
  write.csv(file="FCPS_scrape.csv",row.names=FALSE)

