library(rJava)
library(tabulizer)
library(stringr)
library(httr)

casesCSV <- "KYDailyCases.csv"

if(file.exists(casesCSV)){
  KYDailyCases <- read.csv(casesCSV,header=T,sep=",")
  lastDate <- max(as.Date(KYDailyCases$"Date","%Y-%m-%d"), na.rm=F)
} else {
  lastDate <- as.Date("2021-01-01","%Y-%m-%d")
}

tryPDFYY <- seq(lastDate+1,Sys.Date(), by="days") %>% as.character("%m%d%y")
baseURL <- "https://chfs.ky.gov/cvdaily/COVID19DailyReport"
tryURLYY <- paste(baseURL,tryPDFYY,".pdf",sep="")
tryPDFYYYY <- seq(lastDate+1,Sys.Date(), by="days") %>% as.character("%m%d%Y")
tryURLYYYY <- paste(baseURL,tryPDFYYYY,".pdf",sep="")

lapply(tryURLYY,function(pYY){
  tryCatch({
    res<-GET(pYYYY,SSL_VERIFY_PEER=F)
    if(res$status_code==200){
      bin<-content(RES,"raw")
      writeBin(bin,str_replace(pYYYY,"https://chfs.ky.gov/cvdaily/","./KYDailyCovidReports/"))
      }
    }
    ,error = function(e) print(paste(pYY, "not available")))
})

lapply(tryURLYYYY,function(pYYYY){
  tryCatch({
    res<-GET(pYYYY,SSL_VERIFY_PEER=F)
    if(res$status_code==200){
      bin<-content(RES,"raw")
      writeBin(bin,str_replace(pYYYY,"https://chfs.ky.gov/cvdaily/","./KYDailyCovidReports/"))
      }
    }
    ,error = function(e) print(paste(pYYYY, "not available"))
    )
})

fileList <- list.files("./KYDailyCovidReports/", pattern="*.pdf", full.names=T, recursive=F)

lapply(fileList,function(f) {
  fileDate <- str_replace(str_replace(f,"./KYDailyCovidReports/COVID19DailyReport",""),".pdf","") %>% as.Date("%m%d%y")
  if(fileDate > lastDate){
    newCaseTable <- extract_tables(f,pages=4,output="data.frame")

    c0df <- data.frame(Date=fileDate,
                       County=newCaseTable[[1]]$County,
                       Cases= as.numeric(newCaseTable[[1]]$Cases))
    c1df <- data.frame(Date=fileDate,
                       County=newCaseTable[[1]]$County.1,
                       Cases= as.numeric(newCaseTable[[1]]$Cases.1))
    c2df <- data.frame(Date=fileDate,
                       County=newCaseTable[[1]]$County.2,
                       Cases= as.numeric(newCaseTable[[1]]$Cases.2))
    c3df <- data.frame(Date=fileDate,
                       County=newCaseTable[[1]]$County.3,
                       Cases= as.numeric(newCaseTable[[1]]$Cases.3))
    cases <- union(union(c0df,c1df),union(c2df,c3df))
    
      if(file.exists(casesCSV)){
        write.table(cases,file="KYDailyCases.csv",sep=",",append=T,row.names=F,col.names=F)
      } else {
        write.table(cases,file="KYDailyCases.csv",sep=",",append=T,row.names=F,col.names=T)
      }
  }
}) 