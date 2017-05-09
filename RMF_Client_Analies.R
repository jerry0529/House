library(glm2)
library(MASS)
library(scatterplot3d)
library(xtable)
library(stepp)
library(stepwise)
library(reshape2)
library(ggplot2)
library(mosaic)
library(mda)
library(mdatools)
library(earth)
library(polspline)
library(faraway)
#-----------------------------------------------------------------------------------------
#Refference from : http://www.dataapple.net/?p=84
#讀檔案
rfm_data <- read.csv("E:\\Desk\\SPSS\\SPSS\\SPSS_Date\\csv\\RFM範例-交易資料-94筆.csv")
#改表頭
names <- c("ID","Date","Amount","Gender","Item")
names(rfm_data) <- names
#改Date的型態
rfm_data[,2] <-as.Date(as.character(rfm_data[,2]),"%Y/%m/%d")
head(rfm_data)
dim(rfm_data)
#uid <- rfm_data[!duplicated(rfm_data[,"ID"]),]
#dim(uid)

#計算Recency，使用"交易資料"
getDataFrame <- function(rfm_data,startDate,endDate,tIDColName="ID",tDateColName="Date",tAmountColName="Amount"){
  
  #將日期由大到小排序
  rfm_data <- rfm_data[order(rfm_data[,tDateColName],decreasing = TRUE),]
  #排除在valuation day外的資料
  rfm_data <- rfm_data[rfm_data[,tDateColName]>= startDate,]
  rfm_data <- rfm_data[rfm_data[,tDateColName]<= endDate,]
 
  #移除重複ID，並創立新資料
  new_rfm_data <- rfm_data[!duplicated(rfm_data[,tIDColName]),]
  #計算最後一次交易距離"今天"共多少天
  Recency<-as.numeric(difftime(endDate,new_rfm_data[,tDateColName],units="days"))
  #在新資料新增內容
  new_rfm_data <-cbind(new_rfm_data,Recency)
  
  #將ID遞增排序
  new_rfm_data <- new_rfm_data[order(new_rfm_data[,tIDColName]),]
  #抓取交易次數，重新命名並寫入新資料
  fre <- as.data.frame(table(rfm_data[,tIDColName]))
  Frequency <- fre[,2]
  new_rfm_data <- cbind(new_rfm_data,Frequency)
 
   #計算平均消費金額，重新命名並寫入新資料
  m <- as.data.frame(tapply(rfm_data[,tAmountColName],rfm_data[,tIDColName],sum))
  Monetary <- m[,1]/Frequency
  new_rfm_data <- cbind(new_rfm_data,Monetary)
  
  return(new_rfm_data)
  
}

#設定valuation day
startDate<-as.Date('2015-01-01')
endDate<-as.Date('2015-12-31')
rfm_data <- getDataFrame(rfm_data,startDate,endDate)

#設定RFM的分數
getIndependentScore <- function(rfm_data,r=5,f=5,m=5) {
  
  if (r<=0 || f<=0 || m<=0) return
  
  #依RFM的順序order，R由小到大(R要愈小愈好，F改成負的(F愈大愈好)，M改成負的(M愈大愈好)
  rfm_data <- rfm_data[order(rfm_data$Recency,-rfm_data$Frequency,-rfm_data$Monetary),]
  #計算R的分數，scoring是自定函數
  R_Score <- scoring(rfm_data,"Recency",r)
  #將R_Score寫入資料
  rfm_data <- cbind(rfm_data, R_Score)
  #依FRM的順序order，F改成負的(F愈大愈好)，R由小到大(R要愈小愈好，M改成負的(M愈大愈好)
  rfm_data <- rfm_data[order(-rfm_data$Frequency,rfm_data$Recency,-rfm_data$Monetary),]
  #計算F的分數
  F_Score <- scoring(rfm_data,"Frequency",f)
  #將F_Score寫入資料
  rfm_data <- cbind(rfm_data, F_Score)
  #依MRF的順序order，M改成負的(M愈大愈好)，R由小到大(R要愈小愈好，F改成負的(F愈大愈好)
  rfm_data <- rfm_data[order(-rfm_data$Monetary,rfm_data$Recency,-rfm_data$Frequency),]
  #計算M的分數
  M_Score <- scoring(rfm_data,"Monetary",m)
  #將M_Score寫入資料
  rfm_data <- cbind(rfm_data, M_Score)
  
  #依RFM的順序order，由小到大排序
  rfm_data <- rfm_data[order(-rfm_data$R_Score,-rfm_data$F_Score,-rfm_data$M_Score),]
  
  #計算分數總和，並寫入資料
  Total_Score <- c(100*rfm_data$R_Score + 10*rfm_data$F_Score+rfm_data$M_Score)
  rfm_data <- cbind(rfm_data,Total_Score)
  
  return (rfm_data)
  
}

#設定分組段點
scoring <- function (rfm_data,column,r=5){
  
  #計算原始資料長度
  len <- dim(rfm_data)[1]
  #複製len個0
  score <- rep(0,times=len)
  #一組內的資料數量，取整數，將資料分成r等份
  nr <- round(len / r)
  #若資料量大於r尺度
  if (nr > 0){
    
    rStart <-0
    rEnd <- 0
    #i=1:r
    for (i in 1:r){
      #設定啟始值和末值的初值
      rStart = rEnd+1
      #若啟始值大於i*nr(組數*組內資料數)，if true則換下一個i
      if (rStart> i*nr) next
      #若i=r，已經到最後一組時
      if (i == r){
        #若啟始值小於等於資料長度，則末值等於資料長度
        #若非，則末值等於組數*組內資料數
        if(rStart<=len ) rEnd <- len else next
      }else{
        rEnd <- i*nr
      }
      #設定Recency的分數
      score[rStart:rEnd]<- r-i+1
      #確認相同Rency的ID的資料具有相同的分數
      s <- rEnd+1
      #當i=1,2,3,4時且啟始值小於等於資料長度時
      if(i<r & s <= len){
        #組內啟始值到最後一筆資料
        for(u in s: len){
          #若到組內最後一筆資料時
          if(rfm_data[rEnd,column]==rfm_data[u,column]){
            #該組分數=r-i+1
            score[u]<- r-i+1
            rEnd <- u
          }else{
            break;
          }
        }
        
      }
      
    }
    
  }
  return(score)
  
}

rs <-getIndependentScore(rfm_data)
#write.csv(rs,"E:\\Desk\\SPSS\\SPSS\\SPSS_Date\\csv\\rs.csv")

getScoreWithBreaks <- function(rfm_data,r,f,m) {
  
  #計算Rency的分數
  len = length(r)
  R_Score <- c(rep(1,length(rfm_data[,1])))
  rfm_data <- cbind(rfm_data,R_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=r[i-1]
    }
    p2=r[i]
    
    if(dim(rfm_data[p1<rfm_data$Recency & rfm_data$Recency<=p2,])[1]>0){
      rfm_data[p1<rfm_data$Recency & rfm_data$Recency<=p2,]$R_Score = len - i+ 2
    } 
  }
  
  #計算Frequency的分數	
  len = length(f)
  F_Score <- c(rep(1,length(rfm_data[,1])))
  rfm_data <- cbind(rfm_data,F_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=f[i-1]
    }
    p2=f[i]
    
    if(dim(rfm_data[p1<rfm_data$Frequency & rfm_data$Frequency<=p2,])[1]>0){
      rfm_data[p1<rfm_data$Frequency & rfm_data$Frequency<=p2,]$F_Score = i
    } 
  }
  if(dim(rfm_data[f[len]<rfm_data$Frequency,])[1]>0){
    rfm_data[f[len]<rfm_data$Frequency,]$F_Score = len+1
  } 
  
  #計算Monetary的分數	
  len = length(m)
  M_Score <- c(rep(1,length(rfm_data[,1])))
  rfm_data <- cbind(rfm_data,M_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=m[i-1]
    }
    p2=m[i]
    
    if(dim(rfm_data[p1<rfm_data$Monetary & rfm_data$Monetary<=p2,])[1]>0){
      rfm_data[p1<rfm_data$Monetary & rfm_data$Monetary<=p2,]$M_Score = i
    } 
  }
  if(dim(rfm_data[m[len]<rfm_data$Monetary,])[1]>0){
    rfm_data[m[len]<rfm_data$Monetary,]$M_Score = len+1
  } 
  
  #排序
  rfm_data <- rfm_data[order(-rfm_data$R_Score,-rfm_data$F_Score,-rfm_data$M_Score),]
  #計算總分
  Total_Score <- c(100*rfm_data$R_Score + 10*rfm_data$F_Score+rfm_data$M_Score)
  rfm_data <- cbind(rfm_data,Total_Score)
  
  return(rfm_data)
  
}

#次數值方圖
drawHistograms <- function(rfm_data,r=5,f=5,m=5){
  
  par(mfrow = c(f,r))
  
  names <-rep("",times=m)
  for(i in 1:m) names[i]<-paste("M",i)
  
  for (i in 1:f){
    for (j in 1:r){
      c <- rep(0,times=m)
      for(k in 1:m){
        tmpdf <-rfm_data[rfm_data$R_Score==j & rfm_data$F_Score==i & rfm_data$M_Score==k,]
        c[k]<- dim(tmpdf)[1]
        
      }
      if (i==1 & j==1) 
        barplot(c,col="lightblue",names.arg=names)
      else
        barplot(c,col="lightblue")
      if (j==1) title(ylab=paste("F",i))	
      if (i==1) title(main=paste("R",j))	
      
    }
    
  }
  
  par(mfrow = c(1,1))
  
} 

drawHistograms(rs)

hist(rfm_data$Recency)
hist(rfm_data$Frequency)
hist(rfm_data$Monetary)

#自行設定分數界線
r <-c(80,110,110,117)
f <-c(0,0,1,1)
m <-c(2100,4000,4000,4300)
rs2<-getScoreWithBreaks(rfm_data,r,f,m)
drawHistograms(rs2)
hist(rs2$Recency)
hist(rs2$Frequency)
hist(rs2$Monetary)
#write.csv(rs2,"E:\\Desk\\SPSS\\SPSS\\SPSS_Date\\csv\\rs2.csv")