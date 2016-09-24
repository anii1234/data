getDataFrame(df,startDate,endDate,tIDColName="ID",tDateColName="Date",tAmountColName="Amount")
getDataFrame <- function(df,startDate,endDate,tIDColName="ID",tDateColName="Date",tAmountColName="Amount")
  df <- df[order(df[,tDateColName],decreasing = TRUE),]
  df <- df[df[,tDateColName]>= startDate,]
  df <- df[df[,tDateColName]<= endDate,]
  newdf <- df[!duplicated(df[,tIDColName]),]
  Recency<-as.numeric(difftime(endDate,newdf[,tDateColName],units="days"))
  newdf <-cbind(newdf,Recency)
  newdf <- newdf[order(newdf[,tIDColName]),]
  fre <- as.data.frame(table(df[,tIDColName]))
  Frequency <- fre[,2]
  newdf <- cbind(newdf,Frequency)
  m <- as.data.frame(tapply(df[,tAmountColName],df[,tIDColName],sum))
  Monetary <- m[,1]/Frequency
  newdf <- cbind(newdf,Monetary)
  
  return(newdf)
  getIndependentScore(df,r=5,f=5,m=5)
  getIndependentScore <- function(df,r=5,f=5,m=5) 
    
    if (r<=0 || f<=0 || m<=0) return
    df <- df[order(df$Recency,-df$Frequency,-df$Monetary),]
    R_Score <- scoring(df,"Recency",r)
    df <- cbind(df, R_Score)
    
    df <- df[order(-df$Frequency,df$Recency,-df$Monetary),]
    F_Score <- scoring(df,"Frequency",f)
    df <- cbind(df, F_Score)
    
    df <- df[order(-df$Monetary,df$Recency,-df$Frequency),]
    M_Score <- scoring(df,"Monetary",m)
    df <- cbind(df, M_Score)
    df <- df[order(-df$R_Score,-df$F_Score,-df$M_Score),]
    Total_Score <- c(100*df$R_Score + 10*df$F_Score+df$M_Score)
    df <- cbind(df,Total_Score)
    
    return (df)
    scoring(df,column,r=5);
    scoring <- function (df,column,r=5)
      len <- dim(df)[1]
    
    score <- rep(0,times=len)
    nr <- round(len / r)
    if (nr > 0){
      rStart <-0
      rEnd <- 0
      for (i in 1:r)
        rStart = rEnd+1
      if (rStart> i*nr) next
      
      if (i == r){
        if(rStart<=len ) rEnd <- len else next
      }else{
        rEnd <- i*nr
      }
      score[rStart:rEnd]<- r-i+1
      s <- rEnd+1
      if(i<r & s <= len){
        for(u in s: len){
          if(df[rEnd,column]==df[u,column]){
            score[u]<- r-i+1
            rEnd <- u
          }else{
            break;
          }
        }
        
      }
      
      }

return(score)
    getScoreWithBreaks(df,r,f,m)
    getScoreWithBreaks <- function(df,r,f,m) 
      
      ## scoring the Recency
      len = length(r)
      R_Score <- c(rep(1,length(df[,1])))
      df <- cbind(df,R_Score)
      for(i in 1:len){
        if(i == 1){
          p1=0
        }else{
          p1=r[i-1]
        }
        p2=r[i]
        
        if(dim(df[p1<df$Recency & df$Recency<=p2,])[1]>0) df[p1<df$Recency & df$Recency<=p2,]$R_Score = len - i+ 2
      }
      
      ## scoring the Frequency	
      len = length(f)
      F_Score <- c(rep(1,length(df[,1])))
      df <- cbind(df,F_Score)
      for(i in 1:len){
        if(i == 1){
          p1=0
        }else{
          p1=f[i-1]
        }
        p2=f[i]
        
        if(dim(df[p1<df$Frequency & df$Frequency<=p2,])[1]>0) df[p1<df$Frequency & df$Frequency<=p2,]$F_Score = i
      }
      if(dim(df[f[len]<df$Frequency,])[1]>0) df[f[len]<df$Frequency,]$F_Score = len+1
      
      ## scoring the Monetary	
      len = length(m)
      M_Score <- c(rep(1,length(df[,1])))
      df <- cbind(df,M_Score)
      for(i in 1:len){
        if(i == 1){
          p1=0
        }else{
          p1=m[i-1]
        }
        p2=m[i]
        
        if(dim(df[p1<df$Monetary & df$Monetary<=p2,])[1]>0) df[p1<df$Monetary & df$Monetary<=p2,]$M_Score = i
      }
      if(dim(df[m[len]<df$Monetary,])[1]>0) df[m[len]<df$Monetary,]$M_Score = len+1
      
      #order the dataframe by R_Score, F_Score, and M_Score desc
      df <- df[order(-df$R_Score,-df$F_Score,-df$M_Score),]
      
      # caculate the total score
      Total_Score <- c(100*df$R_Score + 10*df$F_Score+df$M_Score)
      
      df <- cbind(df,Total_Score)
      
      return(df)
      drawHistograms(df,r,f,m)
     
      #################################################################################
      drawHistograms <- function(df,r=5,f=5,m=5){
        
        #set the layout plot window
        par(mfrow = c(f,r))
        
        names <-rep("",times=m)
        for(i in 1:m) names[i]<-paste("M",i)
        
        
        for (i in 1:f){
          for (j in 1:r){
            c <- rep(0,times=m)
            for(k in 1:m){
              tmpdf <-df[df$R_Score==j & df$F_Score==i & df$M_Score==k,]
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
      df1 <-getIndepandentScore(df) 
      head(df1[-(2:3)])
      
      
      
      
      