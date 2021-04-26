##############################
#  Code works - 26/04/2021   #
##############################

library(rvest) #scraping
library(stringr) #str_match_all
library(xml2) #xml_children


setwd("C:\\Users\\XXX\\Documents\\") #Set your working Directory

FUT_2=data.frame() #create empty data frame

pages=147 #Number of pages in https://www.futbin.com/21/players

for (n in 1:pages){ 
  
  FUT=data.frame() #create empty data frame
  league=data.frame() #create empty data frame
  
  url=gsub("[[:space:]]","",paste("https://www.futbin.com/21/players?page=",n))
  
  html <- read_html(url)  
  
  ####PLAYERS
  
  html2=as.data.frame(html_table(html,fill = TRUE))
  
  html2=html2[,-16]
  
  #Height, body type and weight
  
  body=strsplit(html2[,15],'                                                                              ')
  
  body_info=data.frame()
  
  for (j in 1:length(body)){
    
    height=substr(body[[j]][1],1,3)
    
    body_type=trimws(strsplit(body[[j]][2],'(',fixed = TRUE)[[1]][1])
    weight=strsplit(body[[j]][2],'(',fixed = TRUE)[[1]][2]
    weight=gsub('[^0-9\\.]','',weight)
    
    
    body_info=rbind(body_info,cbind(height,body_type,weight))
    
  }
  
  #Split Work rates into attackint and Defending
  
  WR_AT=substr(html2[,8],1,1)
  WR_DEF=substr(html2[,8],5,5)
  
  html3=cbind(html2[,1:7],WR_AT,WR_DEF,html2[,9:14],body_info,html2[,16:17])
  
  FUT=rbind(FUT,html3)
  
  ############TYPE OF CARD
  
  rare <- read_html(url)%>% html_nodes("tr > td") 
  
  rare=data.frame(as.character(str_match_all(rare, "<td><span class=\"(.*?)\"")))
  
  rare_2=data.frame()
  
  for (n_3 in seq(from=2, to=nrow(rare), by=18)){
    #n_2=15
    
    rare_2=rbind(rare_2, as.character(rare[n_3,1]),stringsAsFactors = FALSE)
    
  }
  colnames(rare_2)="TYPE"
  
  rare_2=data.frame(rare_2[rare_2$TYPE!="character(0)",])
  
  rare_3=data.frame()
  
  ###Split type of card and if Rare or Non-rare
  
  for (n_4 in 1:nrow(rare_2)){
    
    split_1=gsub("\\W","",as.data.frame(strsplit(as.character(rare_2[n_4,1])," "))[12,1])
    split_2=gsub("\\W","",as.data.frame(strsplit(as.character(rare_2[n_4,1])," "))[13,1])
    
    split=cbind(split_1,split_2)
    
    rare_3=rbind(rare_3,as.character(split),stringsAsFactors = FALSE)
    
  }
  colnames(rare_3)=c("","X.rare.")
  
  ####LEAGUES
  
  html_league=read_html(url)%>%html_nodes(".players_club_nation")
  
  for (m in 1:30){
    league_1=as.character(xml_children(html_league[m]))
    
    league_2=gsub("\\, |\\,|","",strsplit(league_1, "data-original-title="))
    league_3=as.data.frame(strsplit(league_2,"[\\\"]"))
    
    league_4=cbind(as.character(league_3[10,1]),as.character(league_3[10,2]),as.character(league_3[10,3]))
    
    league=rbind(league,league_4)
  }
  
  FUT_2= rbind(FUT_2,cbind(rare_3, FUT,league)) #bind all data frames
  print("---------------------------------------------------------------------------------------------")
  print(n)
  print("---------------------------------------------------------------------------------------------")
  print(tail(FUT_2))
  
  #Delay the loop 10 seconds
  Sys.sleep(10)
  
}


FUT=FUT_2 #new D.F. 

rm(FUT_2) #Remove DF

#Change colunms names
colnames(FUT)=c("CARD", "RARE-NON" ,"NAME","RAT","POS","VER","PRICE","SKILL","WF","WR_AT","WR_DEF","PAC","SHO","PAS","DRI","DEF",
                "PHY","HEIGHT","BODY_TYPE","WEIGHT","BS","IGS","TEAM","COUNTRY","LEAGUE")

#Normalizing price into millions

for (n in 1:nrow(FUT)){
  
  if (str_sub(FUT[n,7],-1)=='M'){
    FUT[n,7]=as.numeric(substr(FUT[n,7],1,nchar(FUT[n,7])-1))
  }else if(str_sub(FUT[n,7],-1)=='K'){
    FUT[n,7]=as.numeric(substr(FUT[n,7],1,nchar(FUT[n,7])-1))/1000
    
  }else{
    FUT[n,7]=as.numeric(FUT[n,7])/1000000
  }
  
}  

tail(FUT) #Showing tail of DF

#Save the DF
write.csv(FUT,"C://Users//xxx//FUT_20.csv",sep=";",row.names=FALSE)
