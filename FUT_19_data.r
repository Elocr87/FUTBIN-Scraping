library(rvest) #scrapping
library(stringr) #str_match_all


setwd("C:\\Users\\XXX\\Documents\\")

FUT_2=data.frame() #create empty data frame

pages=628 #Number of pages in https://www.futbin.com/19/players

for (n in 1:pages){ 

  FUT=data.frame() #create empty data frame
  league=data.frame() #create empty data frame
  
  url=gsub("[[:space:]]","",paste("https://www.futbin.com/players?page=",n))

  html <- read_html(url)  
  
  ####PLAYERS
  
  html2=as.data.frame(html_table(html,fill = TRUE)[3])
  
  html2=html2[,-16]
  
  html2[,15]=substr(html2[,15],1,3)
  
  #sPLIT WORK RATES INTO ATTACKING AND DEFENDING
  
  WR_AT=substr(html2[,8],1,1)
  WR_DEF=substr(html2[,8],5,5)
  WR=cbind(AT,DEF)
  
  html3=cbind(html2[,1:7],WR_AT,WR_DEF,html2[,9:17])
  
  FUT=rbind(FUT,html3)
  
  ############TYPE OF CARD
  
  rare <- read_html(url)%>% html_nodes("tr > td") 
  
  rare=data.frame(as.character(str_match_all(rare, "<td><span class=\"(.*?)\"")))
  
  rare_2=data.frame()
  
  for (n_3 in seq(from=5, to=nrow(rare), by=18)){
    #n_2=15
    
    rare_2=rbind(rare_2, as.character(rare[n_3,1]),stringsAsFactors = FALSE)
    
  }
  colnames(rare_2)="TYPE"
  
  rare_2=data.frame(rare_2[rare_2$TYPE!="character(0)",])
  
  rare_3=data.frame()
  
  ###SPLITING TYPE OF CARD AND IF RARE OR NON-RARE
  
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
  
  ####GETTING CARD LINKS
  
  rm(futbin_links)
  matched <- as.data.frame(str_match_all(html, "<a href=\"(.*?)\""))
  futbin_links=data.frame("1")
  
  for (n_1 in seq(from=88, to=204, by=4)){
    #n=97
    ##print(matched[n,1])
    
    futbin_links=cbind(futbin_links,as.character(matched[n_1,1]))
    
  }
  
  colnames(futbin_links)=""
  futbin_links=t(futbin_links[2:31])
  
  futbin_links=gsub('<a href="/19/',"",futbin_links)
  futbin_links=gsub('\"',"",futbin_links)
  
  
  FUTBIN_date=data.frame() #create empty data frame
  P_S=data.frame() #☺Player_Stats
  
  #OPENING ALL LINKS  TO GET CARD'S STATS
  
  for (n_2 in 1:nrow(futbin_links)){
    #n_2=2
    
    ####DATES CARD WERE ON
    url_2=gsub("[[:space:]]","",paste("https://www.futbin.com/19/",futbin_links[n_2,1]))
    html_players=read_html(url_2) %>%html_node(".table-info")%>%html_text()
    
    date_1=as.data.frame(strsplit(gsub("(\r|\n)","",html_players)," "))
    date_1=date_1[!apply(date_1 == "", 1, all),]
    date_1=data.frame(date_1)
    
    on=which(date_1 == "on", arr.ind = TRUE)
    date_1=date_1[on[1,1]+1,1]
    
    FUTBIN_date_tmp=data.frame()
    FUTBIN_date_tmp=rbind(FUTBIN_date_tmp,as.character(date_1))
    
    colnames(FUTBIN_date_tmp)[1]='DATE_ON'
    
    FUTBIN_date=rbind(FUTBIN_date,FUTBIN_date_tmp)
    
    rm(FUTBIN_date_tmp)
    
    #PLAYER STATS
    
    html_players_2=read_html(url_2) %>%html_node(".mid-p-nav-blur")%>%html_text()
    player_Stats=as.data.frame(strsplit(gsub("(\r|\n)","",html_players_2)," "))
    player_Stats=player_Stats[!apply(player_Stats == "", 1, all),]
    player_Stats=data.frame(player_Stats)
    
    Games=which(player_Stats == "Games", arr.ind = TRUE)
    Games=player_Stats[Games[1,1]+1,1]
    
    Goals=which(player_Stats == "Goals", arr.ind = TRUE)
    Goals=player_Stats[Goals[1,1]+1,1]
    
    Assists=which(player_Stats == "Assists", arr.ind = TRUE)
    Assists=player_Stats[Assists[1,1]+1,1]
    
    Yellow=which(player_Stats == "Yellow", arr.ind = TRUE)
    Yellow=player_Stats[Yellow[1,1]+1,1]
    
    Red=which(player_Stats == "Red", arr.ind = TRUE)
    Red=player_Stats[Red[1,1]+1,1]
    
    
    P_S=rbind(P_S,cbind(as.character(Games),as.character(Goals),as.character(Assists),as.character(Yellow),as.character(Red)))
    
  }
  
  
  FUT_2= rbind(FUT_2,cbind(rare_3, FUT,league, FUTBIN_date, P_S)) #bind all data frames
  print("---------------------------------------------------------------------------------------------")
  print(n)
  print("---------------------------------------------------------------------------------------------")
  print(tail(FUT_2))
  
}


FUT=FUT_2 #new D.F. 

FUT=cbind(FUT,Sys.Date()-1) #Adding yesterday's day

colnames(FUT)=c("CARD", "RARE" ,"Name","RAT","POS","VER","PRICE","SKILL","WF","WR_AT","WR_DEF","PAC","SHO","PAS","DRI","DEF",
                "PHY","HEIGHT","BS","IGS","TEAM","COUNTRY","LEAGUE",
                "DATE_ON", "G_PLAYED","GOALS_SC","ASSISTS_P","YELLOW_P","RED_P","DATE")

#NORMALIZING PRICE INTO MILLIONS

for (n in 1:nrow(FUT)){
  
  if (str_sub(FUT[n,7],-1)=='M'){
    FUT[n,7]=as.numeric(substr(FUT[n,7],1,nchar(FUT[n,7])-1))
  }else if(str_sub(FUT[n,7],-1)=='K'){
    FUT[n,7]=as.numeric(substr(FUT[n,7],1,nchar(FUT[n,7])-1))/1000
    
  }else{
    FUT[n,7]=as.numeric(FUT[n,7])/1000000
  }
  
}  

FUT=FUT[!(FUT$Name=='Quincy Promes'&FUT$PRICE==0),] #Removing one Record

#ADDING SBC PRICES

FUT[FUT$Name=='Eden Hazard'&FUT$VER=="PL POTM",][7]=0.68025
FUT[FUT$Name=='Kevin De Bruyne'&FUT$VER=="FUTmas SBC",][7]=0.569
FUT[FUT$Name=='Toni Kroos'&FUT$VER=="CL TOTT SBC",][7]=0.1151
FUT[FUT$Name=='Isco'&FUT$VER=="FUTmas SBC",][7]=0.1053
FUT[FUT$Name=='Dani Alves'&FUT$VER=="Flashback SBC",][7]=0.25815
FUT[FUT$Name=='Lorenzo Insigne'&FUT$VER=="CL TOTT SBC",][7]=0.3874
FUT[FUT$Name=='Marco Reus'&FUT$RAT==93&FUT$VER=="Bundes POTM",][7]=0.8654
FUT[FUT$Name=='Coutinho'&FUT$VER=="FUTmas SBC",][7]=0.14065
FUT[FUT$Name=='Thiago Silva'&FUT$VER=="FUTmas SBC",][7]=0.12485
FUT[FUT$Name=='Aubameyang'&FUT$VER=="PL POTM",][7]=0.54275
FUT[FUT$Name=='Marco Reus'&FUT$RAT==91&FUT$VER=="Bundes POTM",][7]=0.36955
FUT[FUT$Name=='Daniel Sturridge'&FUT$VER=="Flashback SBC",][7]=0.1757
FUT[FUT$Name=='Alexandre Pato'&FUT$VER=="Flashback SBC",][7]=0.15795
FUT[FUT$Name=='Raheem Sterling'&FUT$VER=="PL POTM",][7]=0.22645
FUT[FUT$Name=='Roberto Firmino'&FUT$VER=="FUTmas SBC",][7]=0.1726
FUT[FUT$Name=='Andrés Iniesta Luján'&FUT$VER=="FUTmas SBC",][7]=0.4935
FUT[FUT$Name=='Jérôme Boateng'&FUT$VER=="FUTmas SBC",][7]=0.15835
FUT[FUT$Name=='Juanfran'&FUT$VER=="Flashback SBC",][7]=0.15495
FUT[FUT$Name=='Florian Thauvin'&FUT$VER=="SBC",][7]=0.2533
FUT[FUT$Name=='Edin Džeko'&FUT$VER=="SBC",][7]=0.26685 
FUT[FUT$Name=='Milan Škriniar'&FUT$VER=="SBC",][7]=0.26685 
FUT[FUT$Name=='Mario Götze'&FUT$VER=="Flashback SBC",][7]=0.0945
FUT[FUT$Name=='Sergej Milinkovic-Savic'&FUT$VER=="UEL LIVE",][7]=0.1607
FUT[FUT$Name=='Henrikh Mkhitaryan'&FUT$VER=="UEL LIVE SBC",][7]=0.20545
FUT[FUT$Name=='Javier Hernández'&FUT$VER=="Flashback SBC",][7]= 0.0955
FUT[FUT$Name=='Blaise Matuidi'&FUT$VER=="FUTmas SBC",][7]=0.14845
FUT[FUT$Name=='Lucas'&FUT$VER=="PL POTM",][7]=0.0346
FUT[FUT$Name=='Memphis Depay'&FUT$VER=="SBC",][7]=0.2533
FUT[FUT$Name=='Marcos Alonso'&FUT$VER=="UEL LIVE SBC",][7]=0.13
FUT[FUT$Name=='José Callejón'&FUT$VER=="FUTmas SBC",][7]=0.11435
FUT[FUT$Name=='Heung Min Son'&FUT$VER=="FUTmas SBC",][7]=0.21355
FUT[FUT$Name=='Dele Alli'&FUT$VER=="FUTmas SBC",][7]=0.1084
FUT[FUT$Name=='Rodrigo'&FUT$VER=="FUTmas SBC",][7]=0.12255
FUT[FUT$Name=='Jadon Sancho'&FUT$VER=="Bundes POTM",][7]=0.08875
FUT[FUT$Name=='Mateo Kovacic'&FUT$VER=="FUTmas SBC",][7]=0.0738
FUT[FUT$Name=='Aymeric Laporte'&FUT$VER=="CL TOTT SBC",][7]=0.0578
FUT[FUT$Name=='Alejandro Gómez'&FUT$VER=="FUTmas SBC",][7]=0.08315
FUT[FUT$Name=='Antonio Valencia'&FUT$VER=="FUTmas SBC",][7]=0.08685
FUT[FUT$Name=='Koke'&FUT$VER=="Halloween SBC",][7]=0.564
FUT[FUT$Name=='Lucas Torreira'&FUT$VER=="FUTmas SBC",][7]=0.0274
FUT[FUT$Name=='Héctor Herrera'&FUT$VER=="SBC",][7]=0.15445
FUT[FUT$Name=='Kevin-Prince Boateng'&FUT$VER=="Flashback SBC",][7]=0.1516
FUT[FUT$Name=='Patrick van Aanholt'&FUT$VER=="FUTmas SBC",][7]=0.0401
FUT[FUT$Name=='Marcus Rashford'&FUT$VER=="FUTmas SBC",][7]=0.1312
FUT[FUT$Name=='Santiago Arias'&FUT$VER=="FUTmas SBC",][7]=0.01835
FUT[FUT$Name=='David Silva'&FUT$RAT==93,][6]='TOTY Nominee SBC'
FUT[FUT$Name=='David Silva'&FUT$VER=='TOTY Nominee SBC',][7]=0.343
FUT[FUT$Name=='Gareth Bale'&FUT$VER=='TOTY Nominee SBC',][7]=1.53
FUT[FUT$Name=='Zlatan Ibrahimovic'&FUT$VER=='Flashback SBC',][7]=0.2017
FUT[FUT$Name=='Casillas'&FUT$VER=='Flashback SBC',][7]=0.07305
FUT[FUT$Name=='Carlos Tévez'&FUT$VER=='Flashback SBC',][7]=0.1411
FUT[FUT$Name=='Virgil van Dijk'&FUT$VER=="PL POTM",][7]=0.5825
FUT[FUT$Name=='Šime Vrsaljko'&FUT$VER=='TOTY Nominee SBC',][7]=0.6435
FUT[FUT$Name=='Marco Reus'&FUT$RAT==94&FUT$VER=="Bundes POTM",][7]=0.905
FUT[FUT$Name=='Frenkie de Jong'&FUT$RAT==90,][7]=0.153
FUT[FUT$Name=='Wayne Rooney'&FUT$VER=="Flashback SBC",][7]=0.309
FUT[FUT$Name=='Casemiro'&FUT$VER=="SBC",][7]=0.26
FUT[FUT$Name=='Samuel Umtiti'&FUT$VER=="SBC",][7]=0.26
FUT[FUT$Name=='Iago Aspas'&FUT$VER=="SBC",][7]=0.26
FUT[FUT$Name=='Klaas-Jan Huntelaar'&FUT$VER=="Flashback SBC",][7]=0.1094
FUT[FUT$Name=='Fernando Torres'&FUT$VER=="Flashback SBC",][7]=0.274
FUT[FUT$Name=='Marek Hamšik'&FUT$VER=="Premium SBC",][7]=0.379
FUT[FUT$Name=='Carvajal'&FUT$VER=="",][6]='Headliners SBC'
FUT[FUT$Name=='Carvajal'&FUT$VER=='Headliners SBC',][7]=0.161
FUT[FUT$Name=='Marko Arnautovic'&FUT$VER=='Headliners SBC',][7]=0.168
FUT[FUT$Name=='Leighton Baines'&FUT$VER=="Flashback SBC",][7]=0.149
FUT[FUT$Name=='Marcus Rashford'&FUT$VER=="PL POTM",][7]=0.483
FUT[FUT$Name=='Serge Gnabry'&FUT$VER=="SBC",][7]=0.168
FUT[FUT$Name=='Tanguy Ndombele'&FUT$VER=="Weekly Obj- FS",][6]='FUT Future Stars'
FUT[FUT$Name=='Tanguy Ndombele'&FUT$VER=='FUT Future Stars',][7]=0.342
FUT[FUT$Name=='Abdoulaye Doucouré'&FUT$VER=="SBC",][7]=0.293
FUT[FUT$Name=='Jamie Vardy'&FUT$VER=="SBC",][7]=0.293
FUT[FUT$Name=='Ahmed Musa'&FUT$VER=="SBC",][7]=0.112
FUT[FUT$Name=='Joelinton'&FUT$VER=="",][6]='FUT Future Stars'
FUT[FUT$Name=='Joelinton'&FUT$VER=='FUT Future Stars',][7]=0.0534
FUT[FUT$Name=='Allan Saint-Maximin'&FUT$VER=="",][6]='FUT Future Stars'
FUT[FUT$Name=='Allan Saint-Maximin'&FUT$VER=='FUT Future Stars',][7]=0.143
FUT[FUT$Name=='Alphonso Davies'&FUT$VER=="",][6]='FUT Future Stars'
FUT[FUT$Name=='Alphonso Davies'&FUT$VER=='FUT Future Stars',][7]=0.210
FUT[FUT$Name=='Marco Reus'&FUT$RAT==93&FUT$VER=="Bundes POTM",][7]=0.865
FUT[FUT$Name=='Marco Reus'&FUT$RAT==94&FUT$VER=="Bundes POTM",][7]=0.872
FUT[FUT$Name=='Leon Goretzka'&FUT$VER=="Bundes POTM",][7]=0.486
FUT[FUT$Name=='Petr Cech'&FUT$VER=='Premium SBC',][7]=0.106
FUT[FUT$Name=='Dimitri Payet'&FUT$VER=="Flashback SBC",][7]=0.263
FUT[FUT$Name=='David Luiz'&FUT$VER=="Flashback SBC",][7]=0.340
FUT[FUT$Name=='Falcao'&FUT$VER=="Europa SBC",][7]=0.055
FUT[FUT$Name=='Tim Howard'&FUT$VER=="Premium SBC",][7]=0.042
FUT[FUT$Name=='Kostas Manolas'&FUT$VER=="CL SBC",][7]=0.127
FUT[FUT$Name=='Joe Gomez'&FUT$VER=="SBC - Future Stars",][7]=0.544
FUT[FUT$Name=='David Villa'&FUT$VER=="Flashback SBC",][7]=0.184
FUT[FUT$Name=='Daniele De Rossi'&FUT$VER=="Flashback SBC",][7]=0.249
FUT[FUT$Name=='Vincent Kompany'&FUT$VER=="Flashback SBC",][7]=0.269
FUT[FUT$Name=='Arturo Vidal'&FUT$VER=="Flashback SBC",][7]=0.343
FUT[FUT$Name=='Sergio Agüero'&FUT$VER=="PL POTM",][7]=1.41
FUT[FUT$Name=='Timo Werner'&FUT$VER=="FUT Birthday SBC",][7]=0.445
FUT[FUT$Name=='Julian Brandt'&FUT$VER=="Bundes POTM",][7]=0.125


FUT[,7]=gsub("\\.", ",", FUT[,7])
FUT[,25]=gsub("\\,", ".", FUT[,25])
FUT[,26]=gsub("\\.", ",", FUT[,26])
FUT[,27]=gsub("\\.", ",", FUT[,27])
FUT[,28]=gsub("\\.", ",", FUT[,28])
FUT[,29]=gsub("\\.", ",", FUT[,29])

#CHECKING RECORDS
head(FUT)
tail(FUT)

write.csv(FUT,"C://Users//xxx//FUT_19.csv",sep=";",row.names=FALSE)
