  
FUT_2=data.frame()

for (n in 1:114){ #ORO
    #for (n in 1:266){ #PLATA
    
    FUT=data.frame()
    p=data.frame()
    ligas=data.frame()
    
    #n=50
    url=gsub("[[:space:]]","",paste("https://www.futbin.com/players?page=",n,"&version=gold"))
    #url=gsub("[[:space:]]","",paste("https://www.futbin.com/players?page=",n,"&version=silver"))
    
    html <- read_html(url)  
    
    ####JUGADORES
    
    html2=as.data.frame(html_table(html,fill = TRUE)[3])
    
    html2=html2[,-16]
    
    html2[,15]=substr(html2[,15],1,3)
    
    WR_AT=substr(html2[,8],1,1)
    WR_DEF=substr(html2[,8],5,5)
    WR=cbind(AT,DEF)
    
    html3=cbind(html2[,1:7],WR_AT,WR_DEF,html2[,9:17])
    
    FUT=rbind(FUT,html3)
    
    ###TIPO JUGADOR
    
    unico <- read_html(url)%>% html_nodes("tr > td") 
    
    unico=data.frame(as.character(str_match_all(unico, "<td><span class=\"(.*?)\"")))
    
    unico_2=data.frame()
    
    for (n_3 in seq(from=5, to=nrow(unico), by=18)){
      #n_2=15
      
      unico_2=rbind(unico_2, as.character(unico[n_3,1]),stringsAsFactors = FALSE)
      
    }
    colnames(unico_2)="TIPO"
    #unico_2=data.frame(t(unico_2))
    unico_2=data.frame(unico_2[unico_2$TIPO!="character(0)",])
    
    unico_3=data.frame()
    
    for (n_4 in 1:nrow(unico_2)){
      
      split=gsub("\\W","",as.data.frame(strsplit(as.character(unico_2[n_4,1])," "))[13,1])
      
      unico_3=rbind(unico_3,as.character(split),stringsAsFactors = FALSE)
      
    }
    colnames(unico_3)="X.rare."
    ####LIGAS
    
    html_liga=read_html(url)%>%html_nodes(".players_club_nation")
    
    for (m in 1:30){
      a=as.character(xml_children(html_liga[m]))
      
      #a=as.character(xml_children(html[1]))
      
      b=gsub("\\, |\\,|","",strsplit(a, "data-original-title="))
      c=as.data.frame(strsplit(b,"[\\\"]"))
      
      d=cbind(as.character(c[10,1]),as.character(c[10,2]),as.character(c[10,3]))
      
      ligas=rbind(ligas,d)
    }
    
    ####JUGADORES FECHAS y STATS
    
    rm(futbin_links)
    matched <- as.data.frame(str_match_all(html, "<a href=\"(.*?)\""))
    futbin_links=data.frame("1")
    
    for (n_1 in seq(from=86, to=202, by=4)){
      #n=97
      ##print(matched[n,1])
      
      futbin_links=cbind(futbin_links,as.character(matched[n_1,1]))
      
    }
    
    colnames(futbin_links)=""
    futbin_links=t(futbin_links[2:31])
    
    futbin_links=gsub('<a href="/19/',"",futbin_links)
    futbin_links=gsub('\"',"",futbin_links)
    
    FUTBIN_fecha=data.frame()
    P_S=data.frame()
    for (n_2 in 1:nrow(futbin_links)){
      #n_2=2
      ####fechas  
      url_2=gsub("[[:space:]]","",paste("https://www.futbin.com/19/",futbin_links[n_2,1]))
      html_players=read_html(url_2) %>%html_node(".table-info")%>%html_text()
      
      fec=as.data.frame(strsplit(gsub("(\r|\n)","",html_players)," "))
      fec=fec[!apply(fec == "", 1, all),]
      fec=data.frame(fec)
      
      on=which(fec == "on", arr.ind = TRUE)
      fec=fec[on[1,1]+1,1]
      
      FUTBIN_fecha_tmp=data.frame()
      FUTBIN_fecha_tmp=rbind(FUTBIN_fecha_tmp,as.character(fec))
      
      colnames(FUTBIN_fecha_tmp)[1]='DATE_ON'
      
      FUTBIN_fecha=rbind(FUTBIN_fecha,FUTBIN_fecha_tmp)
      
      rm(FUTBIN_fecha_tmp)
      
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
    
    
    FUT_2= rbind(FUT_2,cbind("GOLD",unico_3, FUT,ligas, FUTBIN_fecha, P_S))
    print("---------------------------------------------------------------------------------------------")
    print(n)
    print("---------------------------------------------------------------------------------------------")
    print(tail(FUT_2))
    
  }
  
  
FUT=FUT_2

ncol(FUT)

colnames(FUT)=c("CARD", "RARE-NON" ,"Name","RAT","POS","VER","PRICE","SKILL","WF","WR_AT","WR_DEF","PAC","SHO","PAS","DRI","DEF",
                "PHY","HEIGHT","BS","IGS","TEAM","COUNTRY","LEAGUE",
                "DATE_ON", "G_PLAYED","GOALS_SC","ASSISTS_P","YELLOW_P","RED_P")

nrow(FUT[FUT$Name=='Pelé',])

for (n in 1:nrow(FUT)){
  
  if (str_sub(FUT[n,7],-1)=='M'){
    FUT[n,7]=as.numeric(substr(FUT[n,7],1,nchar(FUT[n,7])-1))
  }else if(str_sub(FUT[n,7],-1)=='K'){
    FUT[n,7]=as.numeric(substr(FUT[n,7],1,nchar(FUT[n,7])-1))/1000
    
  }else{
    FUT[n,7]=as.numeric(FUT[n,7])/1000000
  }
  
}  

FUT=FUT[!(FUT$Name=='Jadon Sancho'&FUT$POS=='LM'),]
FUT=FUT[!(FUT$Name=='Quincy Promes'&FUT$PRICE==0),]

FUT[FUT$Name=='Eden Hazard'&FUT$VER=="PL POTM",][6]=0.68025
FUT[FUT$Name=='Kevin De Bruyne'&FUT$VER=="FUTmas SBC",][6]=0.569
FUT[FUT$Name=='Toni Kroos'&FUT$VER=="CL TOTT SBC",][6]=0.1151
FUT[FUT$Name=='Isco'&FUT$VER=="FUTmas SBC",][6]=0.1053
FUT[FUT$Name=='Dani Alves'&FUT$VER=="Flashback SBC",][6]=0.25815
FUT[FUT$Name=='Lorenzo Insigne'&FUT$VER=="CL TOTT SBC",][6]=0.3874
FUT[FUT$Name=='Marco Reus'&FUT$RAT==90&FUT$VER=="Bundes POTM",][6]=0.8654
FUT[FUT$Name=='Coutinho'&FUT$VER=="FUTmas SBC",][6]=0.14065
FUT[FUT$Name=='Thiago Silva'&FUT$VER=="FUTmas SBC",][6]=0.12485
FUT[FUT$Name=='Aubameyang'&FUT$VER=="PL POTM",][6]=0.54275
FUT[FUT$Name=='Marco Reus'&FUT$RAT==88&FUT$VER=="Bundes POTM",][6]=0.36955
FUT[FUT$Name=='Daniel Sturridge'&FUT$VER=="Flashback SBC",][6]=0.1757
FUT[FUT$Name=='Alexandre Pato'&FUT$VER=="Flashback SBC",][6]=0.15795
FUT[FUT$Name=='Raheem Sterling'&FUT$VER=="PL POTM",][6]=0.22645
FUT[FUT$Name=='Roberto Firmino'&FUT$VER=="FUTmas SBC",][6]=0.1726
FUT[FUT$Name=='Andrés Iniesta Luján'&FUT$VER=="FUTmas SBC",][6]=0.4935
FUT[FUT$Name=='Jérôme Boateng'&FUT$VER=="FUTmas SBC",][6]=0.15835
FUT[FUT$Name=='Juanfran'&FUT$VER=="Flashback SBC",][6]=0.15495
FUT[FUT$Name=='Florian Thauvin'&FUT$VER=="SBC",][6]=0.2533
FUT[FUT$Name=='Edin Džeko'&FUT$VER=="SBC",][6]=0.26685 
FUT[FUT$Name=='Milan Škriniar'&FUT$VER=="SBC",][6]=0.26685 
FUT[FUT$Name=='Mario Götze'&FUT$VER=="Flashback SBC",][6]=0.0945
FUT[FUT$Name=='Sergej Milinkovic-Savic'&FUT$VER=="UEL LIVE",][6]=0.1607
FUT[FUT$Name=='Henrikh Mkhitaryan'&FUT$VER=="UEL LIVE SBC",][6]=0.20545
FUT[FUT$Name=='Javier Hernández'&FUT$VER=="Flashback SBC",][6]= 0.0955
FUT[FUT$Name=='Blaise Matuidi'&FUT$VER=="FUTmas SBC",][6]=0.14845
FUT[FUT$Name=='Lucas'&FUT$VER=="PL POTM",][6]=0.0346
FUT[FUT$Name=='Memphis Depay'&FUT$VER=="SBC",][6]=0.2533
FUT[FUT$Name=='Marcos Alonso'&FUT$VER=="UEL LIVE SBC",][6]=0.13
FUT[FUT$Name=='José Callejón'&FUT$VER=="FUTmas SBC",][6]=0.11435
FUT[FUT$Name=='Heung Min Son'&FUT$VER=="FUTmas SBC",][6]=0.21355
FUT[FUT$Name=='Dele Alli'&FUT$VER=="FUTmas SBC",][6]=0.1084
FUT[FUT$Name=='Rodrigo'&FUT$VER=="FUTmas SBC",][6]=0.12255
FUT[FUT$Name=='Jadon Sancho'&FUT$VER=="Bundes POTM",][6]=0.08875
FUT[FUT$Name=='Mateo Kovacic'&FUT$VER=="FUTmas SBC",][6]=0.0738
FUT[FUT$Name=='Aymeric Laporte'&FUT$VER=="CL TOTT SBC",][6]=0.0578
FUT[FUT$Name=='Alejandro Gómez'&FUT$VER=="FUTmas SBC",][6]=0.08315
FUT[FUT$Name=='Antonio Valencia'&FUT$VER=="FUTmas SBC",][6]=0.08685
FUT[FUT$Name=='Koke'&FUT$VER=="Halloween SBC",][6]=0.564
FUT[FUT$Name=='Lucas Torreira'&FUT$VER=="FUTmas SBC",][6]=0.0274
FUT[FUT$Name=='Héctor Herrera'&FUT$VER=="SBC",][6]=0.15445
FUT[FUT$Name=='Kevin-Prince Boateng'&FUT$VER=="Flashback SBC",][6]=0.1516
FUT[FUT$Name=='Patrick van Aanholt'&FUT$VER=="FUTmas SBC",][6]=0.0401
FUT[FUT$Name=='Marcus Rashford'&FUT$VER=="FUTmas SBC",][6]=0.1312
FUT[FUT$Name=='Santiago Arias'&FUT$VER=="FUTmas SBC",][6]=0.01835


FUT[,7]=gsub("\\.", ",", FUT[,7])
FUT[,26]=gsub("\\.", ",", FUT[,26])
FUT[,27]=gsub("\\.", ",", FUT[,27])
FUT[,28]=gsub("\\.", ",", FUT[,28])
FUT[,29]=gsub("\\.", ",", FUT[,29])


FUT_G=FUT
