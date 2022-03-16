function(input, output, session) {
  #browser() ## permet d'arreter le code pour debogage
  
  ### pour stocker l'objet datamto (input$file1)
  ### datamto est une fonction, il faut l'appeler avec des ()
  datamto <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             dec = input$dec)
  })
  
  ### pour stocker l'objet param (input$file2)
  ### datamto est une fonction, il faut l'appeler avec des ()
  param <- reactive({
    req(input$file2)
    read.csv(input$file2$datapath,
             header = input$header,
             sep = input$sep,
             dec = input$dec)
  })
  
  resultsDataFrame <- reactive({
    # Fonction Potentiel de base Pb=f(FTSW)
    Pb<-function(x,a=1.0572,b=5.3452)
    {
      y<-(log(x)-log(a))/b
      y[y<(-1.5)]=-1.5
      return (y)
    }
    #
    ## source model Walis
    source(file="Walis_2011-02-28.R",encoding="UTF-8")
    ## source model Kmax
    source(file="Module_calcul_Kmax_2012.R",encoding="UTF-8")
    
    par <- param()
    par=data.frame(Valeur=par[,2],row.names=par[,1])
    donnees_mto <- datamto()
    
    ## Rapport de calcul
    #rap <- file("rapport_simul.txt", open="wt") # creation du fichier rapport
    #sink(rap,type="output")  # on ?crit les print()
    #sink(rap,type="message") # pour ajouter les textes d'erreur
    
    if (par["calcul_kmax",1]==1) {
      km=Radiation_Interception_DI(r_lat=par["latitude",1],d_r=par["d_rang",1],hf_r=par["hfeuillage",1],
                                   lf_r=par["lfeuillage",1],poro_r=par["porosite",1],az_r=par["orientation",1])
      print(paste("Kmax calcul?=",round(km,digits=3)))
    }else{km=par["kmax",1]}
    if (par["p",1]==0) {ttsw_herbe=0.001}else{ttsw_herbe=par["TTSWh",1]}
    # Nombre d'annee : nb_ans donne le nombre d'annees dans le fichier
    fmt_date="%d/%m/%Y"
    nb_ans<-length(unique(as.POSIXlt(strptime(donnees_mto[,1],fmt_date))$year))
    debr=format(strptime("01/01","%d/%m")+par["dbr",1]*3600*24,"%d/%m")
    #### parametrage
    ###############################
    parametres=list(p=par["p",1],
                    Tonte="N", 							# caractere {O,N} d'activation des tontes aux dates prevues (renint LAI)(si "N", tontes automatique /LAI)
                    nbtonte=rep(1,nb_ans), 			# Definit le nombre de tonte chaque ann?e de simulation
                    inn=par["inn",1], 							# D?finit l?INN de l?herbe
                    irri="N", 							# caractere {O,N} d'activation de l'irrigation qui branche la colonne "IR" du fichier meteo 
                    dtonte=rep("29/03",nb_ans), 			# date des tontes active par le parametre "Tonte" 
                    dbr=rep(debr,nb_ans), 				# dates de demarrage de la vegetation de la vigne (en nombre >=superieur ou egal au nombre d'annees simulees)
                    arretTr="01/11", 					# date d'arret de transpiration de la vigne
                    ddkmax=par["ddkmax",1], 						# nombre de degre jour pour atteindre le Kmax
                    reinit=ifelse(par["reinit",1]==0,"N","O"),							# caractere {O,N} pour reinitialiser les stocks d'eau en debut d'annee
                    pir=par["pir",1], 								# Permet d'attribuer un CN different (donne par CN_Ruiss2)  l'inter-rang meme en absence d'enherbement. 
                    seuilpeff=par["seuilpeff",1],						# seuil de pluie efficace
                    kmax=rep(km,nb_ans),					# coefficient d'interception du rayonnement de la vigne
                    levee="01/01",						# date de levee de l'enherbement 
                    flagRuiss="O",						# utilisation du module de ruissellement "O" active
                    CN_Ruiss=par["CN_Ruiss",1],						# CN (ruissellement) sous le rang
                    CN_Ruiss2=par["CN_Ruiss2",1],						# CN (ruissellement) dans l?inter-rang
                    herblairate=par["herblairate",1],					# LAI rate de l'enherbement
                    LAIinit=par["LAIinit",1],						# LAI initial de l'enherbement
                    LAIres=par["LAIres",1],							# LAI residuel de l'enherbement apres une tonte
                    LLSmin=par["LLSmin",1],							# LLSmin de l'herbe
                    LAItonte=par["LAItonte",1],							# LAI a partir duquel on declenche une tonte automatiquement
                    U=par["U",1],								# parametre U d'evaporation du sol
                    b1_evap=par["b1_evap",1],							# parametre b1_evap d'evaporation du sol
                    b2_evap=par["b2_evap",1],						# parametre b2_evap d'evaporation du sol
                    FTSWhregultr=par["FTSWhregultr",1],					# FTSW du compartiment herbe a partir de laquelle l'herbe regule sa transpiration
                    FTSWhregulLAI=par["FTSWhregulLAI",1],					# FTSW du compartiment herbe a partir de laquelle l'herbe regule son LAI
                    TTSWh=ttsw_herbe,							# TTSW du compartiment herbe
                    TTSWv=par["TTSWv",1],							# TTSW du compartiment vigne
                    FTSWvregulTR=par["FTSWvregulTR",1],					# FTSW du compartiment vigne a partir de laquelle la vigne regule sa transpiration
                    ASWhinit=ttsw_herbe*par["p_remplissage",1],						# quantite d'eau dans le compartiment vigne en debut de simulation
                    ASWvinit=par["TTSWv",1]*par["p_remplissage",1],						# quantite d'eau dans le compartiment herbe  en debut de simulation
                    format_date=fmt_date            
    )
    
    ### execution de walis
    BH_resultats=walis.model.single(parametres,donnees_mto)
    resultsDataFrame <- data.frame(BH_resultats$meteo,BH_resultats$FTSWtot,Pb(BH_resultats$FTSWtot),BH_resultats$FTSWv,BH_resultats$FTSWh,
                                   BH_resultats$ASWtot,BH_resultats$ASWv,BH_resultats$ASWh,BH_resultats$Tv,
                                   BH_resultats$ETRh,BH_resultats$ESn)
    colnames(resultsDataFrame) <- c("Date","Temp","Pluie","ETP","IR","FTSWtot","FTSWtot_en_Potbase","FTSWv","FTSWh","ASWtot","ASWv","ASWh","Tv","ETRh","ESn")
    return(resultsDataFrame)
  })
  
  
  output$tableMto <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    if(input$disp == "head") {
      return(head(param()))
    }
    else {
      return(param())
    }
    
  })
  
  output$tableParam <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    if(input$disp == "head") {
      return(head(datamto()))
    }
    else {
      return(datamto())
    }
    
  })
  
  # output$distPlot <- renderPlot({
  #     hist(datamto()$Pluie)
  # })
  # 
  
  output$distPlot <- renderPlot({
    library(ggplot2)
    ggplot(resultsDataFrame(), aes(as.Date(Date,format="%d/%m/%Y"),FTSWtot)) + geom_line() + xlab("") + 
      geom_col(aes(as.Date(Date,format="%d/%m/%Y"),Pluie/200),size=1,col="lightblue",fill="white") + 
      scale_y_continuous(sec.axis = sec_axis(~.*200,name = "Pluie"))
  })
  
  # Downloadable csv of selected dataset ----
  ### pour le moment on a mis le fichier meteo, mais il faudrait mettre le fichier des resultats
  ### Ã corriger en creant une fonction reactive pour garder en memoire le fichier resultats de simulation
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("simulation-",Sys.Date(),".csv",sep="")
    },
    content = function(file) {
      write.csv2(resultsDataFrame(),file,row.names = FALSE)
    })    
}
