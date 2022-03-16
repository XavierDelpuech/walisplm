
# Bilan hydrique d'une vigne enherbée
# X.Delpuech (IFV) d'apres F.Celette, A. Ripoche, 2009
# S.Roux : quelques modifications pour paramétrage, lisibilité
#######################


###############################
# DEFINITION DES SOUS-FONCTIONS


#######
#  Fonction Somme de température base  cumsumb(vecteur température, base, ligne de début, ligne de fin)
#  base : definit la valeur de base pour la somme des températures, par défaut = 10
#  x : definit le vecteur d'entrée
#  debut : definit la ligne de debut du calcul, par défaut = 1
#  fin : definit la ligne de fin du calcul, par défaut la dernière ligne de x

cumsumb<-function(x,base=10,debut=1,fin=length(x)) 
{
	somTb<-vector(mode="numeric",length = length(x)) #cree un vecteur vide du même nombre de ligne que le vecteur d'entrée x
	ifelse (x[debut]>base,somTb[debut]<-(x[debut]-base),somTb[debut]<-0)  # Calcul la première valeur de la somme de Temp en base
	for (i in (debut+1):fin)
	{
		ifelse (x[i]>base, somTb[i]<-(somTb[i-1]+x[i]-base), somTb[i]<-somTb[i-1])
	}   # Calcul des SomTb de la ligne 2 à la fin des données météo
	return(somTb)
}

########
# Fonctions calcul du seuil de ruissellement  Sr(CN)

# Fonction calcul du seuil de ruissellement (p5j,cn2,p5j1,p5j2)
SeuilR<-function(p5j,cn2,p5j1,p5j2)
{
	cn1 = (4.2*cn2)/(10-0.058*cn2)
	cn3 = (23*cn2)/(10+0.13*cn2)
	
	#calcul du seuil de ruissellement
	if (p5j<p5j1)
		res = 254*(100/cn1-1) 
	else { 
		if (p5j>p5j2)
			res = 254*(100/cn3-1) 
		else
			res = 254*(100/cn2-1) 
	}
	return(res)
		
}

# Fonction calcul du ruissellement Rs(pluie,seuil ruissellement)
Ruiss<-function(P,sr) # P = pluie en mm , sr = seuil de ruissellement
{
	if (P<0.2*sr) y<-0
	else y<-((P-0.2*sr)^2/(P+0.8*sr))
	return (y)
}
###############################
# paramétrage par defaut


nominal=list(p=0.6,
		Tonte="N", 							# caractere {O,N} d'activation des tontes aux dates prevues (renint LAI)(si "N", tontes automatique /LAI)
		nbtonte=c(2,2,2,0,0,0,0), 			# Définit le nombre de tonte chaque année de simulation
		inn=0.3, 							# Définit l’INN de l’herbe
		irri="N", 							# caractere {O,N} d'activation de l’irrigation qui branche la colonne "IR" du fichier meteo 
		dtonte=rep("29/03",10), 			# date des tontes activé par le parametre "Tonte" 
		dbr=rep("26/03",10), 				# dates de démarrage de la végétation de la vigne (en nombre >=supérieur ou égal au nombre d’années simulées)
		arretTr="01/11", 					# date d’arrêt de transpiration de la vigne
		ddkmax=600, 						# nombre de degré jour pour atteindre le Kmax
		reinit="N",							# caractere {O,N} pour reinitialiser les stocks d'eau en debut d'année
		pir=0, 								# Permet d’attribuer un CN différent (donné par CN_Ruiss2)  à l’inter-rang même en absence d’enherbement. 
		seuilpeff=2,						# seuil de pluie efficace
		kmax=rep(0.43,10),					# coefficient d’interception du rayonnement de la vigne
		levee="01/01",						# date de levée de l’enherbement 
		flagRuiss="O",						# utilisation du module de ruissellement "O" active
		CN_Ruiss=94,						# CN (ruissellement) sous le rang
		CN_Ruiss2=84,						# CN (ruissellement) dans l’inter-rang
		herblairate=0.9,					# LAI rate de l’enherbement
		LAIinit=0.5,						# LAI initial de l’enherbement
		LAIres=0.3,							# LAI résiduel de l’enherbement après une tonte
		LLSmin=700,							# LLSmin de l’herbe
		LAItonte=3,							# LAI à partir duquel on déclenche une tonte automatiquement
		U=2.7,								# paramètre U d’évaporation du sol
		b1_evap=14,							# paramètre b1_evap d’évaporation du sol
		b2_evap=0.15,						# paramètre b2_evap d’évaporation du sol
		FTSWhregultr=0.6,					# FTSW du compartiment herbe à partir de laquelle l’herbe régule sa transpiration
		FTSWhregulLAI=0.9,					# FTSW du compartiment herbe à partir de laquelle l’herbe régule son LAI
		TTSWh=129,							# TTSW du compartiment herbe
		TTSWv=224,							# TTSW du compartiment vigne
		FTSWvregulTR=0.4,					# FTSW du compartiment vigne à partir de laquelle la vigne régule sa transpiration
		ASWhinit=70.92,						# quantité d’eau dans le compartiment vigne en début de simulation
		ASWvinit=178.18,						# quantité d’eau dans le compartiment herbe  en début de simulation
		format_date="%d/%m/%Y"    # format des dates dans le fichier météo d'entrée
		)


###############################
# FONCTION BH
walis.model.single<-function( param, meteo)	
{    

	###############################
	## FONCTION DE BILAN HYDRIQUE

	if(param$p!=0) param$pir<-param$p

	# DEFINITION PARAMETRE DU MODELE

	dat<-as.POSIXlt(strptime(meteo$Date,param$format_date))
	#
	# Nombre d'année : "an" donne le nombre d'années dans le fichier
  ans<-unique(dat$year)  # donne le vecteur des années du fichier
  an<-length(ans)        # donne le nombre d'année dans le fichier
	
	# jdeb donne les lignes de début de chaque année	
  jj<-as.POSIXlt(dat)$yday # vecteur des jours julien
  ll<-c(1:dim(meteo)[1]) # vecteur indice des lignes
  jdeb<-ll[jj==0]
  if (an>1 & jj[1]>1) jdeb=c(1,jdeb)
  jdeb[1]<-1  # au cas ou l'anne ne commence pas au 01/01
  jdeb[an+1]<-(dim(meteo)[1]+1)


	# Calcul des lignes correspondant aux dates du BH (avec cette méthode, il y a un décalage d'un jour les années non bisextiles, 
	#il faudrait rajouter les années...)
	# la fonction as.POSIXlt()$yday donne le jour julien de la date
	leveejj		<-as.POSIXlt(as.Date(param$levee,"%d/%m"))$yday
	dbrjj		<-as.POSIXlt(as.Date(param$dbr,"%d/%m"))$yday
	arretTrjj	<-as.POSIXlt(as.Date(param$arretTr,"%d/%m"))$yday
	tontejj		<-as.POSIXlt(as.Date(param$dtonte,"%d/%m"))$yday
	Ldbr		<-c(1:an)
	Ltonte		<-c(1:sum(param$nbtonte[1:an]))

# Calcul des indices débourrement
# pour la premiere annee, si la date de debourrement est anterieure au premier jour du fichier
if ((dbrjj[1]-jj[1])<0)
{
dbrjj[1]<-jj[1] # forçage au premier jour
print ("Attention : date de dbr pour l'annee 1 forcee au 1er jour")
} 
# au cas ou la date de debourrement est posterieure a la fin du fichier
for (i in 1:an)
{
if (max(jj[dat$year==ans[i]])-dbrjj[i]<0)   # si la date de dbr est posterieur a la fin du fichier
{
dbrjj[i]<-max(jj[dat$year==ans[i]])
print("Attention : date de dbr de la derniere annee forcee au dernier jour")
}
Ldbr[i]<-ll[dat$year==ans[i]&jj==dbrjj[i]]
}
# Calcul des indices levee de l'herbe
Llevee<-ll[jj==leveejj]
if (an>1 & jj[1]>leveejj) Llevee=c(1,Llevee)

# pour l'annee 1, il se peut que la date de levée soit antérieure au 1er jour de données
# par exemple si le fichier ne démarre pas au 01/01  
	Llevee[1] <- leveejj[1]-strptime(meteo$Date[1],param$format_date)$yday
	if (Llevee[1]<0)
	{
		Llevee[1]<-1
		print("Attention : date de levee de l'herbe pour l'annee 1 forcee au premier jour")
	}

## pour l'arret de transpiration
# si le fichier meteo se termine avant la date d'arret
LarretTr<-c(1:an)
for (i in 1:an)
{
if (max(jj[dat$year==ans[i]])-arretTrjj<0)   # si la date d'arret Transpiration est posterieur a la fin du fichier
{
LarretTr[i]<-length(ll)
print("Attention : date d'arret de transpiration de la derniere annee forcee au dernier jour")
}
if (max(jj[dat$year==ans[i]])-arretTrjj>=0)
{
LarretTr[i]<-ll[dat$year==ans[i]&jj==arretTrjj]
}
}

# pour la premiere annee, si la date de tonte est anterieure au premier jour du fichier
for (i in 1:param$nbtonte[1])
{
if ((tontejj[i]-jj[1])<0)
{
tontejj[i]<-jj[1] # forçage au premier jour
print (paste("Attention : date de tonte n°",i," pour l'annee 1 forcee au 1er jour"))
}
} 
i<-1
tt<-1
repeat
{
if (tt>an) break()
if (i>sum(param$nbtonte)) break()
if (max(jj[dat$year==ans[tt]])-tontejj[i]<0)
{
Ltonte[i]<-length(ll)
print(paste("Attention : forcage date de tonte n°",i," au dernier jour"))
}
if (max(jj[dat$year==ans[tt]])-tontejj[i]>0)
{
Ltonte[i]<-ll[dat$year==ans[tt]&jj==tontejj[i]]
}
i<-i+1
if (i>cumsum(param$nbtonte)[tt]) tt<-tt+1
}

	
	# Calcul des vecteurs somme de T°C
	somTb10	<-vector(mode="numeric",length = dim(meteo)[1])
	for (a in 1:an)
		somTb10<-(somTb10+cumsumb(meteo$Temp,debut=Ldbr[a], fin=(jdeb[a+1]-1)))


	#   Calcul de la Pluie efficace
	Peff<-vector(mode="numeric",length=dim(meteo)[1]) #creer un vecteur vide du meme nombre de ligne que meteo
	for (i in 1:dim(meteo)[1])
			ifelse (meteo$Pluie[i]>param$seuilpeff, Peff[i]<-meteo$Pluie[i],Peff[i]<-0) # si pluie >2mm, alors Peff = pluie, sinon =0
	
	
	# Calcul de la Peff pour l'enherbement et la vigne
	Peffh<-(Peff*param$p)
	Peffv<-(Peff*(1-param$p))


	#
	# Somme des pluies des 5 derniers jours
	P5j		<-vector(mode="numeric",length=dim(meteo)[1]) #vecteur vide
	P5j[1]	<-0
	for (i in 2:5)
	  P5j[i]	<-P5j[i-1] + meteo$Pluie[i-1]
	for (i in 6:dim(meteo)[1])
	  P5j[i]<-(meteo$Pluie[i-5]+meteo$Pluie[i-4]+meteo$Pluie[i-3]+meteo$Pluie[i-2]+meteo$Pluie[i-1])
	
	
	#
	# Calcul du ruissellement herbe (inter rang)
	Seuilrui<-vector(mode="numeric",length=dim(meteo)[1]) # vecteur vide 
	Ruissh<-vector(mode="numeric",length=dim(meteo)[1])   #vecteur vide
	for (i in 1:dim(meteo)[1])  # attribution de la valeur de ruissellement en fonction de la pluie des 5 derniers jours
	{
		Seuilrui[i]<-SeuilR(P5j[i],param$CN_Ruiss2,35.6,53.3)
		Ruissh[i]<-Ruiss(meteo$Pluie[i],Seuilrui[i])*param$pir
		if (param$flagRuiss=="N") Ruissh[i]=0
	}

	# Calcul du seuil ruissellement vigne (rang)
	Seuilruiv<-vector(mode="numeric",length=dim(meteo)[1]) # vecteur vide 
	Ruissv<-vector(mode="numeric",length=dim(meteo)[1]) #vecteur vide
	for (i in 1:dim(meteo)[1])  # attribution de la valeur de ruissellement en fonction de la pluie des 5 derniers jours
	{
		Seuilruiv[i]<-SeuilR(P5j[i],param$CN_Ruiss,12.7,28)
		Ruissv[i]<-Ruiss(meteo$Pluie[i],Seuilruiv[i])*(1-param$pir)
		if (param$flagRuiss=="N") Ruissv[i]=0
	}

	#
	# Calcul de la pluie efficace - le ruissellement inter-rang (herbe) et rang (vigne)
	Peffh_Ruiss<-(Peffh-Ruissh)
	Peffv_Ruiss<-(Peffv-Ruissv)

	# Part de l'ETP enherbement
	ETPh<-(meteo$ETP*param$p)

	#
	# Calcul de Temp base 0 à partir de la levée de l'enherbement
	Tb0<-rep(0,dim(meteo)[1]) 
	for (a in 1:an)
		for (i in Llevee[a]:(jdeb[a+1]-1))
			ifelse(meteo$Temp[i]>0,Tb0[i]<-meteo$Temp[i],Tb0[i]<-0)


	# Calcul du kvigne estimé
	kv<-rep(0,dim(meteo)[1]) #vecteur vide
	for (a in 1:an)
		for (i in Ldbr[a]:LarretTr[a])
		{
			kv[i]<-param$kmax[a]
			if (somTb10[i]<param$ddkmax)# 
				kv[i]<-(param$kmax[a]*(somTb10[i]/param$ddkmax))  # croissance linéaire du Kv du dbr jusqu'à somTb10=ddkmax
			if (i>(LarretTr[a]-15) & i<(length(ll)-15))  # 
				kv[i]<-(param$kmax[a]-param$kmax[a]*min(1,1-(LarretTr[a]-i)/15))      # decroissance lineaire à partir de 15j avant l'arrêt de transpiration
		}

	#
	# Calcul de INN
	INN<-rep(0,dim(meteo)[1])
	INN<-INN+param$inn # à ce stade de développement, la valeur de INN est fixé par défaut à 0.3
	
	#
	# Calcul de Es0
	Es0<-(meteo$ETP*(1-kv)*(1-param$p))
	#
	# CALCUL du groupe evaporation du sol
	b_evap	<-(0.5*param$b1_evap*param$b2_evap)
	
	BHSn	<-rep(0,dim(meteo)[1])
	ESn1	<-rep(0,dim(meteo)[1])
	SES0	<-rep(0,dim(meteo)[1])
	ESn2	<-rep(0,dim(meteo)[1])
	ESn		<-rep(0,dim(meteo)[1])
	
	BHSn[1]	<-(param$U*(1-param$p))
	ESn1[1]	<-Es0[1]
	ESn[1]	<-(ESn1[1]+ESn2[1])
	j<-1
	repeat
	{
		j<-(j+1)
		if (j>dim(meteo)[1]) break()
		ESn1[j]<-(min(Es0[j],BHSn[j-1]))
		if (ESn1[j]<Es0[j]) SES0[j]<-(Es0[j]-ESn1[j]+SES0[j-1]) else SES0[j]<-0
		ESn2[j]	<-(max(0,(2*b_evap*SES0[j]+b_evap^2)^0.5-(2*b_evap*SES0[j-1]+b_evap^2)^0.5))
		ESn[j]	<-(ESn1[j]+ESn2[j])
		BHSn[j]	<-(min(param$U*(1-param$p),max(0,(BHSn[j-1]+meteo$Pluie[j]*(1-param$p)-ESn[j]))))
	}

	# Allocations
	crherbe<-vector(mode="numeric",length=dim(meteo)[1]) 
	STb0<-vector(mode="numeric",length=dim(meteo)[1]) 
	seneherbe<-vector(mode="numeric",length=dim(meteo)[1]) 
	LAIsen<-vector(mode="numeric",length=dim(meteo)[1]) 
	KLAI<-vector(mode="numeric",length=dim(meteo)[1]) 
	LAI<-vector(mode="numeric",length=dim(meteo)[1]) 
	Kh<-vector(mode="numeric",length=dim(meteo)[1]) 
	Ktransp<-vector(mode="numeric",length=dim(meteo)[1])
	ETRh<-vector(mode="numeric",length=dim(meteo)[1]) 
	ASWh<-vector(mode="numeric",length=dim(meteo)[1]) 
	FTSWh<-vector(mode="numeric",length=dim(meteo)[1])
	Dh<-vector(mode="numeric",length=dim(meteo)[1]) 
	Shv<-vector(mode="numeric",length=dim(meteo)[1])
	Tv<-vector(mode="numeric",length=dim(meteo)[1]) 
	Dv<-vector(mode="numeric",length=dim(meteo)[1]) 
	ASWv<-vector(mode="numeric",length=dim(meteo)[1]) 
	FTSWv<-vector(mode="numeric",length=dim(meteo)[1]) 
	ASWtot<-vector(mode="numeric",length=dim(meteo)[1]) 
	FTSWtot<-vector(mode="numeric",length=dim(meteo)[1]) 
	
	
	
	#######################
	# INITIALISATIONS
	LAI[1]<-param$LAIinit
	STb0[1]<-Tb0[1]
	LAIsen[1]<-param$LAIinit
	ASWv[1]<-param$ASWvinit
	ASWh[1]<-param$ASWhinit
	FTSWh[1]<-param$ASWhinit/param$TTSWh
	TTSWtot<-(param$TTSWh+param$TTSWv)
	FTSWv[1]<-param$ASWvinit/param$TTSWv
	ASWtot[1]<-(ASWv[1]+ASWh[1])
	FTSWtot[1]<-(ASWtot[1]/TTSWtot)
	Shv[1]<-min(1,FTSWtot[1]/param$FTSWvregulTR) # Effet Stress hydrique sur vigne
	KLAI[1]<-(min(1,max((FTSWh[1]/(param$FTSWhregulLAI-0.1)-0.1/param$FTSWhregulLAI),0)))
	Ktransp[1]<-min(1,FTSWh[1]/param$FTSWhregultr)  # Effet stress hydrique sur la transpiration
	Kh[1]<-0.5
	ETRh[1]<-(ETPh[1]*(1-kv[1])*Kh[1]*Ktransp[1])
	j<-1
	a<-1
	if (param$Tonte=="O") param$LAItonte<-100 # cette valeur désactive LAItonte si les dates de tontes sont actives
	tont<-1 # compteur de date de tonte
	
	
	#######################
	# BOUCLE JOURNALIERE
	repeat
	{

		j<-j+1
		if (j>dim(meteo)[1]) break()

		# Calcul de la sénescence folaire de l'herbe
		crherbe[j]	<-(param$herblairate*1.71*0.001*min(18,Tb0[j])^2*KLAI[j-1]*INN[j-1])
		STb0[j]		<-STb0[j-1]+Tb0[j]
		if (STb0[j-1]>param$LLSmin | LAI[j-1]==param$LAIres) LAIsen[j]<-LAI[j-1] else LAIsen[j]<-LAIsen[j-1]
		if (STb0[j-1]>param$LLSmin) STb0[j]<-0
		seneherbe[j]<-(LAIsen[j]*max(0,Tb0[j]/param$LLSmin))
		
		#gestion de la dynamique du LAI incluant les eventuelles tontes
		if (LAI[j-1]>param$LAItonte) 
			LAI[j]<-param$LAIres 
		else 
			LAI[j]<-max(0,LAI[j-1]+crherbe[j]-seneherbe[j])
		if (j==Ltonte[tont] & param$Tonte=="O") 
		{
			LAI[j]<-param$LAIres
			STb0[j]<-0
			tont<-(tont+1)
		}
		if (tont>sum(param$nbtonte)) 
			tont<-sum(param$nbtonte)

		Kh[j]	<-(0.95*(1-exp(-0.6*LAI[j])))
		ETRh[j]	<-(ETPh[j]*(1-kv[j])*Kh[j]*Ktransp[j-1])
		Shv[j]	<-min(1,FTSWtot[j-1]/param$FTSWvregulTR) # Effet Stress hydrique sur vigne
		if (j<LarretTr[a]) Tv[j]<-(kv[j]*meteo$ETP[j]*Shv[j])
		ASWh[j]<-max(0.000001,min(param$TTSWh,ASWh[j-1]+Peffh_Ruiss[j]-ETRh[j]-(ASWh[j-1]/ASWtot[j-1])*Tv[j]))
		FTSWh[j]<-(ASWh[j]/param$TTSWh)
		Ktransp[j]	<-min(1,max(FTSWh[j]/param$FTSWhregultr,0))  # Effet stress hydrique sur la transpiration
		KLAI[j]		<-(min(1,max((FTSWh[j]/(param$FTSWhregulLAI-0.1)-0.1/param$FTSWhregulLAI-0.1),0))) # Effet stress hydrique sur LAI
		Dh[j]		<-max(0,ASWh[j-1]+Peffh_Ruiss[j]-param$TTSWh)
		Dv[j]		<-max(0,ASWv[j-1]+Peffv_Ruiss[j]-param$TTSWv)
		ASWv[j]		<-max(0,min(param$TTSWv,ASWv[j-1]+Peffv_Ruiss[j]-(ASWv[j-1]/ASWtot[j-1])*Tv[j]-ESn[j]+Dh[j]))
		if (param$irri=="O") ASWv[j]<-ASWv[j]+meteo$IR[j] # s'il  a de l'irrigation, on la rajoute directement à l'ASWv
		FTSWv[j]<-(ASWv[j]/param$TTSWv)
		ASWtot[j]<-(ASWv[j]+ASWh[j])
		FTSWtot[j]<-(ASWtot[j]/TTSWtot)
		if (param$reinit=="O" & j==(jdeb[a+1]-1)) ASWv[j]<-param$TTSWv
		if (param$reinit=="O" & j==(jdeb[a+1]-1)) ASWh[j]<-param$TTSWh
		if (j==(jdeb[a+1]-1)) a<-(a+1)
	}
	# FIN DE LA BOUCLE SUR LES JOURS DE SIMULATION


	#
	#BHr<-data.frame(meteo,Peff,
	BHr<-list(meteo,Peff, Kh,ETRh,crherbe,seneherbe,KLAI,Tb0,LAI,kv,Tv,Shv,BHSn,Es0,ESn1,ESn2,SES0,ESn,ETPh,Dh,Dv,Ruissv,Ruissh,ASWv,ASWh,ASWtot=ASWtot,FTSWh,FTSWv,FTSWtot,Peffh_Ruiss,Peffv_Ruiss,P5j)
	names(BHr)<-c("meteo","Peff", "Kh","ETRh","crherbe","seneherbe","KLAI","Tb0","LAI","kv","Tv","Shv","BHSn","Es0","ESn1","ESn2","SES0","ESn","ETPh","Dh","Dv","Ruissv","Ruissh","ASWv","ASWh","ASWtot","FTSWh","FTSWv","FTSWtot","Peffh_Ruiss","Peffv_Ruiss","P5j")
  return(BHr)
} # FIN de la fonction Bilan hydrique
#



#######################################################
# application du modèle sur des matrices de paramètres

walis.model <- function(param, clim_data)
return(apply(param,walis.model.single,clim_data))

	
#######################################################
