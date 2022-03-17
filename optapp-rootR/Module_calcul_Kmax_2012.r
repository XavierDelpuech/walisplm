# Auteurs :Philippe Pieri, Benjamin Bois - INRA, Equipe Ecophysiologie et Agronomie Viticole; 71, Rue Edouard Bourlaux B.P. 81, 33 883 VILLENAVE DORNON Cedex- contact : bbois@bordeaux.inra.fr
# Traduction en langage R : Xavier Delpuech, IFV, Domaine de Donadille, 30230 Rodilhan. xavier.delpuech@vignevin.com , février 2012
# Bibliographie
# LEBON, E., DUMAS, V., PIERI, P. and SCHULTZ, H. R., 2003. Modelling the seasonal dynamics of the soil water balance of vineyards. Functional Plant Biology 30 (6).
# RIOU, C., PIERI, P. and CLECH, B. L., 1994. Water use of grapevines well supplied with water. Simplified expression of transpiration. Vitis 33 (3).
# RIOU, C., VALANCOGNE, C. and PIERI, P., 1989. Un modele simple d'interception du rayonnement solaire par la vigne. Verification experimentale. Agronomie 9 (5).
# HARGREAVES, G. H. and SAMANI, Z. A., 1982. Estimating Potential Evapotranspiration. Journal of the Irrigation and Drainage Division 108 p. 223-230.
# HARGREAVES, G. L., HARGREAVES, G. H. and RILEY, J. P., 1985. Agricultural benefits for Senegal River Basin. Journal of Irrigation and Drainage Engineering 111 p. 111-124.
# BRISSON, N., PERRIER, A. (1991) A semi-empirical model of bare soil evaporation for crop simulation models Water Resources Research 27, p.719-727
###
#### sous fonction Radiation_Interception_DI
Radiation_Interception_DI<-function(r_lat=44.5, r_longit=0, i_year=2000, i_doy=183, d_r=1.5, hf_r=1, lf_r=0.4, 
az_r=0, poro_r=0.25, alb_f=0.22, alb_s=0.18, Dt=1800, r_rgi=20)
{
#Dt = 1800
#r_lat = latitude    # =44.5
#r_longit = 0
#az_r = azimut # =0   Azimut en degrés (ex: 0=NS ; 90=EW ; -45 = NW-SE )
#d_r = d      # =1.5 Distance inter-rang (en mètres)
#alb_f = 0.22
#alb_s = alb_sol  # =0.18
#i_year = n_an
#i_doy = doy   # par defaut au 1er juillet
#r_rgi = rg    # par defaut = 20
#hf_r = hautf # =1 Hauteur de feuillage (en metres)
#lf_r = largf  # =0.4 Largeur de feuillage (en metres)
#poro_r = poro   # =0.25 Porosite min en ete, de 0 à 1 (0=aucun trou dans le feuillage)

#calculates daily integrals for radiation interception by vine rows and soil in a vineyard
#parameters
rDt = Dt
rLatitude = r_lat
rLongitude = r_longit
iYear = i_year
rOrientRangs = az_r     #à l#entrée :   0 pour rangs NS, 90 pour EW et -45 pour NW-SE
    #ici en entrée, rOrientRangs  en degrés :    ( 0=NS ; 90=EW )  ( -45 = NW-SE )
    #passage direction des rangs à angle de la normale aux rangs avec la direction NS
rOrientRangs = rOrientRangs + 90                            #orientée de N à S
        #If rOrientRangs >= 180 Then rOrientRangs = rOrientRangs - 180     CORRIGE LE 31/03/98 :
if (rOrientRangs >= 90) rOrientRangs = rOrientRangs - 180
#donc angle normale = -90 pour rangs NS, 0 pour EW , 45 pour NW-SE et -45 pour NE-SW
#orientation comme angles d#azimut solaire, à partir du Sud, négatifs le matin et positifs l#après-midi
rEcartRangs = d_r
rHautRang = hf_r
rLargRang = lf_r
rPorosite = poro_r
rAlbedoFeuilles = alb_f
rAlbedoSol = alb_s

 #variables d#entree :
iSimulDay = i_doy
rRgiJour = r_rgi * 1000000               #MJ.m^-2  à  J.m^-2

# *****************************************  DEBUT  JOURNEE  ***************************************

rSJRytGlo = 0           #intialisation des intégrales journalières Rayonnement
rSJRytDif = 0
rSJRytGloVigne = 0
rSJRytGloSol = 0
rSJAlbedoVignoble = 0
rSJRgvRel = 0
rSJRgsRel = 0

iSJtot = 0                              #compteurs
iSJdiurne = 0

# *****************************************  DEBUT CALCULS  ***************************************

#    Call ParSolAnn((iYear))                 #sous programmes astronomiques annuel et journaliers
# ParSolAnn(ian) # ian as integer
#         CALCUL DES PARAMETRES SOLAIRES ANNUELS
#
#           rOmegaRadparJour, vitesse de la Terre (radian)  (par jour)              integer
#           rDoyEqui, date equinoxe (jour)
#           rLongitPerigRad, longitude perigee (radian)
#           rExentr, excentricite
#           rDoyPerig, date perigee (jour)
#           rEpsilonRad, obliquite(radian)
#
## ian=à définir
ian=iYear
rOmegaRadparJour = 2 * pi / 365.25636
rDoyEqui = 80.08 + 0.2422 * (ian - 1900) - trunc((ian - 1901) / 4)
rLongitPerigRad = -1.374953 + 0.000300051 * (ian - 1900)
rExentr = 0.016751 - 0.00000042 * (ian - 1900)
rDoyPerig = rDoyEqui + (rLongitPerigRad - 2 * rExentr * sin(rLongitPerigRad)) / rOmegaRadparJour
rEpsilonRad = 0.40932 - 0.000002271 * (ian - 1900)
ian = 1
 
#    Call ParSolJour((iYear), (iSimulDay)) # remplacé par le sous-programme lui-même
#ParSolJour(ian As Integer, iNJ As Integer)
#   Calcul de la LONGITUDE CELESTE, de la DECLINAISON du Soleil
#      (radians), de l'EQUATION du TEMPS (minutes) pour le jour iNJ de
#      de l'annee iAn.

#   ATTENTION
#        Ce SOUS-PROGRAMME ne peut etre executé qu'apres le SOUS-PRO-
#        GRAMME  PSOLAN(iAn, ...) qui definit les parametres solaires annuels
#
#      COMMON /PARAN/rOmegaRadparJour,rDoyEqui,rLongitPerigRad,rExentr,rDoyPerig,rEpsilonRad
#      COMMON /PARJOU/rLongitCelesteRad,rDeltaRad,rEquaTemps
ian=iYear
iNJ=iSimulDay
rNJ = iNJ + 0.5
#   LONGITUDE CELESTE du Soleil     (radians)
rLongitCelesteRad = rLongitPerigRad + rOmegaRadparJour * (rNJ - rDoyPerig) + 2 * rExentr * sin(rOmegaRadparJour * (rNJ - rDoyPerig))
#   DECLINAISON du Soleil           (radians)
rDeltaRad = asin(sin(rLongitCelesteRad) * sin(rEpsilonRad))
#   EQUATION DU TEMPS               (minutes)
rEquaTemps = 720 / pi * (atan(tan(rLongitCelesteRad) * cos(rEpsilonRad)) - rOmegaRadparJour * (rNJ - rDoyPerig) - rLongitPerigRad)
while (rEquaTemps <= -100) (rEquaTemps = rEquaTemps + 720)
### fin sous-programme

# Call ParSolJourBis((iYear), (iSimulDay)) # remplacé par le sous-programme lui-même
# ParSolJourBis(ian As Integer, iNJ As Integer)
#   Calcul de l'HEURE de PASSAGE du Soleil au MERIDIEN, de l'HEURE du
#      LEVER et du COUCHER du Soleil (h TU), de la DUREE du JOUR (h)
#
#   Calcul du RAYONNEMENT GLOBAL EXTRATERRESTRE (Joule.m-2). On a admis
#      une valeur de la constante solaire de 1353 watt.m-2
#
#   Pour passer à l'heure legale, ajouter le décalage par rapport a l'heure TU
#   (ex.: en FRANCE, +1h ou +2h selon la periode de l'annee, en GUADELOUPE, -4h, etc...)
#
#   ATTENTION
#        Avant d'utiliser ce SOUS PROGRAMME, il est indispensable
#        d'executer dans l'ordre les SOUS PROGRAMMES:
#            PSOLAN(iAn) pour definir les parametres annuels
#            PSOLJ(NJ) pour definir les parametres journaliers
#        Il faut aussi preciser le lieu en définissant
#
#Dim rNJ, rLatRad, rRefrac, X, a, b, C                               As Double
#Global rHrMidi, rDurJour, rHrLevSol, rHrCouSol, rRytGloExtraAtmJour        As Double
ian=iYear
iNJ=iSimulDay
rNJ = iNJ + 0.5
rLatRad = rLatitude * pi / 180

#HEURE DE PASSAGE du Soleil  au MERIDIEN
rHrMidi = 12 + rLongitude / 15 + rEquaTemps / 60

#Durée du jour, avec rRefrac, Correction de réfraction atmosphérique
rRefrac = -0.0106
if (abs(rLatitude)== 90) X=(rLatRad * (rRefrac - rDeltaRad) * 1000000) else X=(sin(rRefrac) - sin(rLatRad) * sin(rDeltaRad)) / cos(rLatRad) / cos(rDeltaRad)
if (X==0) rDurJour = 12
if (X<=(-1))rDurJour = 24
if (X>(-1)&(X<0)) rDurJour = 24 * (1 + atan((1 - X * X)^0.5 / X) / pi)
if (X>=1) rDurJour = 0
if ((X>0)&(X<1)) rDurJour = 24 * atan((1 - X * X)^0.5 / X) / pi

#HEURES de LEVER et de COUCHER du Soleil
rHrLevSol = rHrMidi - rDurJour / 2
rHrCouSol = rHrMidi + rDurJour / 2

#RAYONNEMENT GLOBAL EXTRATERRESTRE journalier
X = abs(rLatRad + rDeltaRad)
if (rLatRad==0) {a=cos(rDeltaRad)
    }else {if (rDeltaRad==0) {a=cos(rLatRad)
          }else {if (X>pi/2) {b = pi
                             a = b * sin(rLatRad) * sin(rDeltaRad)
                }else {if (abs(rDeltaRad-rLatRad)>pi/2) {a=0
                      }else {
                            C = (1 - (sin(rLatRad) * sin(rLatRad) + sin(rDeltaRad) * sin(rDeltaRad)))^0.5 / sin(rLatRad) / sin(rDeltaRad)
                            b = C + atan(-C)
                            if (rLatRad * rDeltaRad > 0) {b=b+pi}else{}
                            a = b * sin(rLatRad) * sin(rDeltaRad)
                            }
                      }
                }
          }
rRytGloExtraAtmJour = 37210000 * (1 + 0.033 * cos(rOmegaRadparJour * (rNJ - rDoyPerig))) * a
### fin sous-programme

if (rRytGloExtraAtmJour <= 0) print("rRytGloExtraAtmJour <= 0")


if (rHautRang>0) {  #********************  DEBUT BOUCLE rHautRang > 0  ***********************************
                    #MsgBox "rLAI >0  := " & rLAI & snl() & "jour et heure = " & iSimulDay & "  ,  " & rHr

rLatRad = rLatitude * pi / 180               #Latitude en radians
rOrientRangsRad = rOrientRangs * pi / 180    #Où rOrientRangs = ANGLE NORMALE AU RANG AVEC DIRECTION NORD-->SUD (DEGRES)

                             #Début Appel Sous-Programmes Rayonnement
#       Sub AbsDiffus()
#       ex-subroutine KDIFFU(rKdif, rKdifSol, imodel)
#       Calcul des coefficients d'interception du rayonnement diffus :
#       rKdif,  rayonnement venant du ciel et rKdifSol, rayonnement venant du sol
#       1 variante : mur poreux sauf haut et bas, ex. Vigne  (issu de MODELM.FOR)
#  c    porosité du feuillage rPorosite
#      common /ccultu/rEcartRangs,rHautTotMax,rHautRang,rLargRang,rOrientRangs,rAlbedoFeuilles,rAlbedoSol,rPorosite
#Dim rKDIF1, rKDIF2, rKDIF3  As Double          DECLARE en GLOBAL

# ... fraction du rayonnement diffus intercepté par la masse foliaire de la vigne
        #par les parties supérieures des rangs (horizontales et non poreuses)
        rKDIF1 = rLargRang / rEcartRangs
        #facteur de forme entre 2 rangs voisins : auparavant dans sub Calcul_PAR()
        rFF0 = tan(0.5 * atan(rHautRang / (rEcartRangs - rLargRang)))
        #facteur de forme entre les parois horizontales entre les rangs, supérieure et inférieure.
        rFFdif = tan(0.5 * atan((rEcartRangs - rLargRang) / rHautRang))
        #par les parois verticales (d'abord supposées non poreuses)                 'eq 16 de Riou et al., 1989
        rKDIF2 = (1 - rFFdif)
        rKDIF2 = rKDIF2 * (1 - rLargRang / rEcartRangs)
        #par les parties inférieures des rangs (horizontales et non poreuses)       'eq 20 de Riou et al., 1989
        rKDIF3 = (sin(0.5 * atan(2 * rHautRang / rLargRang))) ^ 2
        rKDIF3 = rKDIF3 - (sin(0.5 * atan(rHautRang / (rEcartRangs - rLargRang / 2)))) ^ 2
        rKDIF3 = rKDIF3 * 2 * rLargRang / rEcartRangs
        rKdif = rKDIF1 + (1 - rPorosite) * rKDIF2 + rPorosite * rKDIF3              #eq 21 de Riou et al., 1989
        rKdifSol = rKdif
##### fin                              

# Sub CoefAbsRytDiff()
#       ex-subroutine ARDIF (rKdif,rKdifSol,cardif,crrdif)
#       calcul des taux d'absorption (cardif) et de réflexion (crrdif) du rayt.diffus
#       common /ccultu/rEcartRangs,rHautTotMax,rHautMax,rLargMax,rOrientRangs,rAlbedoFeuilles,rAlbedoSol,rPorositeMin
rCardif = (1 - rAlbedoFeuilles) * ((1 - rAlbedoSol * rKdifSol) * rKdif + rAlbedoSol * rKdifSol)
rCrrdif = (rAlbedoFeuilles - rAlbedoSol * (1 - rKdifSol) - rAlbedoFeuilles * rAlbedoSol * rKdifSol / 2) * rKdif
rCrrdif = rCrrdif + (rAlbedoSol * (1 - rKdifSol) + rAlbedoFeuilles * rAlbedoSol * rKdifSol / 2)
# fin sub

#            Sub PosGeomLim()
#       Positions Géométriques Limites
#       ex-subroutine cbetal(rBetaLim, nbetal, imodel)
#       interception du Rayonnement Solaire Direct
#       calcul des nbetal valeurs de rBetaLim(),
#       rapports correspondant a des positions caracteristiques limites
#       (composante Horizontale du Rayt DIRECT sur composante Verticale)
      rBetaLim_1 = rLargRang / rHautRang
      rBetaLim_2 = (rEcartRangs - rLargRang) / rHautRang
      rBetaLim_3 = rEcartRangs / rHautRang
      rBetaLim_4 = (rEcartRangs + rLargRang) / rHautRang
      rBetaLim_5 = (2 * rEcartRangs - rLargRang) / rHautRang
      rBetaLim_6 = (2 * rEcartRangs) / rHautRang
      rBetaLim_7 = (3 * rEcartRangs) / rHautRang
# End Sub


   # premier indice pour boucle horaire :
   #(le temps rTempsTUFirst lu dans data_H doit être en heures TU decimales et centrées)
   j = 0                                                        #calculs à partir de 0 h

# *****************************************  DEBUT BOUCLE HORAIRE  ***************************************
   repeat
   {
   if (j >= trunc(24 / (rDt / 3600))) break()  

    rHr = rDt / 3600 / 2 + j * rDt / 3600

    #MsgBox "jour et heure = " & iSimulDay & "  ,  " & rHr

# sous-programme        ParSolHor((rHr))                #paramètre rHr = Heure TU (h)
### ParSolHor
#      Calcul des rHautSol et rAzimSol du Soleil (degres) à l#heure rHr (en h TU)
#      du jour iNJ de l#annee iAn pour un lieu de Latitude rLatitude, de
#      Longitude rLongitude (degres), d#équation du temps rEquaTemps (minutes),
#      et de déclinaison du Soleil rDeltaRad (radians)
#
#      ATTENTION
#        Avant l#utilisation de ce SOUS-PROGRAMME, il faut avoir executé
#      les SOUS-PROGRAMMES ParSolAnn(iAn%) et ParSolJour(iNJ%, iAn%)
#        Il faut avoir precisé le lieu par sa rLatitude et sa rLongitude (degres)
#
rLatRad = rLatitude * pi / 180
# ... Calcul Heure en Temps Solaire Vrai, rHrTSV et Angle Horaire (radians), rHrRad
rHrTSV = rHr - 12 - rLongitude / 15 - rEquaTemps / 60
rHrRad = rHrTSV * pi / 12
# ... Calcul de la HAUTEUR du Soleil
X = sin(rLatRad) * sin(rDeltaRad) + cos(rLatRad) * cos(rDeltaRad) * cos(rHrRad)
if (X < 0) rHautSol = 0  #-99.9                    #le Soleil n#est pas levé
rHautSol=ifelse ((X==1), 90, asin(X) * 180 / pi)          # XD : vérifier les radians
# ... Calcul de l'AZIMUT du Soleil
#        codage signe: Nord    -180°
#                      Est      -90°
#                      Sud        0°
#                      Ouest     90°
#                      Nord     180°
#
X = sin(rLatRad) * cos(rHrRad) - cos(rLatRad) * tan(rDeltaRad)
if (X==0) {
b=90
a=0
          } else { if (X<0) {
                            b =180
                            } else { b=0
                            }
                   a = atan(sin(rHrRad) / X) * 180 / pi
          }

if (rHrRad==0) {
rAzimSol = 0
  }else{ if(rHrRad < 0) {
       rAzimSol = a - b
       }else{ rAzimSol = a + b
       }
  }
# fin sous-programme  ParSolHor

rHeureTSV = rHr - rLongitude / 15 - rEquaTemps / 60   #temps solaire vrai (h)
rAHDeg = (rHeureTSV - 12) * 15                        #angle horaire  (degrés)
rAHRad = rAHDeg * pi / 180                          #angle horaire  (radians)
rHautSolRad = rHautSol * pi / 180                   #hauteur soleil (radians)
rAzimSolRad = rAzimSol * pi / 180                   #azimut soleil (radians)

iSJtot = iSJtot + 1                   #compteur jour

            #Microclimat_Feuilles                         #MsgBox "Microclimat_Feuilles  Terminé"

            #rSJTleaf = rSJTleaf + rTleaf                #mise à jour des intégrales journalières microclimat
            #rSJDefSat = rSJDefSat + rDefSat

if (rHautSol>0) {        #DEBUT TEST rHautSol > 0  pour partie Ryt Solaire
#rSJTleafDiurne = rSJTleafDiurne + rTleaf    #mise à jour des intégrales journ. microclimat periode diurne
#rSJDefSatDiurne = rSJDefSatDiurne + rDefSat
iSJdiurne = iSJdiurne + 1                   #compteur periode diurne
r = 1353 * (1 + 2 * rExentr * cos(rOmegaRadparJour * iSimulDay + 0.5 - rDoyPerig))
rRytGloExtraAtm = r * sin(rHautSol * pi / 180)
#If bOption11 = False Then
rRytGlo = rRytGloExtraAtm * rRgiJour / rRytGloExtraAtmJour
#End If

                if (rRytGloExtraAtm>0) {             #Début calcul Rayonnement Diffus
                    GSG0 = rRytGlo / rRytGloExtraAtm
                    }else{GSG0 = 0
                    }


                #DSG = 1.09 - 2.6896 * GSG0 * GSG0 + 1.2843 * GSG0 * GSG0 * GSG0   # relation journalière à Bordeaux (CV)
                DSG = 1.09 - 2.44 * GSG0 * GSG0 + 1.084 * GSG0 * GSG0 * GSG0   # relation horaire à Bordeaux (CV)

                if (DSG>1) DSG = 1
                if (GSG0 >= 0.2) {
                    rRytDif = rRytGlo * DSG
                    rRytDir = rRytGlo - rRytDif
                } else {
                    rRytDif = rRytGlo
                    rRytDir = 0
                }

                if (rRytDif < 0) rRytDif = 0
                if (rRytDir < 0) rRytDir = 0

        # ... rapport composante horizontale (--> paroi verticale)
        # ... / composante verticale (--> paroi horizontale) du Ryt Direct (RDIR)
                rBeta1 = cos(rOrientRangsRad) * tan(rLatRad)
                rBeta2 = sin(rDeltaRad) * cos(rOrientRangsRad) / (sin(rHautSolRad) * cos(rLatRad))
                rBeta3 = sin(rOrientRangsRad) * cos(rDeltaRad) * sin(rAHRad) / sin(rHautSolRad)
                rBeta = abs(rBeta1 - rBeta2 + rBeta3)

# Sub AbsDirect()
#       ex-subroutine kdirec(rBetaLim, nbetal, imodel, rKdir, rBeta)
#       interception du Rayonnement Solaire Direct par le feuillage
#       rBetaLim() rapports correspondant a des valeurs caracteristiques de rBeta
#       rBeta = rapport composante Horizontale du Rayt DIRECT / composante Verticale

    if (rBeta > rBetaLim_1) {
        if (rBeta > rBetaLim_2) {
            if (rBeta > rBetaLim_3) {
                if (rBeta > rBetaLim_4) {
                    if (rBeta > rBetaLim_5) {
                        if (rBeta > rBetaLim_6) {
                            if (rBeta >= rBetaLim_7) {
                                rKdir = (rEcartRangs - rPorosite * rPorosite * rPorosite * (rEcartRangs - rLargRang)) / rEcartRangs
                            }else{
                                rKdir = (rEcartRangs - rPorosite * rPorosite * (rEcartRangs - rLargRang) + rPorosite * rPorosite * (1 - rPorosite) * (rBeta * rHautRang - 2 * rEcartRangs - rLargRang)) / rEcartRangs
                            }
                        }else{
                            rKdir = (rEcartRangs - rPorosite * rPorosite * (rBeta * rHautRang - rEcartRangs - rLargRang)) / rEcartRangs
                        }
                    }else{
                        rKdir = (rEcartRangs - rPorosite * (2 * rEcartRangs - rLargRang - rBeta * rHautRang) - rPorosite * rPorosite * (rBeta * rHautRang - rEcartRangs - rLargRang)) / rEcartRangs
                    }
                }else{
                    rKdir = (rEcartRangs - rPorosite * (2 * rEcartRangs - rLargRang - rBeta * rHautRang)) / rEcartRangs
                }
            }else{
                rKdir = (rEcartRangs - rPorosite * (rBeta * rHautRang - rLargRang)) / rEcartRangs
            }
         }else{
            rKdir = ((rBeta * rHautRang + rLargRang) - rPorosite * (rBeta * rHautRang - rLargRang)) / rEcartRangs
         }
    }else{
        rKdir = ((rBeta * rHautRang + rLargRang) / rEcartRangs)
    }
#End Sub

#Sub CoefAbsRytDir()
#       ex-subroutine ARDIR(rKdifSol, rKdir, cardir, crrdir)
#       calcul des taux d'absorption (cardir) et de de réflexion (crrdir) du rayt. direct
rCardir = (1 - rAlbedoFeuilles) * ((1 - rAlbedoSol * rKdifSol) * rKdir + rAlbedoSol * rKdifSol)
rCrrdir = (rAlbedoFeuilles - rAlbedoSol * (1 - rKdifSol) - rAlbedoFeuilles * rAlbedoSol * rKdifSol / 2) * rKdir
rCrrdir = rCrrdir + (rAlbedoSol * (1 - rKdifSol) + rAlbedoFeuilles * rAlbedoSol * rKdifSol / 2)
#End Sub


        }else{                #SUITE TEST rHautSol > 0 donc ici #If rHautSol <= 0
            rHautSol = 0            #pour partie ryt sol
            rAzimSol = 0
            rRytGloExtraAtm = 0
            rRytGlo = 0
            rRytDir = 0
            rRytDif = 0
            rCardir = 0
            rCrrdir = 0
            rBeta = 0
       }              #FIN TEST rHautSol > 0  pour partie Ryt Solaire

        rRytGloVigne = rCardir * rRytDir + rCardif * rRytDif
        rRytGloSol = (1 - rCardir - rCrrdir) * rRytDir + (1 - rCardif - rCrrdif) * rRytDif

        if (rRytGlo > 0) {
            rAlbedoVignoble = (rCrrdir * rRytDir + rCrrdif * rRytDif) / rRytGlo
            rRgvRel = rRytGloVigne / ((1 - rAlbedoVignoble) * rRytGlo)
            rRgsRel = rRytGloSol / ((1 - rAlbedoVignoble) * rRytGlo)
            }
        if (rRytGlo <= 0) {
            rAlbedoVignoble = 0
            rRgvRel = 0
            rRgsRel = 0
            }

        rSJRytGlo = rSJRytGlo + rRytGlo * rDt                 #Mise à Jour des Intégrales Journalières / Rayonnement
        rSJRytDif = rSJRytDif + rRytDif * rDt
        rSJRytGloVigne = rSJRytGloVigne + rRytGloVigne * rDt
        rSJRytGloSol = rSJRytGloSol + rRytGloSol * rDt


    j = j + 1
    }             #FIN BOUCLE HORAIRE commencee par : Do While (j < CInt(24 / (rDt / 3600)))

    # ------------------------------  mise à jour des intégrales journalières (rSJ...)  ---------------------
    #                                 et sur toute la duree du fichier d#entree (rSom...)

        if (rSJRytGlo>0){
        rSJAlbedoVignoble = 1 - (rSJRytGloVigne + rSJRytGloSol) / rSJRytGlo     #ratios journaliers / Rayonnement
        rSJRgvRel = rSJRytGloVigne / ((1 - rSJAlbedoVignoble) * rSJRytGlo)
        rSJRgsRel = rSJRytGloSol / ((1 - rSJAlbedoVignoble) * rSJRytGlo)
        }

}       #fin boucle if rHautRang > 0  (realisation calculs int ryt dans boucle horaire)

    # ------------------------------  sorties journalières  ------------------------------------
    #                           (intégrales sur la durée de la journée)
sor_ryt=data.frame(c(1:25))    
sor_ryt[1,1]= iSimulDay
sor_ryt[2,1]= iYear
sor_ryt[3,1]= rLatitude
sor_ryt[4,1]= rHrLevSol
sor_ryt[5,1]= rHrCouSol
sor_ryt[6,1]= rDurJour
sor_ryt[7,1]= rRytGloExtraAtmJour / 1000000
sor_ryt[8,1]= rRgiJour / 1000000

        if (rHautRang>0){   #donc si realisation calculs int ryt (et event. photos.) dans boucle horaire
            sor_ryt[9,1] = rEcartRangs
            sor_ryt[10,1] = az_r     #comme en entrée :   0 pour rangs NS, 90 pour EW et -45 pour NW-SE
            sor_ryt[11,1] = rHautRang
            sor_ryt[12,1] = rLargRang
            sor_ryt[13,1] = rPorosite
            sor_ryt[14,1] = rPorosite
            sor_ryt[15,1] = tan(0.5 * atan(rHautRang / (rEcartRangs - rLargRang)))     #Fact de Forme entre 2 rangs

            sor_ryt[16,1] = rSJRytGlo / 1000000
            sor_ryt[17,1] = (rSJRytGlo - rSJRytDif) / 1000000
            sor_ryt[18,1] = rSJRytDif / 1000000
            sor_ryt[19,1] = rSJRytGloVigne / 1000000
            sor_ryt[20,1] = rSJRytGloSol / 1000000
            sor_ryt[21,1] = rSJAlbedoVignoble
            sor_ryt[22,1] = rSJRgvRel
            sor_ryt[23,1] = rSJRgsRel
            sor_ryt[24,1] = iSJdiurne #nb d#intervalles utilisés pour calculer les integrales SJ (jour entier)
            sor_ryt[25,1] = iSJtot     #nb d#intervalles utilisés pour calculer les integrales SJ (per. diurne)
        }else{
            for (i in 9:25) {
                sor_ryt[i,1] = 0      
                 }
        }      #FIN TEST   If rHautRang > 0 Then
                                #(=test si realisation effective des calculs d#int ryt dans boucle horaire)

#résultats journaliers :
    #Cells(i_ligne, 49).Value = sor_ryt(21)                              #alb
    #Cells(i_ligne, 50).Value = sor_ryt(22)                              #k_rgv
return(sor_ryt[22,1])    # retourne en sortie le kmax de la vigne
} # fin de la fonction  Radiation_Interception_DI


