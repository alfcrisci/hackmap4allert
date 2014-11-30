################################################################################################
# PURPOSE:
# Code to grab in csv format alerting color code by alert zone for Tuscany Region
# 
# REQUIRED R PACKAGES :
# raster rgdal maptools shapefiles plyr
#
# INSTITUTIONS:
# Consorzio LaMMA - www.lamma.rete.toscana.it &
# Istituto di Biometeorologia - www.ibimet.cnr.it
#
# DEVELOPERS:
# Alfonso Crisci - a.crisci@ibimet.cnr.it; alfcrisci@gmail.com
#
# CREDITS AND REFERENCES:
# #emergenzehack
# https://sites.google.com/site/rodriguezsanchezf/news/usingrasagis
# http://johndharrison.blogspot.it/2014/03/rselenium-package.html
# http://www.r-bloggers.com/using-colorized-png-pictograms-in-r-base-plots/
#####################################################################################

#################################################################################################
# Load R libraries  

if (!require(png)) {install.packages("png"); library(png)}
if (!require(RCurl)) {install.packages("RCurl"); library(RCurl)}
if (!require(plyr)) {install.packages("plyr"); library(plyr)}
if (!require(hash)) {install.packages("hash"); library(hash)}
if (!require(maptools)) {install.packages("maptools"); library(maptools)}

####################################
# Setup working dir if necessary

setwd("D:\\lav_msg")

#################################################################################################
# Elaborate date of today and tomorrow char strings YYYYMMDD 

oggi=gsub("-","",Sys.Date())
domani=gsub("-","",Sys.Date()+1)

##################################################################################################
# Load point coordinates Toscana map allerta stored in a csv file prevously built.

allerta_pos=read.csv("allerta_pos.csv")

#################################################################
# Build and load web address and store image 

webrooot='http://www.sir.toscana.it/supports/images/risks/';
rischio=c("_idrogeologico-idraulico","_vento","_mareggiate","_ghiaccio")

list_images_oggi=list()
list_images_domani=list()

for ( i in rischio) {
                     namefileoggi=paste0(webrooot,oggi,i,".png")
				     list_images_oggi[[i]]=as.raster(readPNG(getURLContent(namefileoggi)))
					 namefiledomani=paste0(webrooot,domani,i,".png")
				     list_images_domani[[i]]=as.raster(readPNG(getURLContent(namefiledomani)))
					 
                    }


list_color_oggi=list()

for ( j in seq(list_images_oggi)) {temp=list();
                                   for ( i in 1:nrow(allerta_pos)) {
                                                                  temp[[i]]=as.character(list_images_oggi[[j]][allerta_pos$Ypos[i],allerta_pos$Xpos[i]]);
                                                                  }
								   list_color_oggi[[j]]=unlist(temp);
								   rm(temp);
                                   }
								   
allerta_colore_oggi=data.frame(zona=allerta_pos$Zona,
                               date_target=oggi,
							   class_data="oggi",
							   time_download=Sys.time(),
                               ita_code=paste0("Tosc-",allerta_pos$Zona),
                               idro=list_color_oggi[[1]],
                               vento=list_color_oggi[[2]],
							   mareggiate=list_color_oggi[[3]],
							   ghiaccio=list_color_oggi[[4]],
							   stringsAsFactors =F)

color_alert=c("#99CC33",
              "#FFFF00",
			  "#FFAA00",
			  "#FF0000")
			  
			  
level_alert=c(1,
              2,
			  3,
			  4)
			  
color_hash=hash(color_alert,level_alert)			  
							   
allerta_colore_oggi[,6]<- as.vector(sapply(allerta_colore_oggi[,6],function(x) {color_hash[[x]]}))
allerta_colore_oggi[,7]<- as.vector(sapply(allerta_colore_oggi[,7],function(x) {color_hash[[x]]}))
allerta_colore_oggi[,8]<- as.vector(sapply(allerta_colore_oggi[,8],function(x) {color_hash[[x]]}))
allerta_colore_oggi[,9]<- as.vector(sapply(allerta_colore_oggi[,9],function(x) {color_hash[[x]]}))
							   
							   
write.csv(allerta_colore_oggi,file=paste0("allerta_toscana_oggi_",as.character(oggi),".csv"),row.names=F)
								   
#################################################################################################################################

list_color_domani=list()

for ( j in seq(list_images_domani)) {temp=list();
                                     for ( i in 1:nrow(allerta_pos)) {
                                                                  temp[[i]]=as.character(list_images_domani[[j]][allerta_pos$Ypos[i],allerta_pos$Xpos[i]]);
                                                                  }
								     list_color_domani[[j]]=unlist(temp);
								     rm(temp);
                                    }

allerta_colore_domani=data.frame(zona=allerta_pos$Zona,
                                 date_target=domani,
								 class_data="domani",
							     time_download=Sys.time(),
                                 ita_code=paste0("Tosc-",allerta_pos$Zona),
                                 idro=list_color_domani[[1]],
                                 vento=list_color_domani[[2]],
							     mareggiate=list_color_domani[[3]],
							     ghiaccio=list_color_domani[[4]],
								 stringsAsFactors =F)


allerta_colore_domani[,6]<- as.vector(sapply(allerta_colore_domani[,6],function(x) {color_hash[[x]]}))
allerta_colore_domani[,7]<- as.vector(sapply(allerta_colore_domani[,7],function(x) {color_hash[[x]]}))
allerta_colore_domani[,8]<- as.vector(sapply(allerta_colore_domani[,8],function(x) {color_hash[[x]]}))
allerta_colore_domani[,9]<- as.vector(sapply(allerta_colore_domani[,9],function(x) {color_hash[[x]]}))
	
write.csv(allerta_colore_domani,file=paste0("allerta_toscana_domani_",as.character(domani),".csv"),row.names=F)
									
##################################################################################################
##################################################################################################
# Supplementary code and informations

# The object is a numerical array with four layers (red, green, blue, alpha; short RGBA). 
# Let’s have a look at the first layer (red) and replace all non-zero entries by a one and the zeros by a dot. 
# to plot 
# ratio_img_temp <- nrow(rimage_temp) / ncol(rimage_temp)
# plot(c(0,1), c(0,r), type = "n", xlab = "", ylab = "", asp=1)
# rasterImage(rimage_temp, 0, 0, 1, ratio_img_temp) 
