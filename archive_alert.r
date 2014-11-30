#####################################################################################################################################
# Code to grab in csv format alerting color code by alert zone for Tuscany Region
#
# Author: Alfonso Crisci IBIMET CNR a.crisci@
#####################################################################################################################################


#################################################################################################
# Load R libraries  

library(png)
library(RCurl)
library(plyr)

####################################
# Setup working dir 

setwd("D:\\lav_msg")


#################################################################################################
# Elaborate  Date char strings YYYYMMDD 


seq_date=seq(as.Date("2012/06/01"), as.Date("2014/11/26"),by="days")

seq_date_string=gsub("-","",seq_date)

webrooot='http://www.sir.toscana.it/supports/images/risks/';
rischio=c("_idrogeologico-idraulico","_vento","_mareggiate","_ghiaccio")

list_images_archive_idro=list()
list_images_archive_vento=list()
list_images_archive_mareggiate=list()
list_images_archive_ghiaccio=list()

for ( j in 730:length(seq_date_string)) {
                     
					 namefileidro=paste0(webrooot,seq_date_string[j],rischio[1],".png")
				     list_images_archive_idro[[j]]=as.raster(readPNG(getURLContent(namefileidro)))
					 
					 namefilevento=paste0(webrooot,seq_date_string[j],rischio[2],".png")
				     list_images_archive_vento[[j]]=as.raster(readPNG(getURLContent(namefilevento)))
                     
					 namefilemare=paste0(webrooot,seq_date_string[j],rischio[3],".png")
				     list_images_archive_mareggiate[[j]]=as.raster(readPNG(getURLContent(namefilemare)))
                     
					 namefileghiaccio=paste0(webrooot,seq_date_string[j],rischio[4],".png")
				     list_images_archive_ghiaccio[[j]]=as.raster(readPNG(getURLContent(namefileghiaccio)))
					 
                     #saveRDS(list_images_archive_idro,"list_images_archive_idro.rds")
					 #saveRDS(list_images_archive_vento,"list_images_archive_vento.rds")
					 #saveRDS(list_images_archive_mareggiate,"list_images_archive_mareggiate.rds")
					 #saveRDS(list_images_archive_ghiaccio,"list_images_archive_ghiaccio.rds")
					 

					}
					
					
saveRDS(list_images_archive_idro,"list_images_archive_idro.rds")
saveRDS(list_images_archive_vento,"list_images_archive_vento.rds")
saveRDS(list_images_archive_mareggiate,"list_images_archive_mareggiate.rds")
saveRDS(list_images_archive_ghiaccio,"list_images_archive_ghiaccio.rds")
					 					
list_images_archive_idro=readRDS("list_images_archive_idro.rds")
list_images_archive_vento=readRDS("list_images_archive_vento.rds")
list_images_archive_mareggiate=readRDS(,"list_images_archive_mareggiate.rds")
list_images_archive_ghiaccio=readRDS("list_images_archive_ghiaccio.rds")

										
##################################################################################################
# Load point coordinates Toscana map allerta stored in a csv file prevously built.

allerta_pos=read.csv("allerta_pos.csv")
					
list_color_idro=list()
for ( j in seq(list_images_archive_idro)) {temp=list();
                                     for ( i in 1:nrow(allerta_pos)) {
                                                                  temp[[i]]=as.character(list_images_archive_idro[[j]][allerta_pos$Ypos[i],allerta_pos$Xpos[i]]);
                                                                  }
								     list_color_idro[[j]]=unlist(temp);
								     rm(temp);
                                    }
									
list_color_vento=list()
for ( j in seq(list_images_archive_vento)) {temp=list();
                                     for ( i in 1:nrow(allerta_pos)) {
                                                                  temp[[i]]=as.character(list_images_archive_vento[[j]][allerta_pos$Ypos[i],allerta_pos$Xpos[i]]);
                                                                  }
								     list_color_vento[[j]]=unlist(temp);
								     rm(temp);
                                    }

list_color_mareggiate=list()
for ( j in seq(list_images_archive_mareggiate)) {temp=list();
                                     for ( i in 1:nrow(allerta_pos)) {
                                                                  temp[[i]]=as.character(list_images_archive_mareggiate[[j]][allerta_pos$Ypos[i],allerta_pos$Xpos[i]]);
                                                                  }
								     list_color_mareggiate[[j]]=unlist(temp);
								     rm(temp);
                                    }

list_color_ghiaccio=list()
for ( j in seq(list_images_archive_ghiaccio)) {temp=list();
                                     for ( i in 1:nrow(allerta_pos)) {
                                                                  temp[[i]]=as.character(list_images_archive_ghiaccio[[j]][allerta_pos$Ypos[i],allerta_pos$Xpos[i]]);
                                                                  }
								     list_color_ghiaccio[[j]]=unlist(temp);
								     rm(temp);
                                    }


saveRDS(list_color_idro,"list_color_idro.rds")
saveRDS(list_color_vento,"list_color_vento.rds")
saveRDS(list_color_mareggiate,"list_color_mareggiate.rds")
saveRDS(list_color_ghiaccio,"list_color_ghiaccio.rds")
list_color_idro=readRDS("list_color_idro.rds")
list_color_vento=readRDS("list_color_vento.rds")
list_color_mareggiate=readRDS(,"list_color_mareggiate.rds")
list_color_ghiaccio=readRDS("list_color_ghiaccio.rds")

color_alert=c("#99CC33","#FFFF00","#FFAA00","#FF0000")
level_alert=c(1,2,3,4)

#									
############################################################################################################################################################################									
									

allerta_colore_idro=as.data.frame(t(as.data.frame(list_color_idro,stringsAsFactors=F)),stringsAsFactors=F)
row.names(allerta_colore_idro)=NULL
names(allerta_colore_idro)=allerta_pos$Zona
allerta_colore_idro$Data=seq_date
write.csv(allerta_colore_idro,file=paste0("allerta_toscana_storico_idro",".csv"),row.names=F)




		
allerta_colore_vento=as.data.frame(t(as.data.frame(list_color_vento,stringsAsFactors=F)),stringsAsFactors=F)

row.names(allerta_colore_vento)=NULL
names(allerta_colore_vento)=allerta_pos$Zona
allerta_colore_vento$Data=seq_date
write.csv(allerta_colore_vento,file=paste0("allerta_toscana_storico_vento",".csv"),row.names=F)


allerta_colore_mareggiate=as.data.frame(t(as.data.frame(list_color_mareggiate,stringsAsFactors=F)),stringsAsFactors=F)
row.names(allerta_colore_mareggiate)=NULL
names(allerta_colore_mareggiate)=allerta_pos$Zona
allerta_colore_mareggiate$Data=seq_date
write.csv(allerta_colore_mareggiate,file=paste0("allerta_toscana_storico_mareggiate",".csv"),row.names=F)


allerta_colore_ghiaccio=as.data.frame(t(as.data.frame(list_color_ghiaccio,stringsAsFactors=F)),stringsAsFactors=F)
row.names(allerta_colore_ghiaccio)=NULL
names(allerta_colore_ghiaccio)=allerta_pos$Zona
allerta_colore_ghiaccio$Data=seq_date
write.csv(allerta_colore_ghiaccio,file=paste0("allerta_toscana_storico_ghiaccio",".csv"),row.names=F)



		