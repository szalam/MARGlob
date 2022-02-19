rm(list=ls())
#===========================================================
#libraries
#===========================================================
library(ggplot2)
library(raster)
library(stringr)
library(scales)
library(data.table)
library(reshape2)
library(ggthemes)
library(plyr)

library(wesanderson)
library(viridis)
library(RColorBrewer)

#===========================================================
#user input required
#===========================================================
flag = 1 #  1: influent type, 2: effluent type

#===========================================================
#directories
#===========================================================
wd = list()
wd$data = 'C:/sarfaraz/Project_learning/GITHUB_repo/MARGlob/00_data/'
wd$output = 'C:/sarfaraz/Project_learning/GITHUB_repo/MARGlob/02_figures/'
setwd(wd$data)

#===========================================================
#read data
df = read.csv('mar_location_source.csv')
df = df[,c('specific_m','influent_s')]
df.sel = df
head(df.sel)

#remove NAs
summary(df.sel)
id.tmp = which(is.na(df.sel[,2])==T)
if(length(id.tmp)>0){df.sel = df.sel[-id.tmp,]}
id.tmp = which(is.na(df.sel[,1])==T)
if(length(id.tmp)>0){df.sel = df.sel[-id.tmp,]}

#Assign names
df.sel[df.sel[,2]=='River water',2] = 'Surface water'
df.sel[df.sel[,2]=='Lake water',2] = 'Surface water'
df.sel[df.sel[,2]=='Reclaimed wastewater',2] = 'Wastewater'
df.sel[df.sel[,2]=='Storm water',2] = 'Stormwater'

df.sel[df.sel[,1]=='Sand Storage Dams',1] = 'Sand dam'

id.tmp = which(df.sel[,2]=='no data')
if(length(id.tmp)>0){df.sel = df.sel[-id.tmp,]}
id.tmp = which(df.sel[,2]=='Brackish water')
if(length(id.tmp)>0){df.sel = df.sel[-id.tmp,]}
id.tmp = which(df.sel[,2]=='Physical Aquifer Management')
if(length(id.tmp)>0){df.sel = df.sel[-id.tmp,]}
id.tmp = which(df.sel[,2]=='Distilled water')
if(length(id.tmp)>0){df.sel = df.sel[-id.tmp,]}
id.tmp = which(df.sel[,2]=='Tap water')
if(length(id.tmp)>0){df.sel = df.sel[-id.tmp,]}

id.tmp = which(df.sel[,1]=='no data')
if(length(id.tmp)>0){df.sel = df.sel[-id.tmp,]}
id.tmp = which(df.sel[,1]=='Rooftop Rainwater Harvesting')
if(length(id.tmp)>0){df.sel = df.sel[-id.tmp,]}
id.tmp = which(df.sel[,1]=='Reverse Drainage')
if(length(id.tmp)>0){df.sel = df.sel[-id.tmp,]}
id.tmp = which(df.sel[,1]=='Excess Irrigation')
if(length(id.tmp)>0){df.sel = df.sel[-id.tmp,]}
id.tmp = which(df.sel[,1]=='Reverse Drainage')
if(length(id.tmp)>0){df.sel = df.sel[-id.tmp,]}
id.tmp = which(df.sel[,1]=='Barriers and Bunds')
if(length(id.tmp)>0){df.sel = df.sel[-id.tmp,]}

wat.src = unique(df.sel[,2])
mar.typ = unique(df.sel[,1])

comb.stat = 0
for (i in 1:length(mar.typ)) {
  tmp.id = which(df.sel[,1]==mar.typ[i])
  tmp.df = df.sel[tmp.id,]
  
  for (j in 1:length(wat.src)) {
    tot.typ = length(which(tmp.df[,2]==wat.src[j]))
    if(length(tot.typ)==0){tot.typ = 0}
    
    comb.stat = rbind(comb.stat,data.frame(wat.src = wat.src[j], mar.typ = mar.typ[i], tot.perc = tot.typ*100/nrow(tmp.df)))
  }
  
}

#order change
comb.stat = comb.stat[-1,]
comb.stat[,1] = factor(as.character(comb.stat[,1]), levels = unique(comb.stat[,1]))
y = c("Groundwater","Surface water","Stormwater","Wastewater")
comb.stat = comb.stat[order(match(comb.stat[,1], y)),]
comb.stat[,1] = factor(as.character(comb.stat[,1]), levels = unique(comb.stat[,1]))

#plot
p= ggplot(data = comb.stat, aes(y=mar.typ, x=wat.src,fill=tot.perc))+
  geom_tile(color="white") +
  scale_fill_viridis()+
  # geom_point(aes(size=ifelse(RMSE==0.0, "dot","no_dot")),color="red",shape=8) +
  scale_size_manual(values=c(dot=2, no_dot=NA), guide="none")+
  theme_tufte(base_family="Helvetica")+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=15))+
  theme(axis.text.x=element_text(size=15))+
  #theme(legend.position = "none" )+
  theme(panel.background = element_blank())+
  theme(plot.background=element_blank())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+
  xlab(" ")+ ylab(' ')+#+ggtitle("b) Evapotranspiration")+
  guides(fill=guide_legend(title="Occurrence[%]"))

p

#Export figures
if(flag==1){
  ggsave(p,filename=paste0(wd$output,"02_MARwise_InfluentType_perc_alldata",".png",sep=""),
         width = 20, height = 15, units = "cm")
}
if(flag==2){
  ggsave(p,filename=paste0(wd$output,"02_MARwise_effluentType_perc",".png",sep=""),
         width = 20, height = 15, units = "cm")
}
