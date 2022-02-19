rm(list=ls())

#===================================================
#libraries
#===================================================
library(ggplot2)
library(raster)
library(stringr)
library(viridis)
library(scales)
library(data.table)
library(reshape2)
library(ggthemes)
library(plyr)

library(RColorBrewer)
library(wesanderson)
library(viridis)
library(RColorBrewer)

#===================================================
#User input required
flag = 1 # 1: MAR type, 2: influent type, 3: effluent type

#===================================================
#directories
#===================================================
wd = list()
wd$data = 'C:/sarfaraz/Project_learning/GITHUB_repo/MARGlob/00_data/'
wd$output = 'C:/sarfaraz/Project_learning/GITHUB_repo/MARGlob/02_figures/'
wd$soil_map = 'C:/sarfaraz/Project_learning/GITHUB_repo/MARGlob/00_data/01_raster/'
setwd(wd$data)

#===================================================
#Data pre-processing
#===================================================
#reading data
df = read.csv('globalmar_202005_02_MAR_influentsource.csv', header = T)[,c(1:11)]

# coordinates(df)= ~ Longitude+ Latitude
coordinates(df)= ~ longitude+ latitude

setwd(wd$soil_map)
r = raster('HYSOGs250m.tif')


rasValue=extract(r, df)
c.r.all=cbind(df,rasValue)
names(c.r.all)[10] = 'soil_class_val'
head(c.r.all)
c.df = cbind(data.frame(c.r.all) , soil_grp = 'tmp')
head(c.df)
dim(c.df)
unique(c.df[,10])

#remove na
id.tmp = which(is.na(c.df[,10]) == T)
c.df = c.df[-id.tmp,]
id.tmp = which(c.df[,9] == 'no data')
c.df = c.df[-id.tmp,]
id.tmp = which(c.df[,9] == 'Barriers and Bunds')
c.df = c.df[-id.tmp,]
id.tmp = which(c.df[,9] == 'Excess Irrigation')
c.df = c.df[-id.tmp,]
id.tmp = which(c.df[,9] == 'Reverse Drainage')
c.df = c.df[-id.tmp,]
id.tmp = which(c.df[,9] == 'Rooftop Rainwater Harvesting')
c.df = c.df[-id.tmp,]

id.tmp = which(c.df[,9] == 'Sand Storage Dams')
c.df[id.tmp,9] = 'Sand Dams'
id.tmp = which(c.df[,9] == 'Surface flooding and ditch-and-furrow')
c.df[id.tmp,9] = 'Surface flooding'
id.tmp = which(c.df[,9] == 'Aquifer storage and recovery')
c.df[id.tmp,9] = 'ASR'
id.tmp = which(c.df[,9] == 'Channel infiltration/spreading')
c.df[id.tmp,9] = 'Channel spreading'
id.tmp = which(c.df[,9] == 'Aquifer storage using wells')
c.df[id.tmp,9] = 'Storage using wells'

#storing the name of each class type
c.df[,14] = as.character(c.df[,14])
id.tmp = which(c.df[,10] == 1)
c.df[id.tmp,14] = 'A'
id.tmp = which(c.df[,10] == 2)
c.df[id.tmp,14] = 'B'
id.tmp = which(c.df[,10] == 3)
c.df[id.tmp,14] = 'C'
id.tmp = which(c.df[,10] == 4)
c.df[id.tmp,14] = 'D'
id.tmp = which(c.df[,10] == 11)
c.df[id.tmp,14] = 'A/D'
id.tmp = which(c.df[,10] == 12)
c.df[id.tmp,14] = 'B/D'
id.tmp = which(c.df[,10] == 13)
c.df[id.tmp,14] = 'C/D'
id.tmp = which(c.df[,10] == 14)
c.df[id.tmp,14] = 'D/D'

write.csv(c.df, paste0(wd$output,'soil_stationwise.csv'))

df.sel = c.df[,c(9,14)]
sl.grp = unique(df.sel[,2])
mar.typ = unique(df.sel[,1])

comb.stat = 0
for (i in 1:length(mar.typ)) {
  tmp.id = which(df.sel[,1]==mar.typ[i])
  tmp.df = df.sel[tmp.id,]
  
  for (j in 1:length(sl.grp)) {
    tot.typ = length(which(tmp.df[,2]==sl.grp[j]))
    if(length(tot.typ)==0){tot.typ = 0}
    
    comb.stat = rbind(comb.stat,data.frame(sl.grp = sl.grp[j], mar.typ = mar.typ[i], tot.perc = tot.typ*100/nrow(tmp.df)))
  }
  
}

comb.stat= comb.stat[-1,]
unique(comb.stat[,1])

# lets add zero values for A/D as there is no A/D
id.tmp = which(comb.stat[,1]=='A')
tmp = comb.stat[id.tmp,]
tmp[,1] = 'A/D'
tmp[,3] = 0
comb.stat = rbind(comb.stat,tmp)
head(comb.stat)

y = c("A","B","C","D","A/D",'B/D','C/D','D/D')
comb.stat = comb.stat[order(match(comb.stat[,1], y)),]

comb.stat[,1] = factor(as.character(comb.stat[,1]), levels = unique(comb.stat[,1]))
comb.stat[,2] = factor(as.character(comb.stat[,2]), levels = unique(comb.stat[,2]))

# write.csv(comb.stat,paste0(wd$data,'/MAR_soilType.csv'))

#===================================================
#Plot
#===================================================
p=ggplot(data = comb.stat, aes(y=mar.typ, x=sl.grp,fill=tot.perc))+
  geom_tile(color="white") +
  scale_fill_viridis()+
  # geom_point(aes(size=ifelse(RMSE==0.0, "dot","no_dot")),color="red",shape=8) +
  # scale_size_manual(values=c(dot=2, no_dot=NA), guide="none")+
  theme_tufte(base_family="Helvetica")+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=15))+
  theme(axis.text.x=element_text(size=15))+
  #theme(legend.position = "none" )+
  theme(panel.background = element_blank())+
  theme(plot.background=element_blank())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab(" ")+ ylab(' ')+#+ggtitle("b) Evapotranspiration")+
  guides(fill=guide_legend(title="Occurrence[%]"))

#export figure
ggsave(p,filename=paste0(wd$output,"03_MARwise_soil_group_global",".png",sep=""),
       width = 15, height = 12, units = "cm")
