rm(list=ls())

#libraries
library(ggplot2)
library(wesanderson)
library(viridis)
library(RColorBrewer)

#flags
flag = 2 # 1: MAR type, 2: influent type, 3: effluent type

#directories
wd = list()
wd$data = 'C:/sarfaraz/Project_learning/GITHUB_repo/MARGlob/00_data/'
wd$output = 'C:/sarfaraz/Project_learning/GITHUB_repo/MARGlob/02_figures/'
setwd(wd$data)

#reading data
df = read.csv('MAR_AI_class.csv',header = T)
df$ClClass = NA
df[df$RASTERVALU==1,7] = 'Hyper-arid'
df[df$RASTERVALU==2,7] = 'Arid'
df[df$RASTERVALU==3,7] = 'Semi-arid'
df[df$RASTERVALU==4,7] = 'Dry sub-humid'
df[df$RASTERVALU==5,7] = 'Humid'

df = df[!is.na(df$ClClass),]
#providing column names
# colnames(df) = c('Contaminant_type', 'Compound', 'Source', 'Unit', 'Concentration', 'Reference')
df.sel =df 

df.sel[df.sel[,3]=='River water',3] = 'Surface water'
df.sel[df.sel[,3]=='Lake water',3] = 'Surface water'
df.sel[df.sel[,3]=='Reclaimed wastewater',3] = 'Wastewater'
df.sel[df.sel[,3]=='Storm water',3] = 'Stormwater'

id.tmp = which(df.sel[,3]=='no data')
if(length(id.tmp)>0){df.sel = df.sel[-id.tmp,]}
id.tmp = which(df.sel[,3]=='Brackish water')
if(length(id.tmp)>0){df.sel = df.sel[-id.tmp,]}
id.tmp = which(df.sel[,3]=='Tap water')
if(length(id.tmp)>0){df.sel = df.sel[-id.tmp,]}

df = df.sel

#separating column 1,2,3 and 5
if(flag==1){df.sel = df[,c(7,2)]}
if(flag==2){df.sel = df[,c(7,3)]}
if(flag==3){df.sel = df[,c(7,4)]}

head(df.sel)

#remove NAs
summary(df.sel)
id.tmp = which(is.na(df.sel[,2])==T)
if(length(id.tmp)>0){df.sel = df.sel[-id.tmp,]}
id.tmp = which(is.na(df.sel[,1])==T)
if(length(id.tmp)>0){df.sel = df.sel[-id.tmp,]}

clim = unique(df.sel[,1])
mar.typ = unique(df.sel[,2])

comb.stat = 0
for (i in 1:length(clim)) {
  tmp.id = which(df.sel[,1]==clim[i])
  tmp.df = df.sel[tmp.id,]
  
  for (j in 1:length(mar.typ)) {
    tot.typ = length(which(tmp.df[,2]==mar.typ[j]))
    if(length(tot.typ)==0){tot.typ = 0}
    
    comb.stat = rbind(comb.stat,data.frame(climate = clim[i], mar.typ = mar.typ[j], tot.typ = tot.typ))
  }
  
}

comb.stat = comb.stat[-1,]

col.co = wes_palette("Zissou1", 12, type = "continuous")

comb.stat[,1] = factor(as.character(comb.stat[,1]), levels = unique(comb.stat[,1]))

p=ggplot(comb.stat, aes(x = climate,y = tot.typ,fill = mar.typ))+ 
 geom_bar(stat="identity",color='black')+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+
  # facet_grid(~Compound)+
  xlab('Climate classification')+ylab('MAR cases count')
p


#Exporting figures
if(flag==1){
  p= p+ scale_fill_manual(name='MAR types',values=brewer.pal(n = length(mar.typ), name = "Paired"))+
    guides(fill=guide_legend(title="MAR Type"))
  ggsave(p,filename=paste0(wd$output,"01_climate_wise_count_MarType_allStation",".png",sep=""),
       width = 23, height = 15, units = "cm")
}
if(flag==2){
  p= p+ scale_fill_manual(name='Influent types',values=brewer.pal(n = length(mar.typ), name = "Paired"))+
    guides(fill=guide_legend(title="Influent Type"))
  ggsave(p,filename=paste0(wd$output,"01_climate_wise_count_InfluentType_allStation",".png",sep=""),
         width = 23, height = 15, units = "cm")
  }
if(flag==3){
  p= p+ scale_fill_manual(name='Effluent types',values=brewer.pal(n = length(mar.typ), name = "Paired"))+
    guides(fill=guide_legend(title="Effluent Type"))
  ggsave(p,filename=paste0(wd$output,"01_climate_wise_count_EffluentType_allStation",".png",sep=""),
         width = 2, height = 15, units = "cm")
  }


#==========================================================
# Calculating the percentage
#==========================================================
comb.perc = comb.stat
for (i in 1:length(clim)) {
  
  id.tmp = which(comb.perc[,1] == clim[i])
  tot.case = sum(comb.perc[id.tmp,3])
  comb.perc[id.tmp,3] = comb.perc[id.tmp,3]*100/tot.case
}
head(comb.perc)

y = c('Hyper-arid',"Arid","Semi-arid","Dry sub-humid","Humid")
comb.perc = comb.perc[order(match(comb.perc[,1], y)),]
y = c("Groundwater","Surface water","Stormwater","Wastewater")
comb.perc = comb.perc[order(match(comb.perc[,2], y)),]


comb.perc[,1] = factor(as.character(comb.perc[,1]), levels = unique(comb.perc[,1]))
comb.perc[,2] = factor(as.character(comb.perc[,2]), levels = unique(comb.perc[,2]))

#Plot
p=ggplot(comb.perc, aes(x = climate,y = tot.typ,fill = mar.typ))+ 
  geom_bar(stat="identity",color='black',size=.5)+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+
  # facet_grid(~Compound)+
  xlab('Climate classification')+ylab('Occurrence [%]')+
  scale_y_continuous(breaks=seq(from=0,to=100,by=10))

#Exporting figure
if(flag==1){
  p= p+ scale_fill_manual(name='MAR types',values=brewer.pal(n = length(mar.typ), name = "Paired"))+
    guides(fill=guide_legend(title="MAR Type"))
  ggsave(p,filename=paste0(wd$output,"01_climate_wise_count_MarType_perc_AllStation",".png",sep=""),
         width = 22, height = 15, units = "cm")
}
if(flag==2){
  # p= p+ scale_fill_manual(name='Influent types',values=brewer.pal(n = length(mar.typ), name = "Paired"))
  p= p+ scale_fill_manual(name='Influent types',values=c('dodgerblue3','palegreen3','tan1','firebrick3'))+
    guides(fill=guide_legend(title="Influent Type"))
  ggsave(p,filename=paste0(wd$output,"01_climate_wise_count_InfluentType_perc_AllStation",".png",sep=""),
         width = 21, height = 15, units = "cm")
}
if(flag==3){
  p= p+ scale_fill_manual(name='Effluent types',values=brewer.pal(n = length(mar.typ), name = "Paired"))+
    guides(fill=guide_legend(title="Effluent Type"))
  ggsave(p,filename=paste0(wd$output,"01_climate_wise_count_EffluentType_perc_AllStation",".png",sep=""),
         width = 23, height = 15, units = "cm")
}

