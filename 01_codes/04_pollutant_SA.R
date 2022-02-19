rm(list=ls())

#=================================================
#libraries
#=================================================
library(ggplot2)
library(ggfittext)
library(scales)
library(ggpubr)
library(dplyr)

#=================================================
#directories
#=================================================
wd = list()
wd$data = 'C:/sarfaraz/Project_learning/GITHUB_repo/MARGlob/00_data/'
wd$output = 'C:/sarfaraz/Project_learning/GITHUB_repo/MARGlob/02_figures/'
setwd(wd$data)

#=================================================
#Pre-processing
#=================================================
#reading data
df = read.csv('2020-30-04-MAR-Pollutant-concentration_SM.csv',header = T)
#providing column names
colnames(df) = c('Contaminant_type', 'Compound', 'Source', 'Unit', 'Concentration', 'Reference')

#separating column 1,2,3 and 5
df.sel = df[,c(1,2,3,5)]
head(df.sel)

#preliminary survey
#----------------------------------------
#what are the contaminants?
unique(df.sel[,1])
# [1] Anions         Cations        Nutrient       Organics       Metals         Trace Organics
# [7] Pathogens      Particles               

#what are the compounds?
unique(df.sel[,2])
# there are 113 unique compounds

#what are the coumpunds?
unique(df.sel[,3])
# Influent   Effluent   Background Influent 
# for some reason influent is identified twice.

#-----------------------------------------

#removing compund type Background as we are not interested in that
id.tmp = which(df.sel[,3] == 'Background')
length(id.tmp) #90
df.sel = df.sel[-id.tmp,]

#Since there are Influent twice (may be twice due to space difference after the name),
#assigning name manually
id.tmp = which(df.sel[,3] == 'Effluent')
tmp.ef = data.frame(df.sel[id.tmp,c(1,2,4)],Source = 'Effluent')
tmp.in = data.frame(df.sel[-id.tmp,c(1,2,4)],Source = 'Influent')
df.mod = rbind(tmp.in, tmp.ef)

#Checking compunds under each contaminant type
#-----------------------------------------
#using function
sep.func = function(df.mod,comp_type)
{
  #separate trace organics data
  id.tmp = which(df.mod[,1] == comp_type)
  df.tmp = df.mod[id.tmp,]
  return(unique(df.tmp[,2]))
}

#checking numbers of observations
length(sep.func(df.mod, comp_type = 'Organics'))#4
length(sep.func(df.mod, comp_type = 'Trace Organics'))#52
length(sep.func(df.mod, comp_type = 'Metals'))#13
length(sep.func(df.mod, comp_type = 'Pathogens')) #11
length(sep.func(df.mod, comp_type = 'Particles'))#1
length(sep.func(df.mod, comp_type = 'Anions')) #8
length(sep.func(df.mod, comp_type = 'Cations')) #8


#--------------------------------------------------
#Assigning names units [all are not usefull]
type.combine.all = c('E. coli','DOC','Turbidity','Fe','As','Mn','Al','Cu','Zn','TOC','DOC',
                     'Cl','SO4','K','Ca','Mg')

#replance duplicate Influent
id.tmp = which(df.sel[,3]=="Influent ")
df.sel[id.tmp,3] = "Influent"

#lets plot metal only
met = c('Fe','As','Mn','Al','Cu','Zn')
df.sel.metal = df.sel[df.sel$Compound %in% met,]

# br=c(1, 100, 10000, 1000000)

stat_box_data <- function(y, upper_limit = max(log10(df.sel.metal$Concentration)) * 1.2) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('n =', length(y))
    )
  )
}


vertical.lines <- c(1.5, 2.5,3.5,4.5,5.5)

y = c("Influent","Effluent")
df.sel.metal = df.sel.metal[order(match(df.sel.metal[,3], y)),]
df.sel.metal[,3] = factor(as.character(df.sel.metal[,3]), levels = unique(df.sel.metal[,3]))

#=================================================
#Plot
#=================================================
 p= ggplot(data=df.sel.metal, aes(x=Compound, y= Concentration, fill = factor(Source))) + 
    geom_boxplot(aes(color = Source), width = 0.4, size = 0.6, position=position_dodge(0.6), outlier.shape = NA)+
    geom_vline(xintercept=vertical.lines,   # Ignore NA values for mean
               color="grey85", linetype="dashed", size=0.5)+
    geom_jitter(position=position_dodge(0.5),size=1)+
    # scale_x_discrete(labels=c("Infiltration basin", "SAT", "ASR", "Dune Infiltration"))+
    labs(y=expression("Metal concentration"~~(µg/L)))+
    scale_y_log10(breaks=10^(0:5),labels=trans_format("log10", math_format(10^.x)))+
    coord_cartesian(ylim=c(1,100000))+
    scale_color_manual(values=c("red4", "#009E73"))+
    scale_fill_manual(values=c("grey60", "white"))+
   stat_compare_means(aes(label=..p.signif..),label.y = 4.6, angle = 0, size = 5)+
   
   stat_summary(
     fun.data = stat_box_data, 
     geom = "text", 
     hjust = 0.5,
     vjust = 0.4, angle = 90,position=position_dodge(width=0.6), size = 5
   )
   
 
 p = p +
    theme_bw()+
    theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold"),
          axis.text.x=element_text(size=15),
          axis.title.x=element_blank(),
          axis.text.y=element_text(size=15),
          axis.title.y=element_text(size = 15),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
    )+
   labs(color  = " ", fill = " ")+
   theme(legend.text=element_text(size=15))
  p.met=p
  
  #export plot
  ggsave(p,filename=paste0(wd$output,"figure/",'04_metal_conentration.png'),
         width = 19, height = 16, units = "cm")

  
  stat_box_data <- function(y, upper_limit = max(df.sel.2$Concentration) * 1.2) {
    return(
      data.frame(
        y = 0.95 * upper_limit,
        label = paste('n =', length(y))
      )
    )
  }
  
  
  #=================================================
  #plot DOC TOC, Turbidity only
  #=================================================
  met = c('DOC','TOC','Turbidity')
  df.sel.2 = df.sel[df.sel$Compound %in% met,]
  
  y = c("Influent","Effluent")
  df.sel.2 = df.sel.2[order(match(df.sel.2[,3], y)),]
  df.sel.2[,3] = factor(as.character(df.sel.2[,3]), levels = unique(df.sel.2[,3]))
  
  vertical.lines <- c(1.5, 2.5,3.5)
  
  p=ggplot(data=df.sel.2, aes(x=Compound, y= Concentration, fill = factor(Source))) + 
    geom_boxplot(aes(color = Source), width = 0.4, size = 0.6, position=position_dodge(0.6), outlier.shape = NA)+
    geom_vline(xintercept=vertical.lines,   # Ignore NA values for mean
               color="grey85", linetype="dashed", size=0.5)+
    geom_jitter(position=position_dodge(0.5),size=1)+
    scale_color_manual(values=c("red4", "#009E73"))+
    scale_fill_manual(values=c("grey60", "white"))+
    stat_compare_means(aes(label=..p.signif..),label.y = 15, angle = 0, size = 5)+
    stat_summary(
      fun.data = stat_box_data,
      geom = "text",
      hjust = 0.5,
      vjust = 0.4, angle = 90,position=position_dodge(width=0.6), size = 5
    )+
    scale_y_continuous(
      # Features of the first axis
      name = expression("DOC and TOC concentration"~~(mg/L)),
      # Add a second axis and specify its features
      sec.axis = sec_axis( trans=~.+0, name="Turbidity (NTU)")
    )
    # scale_y_continuous("Turbidity (NTU)", sec.axis = sec_axis(~ . + 0, name = derive()))
  
  p = p +
    theme_bw()+
    theme(legend.position = "none",
          legend.title=element_blank(),
          legend.direction = "horizontal")+
    theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold"),
          axis.text.x=element_text(size=15),
          axis.title.x=element_blank(),
          axis.text.y=element_text(size=15),
          axis.title.y=element_text(size = 15),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
    )
    # labs(color  = " ", fill = " ")
  
  p.doc = p
  
  #Export
  ggsave(p,filename=paste0(wd$output,"figure/",'04_DOCtocTur_conentration.png'),
         width = 16, height = 16, units = "cm")
  
  #=================================================
  #Plot  Ecoli, salmonella, giardia, enterococci and helminth together only
  #=================================================
  # ecoli and others-----------------------------------------------
  # pathog = c('E. coli','Salmonella','Giardia','Enterococci','Helminth')
  stat_box_data <- function(y, upper_limit = max(log10(df.sel.pathog$Concentration)) * .55) {
    return( 
      data.frame(
        y = 0.95 * upper_limit,
        label = paste('n =', length(y))
      )
    )
  }
  pathog = c('E. coli','Enterococci','Clostridium','Somatic')
  df.sel.pathog = df.sel[df.sel$Compound %in% pathog,]
  
  
  vertical.lines <- c(1.5, 2.5,3.5)
  
  y = c("Influent","Effluent")
  df.sel.pathog = df.sel.pathog[order(match(df.sel.pathog[,3], y)),]
  df.sel.pathog[,3] = factor(as.character(df.sel.pathog[,3]), levels = unique(df.sel.pathog[,3]))
  
  df.sel.pathog = filter(df.sel.pathog, Concentration>0)
  # vertical.lines <- c(1.5, 2.5,3.5,4)
  
#===============================================================
#plot
 p= ggplot(data=df.sel.pathog, aes(x=Compound, y= Concentration, fill = factor(Source))) + 
    geom_boxplot(aes(color = Source), width = 0.4, size = 0.6, position=position_dodge(0.6), outlier.shape = NA)+
    geom_vline(xintercept=vertical.lines,   # Ignore NA values for mean
               color="grey85", linetype="dashed", size=0.5)+
    geom_jitter(position=position_dodge(0.5),size=1)+
    # scale_x_discrete(labels=c("Infiltration basin", "SAT", "ASR", "Dune Infiltration"))+
    labs(y=expression("n/100 mL"))+
    scale_y_log10(breaks=10^(-7:8),labels=trans_format("log10", math_format(10^.x)))+
    coord_cartesian(ylim=c(1,10000000))+
    scale_color_manual(values=c("red4", "#009E73"))+
    scale_fill_manual(values=c("grey60", "white"))+
    stat_compare_means(aes(label=..p.signif..),label.y = 6.5, angle = 0, size = 5)+
    stat_summary(
      fun.data = stat_box_data,
      geom = "text",
      hjust = 0.5,
      vjust = 0.4, angle = 90,position=position_dodge(width=0.9), size = 5
    )
  
  p = p +
    theme_bw()+
    theme(legend.position = "none",
          legend.title=element_blank(),
          legend.direction = "horizontal")+
    theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold"),
          axis.text.x=element_text(size=15),
          axis.title.x=element_blank(),
          axis.text.y=element_text(size=15),
          axis.title.y=element_text(size =15),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
    )
  p
  p.path = p
  
  
  #Export plots
  ggsave(p,filename=paste0(wd$output,"figure/",'04_Pathogen_conentration.png'),
         width = 16, height = 16, units = "cm")
  
  l = ggarrange(p.doc, p.path, p.met, 
            labels = c("(a)", "(b)", "(c)"),
            ncol = 3, nrow = 1)
  
  ggsave(l,filename=paste0(wd$output,"figure/",'04_combined_pollutant.png'),
         width = 44, height = 14, units = "cm")
