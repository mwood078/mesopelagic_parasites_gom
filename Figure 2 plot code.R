######################################################################
######################################################################
#### Project: Diet and parasites of the mesopelagic Gulf of Mexico ###
####              File: Development of Figure 2                    ###
####               Developer: Matt Woodstock                       ###
######################################################################
######################################################################

## Citation: Woodstock MS, CA Blanar, TT Sutton. (2020) 
##   Diet and parasites of mesopelagic fishes in the Gulf 
##   of Mexico. Marine Biology 

## Clear Workspace ##
rm(list=ls())

## Load libraries ##
library(ggplot2)
library(ggpubr)
library(gplots)

## Set working directory ##
workdir<- "C:/Users/mwoodsto/Documents/Mar_Bio_Parasite Mansucript Review"
setwd(workdir)


## Load data ##
dat<-read.csv("Parasite Proportions.csv")
dat_diet <- read.csv("Diet_prop.csv")

## Transform data into the proper format ##
dat <- dat[1:18,] #Make sure dataframe is only 18 rows#

y <- c(as.character(dat[,2]))
x <- c("Nem.","Oth. Ces.","Tetra.","Tryp.","Oth. Dig.","Opec.","Acanth.")
data<-expand.grid(X=x,Y=y)

z <- vector(mode="numeric", length=length(x)*length(y))
count<-1
for (a in 1:length(dat$Species)){
  for (b in 3:9){
    fam[count] <- dat[a,1]
    z[count]<-dat[a,b]
    count<-count+1
  }
}

data$Proportion<-z

## Diet data ##

y2 <- c(as.character(dat_diet[,1]))
x2 <- c("Cop.","Pte.","Ost.","Eup.","Mys.","Sto.","Dec.","Amp.","Fish","Gel.","Squ.","Pol.")
data_diet<-expand.grid(X2=x2,Y2=y2)

z2 <- vector(mode="numeric", length=length(x2)*length(y2))
count<-1
for (a in 1:length(dat_diet$X)){
  for (b in 2:13){
    z2[count]<-dat_diet[a,b]
    count<-count+1
  }
}

data_diet$Proportion<-z2

## Create Plot ##
dev.new()

plots <- list()
plots[[1]]<-ggplot(data_diet, aes(x=X2, y=Y2, fill= Proportion)) + ylab("") + xlab("Prey Taxon")+
  scale_fill_gradientn(colours=c("white","red"),breaks=c(0,0.5,1),labels=c("0.0","0.5","1.0"),
                       limits=c(0,1))+
  theme(axis.text = element_text(size=14),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=12),
        axis.title = element_text(size=14),
        legend.position = "left",
        legend.text = element_text(size=12),
        legend.title = element_text(size=14,vjust=1),
        axis.ticks = element_blank(),
        title = element_text(size=14),
        legend.key.size = unit(0.5,"cm"),
        
        panel.background = element_rect(fill=NA,color="black"))+
  ggtitle("a")+
  geom_tile()

plots[[1]] = ggplotGrob(plots[[1]])

plots[[2]] <- ggplot(data, aes(x=X, y=Y, fill= Proportion)) + ylab("") + xlab("Parasite Taxon")+
  scale_fill_gradientn(colours=c("white","red"),breaks=c(0,0.5,1),labels=c("0.0","0.5","1.0"),
                       limits=c(0,1))+
  theme(axis.text = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(face = "italic",hjust = 0.5),
        axis.title = element_text(size=14),
        legend.position = "none",
        legend.text = element_text(size=14),
        legend.title = element_text(size=16),
        axis.ticks = element_blank(),
        title=element_text(size=14),
        panel.background = element_rect(fill=NA,color="black"))+
  ggtitle("b")+
  geom_tile()
plots[[2]] = ggplotGrob(plots[[2]])

p<-ggarrange(plotlist = plots,
             ncol = 2, nrow = 1)
p

