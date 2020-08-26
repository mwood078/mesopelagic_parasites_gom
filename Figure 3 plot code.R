######################################################################
######################################################################
#### Project: Diet and parasites of the mesopelagic Gulf of Mexico ###
####              File: Development of Figure 1                    ###
####               Developer: Matt Woodstock                       ###
######################################################################
######################################################################

## Citation: Woodstock MS, CA Blanar, TT Sutton. (2020) 
##   Diet and parasites of mesopelagic fishes in the Gulf 
##   of Mexico. Marine Biology 


## Clear Workspace ##
rm(list=ls())

## Set wd ##
setwd("C:/Users/mwoodsto/Documents/Mar_Bio_Parasite Mansucript Review")

## Load data ##
diet<-read.csv("Diet.csv")

## Load packages ##

library(vegan) #2.5-6
library(MASS)


#Reconfigure Diet####

sum_diet<-aggregate(diet[,c(6:17)],by=list(diet$Species),FUN=sum)
row.names(sum_diet)<-sum_diet[,1]
sum_diet<-sum_diet[,-1]
prop_diet<-data.frame(0,dim=c(18,12))



for (a in 1:18){
  for (b in 1:12){
    prop_diet[a,b]<-sum_diet[a,b]/sum(sum_diet[a,c(1:12)])
  }
}
row.names(prop_diet)<-rownames(sum_diet)
colnames(prop_diet)<-colnames(diet[6:17])


d <- vegdist(prop_diet, method="bray", binary=FALSE, diag=FALSE, upper=FALSE,
             na.rm = FALSE,)  # distance matrix

fit <- isoMDS(d, k=2) # k is the number of dim
fit # view results

# plot solution ####
x <- fit$points[,1]
y <- fit$points[,2]

plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Nonmetric MDS", type="n")
text(x, y, labels = row.names(prop_diet), cex=.7)


NMDS.log<-log(dune+1)
diet <- metaMDS(prop_diet)

NMDS = data.frame(MDS1 = diet$points[,1], MDS2 = diet$points[,2])

vec.sp<-envfit(diet$points,prop_diet,perm=1000)
vec.sp.df<-as.data.frame(vec.sp$vectors$arrows*sqrt(vec.sp$vectors$r))
vec.sp.df$species<-rownames(vec.sp.df)
vec.sp.df$species[10]<-"Gelatinous zoop."
vec.sp.df$species[7]<-"Decapod"


prop_diet[,13]<-c("Generalist",3,3,4,3,4,6,2,1,3,3,6,6,6,4,5,5,3)
prop_diet[,14]<-seq(1:18)

p<-ggplot(data = NMDS, aes(MDS1, MDS2)) + ylim(-2,1.1)+
  geom_text(aes(label = prop_diet[,14],colour=c("firebrick4",
                                                "steelblue",
                                                "steelblue",
                                                "peru",
                                                "steelblue",
                                                "peru",
                                                "darkolivegreen3",
                                                "orchid",
                                                "aquamarine",
                                                "steelblue",
                                                "steelblue",
                                                "darkolivegreen3",
                                                "darkolivegreen3",
                                                "darkolivegreen3",
                                                "peru",
                                                "firebrick4",
                                                "firebrick4",
                                                "steelblue")),
            size=10)+
  theme_bw()+  
  
  geom_segment(data=vec.sp.df[1,],aes(x=0,xend=MDS1,y=0,yend=MDS2),
               arrow = arrow(length = unit(0.5, "cm")),colour="grey") + 
  geom_text(data=vec.sp.df[1,],aes(x=MDS1,y=MDS2,label=species),size=6)+
  geom_segment(data=vec.sp.df[3,],aes(x=0,xend=MDS1,y=0,yend=MDS2),
               arrow = arrow(length = unit(0.5, "cm")),colour="grey") + 
  geom_text(data=vec.sp.df[3,],aes(x=MDS1,y=MDS2,label=species),size=6)+
  geom_segment(data=vec.sp.df[7,],aes(x=0,xend=MDS1,y=0,yend=MDS2),
               arrow = arrow(length = unit(0.5, "cm")),colour="grey") + 
  geom_text(data=vec.sp.df[7,],aes(x=MDS1,y=MDS2,label=species),size=6)+
  geom_segment(data=vec.sp.df[9,],aes(x=0,xend=MDS1,y=0,yend=MDS2),
               arrow = arrow(length = unit(0.5, "cm")),colour="grey") + 
  geom_text(data=vec.sp.df[9,],aes(x=MDS1,y=MDS2,label=species),size=6)+
  geom_segment(data=vec.sp.df[10,],aes(x=0,xend=MDS1,y=0,yend=MDS2),
               arrow = arrow(length = unit(0.5, "cm")),colour="grey") + 
  geom_text(data=vec.sp.df[10,],aes(x=MDS1,y=MDS2,label=species),size=6)+
  coord_fixed()
p+  theme(legend.position = "none",
          axis.text = element_text(size=14),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(face = "italic",hjust = 0.5),
          axis.title = element_text(size=14),
          axis.ticks = element_blank(),
          title=element_text(size=14),
          panel.background = element_rect(fill=NA,color="black"))
p+ scale_color_manual(name="Feeding Guilds",guide="legend",
                      values=c("aquamarine","orchid","steelblue",
                               "peru","firebrick4",
                               "darkolivegreen3")
                      ,labels=c("Gelatinous Zooplankton",
                                "Upper-Trophic Level",
                                "Generalist Mesozooplankton",
                                "Copepods/Ostracods",
                                "Copepods/Mesozooplankton",
                                "Copepods"))+
  theme(legend.text = element_text(size=14),
        legend.title = element_text(size=16))+
  annotate(geom="text",x=1.55,y=1.1,label="Stress: 0.07",size=5)

p
