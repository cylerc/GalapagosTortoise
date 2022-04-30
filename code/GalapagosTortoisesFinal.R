#This .R file contains analyses associated with all metric, statistical, and visual
  #actions for the manuscript titled: Stable isotope ecology and the 1850s Floreana 
    #Island Galapagos tortoise (Chelonoidis niger niger) extinction
#We plan to finalize this .R file with full manuscript and author details prior to 
  #publication

#load libraries
library(ggplot2)
library(gridExtra)
library(spData)
library(ggrepel)
library(RColorBrewer)
library(dplyr)
library(extrafont)
library(ggpubr)
library(siar)

#load data (set working directory to the "data" folder)
master<-read.csv("MasterFinal.csv") #a file containing all stable isotope (SI) data
getOption("max.print")
nonzoo<-master[c(1:33,37:38,40:53,56:57),] #separate data into wild tortoises
zoo<-master[c(34:36,39,54:55),] #separate data into captive (zoo) tortoises
ele<-read.csv("elevation.csv") #a file containing SI data tied to island elevation
ccol<-ele[c(1:10),] #separate data into specific isotope systems (carbon-collagen)
nit<-ele[c(11:20),] #separate data into specific isotope systems (nitrogen)
hy<-ele[c(21:30),] #separate data into specific isotope systems (hydrogen)
cap<-ele[c(31:40),] #separate data into specific isotope systems (carbon-apatite)
ox<-ele[c(41:50),] #separate data into specific isotope systems (oxygen)
plants<-read.csv("plants.csv") #a file containing plant SI ratios per Island
dis<-read.csv("MasterFinal_dis.csv") #a file containing SI ratios per tissue
carb<-dis[c(1:23),] #separate data into specific isotope systems (carbon)
nit<-dis[c(24:41),] #separate data into specific isotope systems (nitrogen)
hyd<-dis[c(42:59),] #separate data into specific isotope systems (hydrogen)
sex_rev<-master[c(7:12,14,16,18,21:23,44:46,49:50),] #a file containing SI ratios per sex (collagen)
sex_rev2<-master[c(9:12,18,21:23,44:45,49:50,54),] #a file containing SI ratios per sex (apatite)
rabida<-master[c(2,20),] #separate data into specific islands/taxa (Rabida)
micro<-master[c(4),]  #separate data into specific islands/taxa (Isabela-Darwin)
guen<-master[c(24:27),]  #separate data into specific islands/taxa (Isabela-Sierra Negra)
hood<-master[c(3,5:8,19),]  #separate data into specific islands/taxa (Espanola)
porteri<-master[c(9:12,43,56:57),]  #separate data into specific islands/taxa (Santa Cruz)
chath<-master[c(13:14,16:17,46),]  #separate data into specific islands/taxa (San Cristobal)
abing<-master[c(15,18,41,44:45),]  #separate data into specific islands/taxa (Pinta)
darw<-master[c(28:31),]  #separate data into specific islands/taxa (Santiago)
nig<-master[c(32:33,40,52),]  #separate data into specific islands/taxa (Floreana)
dunc<-master[c(21:23,37:38,42,47:51,53),]  #separate data into specific islands/taxa (Pinzon)
isabela<-master[c(4,24:27),]  #separate data into specific islands/taxa (Isabela-All)
captive<-read.csv("zoo.csv") #a file containing SI ratios for captive (zoo) tortoises

##### Figure 2: Bar plots of single isotopes systems and summary metrics #####
#carbon-collagen
nonzoo$Island.Name <- factor(nonzoo$Island.Name, levels=c("Pinta", 
                                                      "Isabela", 
                                                      "Santiago",
                                                      "Rabida", 
                                                      "Pinzon", 
                                                      "Santa Cruz",
                                                      "San Cristobal",
                                                      "Floreana", 
                                                      "Espanola",
                                                      "Unk-Gold Rush"))

colourCount = length(unique(nonzoo$Island.Name))
getPalette = colorRampPalette(brewer.pal(9, "Greens"))

c1<-ggplot(nonzoo, aes(x=reorder(Island.Name, d13Ccol, FUN=median), 
                   y=d13Ccol))+ 
  geom_boxplot(fill=getPalette(colourCount))+
  geom_point()+
  xlab("")+
  ylab(expression(delta^{13}*"C"[collagen]))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 12),
        axis.text.x = element_text(face="bold.italic", vjust=0.5, color="black", size=9),
        axis.text.y = element_text(vjust=0.5, color="black", size=12, face="bold"), 
        axis.title.y = element_text(vjust=1.0, color="black", size=14, face="bold"),
        axis.title.x = element_text(vjust=0.1, color="black", size=14, face="bold"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        strip.text = element_text(size = 15))
c1

#nitrogen
colourCount = length(unique(nonzoo$Island.Name))
getPalette = colorRampPalette(brewer.pal(9, "Reds"))

n1<-ggplot(nonzoo, aes(x=reorder(Island.Name, d15N, FUN=median), 
                   y=d15N))+ 
  geom_boxplot(fill=getPalette(colourCount))+
  geom_point()+
  xlab("")+
  ylab(expression(delta^{15}*"N"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 12),
        axis.text.x = element_text(face="bold.italic", vjust=0.5, color="black", size=9),
        axis.text.y = element_text(vjust=0.5, color="black", size=12, face="bold"), 
        axis.title.y = element_text(vjust=1.0, color="black", size=14, face="bold"),
        axis.title.x = element_text(vjust=0.1, color="black", size=14, face="bold"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        strip.text = element_text(size = 15))
n1

#hydrogen
colourCount = length(unique(nonzoo$Island.Name))
getPalette = colorRampPalette(brewer.pal(9, "Greys"))

d1<-ggplot(nonzoo, aes(x=reorder(Island.Name, dD.bone, FUN=median), 
                   y=dD.bone))+ 
  geom_boxplot(fill=getPalette(colourCount))+
  geom_point()+
  xlab("")+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 12),
        axis.text.x = element_text(face="bold.italic", vjust=0.5, color="black", size=9),
        axis.text.y = element_text(vjust=0.5, color="black", size=12, face="bold"), 
        axis.title.y = element_text(vjust=1.0, color="black", size=14, face="bold"),
        axis.title.x = element_text(vjust=0.1, color="black", size=14, face="bold"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        strip.text = element_text(size = 15))
d1

#carbon-apatite
nonzoo$Island.Name <- factor(nonzoo$Island.Name, levels=c( 
                                                          "Pinzon", 
                                                          "Unk-Gold Rush",
                                                          "Isabela",
                                                          "Floreana",
                                                          "Santa Cruz", 
                                                          "Pinta",
                                                          "Rabida"))
c2<-ggplot(nonzoo %>% filter(!is.na(d13Cap)), aes(Island.Name, d13Cap, fill=Island.Name))+ 
  geom_boxplot()+
  geom_point()+
  xlab("")+
  ylab(expression(delta^{13}*"C"[apatite]))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 12),
        axis.text.x = element_text(face="bold.italic", vjust=0.5, color="black", size=9),
        axis.text.y = element_text(vjust=0.5, color="black", size=12, face="bold"), 
        axis.title.y = element_text(vjust=1.0, color="black", size=14, face="bold"),
        axis.title.x = element_text(vjust=0.1, color="black", size=14, face="bold"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        strip.text = element_text(size = 15))+
  scale_fill_brewer(palette="Oranges")
c2

#oxygen
nonzoo$Island.Name <- factor(nonzoo$Island.Name, levels=c(
                                            "Rabida",
                                            "Pinzon",
                                            "Isabela",
                                            "Pinta",
                                            "Santa Cruz", 
                                            "Floreana",
                                            "Unk-Gold Rush"))
o1<-ggplot(nonzoo %>% filter(!is.na(d18O)), aes(Island.Name, d18O, fill=Island.Name))+ 
  geom_boxplot()+
  geom_point()+
  xlab("")+
  ylab(expression(delta^{18}*"O"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 12),
        axis.text.x = element_text(face="bold.italic", vjust=0.5, color="black", size=9),
        axis.text.y = element_text(vjust=0.5, color="black", size=12, face="bold"), 
        axis.title.y = element_text(vjust=1.0, color="black", size=14, face="bold"),
        axis.title.x = element_text(vjust=0.1, color="black", size=14, face="bold"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        strip.text = element_text(size = 15))+
  scale_fill_brewer(palette="Blues")
o1

#save together as single plot
ggarrange(c1, n1, d1, c2, o1, 
          ncol = 1, nrow = 5)
  ggsave("BoxAll_FINAL.pdf", width = 12, height = 12, units = "in")

#Note for readers: When using ggarrange, the plots do not align on the y-axis.
  #We saved this image, opened it within Inkscape (open access software) and 
    #re-aligned the y-axes from all plots for the final figure.
  
#Summary metrics
summary(nonzoo$d13Ccol)
summary(nonzoo$d15N)
summary(nonzoo$dD.bone)
summary(nonzoo$d13Cap)
summary(nonzoo$d18O)


##### Figure 3: Isotope systems by elevation (and S1-S3) ####
#carbon-collagen
colplot<-ggplot(ccol, aes(x=Elevation, y=Mean))+
  geom_point()+
  geom_label_repel(label=ccol$Island, size=4, 
                   box.padding =1, max.overlaps = Inf)+
  ylim(-22,-13)+
  xlab("Elevation")+
  ylab(expression(delta^{13}*"C"[collagen]))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 12),
        axis.text.x = element_text(face="bold", vjust=0.5, color="black", size=9),
        axis.text.y = element_text(vjust=0.5, color="black", size=12, face="bold"), 
        axis.title.y = element_text(vjust=1.0, color="black", size=14, face="bold"),
        axis.title.x = element_text(vjust=0.1, color="black", size=14, face="bold"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "",
        strip.text = element_text(size = 15))
colplot

#nitrogen
nitplot<-ggplot(nit, aes(x=Elevation, y=Mean))+
  geom_point()+
  geom_label_repel(label=nit$Island, size=4, 
                   box.padding =1, max.overlaps = Inf)+
  #ylim(-22,-13)+
  xlab("Elevation")+
  ylab(expression(delta^{15}*"N"[]))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 12),
        axis.text.x = element_text(face="bold", vjust=0.5, color="black", size=9),
        axis.text.y = element_text(vjust=0.5, color="black", size=12, face="bold"), 
        axis.title.y = element_text(vjust=1.0, color="black", size=14, face="bold"),
        axis.title.x = element_text(vjust=0.1, color="black", size=14, face="bold"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "",
        strip.text = element_text(size = 15))
nitplot

#save Figure 3 (carbon and nitrogen plot)
ggarrange(colplot, nitplot,
          ncol = 2, nrow = 1)
ggsave("EleCN_FINAL.tiff", width = 12, height = 6, units = "in")

#hydrogen
ggplot(hy, aes(x=Elevation, y=Mean))+
  geom_point()+
  geom_label_repel(label=hy$Island, size=4, 
                   box.padding =1, max.overlaps = Inf)+
  #ylim(-22,-13)+
  xlab("Elevation")+
  ylab(expression(delta^{}*"D"[]))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 12),
        axis.text.x = element_text(face="bold", vjust=0.5, color="black", size=9),
        axis.text.y = element_text(vjust=0.5, color="black", size=12, face="bold"), 
        axis.title.y = element_text(vjust=1.0, color="black", size=14, face="bold"),
        axis.title.x = element_text(vjust=0.1, color="black", size=14, face="bold"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "",
        strip.text = element_text(size = 15))

#save Supplemental Figure 1
ggsave("EledD_FINAL.tiff", width = 10, height = 6, units = "in")

#carbon-apatite
ggplot(cap, aes(x=Elevation, y=Mean))+
  geom_point()+
  geom_label_repel(label=cap$Island, size=4, 
                   box.padding =1, max.overlaps = Inf)+
  #ylim(-22,-13)+
  xlab("Elevation")+
  ylab(expression(delta^{13}*"C"[apatite]))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 12),
        axis.text.x = element_text(face="bold", vjust=0.5, color="black", size=9),
        axis.text.y = element_text(vjust=0.5, color="black", size=12, face="bold"), 
        axis.title.y = element_text(vjust=1.0, color="black", size=14, face="bold"),
        axis.title.x = element_text(vjust=0.1, color="black", size=14, face="bold"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "",
        strip.text = element_text(size = 15))

#save Supplemental Figure 2
ggsave("Eled13cap_FINAL.tiff", width = 10, height = 6, units = "in")

#oxygen
ggplot(ox, aes(x=Elevation, y=Mean))+
  geom_point()+
  geom_label_repel(label=ox$Island, size=4, 
                   box.padding =1, max.overlaps = Inf)+
  #ylim(-22,-13)+
  xlab("Elevation")+
  ylab(expression(delta^{18}*"O"[]))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 12),
        axis.text.x = element_text(face="bold", vjust=0.5, color="black", size=9),
        axis.text.y = element_text(vjust=0.5, color="black", size=12, face="bold"), 
        axis.title.y = element_text(vjust=1.0, color="black", size=14, face="bold"),
        axis.title.x = element_text(vjust=0.1, color="black", size=14, face="bold"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "",
        strip.text = element_text(size = 15))

#save Supplemental Figure 3
ggsave("Eled18O_FINAL.tiff", width = 10, height = 6, units = "in")


##### Figure 4: Examining broad comparisons in isotopes and statistics (and S4-S8) #####
#all samples d13Ccollagen and d15n
lm_eqn <- function(df){
  m <- lm(d13Ccol ~ d15N, nonzoo);
  eq <- substitute(italic(d13Ccol) == a + b %.% italic(d15N)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

cn1<-ggplot(nonzoo, aes(d13Ccol, d15N))+
  geom_point(size=4)+
  geom_smooth(method="lm")+
  #ylim(5,20)+
  #xlim(-25,-10)+
  #facet_wrap(~Taxon)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{15}*"N"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
cn1
lm<-lm(nonzoo$d13Ccol~nonzoo$d15N)
summary(lm)

#Supplemental Figure 4 (by island d13Ccollagen and d15n)
ggplot(nonzoo, aes(d13Ccol, d15N))+
  geom_point(size=2)+
  #ylim(5,20)+
  #xlim(-25,-10)+
  facet_wrap(~Island.Name)+
  #theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{15}*"N"))+
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=10),
        axis.text.y = element_text(vjust=0.5, color="black", size=10), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))

#all samples d13C and dD
lm_eqn <- function(df){
  m <- lm(d13Ccol ~ dD.bone, nonzoo);
  eq <- substitute(italic(d13Ccol) == a + b %.% italic(dD.bone)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}
ch1<-ggplot(nonzoo, aes(d13Ccol, dD.bone))+
  geom_point(size=4)+
  geom_smooth(method="lm")+
  #ylim(5,20)+
  #xlim(-25,-10)+
  #facet_wrap(~Taxon)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
ch1
lm<-lm(nonzoo$d13Ccol~nonzoo$dD.bone)
summary(lm)

#Supplemental Figure 5 (by island d13Ccollagen and dD)
ggplot(nonzoo, aes(d13Ccol, dD.bone))+
  geom_point(size=2)+
  #ylim(5,20)+
  #xlim(-25,-10)+
  facet_wrap(~Island.Name)+
  #theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=10),
        axis.text.y = element_text(vjust=0.5, color="black", size=10), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))

#all samples dD and d15N
lm_eqn <- function(df){
  m <- lm(dD.bone ~ d15N, nonzoo);
  eq <- substitute(italic(dD.bone) == a + b %.% italic(d15N)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

hn1<-ggplot(nonzoo, aes(dD.bone, d15N))+
  geom_point(size=4)+
  geom_smooth(method="lm")+
  #ylim(5,20)+
  #xlim(-25,-10)+
  #facet_wrap(~Taxon)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{}*"D"))+
  ylab(expression(delta^{15}*"N"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
hn1
lm<-lm(nonzoo$dD.bone~nonzoo$d15N)
summary(lm)

#Supplemental Figure 6 (by island dD and d15N)
ggplot(nonzoo, aes(dD.bone, d15N))+
  geom_point(size=2)+
  #ylim(5,20)+
  #xlim(-25,-10)+
  facet_wrap(~Island.Name)+
  #theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{}*"D"))+
  ylab(expression(delta^{15}*"N"))+
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=10),
        axis.text.y = element_text(vjust=0.5, color="black", size=10), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))

#all samples d13Capatite and d18O
lm_eqn <- function(df){
  m <- lm(d13Cap ~ d18O, nonzoo);
  eq <- substitute(italic(d13Cap) == a + b %.% italic(d18O)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

co1<-ggplot(nonzoo, aes(d13Cap, d18O))+
  geom_point(size=4)+
  geom_smooth(method="lm")+
  #ylim(5,20)+
  #xlim(-25,-10)+
  #facet_wrap(~Taxon)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[apatite]))+
  ylab(expression(delta^{18}*"O"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
co1
lm<-lm(nonzoo$d13Cap~nonzoo$d18O)
summary(lm)

#Supplemental Figure 7 (by island d13Capatite and d18O)
ggplot(nonzoo, aes(d13Cap, d18O))+
  geom_point(size=2)+
  #ylim(5,20)+
  #xlim(-25,-10)+
  facet_wrap(~Island.Name)+
  #theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[apatite]))+
  ylab(expression(delta^{18}*"O"))+
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=9),
        axis.text.y = element_text(vjust=0.5, color="black", size=10), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))

#all samples d18O and dD
lm_eqn <- function(df){
  m <- lm(dD.bone ~ d18O, nonzoo);
  eq <- substitute(italic(dD.bone) == a + b %.% italic(d18O)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

ho1<-ggplot(nonzoo, aes(dD.bone, d18O))+
  geom_point(size=4)+
  geom_smooth(method="lm")+
  #ylim(5,20)+
  #xlim(-25,-10)+
  #facet_wrap(~Taxon)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{}*"D"))+
  ylab(expression(delta^{18}*"O"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
ho1
lm<-lm(nonzoo$dD.bone~nonzoo$d18O)
summary(lm)

#Supplemental Figure 8 (by island dD and d18O)
ggplot(nonzoo, aes(dD.bone, d18O))+
  geom_point(size=2)+
  #ylim(5,20)+
  #xlim(-25,-10)+
  facet_wrap(~Island.Name)+
  #theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{}*"D"))+
  ylab(expression(delta^{18}*"O"))+
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=10),
        axis.text.y = element_text(vjust=0.5, color="black", size=10), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))

#save Figure 4 (all plots together)
arrange<-grid.arrange(cn1, ch1, hn1, co1, ho1, ncolumn=3)
ggsave("Allcomparisons.tiff", arrange, width = 8.5, height = 11, units = "in")

##### Figure 5: Collagen-Apatite spacing and discrimination value#####
p1<-ggplot(master, aes(d13Ccol,d13Cap))+
  geom_point(size=3.5)+
  #geom_smooth(method="lm")+
  geom_abline(intercept = 10.6, slope = 1.71, lty = 2) +
  geom_abline(intercept = 21.4, slope = 1.74) +
  annotate("text", x=-20, y=-2,  label = "C3 Protein", size=5)+
  annotate("text", x=-10, y=-11, label = "C4 Protein", size=5)+
  #geom_text(x=-20, y=-2, label="C3 Protein")+
  #geom_text(x=-10, y=-11, label="C4 Protein")+
  xlim(-25, -5)+
  ylim(-15,0)+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{13}*"C"[apatite]))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15, face="bold"),
        axis.text.y = element_text(vjust=0.5, color="black", size=15, face="bold"), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15, face="bold"),
        axis.title.x = element_text(vjust=0.1, color="black", size=15, face="bold"))
p1
#bone collagen to apatite d13C
lm_eqn <- function(df){
  m <- lm(d13Ccol ~ d13Cap, tort);
  eq <- substitute(italic(d13Ccol) == a + b %.% italic(d13Cap)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}
p2<-ggplot(master, aes(d13Ccol,d13Cap))+
  geom_point(size=3.5)+
  geom_smooth(method="lm")+
  xlim(-23.5, -12)+
  ylim(-13,-1)+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{13}*"C"[apatite]))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15, face="bold"),
        axis.text.y = element_text(vjust=0.5, color="black", size=15, face="bold"), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15, face="bold"),
        axis.title.x = element_text(vjust=0.1, color="black", size=15, face="bold"))
p2
lm<-lm(master$d13Ccol~master$d13Cap)
summary(lm)

#save Figure 5
g<-grid.arrange(p2, p1, nrow=1)
ggsave("Col-Ap.tiff", width = 11.5, height = 6, units = "in")

#obtain discrimination value
f<-(master$d13Ccol-master$d13Cap)
f
p<-na.omit(f)
p
m<-mean(p)
m

##### Figures S9-S11: Plant analysis and SIAR results#####
ggplot(plants, aes(x=d13C, y=d15N, color=Island))+
  geom_point(size=2.5, alpha=.75)+
  facet_wrap(~Taxon, ncol=4)+
  xlab(expression(delta^{13}*"C"[]))+
  ylab(expression(delta^{15}*"N"[]))+
  theme(
    panel.background = element_rect(fill = NA, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    axis.text.x = element_text(face="bold", vjust=0.5, color="black", size=9),
    axis.text.y = element_text(vjust=0.5, color="black", size=12, face="bold"), 
    axis.title.y = element_text(vjust=1.0, color="black", size=14, face="bold"),
    axis.title.x = element_text(vjust=0.1, color="black", size=14, face="bold"),
    strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"))+
scale_color_manual(values = c("Espanola" = "black",
                              "Pinta" = "orange",
                              "Santa Fe" = "blue"))
#save Supplemental Figure 9
ggsave("Plants_FINAL.tiff", width = 10, height = 6, units = "in")

#SIAR analysis
data<-read.table('ConsumerData.txt',header=TRUE)
sources<-read.table('SourceData.txt',header=TRUE)                        
tef<-read.table('TEFData.txt',header=TRUE)   
data
sources
tef

model1<-siarmcmcdirichletv4(data,sources,tef,concdep=0,500000,50000)
siarplotdata(model1)     
#when selecting proportions by source, SIAR allows you to enter "1" or "2" for "C3" or "C4" based on our dataset
  #C3 is saved as Supplemental Figure 10
    #C4 is saved as Supplemental Figure 11
siarproportionbysourceplot(model1)

#it is possible to extra proportion values from the SIAR output
  #here you must select the # corresponding to the group column in the output data
    #the example shown is for column "1" which is the proportion of C3 plants for Espanola tortoises (C. n. hoodensis)
      #See Table 3 for these results
out<-model1$output   
out
mean(out[,1])


##### Figures S12-S14: Diet-tissue fractionation #####
#carbon
ggplot(carb, aes(Tissue, Value, group=Specimen))+
  geom_line(linetype = "dashed")+
  geom_point(size=4, alpha=.5)+
  geom_label_repel(aes(label = Label),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  xlab("Tissue")+
  ylab(expression(delta^{13}*"C"[]))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 12),
        axis.text.x = element_text(face="bold", vjust=0.5, color="black", size=9),
        axis.text.y = element_text(vjust=0.5, color="black", size=12, face="bold"), 
        axis.title.y = element_text(vjust=1.0, color="black", size=14, face="bold"),
        axis.title.x = element_text(vjust=0.1, color="black", size=14, face="bold"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        strip.text = element_text(size = 15))

#save Supplemental Figure 12  
ggsave("Dis_d13C_FINAL.tiff", width = 11, height = 6, units = "in")

#nitrogen
ggplot(nit, aes(Tissue, Value, group=Specimen))+
  geom_line(linetype = "dashed")+
  geom_point(size=4, alpha=.5)+
  geom_label_repel(aes(label = Label),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  xlab("Tissue")+
  ylab(expression(delta^{15}*"N"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 12),
        axis.text.x = element_text(face="bold", vjust=0.5, color="black", size=9),
        axis.text.y = element_text(vjust=0.5, color="black", size=12, face="bold"), 
        axis.title.y = element_text(vjust=1.0, color="black", size=14, face="bold"),
        axis.title.x = element_text(vjust=0.1, color="black", size=14, face="bold"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        strip.text = element_text(size = 15))

#save Supplemental Figure 13  
ggsave("Dis_d15N_FINAL.tiff", width = 11, height = 6, units = "in")

#hydrogen
ggplot(hyd, aes(Tissue, Value, group=Specimen))+
  geom_line(linetype = "dashed")+
  geom_point(size=4, alpha=.5)+
  geom_label_repel(aes(label = Label),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  xlab("Tissue")+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 12),
        axis.text.x = element_text(face="bold", vjust=0.5, color="black", size=9),
        axis.text.y = element_text(vjust=0.5, color="black", size=12, face="bold"), 
        axis.title.y = element_text(vjust=1.0, color="black", size=14, face="bold"),
        axis.title.x = element_text(vjust=0.1, color="black", size=14, face="bold"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        strip.text = element_text(size = 15))

#save Supplemental Figure 14  
ggsave("Dis_dD_FINAL.tiff", width = 11, height = 6, units = "in")

##### Figure 6: Sex based differences (and S15-18) #####
#carbon-collagen
ggplot(sex_rev, aes(Sex, d13Ccol))+ 
  geom_boxplot()+
  geom_point()+
  facet_wrap(~Taxon)+
  xlab("Sex")+
  ylab(expression(delta^{13}*"C"[collagen]))+
  theme(
    panel.background = element_rect(fill = NA, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    axis.text.x = element_text(face="bold", vjust=0.5, color="black", size=9),
    axis.text.y = element_text(vjust=0.5, color="black", size=12, face="bold"), 
    axis.title.y = element_text(vjust=1.0, color="black", size=14, face="bold"),
    axis.title.x = element_text(vjust=0.1, color="black", size=14, face="bold"),
    strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"))

#save Figure 6
ggsave("Sex_d13Ccol_FINAL.tiff", width = 7, height = 5, units = "in")

#nitrogen
ggplot(sex_rev, aes(Sex, d15N))+ 
  geom_boxplot()+
  geom_point()+
  facet_wrap(~Taxon)+
  xlab("Sex")+
  ylab(expression(delta^{15}*"N"))+
  theme(
    panel.background = element_rect(fill = NA, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    axis.text.x = element_text(face="bold", vjust=0.5, color="black", size=9),
    axis.text.y = element_text(vjust=0.5, color="black", size=12, face="bold"), 
    axis.title.y = element_text(vjust=1.0, color="black", size=14, face="bold"),
    axis.title.x = element_text(vjust=0.1, color="black", size=14, face="bold"),
    strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"))

#save Supplemental Figure 15
ggsave("Sex_d15N_FINAL.tiff", width = 7, height = 5, units = "in")

#hydrogen
ggplot(sex_rev, aes(Sex, dD.bone))+ 
  geom_boxplot()+
  geom_point()+
  facet_wrap(~Taxon)+
  xlab("Sex")+
  ylab(expression(delta^{}*"D"))+
  theme(
    panel.background = element_rect(fill = NA, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    axis.text.x = element_text(face="bold", vjust=0.5, color="black", size=9),
    axis.text.y = element_text(vjust=0.5, color="black", size=12, face="bold"), 
    axis.title.y = element_text(vjust=1.0, color="black", size=14, face="bold"),
    axis.title.x = element_text(vjust=0.1, color="black", size=14, face="bold"),
    strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"))

#save Supplemental Figure 16
ggsave("Sex_dD_FINAL.tiff", width = 7, height = 5, units = "in")

#carbon-apatite
ggplot(sex_rev2, aes(Sex, d13Cap))+ 
  geom_boxplot()+
  geom_point()+
  facet_wrap(~Taxon)+
  xlab("Sex")+
  ylab(expression(delta^{13}*"C"[apatite]))+
  theme(
    panel.background = element_rect(fill = NA, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    axis.text.x = element_text(face="bold", vjust=0.5, color="black", size=9),
    axis.text.y = element_text(vjust=0.5, color="black", size=12, face="bold"), 
    axis.title.y = element_text(vjust=1.0, color="black", size=14, face="bold"),
    axis.title.x = element_text(vjust=0.1, color="black", size=14, face="bold"),
    strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"))

#save Supplemental Figure 17
ggsave("Sex_d13Cap_FINAL.tiff", width = 7, height = 5, units = "in")

#oxygen
ggplot(sex_rev2, aes(Sex, d18O))+ 
  geom_boxplot()+
  geom_point()+
  facet_wrap(~Taxon)+
  xlab("Sex")+
  ylab(expression(delta^{18}*"O"))+
  theme(
    panel.background = element_rect(fill = NA, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    axis.text.x = element_text(face="bold", vjust=0.5, color="black", size=9),
    axis.text.y = element_text(vjust=0.5, color="black", size=12, face="bold"), 
    axis.title.y = element_text(vjust=1.0, color="black", size=14, face="bold"),
    axis.title.x = element_text(vjust=0.1, color="black", size=14, face="bold"),
    strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"))

#save Supplemental Figure 18
ggsave("Sex_d18O_FINAL.tiff", width = 7, height = 5, units = "in")

##### Figure 7: Island specific changes, excluding captive samples (and S19-S27) #####
#Floreana (d13Ccol, d15N, dD, d13Cap, d18O)
nig
ncn<-ggplot(nig, aes(d13Ccol, d15N))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(5,20)+
  xlim(-25,-10)+
  geom_vline(xintercept=-13.9, linetype = "dashed", colour="red")+
  annotate("text", x =-14.75, y = 17, label = "310?80", size=4)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{15}*"N"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
ncn
nch<-ggplot(nig, aes(d13Ccol, dD.bone))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-20,-80)+
  xlim(-25,-10)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
nch
nnh<-ggplot(nig, aes(d15N, dD.bone))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-20,-80)+
  xlim(5,20)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{15}*"N"[]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
nnh
nco<-ggplot(nig, aes(d13Cap, d18O))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-10,0)+
  xlim(-13,-1)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[apatite]))+
  ylab(expression(delta^{18}*"O"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
nco
nho<-ggplot(nig, aes(dD.bone, d18O))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-10,0)+
  xlim(-20,-80)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{}*"D"[]))+
  ylab(expression(delta^{18}*"O"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
nho

#save Figure 7
narrange<-grid.arrange(ncn, nch, nnh, nco, nho)
ggsave("Time_Floreana.tiff", narrange, width = 8.5, height = 11, units = "in")

#Espanola (d13Ccol, d15N, and dD only)
hood
ecn<-ggplot(hood, aes(d13Ccol, d15N))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(5,20)+
  xlim(-25,-10)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{15}*"N"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
ecn
ech<-ggplot(hood, aes(d13Ccol, dD.bone))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-20,-80)+
  xlim(-25,-10)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
ech
enh<-ggplot(hood, aes(d15N, dD.bone))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-20,-80)+
  xlim(5,20)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{15}*"N"[]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
enh

#save Supplemental Figure 19
earrange<-grid.arrange(ecn, ech, enh)
ggsave("Time_Espanola.tiff", earrange, width = 8.5, height = 11, units = "in")

#Rabida (d13Ccol, d15N, dD, d13Cap, d18O)
rabida
rcn<-ggplot(rabida, aes(d13Ccol, d15N))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(5,20)+
  xlim(-25,-10)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{15}*"N"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
rcn
rch<-ggplot(rabida, aes(d13Ccol, dD.bone))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-20,-80)+
  xlim(-25,-10)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
rch
rnh<-ggplot(rabida, aes(d15N, dD.bone))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-20,-80)+
  xlim(5,20)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{15}*"N"[]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
rnh
rco<-ggplot(rabida, aes(d13Cap, d18O))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-10,0)+
  xlim(-13,-1)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[apatite]))+
  ylab(expression(delta^{18}*"O"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
rco
rho<-ggplot(rabida, aes(dD.bone, d18O))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-10,0)+
  xlim(-20,-80)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{}*"D"[]))+
  ylab(expression(delta^{18}*"O"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
rho

#save Supplemental Figure 20
rarrange<-grid.arrange(rcn, rch, rnh, rco, rho)
ggsave("Time_Rabida.tiff", rarrange, width = 8.5, height = 11, units = "in")

#San Cristobal (d13Ccol, d15N, dD only)
chath
ccn<-ggplot(chath, aes(d13Ccol, d15N))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(5,20)+
  xlim(-25,-10)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{15}*"N"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
ccn
cch<-ggplot(chath, aes(d13Ccol, dD.bone))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-20,-80)+
  xlim(-25,-10)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
cch
cnh<-ggplot(chath, aes(d15N, dD.bone))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-20,-80)+
  xlim(5,20)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{15}*"N"[]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
cnh

#save Supplemental Figure 21
carrange<-grid.arrange(ccn, cch, cnh)
ggsave("Time_SanCristobal_FINAL.tiff", carrange, width = 8.5, height = 11, units = "in")

#Pinzon (d13Ccol, d15N, dD, d13Cap, d18O)
dunc
dcn<-ggplot(dunc, aes(d13Ccol, d15N))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(5,20)+
  xlim(-25,-10)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{15}*"N"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
dcn
dch<-ggplot(dunc, aes(d13Ccol, dD.bone))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-20,-80)+
  xlim(-25,-10)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
dch
dnh<-ggplot(dunc, aes(d15N, dD.bone))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-20,-80)+
  xlim(5,20)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{15}*"N"[]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
dnh
dco<-ggplot(dunc, aes(d13Cap, d18O))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-10,0)+
  xlim(-13,-1)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[apatite]))+
  ylab(expression(delta^{18}*"O"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
dco
dho<-ggplot(dunc, aes(dD.bone, d18O))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-10,0)+
  xlim(-20,-80)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{}*"D"[]))+
  ylab(expression(delta^{18}*"O"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
dho

#save Supplemental Figure 22
darrange<-grid.arrange(dcn, dch, dnh, dco, dho)
ggsave("Time_Pinzon.tiff", darrange, width = 8.5, height = 11, units = "in")

#Santiago (d13Ccol, d15N, dD)
darw
dacn<-ggplot(darw, aes(d13Ccol, d15N))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(5,20)+
  xlim(-25,-10)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{15}*"N"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
dacn
dach<-ggplot(darw, aes(d13Ccol, dD.bone))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-20,-80)+
  xlim(-25,-10)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
dach
danh<-ggplot(darw, aes(d15N, dD.bone))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-20,-80)+
  xlim(5,20)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{15}*"N"[]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
danh

#save Supplemental Figure 23
daarrange<-grid.arrange(dacn, dach, danh)
ggsave("Time_Santiago.tiff", daarrange, width = 8.5, height = 11, units = "in")

#Isabela guentheri (d13Ccol, d15N, dD, d13Cap, d18O)
guen
gcn<-ggplot(guen, aes(d13Ccol, d15N))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(5,20)+
  xlim(-25,-10)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{15}*"N"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
gcn
gch<-ggplot(guen, aes(d13Ccol, dD.bone))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-20,-80)+
  xlim(-25,-10)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
gch
gnh<-ggplot(guen, aes(d15N, dD.bone))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-20,-80)+
  xlim(5,20)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{15}*"N"[]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
gnh
gco<-ggplot(guen, aes(d13Cap, d18O))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-10,0)+
  xlim(-13,-1)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[apatite]))+
  ylab(expression(delta^{18}*"O"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
gco
gho<-ggplot(guen, aes(dD.bone, d18O))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-10,0)+
  xlim(-20,-80)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{}*"D"[]))+
  ylab(expression(delta^{18}*"O"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
gho

#save Supplemental Figure 24
garrange<-grid.arrange(gcn, gch, gnh, gco, gho)
ggsave("Time_Isabela_guentheri.tiff", garrange, width = 8.5, height = 11, units = "in")

#Isabela microphyes (d13Ccol, d15N, dD, d13Cap, d18O)
micro
mcn<-ggplot(micro, aes(d13Ccol, d15N))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(5,20)+
  xlim(-25,-10)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{15}*"N"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
mcn
mch<-ggplot(micro, aes(d13Ccol, dD.bone))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-20,-80)+
  xlim(-25,-10)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
mch
mnh<-ggplot(micro, aes(d15N, dD.bone))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-20,-80)+
  xlim(5,20)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{15}*"N"[]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
mnh
mco<-ggplot(micro, aes(d13Cap, d18O))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-10,0)+
  xlim(-13,-1)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[apatite]))+
  ylab(expression(delta^{18}*"O"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
mco
mho<-ggplot(micro, aes(dD.bone, d18O))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-10,0)+
  xlim(-20,-80)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{}*"D"[]))+
  ylab(expression(delta^{18}*"O"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
mho

#save Supplemental Figure 25
marrange<-grid.arrange(mcn, mch, mnh, mco, mho)
ggsave("Time_Isabela_microphyes.tiff", marrange, width = 8.5, height = 11, units = "in")

#Santa Cruz (d13Ccol, d15N, dD, and only one d13Cap, d18O)
porteri
pcn<-ggplot(porteri, aes(d13Ccol, d15N))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(5,20)+
  xlim(-25,-10)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{15}*"N"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
pcn
pch<-ggplot(porteri, aes(d13Ccol, dD.bone))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-20,-80)+
  xlim(-25,-10)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
pch
pnh<-ggplot(porteri, aes(d15N, dD.bone))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-20,-80)+
  xlim(5,20)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{15}*"N"[]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
pnh
pco<-ggplot(porteri, aes(d13Cap, d18O))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-10,0)+
  xlim(-13,-1)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[apatite]))+
  ylab(expression(delta^{18}*"O"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
pco
pho<-ggplot(porteri, aes(dD.bone, d18O))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-10,0)+
  xlim(-20,-80)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{}*"D"[]))+
  ylab(expression(delta^{18}*"O"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
pho

#save Supplemental Figure 26
parrange<-grid.arrange(pcn, pch, pnh, pco, pho)
ggsave("Time_SantaCruz.tiff", parrange, width = 8.5, height = 11, units = "in")

#Pinta (d13Ccol, d15N, dD, and d13Cap, d18O)
abing
acn<-ggplot(abing, aes(d13Ccol, d15N))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(5,20)+
  xlim(-25,-10)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{15}*"N"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
acn
ach<-ggplot(abing, aes(d13Ccol, dD.bone))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-20,-80)+
  xlim(-25,-10)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[collagen]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
ach
anh<-ggplot(abing, aes(d15N, dD.bone))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-20,-80)+
  xlim(5,20)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{15}*"N"[]))+
  ylab(expression(delta^{}*"D"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
anh
aco<-ggplot(abing, aes(d13Cap, d18O))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-10,0)+
  xlim(-13,-1)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{13}*"C"[apatite]))+
  ylab(expression(delta^{18}*"O"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
aco
aho<-ggplot(abing, aes(dD.bone, d18O))+
  geom_point(size=4)+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ylim(-10,0)+
  xlim(-20,-80)+
  theme_classic()+
  ggtitle("")+
  xlab(expression(delta^{}*"D"[]))+
  ylab(expression(delta^{18}*"O"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=15),
        axis.text.y = element_text(vjust=0.5, color="black", size=15), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))
aho

#save Supplemental Figure 27
aarrange<-grid.arrange(acn, ach, anh, aco, aho)
ggsave("Time_Pinta.tiff", aarrange, width = 8.5, height = 11, units = "in")

##### Figures S28-S30: Wild to captive dietary comparisons ####
captive_mod <- captive %>%
  # Rename 4 to 4wd, f to Front, r to Rear
  mutate(Isotope = recode(Isotope, "d13Cap" = "delta^{13}*C[apatite]", 
                          "d18O" = "delta^{18}*O[apatite]", 
                          "d13Ccol" = "delta^{13}*C[collagen]",
                          "d15N" = "delta^{15}*N",
                          "dD" = "delta^{}*D"))
captive_mod

isabela<-captive_mod[c(21:60),]
pinzon<-captive_mod[c(61:104),]
santa<-captive_mod[c(105:136),]

#Isabela Island
ggplot(isabela, aes(x=Captive, y=Value))+ 
  geom_boxplot()+
  geom_point()+
  facet_wrap(~Isotope, labeller = label_parsed)+
  xlab("")+
  ylab("")+
  ggtitle("Isabela Island")+
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=10),
        axis.text.y = element_text(vjust=0.5, color="black", size=10), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))

#save Supplemental Figure 28
ggsave("Captive_Isabela.tiff", width = 11.5, height = 6, units = "in")

#Pinzon Island
ggplot(pinzon, aes(x=Captive, y=Value))+ 
  geom_boxplot()+
  geom_point()+
  facet_wrap(~Isotope, labeller = label_parsed)+
  xlab("")+
  ylab("")+
  ggtitle("Pinz?n Island")+
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=10),
        axis.text.y = element_text(vjust=0.5, color="black", size=10), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))

#save Supplemental Figure 29
ggsave("Captive_Pinzon.tiff", width = 11.5, height = 6, units = "in")

#Santa Cruz Island
ggplot(santa, aes(x=Captive, y=Value))+ 
  geom_boxplot()+
  geom_point()+
  facet_wrap(~Isotope, labeller = label_parsed)+
  xlab("")+
  ylab("")+
  ggtitle("Santa Cruz Island")+
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text.x = element_text(colour = "black", size = 15),
        axis.text.x = element_text(vjust=0.5, color="black", size=10),
        axis.text.y = element_text(vjust=0.5, color="black", size=10), 
        axis.title.y = element_text(vjust=1.0, color="black", size=15),
        axis.title.x = element_text(vjust=0.1, color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))

#save Supplemental Figure 30
ggsave("Captive_SantaCruz.tiff", width = 11.5, height = 6, units = "in")