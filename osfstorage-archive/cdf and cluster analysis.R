rm(list=ls())
library(rgdal)
library(ggplot2)
library(reshape)
library(doBy)
library(plyr)
library(dplyr)
library(sm)
library(data.table)
library(Hmisc)
library(xtable)
library(mclust)
library(latticeExtra)

#first set your local directory to the location of the R and data files with setwd()
setwd("")
source("function_definitions.R", local=TRUE)

input_fc <- 'RGGI_and_EJ_analysis.gdb'
ct.sp             <- rgdal::readOGR(dsn=input_fc,   layer = "ACS_2018_5YR_TRACT")
ct.race.ethnicity <- sf::st_read(   dsn = input_fc, layer = "X03_HISPANIC_OR_LATINO_ORIGIN")
ct.poverty        <- sf::st_read(   dsn = input_fc, layer = "X17_POVERTY")

ct.sp2            <- sp::merge(ct.sp,  ct.race.ethnicity  , by.x="GEOID_Data", by.y="GEOID")
ct.sp3            <- sp::merge(ct.sp2, ct.poverty         , by.x="GEOID_Data", by.y="GEOID")
ct.sp4            <- sp::merge(ct.sp3, states.lookup.table, by.x="STATEFP"   , by.y="fips_text")
ct.rggi.sp        <- subset(ct.sp4,(abb =='CT' | 
                                    abb =='DE' |
                                    abb =='MA' |
                                    abb =='MD' |
                                    abb =='NJ' |
                                    abb =='NY' |
                                    abb =='ME' |
                                    abb =='NH' |
                                    abb =='RI' |
                                    abb =='VT' ))

#1. CREATE NEAR DISTANCE COLUMN
#rggi.plants.sp are EGUs but need facilities so as not to overcount population
rggi.fencelines.sp <- rggi.plants.sp[!duplicated(rggi.plants.sp$ORSPL),]
nrow(rggi.fencelines.sp)
nrow(rggi.plants.sp)

near_distance <- near.distance(ct.rggi.sp,rggi.fencelines.sp)
#ct.rggi.sp2 <- sp::merge(ct.sp2, near_distance[,c("GEOID_Data","near_dist")], by.x="GEOID_Data", by.y="GEOID_Data")
ct.rggi.near.sp <- sp::merge(ct.rggi.sp, near_distance, by.x="GEOID_Data", by.y="GEOID_Data")

#2. 
#Rename population columns
colnames(ct.rggi.near.sp@data)[which(colnames(ct.rggi.near.sp@data)=="B03002e1")]  <- "TOTAL_POPULATION"
colnames(ct.rggi.near.sp@data)[which(colnames(ct.rggi.near.sp@data)=="B03002e12")] <- "HISPANIC_TOTAL_POP"
colnames(ct.rggi.near.sp@data)[which(colnames(ct.rggi.near.sp@data)=="B03002e3")]  <- "WHITE_NH_TOTAL_POP"
colnames(ct.rggi.near.sp@data)[which(colnames(ct.rggi.near.sp@data)=="B03002e4")]  <- "BLACK_NH_TOTAL_POP"
colnames(ct.rggi.near.sp@data)[which(colnames(ct.rggi.near.sp@data)=="B03002e5")]  <- "NATIVE_NH_TOTAL_POP"
colnames(ct.rggi.near.sp@data)[which(colnames(ct.rggi.near.sp@data)=="B03002e6")]  <- "ASIAN_NH_TOTAL_POP"
colnames(ct.rggi.near.sp@data)[which(colnames(ct.rggi.near.sp@data)=="B03002e7")]  <- "HIPI_NH_TOTAL_POP"
colnames(ct.rggi.near.sp@data)[which(colnames(ct.rggi.near.sp@data)=="B03002e8")]  <- "OTHER_NH_TOTAL_POP"
colnames(ct.rggi.near.sp@data)[which(colnames(ct.rggi.near.sp@data)=="B03002e9")]  <- "TWO_OR_MORE_NH_TOTAL_POP"

#generate variables
ct.rggi.near.sp$POP_IN_POVERTY     <- ct.rggi.near.sp$C17002e2 + ct.rggi.near.sp$C17002e3
ct.rggi.near.sp$POP_NOT_IN_POVERTY <- ct.rggi.near.sp$C17002e1 -ct.rggi.near.sp$POP_IN_POVERTY     
ct.rggi.near.sp$PER_POVERTY        <- round(ct.rggi.near.sp$POP_IN_POVERTY / (ct.rggi.near.sp$POP_IN_POVERTY + ct.rggi.near.sp$POP_NOT_IN_POVERTY) * 100,2)

#generate WHITE and NOT_WHITE population variables
ct.rggi.near.sp$NOT_WHITE_TOTAL_POP <-   ct.rggi.near.sp@data$HISPANIC_TOTAL_POP + 
  ct.rggi.near.sp@data$BLACK_NH_TOTAL_POP + 
  ct.rggi.near.sp@data$NATIVE_NH_TOTAL_POP + 
  ct.rggi.near.sp@data$ASIAN_NH_TOTAL_POP +
  ct.rggi.near.sp@data$HIPI_NH_TOTAL_POP + 
  ct.rggi.near.sp@data$OTHER_NH_TOTAL_POP +
  ct.rggi.near.sp@data$TWO_OR_MORE_NH_TOTAL_POP
ct.rggi.near.sp$PER_NOT_WHITE       <- round(ct.rggi.near.sp$NOT_WHITE_TOTAL_POP / ct.rggi.near.sp$TOTAL_POPULATION * 100,2)

#generate poverty population variables

df <- ct.rggi.near.sp@data
#sort by distance
df.ordered <- df[order(df$near_dist),]

#Calculate sum of pop
sumTOTAL_POPULATION    <- sum(df.ordered$TOTAL_POPULATION)
sumNOT_WHITE_TOTAL_POP <- sum(df.ordered$NOT_WHITE_TOTAL_POP)
sumWHITE_NH_TOTAL_POP  <- sum(df.ordered$WHITE_NH_TOTAL_POP)
sumPOVERTY_POP         <- sum(df.ordered$POP_IN_POVERTY)
sumNOT_IN_POVERTY_POP  <- sum(df.ordered$POP_NOT_IN_POVERTY)

#calculate cumulative sum of pop
df.ordered$cumTOTAL_POPULATION    <- cumsum(df.ordered$TOTAL_POPULATION)
df.ordered$cumNOT_WHITE_TOTAL_POP <- cumsum(df.ordered$NOT_WHITE_TOTAL_POP)
df.ordered$cumWHITE_NH_TOTAL_POP  <- cumsum(df.ordered$WHITE_NH_TOTAL_POP)
df.ordered$cumPOVERTY_POP         <- cumsum(df.ordered$POP_IN_POVERTY)
df.ordered$cumNOT_IN_POVERTY_POP  <- cumsum(df.ordered$POP_NOT_IN_POVERTY)

subtitle <- "Distance (in Miles)\nfrom EGUs in RGGI states"
ylabel    <- 'Cumulative Population (%)'
title     <- " "
scale_factor = 80
tiff("figure3.tif")
df.ordered <-subset(df.ordered, near_dist<=10)
plot(df.ordered$near_dist,   df.ordered$cumTOTAL_POPULATION    /sumTOTAL_POPULATION    * scale_factor, type    = 'l', ylim = c(0, scale_factor), col="white", xlab="", ylab = ylabel, main = title,sub=subtitle,lwd=0)
lines(df.ordered$near_dist,  df.ordered$cumPOVERTY_POP         /sumPOVERTY_POP         * scale_factor, type    = 's', ylim = c(0, scale_factor), col="red", lwd=2)
lines(df.ordered$near_dist,  df.ordered$cumNOT_WHITE_TOTAL_POP /sumNOT_WHITE_TOTAL_POP * scale_factor, type    = 's', ylim = c(0, scale_factor), col="orange", lwd=2)
lines(df.ordered$near_dist,  df.ordered$cumNOT_IN_POVERTY_POP  /sumNOT_IN_POVERTY_POP  * scale_factor, type    = 's', ylim = c(0, scale_factor), col="green", lwd=2)
lines(df.ordered$near_dist,  df.ordered$cumWHITE_NH_TOTAL_POP  /sumWHITE_NH_TOTAL_POP * scale_factor, type    = 's', ylim = c(0, scale_factor), col="blue", lwd=2)

x<-max(df.ordered$near_dist)
legend(x*0.78,0.15* scale_factor,c("White","People of color","In poverty", "Not in poverty"), cex=0.8,text.col=c("blue","orange","red","green"))
dev.off()


#DISTRIBUTION OF EGUs IN FACILITIES AND EJCs/non-EJCs
#assign EJCLUSTER value to one EGU with NA
rggi.plants.sp@data$EJ_CLUSTER[rggi.plants.sp@data$ORSPL=="54805"] <- 0

nrow(rggi.plants.sp@data)
ff <- rggi.plants.sp@data
rggi.fencelines <- distinct(rggi.plants.sp@data, ORSPL, .keep_all = TRUE)
#number of individual facilities
nrow(rggi.fencelines)
#number of EGUs by facility
egus.by.fenceline <- data.frame(table(rggi.plants.sp$ORSPL))
table(egus.by.fenceline$Freq)
#number of facilities with EGU frequency
fenceline.frequency.by.egus <- egus.by.fenceline[egus.by.fenceline$Freq>1,]
nrow(fenceline.frequency.by.egus)

#EGU frequency by ECJ/non-EJC status
ej.n    <- table(rggi.plants.sp$EJ_CLUSTER)[2]
nonej.n <- table(rggi.plants.sp$EJ_CLUSTER)[1]
ej.per    <- round(ej.n/310 * 100,1)
nonej.per <- round(nonej.n/310 * 100,1)
t1 <- paste(ej.n, " (", ej.per, "%" ,") EGUs were sited in EJCs", sep="")
t2 <- paste(nonej.n, " (", nonej.per, "%" ,") EGUs were sited in non-EJCs", sep="")
t1
t2

#co-located EGU frequency
rggi.ej    <- subset(rggi.plants.sp@data, EJ_CLUSTER==1)
rggi.nonej <- subset(rggi.plants.sp@data, EJ_CLUSTER==0)
egu.frequency.in.rggi.ej    <- table(data.frame(table(rggi.ej$ORSPL))$Freq)
egu.frequency.in.rggi.ej
round(52/62 *100,1)

egu.frequency.in.rggi.nonej <- table(data.frame(table(rggi.nonej$ORSPL))$Freq)
egu.frequency.in.rggi.nonej
round(168/(168+24+7)*100,1)

#frequency of EGUs in CTs by EJC status
rggi.ej2 <- rggi.ej[,c("GEOID")]
rggi.nonej2 <- rggi.nonej[,c("GEOID")]
breks <- c(0,1,2,3,4,5)
ej1<-Freq(rggi.ej2)
ej2<-Freq(ej1$freq,breaks=breks)
ej2
nonej1<-Freq(rggi.nonej2)
nonej2<-Freq(nonej1$freq,breaks=breks)
nonej2

#4. CLUSTER ANALYSIS
#generate wss plots
geoids <- unique(rggi.plants.sp$GEOID)
length(geoids) #219 but there's an NA

ct2010df_ses<-subset(ct.rggi.near.sp@data, GEOID %in% geoids, select=c(PER_NOT_WHITE,PER_POVERTY))
nrow(ct2010df_ses)
ct2010df_ses <- ct2010df_ses[complete.cases(ct2010df_ses),]
wss  <- (nrow(ct2010df_ses)-1)*sum(apply(ct2010df_ses,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(ct2010df_ses, centers=i)$withinss)
tiff("supplemental_figure1.tif")
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Optimal k for k-means cluster analysis", sub="")
dev.off()

fit <- kmeans(ct2010df_ses, 2) 
# get poverty and not-white means by cluster
aggregate(ct2010df_ses,by=list(fit$cluster),FUN=mean)
# append cluster assignment
model1_df_out <- data.frame(PER_NOT_WHITE=as.numeric(ct2010df_ses$PER_NOT_WHITE),
                            PER_POVERTY  =as.numeric(ct2010df_ses$PER_POVERTY),
                            cluster      =as.character(fit$cluster))

#recode so that membership in PECJ cluster = 1, non = 0
model1_df_out$cluster <-dplyr::recode(model1_df_out$cluster,'1'='0','2'='1')
table(model1_df_out$cluster)

n.ej <- table(model1_df_out$cluster)[1]
n.nonej <- table(model1_df_out$cluster)[2]
pal<-c("red","blue","blue")
#interesting subset of non-EJ but with ~high poverty
#33 is on std dev more than the poverty mean in EJCs
outliers <- subset(model1_df_out,PER_POVERTY>33 & cluster==1)
nrow(outliers)
n.ej
n.nonej
legend.label1 <- paste("EJCs (", n.ej,")",sep="")
legend.label2 <- paste("non-EJCs (", n.nonej - nrow(outliers),")",sep="")
legend.label3 <- paste("non-EJC outliers (", nrow(outliers),")",sep="")

tiff("figure4.tif")
p1 <- xyplot(PER_NOT_WHITE~PER_POVERTY,groups=cluster,data=model1_df_out, 
          xlab="Percent in poverty", 
          ylab="Percent people of color",
          main="",
          col=pal,
          key=list(col=pal, points=T, columns=3, pch=c(1,1,16),fill=pal,text=list(c(legend.label1,legend.label2,legend.label3)))
)
p2 <- xyplot(PER_NOT_WHITE~PER_POVERTY, pch = 20, col = "blue", data=outliers)
pp <- p1 + latticeExtra::as.layer(p2)
pp
dev.off()

#poverty, not white means by ej cluster
ses.summary <- doBy::summary_by(formula = PER_NOT_WHITE + PER_POVERTY ~cluster, FUN=c(mean,sd,length), data=model1_df_out)