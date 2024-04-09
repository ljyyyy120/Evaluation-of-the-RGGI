#PLANT AGE AND FUEL TYPE GRAPHS
setwd("C:/Users/rama/OneDrive - Union of Concerned Scientists/EJ paper/PLoS/public_data")
source("function_definitions.R", local=TRUE)

title.ej  <- "a. EJCs"
title.nonej <- "b. non-EJCs"
ylim <- c(0,100)

get.majority.fuel.in.year.online <-function(year,plants)
{
  i       <- grep(fuel.type.cols[year-1994], colnames(plants))
  df      <- plants[plants$FIRST_YEAR_OF_OPERATION<=year & (plants$LAST_YEAR_OF_OPERATION>=year | is.na(plants$LAST_YEAR_OF_OPERATION)),i]
  df.freq <- plyr::count(df)
  if(nrow(df.freq)==0)
  {
    df.freq[1,] <- c("NA",0)
  }
  df.freq$year <- year
  colnames(df.freq)[1] <- "Fuel"
  return(df.freq)
}

get.generation.in.year.online <-function(year,plants)
{
  i       <- grep(fuel.type.cols[year-1994], colnames(plants))
  #generation cols start in col number 36 in plants
  gen.col.index <- year-1994 + 20
  #d      <- plants[plants$FIRST_YEAR_OF_OPERATION==year,i]
  df      <- plants[plants$FIRST_YEAR_OF_OPERATION<=year & (plants$LAST_YEAR_OF_OPERATION>=year | is.na(plants$LAST_YEAR_OF_OPERATION)),c(i,gen.col.index)]
  df.freq <- count(df)
  if(nrow(df.freq)==0)
  {
    df.freq[1,] <- c("NA",0)
  }

  df.stats <- aggregate(df[2], by=df[1], FUN=sum,na.rm=T)
  df.stats$year <- year
  df.stats$freq    <- table(df[,1])
  colnames(df.stats)[1] <- "Fuel"
  colnames(df.stats)[2] <- "Generation"
  return(df.stats)
}

get.capacity.factor.in.year.online <-function(year,plants)
{
  i       <- grep(fuel.type.cols[year-1994], colnames(plants))
  #CF cols start in col number 160 in plants
  cf.col.index <- year-1994 + 159
  df      <- plants[plants$FIRST_YEAR_OF_OPERATION<=year & (plants$LAST_YEAR_OF_OPERATION>=year | is.na(plants$LAST_YEAR_OF_OPERATION)),c(i,cf.col.index)]
  
  df.stats <- aggregate(df[2], by=df[1], FUN=mean,na.rm=T)
  df.stats$year <- year
  colnames(df.stats)[1] <- "Fuel"
  colnames(df.stats)[2] <- "CF"
  return(df.stats)
}

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

year.list2 <- year.list[which(year.list !=2001 & year.list!=2002)]

# EGUs --------------------------------------------------------------------
fuel.all.ej<-(lapply(year.list2,plants.ej, FUN=get.majority.fuel.in.year.online))
fuel.all.nonej<-(lapply(year.list2,plants.nonej, FUN=get.majority.fuel.in.year.online))
fuel.ej <- data.frame(Fuel=character,freq=integer,year=numeric, stringsAsFactors = F)
for(f in fuel.all.ej)
{
  fuel.ej <-rbind(fuel.ej, f)
}
fuel.nonej <- data.frame(Fuel=character,freq=integer,year=numeric, stringsAsFactors = F)
for(f in fuel.all.nonej)
{
  fuel.nonej <-rbind(fuel.nonej, f)
}
  
biomass.color <- wes_palettes$Moonrise3[1]
coal.color    <- wes_palettes$Moonrise3[2]
ng.color      <- wes_palettes$Moonrise3[3]
oil.color     <- wes_palettes$Moonrise3[4]
na.color      <- wes_palettes$Moonrise3[5]

fuel.ej.sorted <- with(fuel.ej, fuel.ej[order(Fuel,freq),])
fuel.ej.sorted.v <- fuel.ej.sorted[!fuel.ej.sorted$Fuel=='NA',]
fuel.nonej.sorted <- with(fuel.nonej, fuel.nonej[order(Fuel,freq),])
fuel.nonej.sorted.v <- fuel.nonej.sorted[!fuel.nonej.sorted$Fuel=='NA',]

fuel.ej.sorted$order<-as.character(0)
fuel.ej.sorted$order[fuel.ej.sorted$Fuel=="Natural Gas"]=4
fuel.ej.sorted$order[fuel.ej.sorted$Fuel=="Biomass"]    =3
fuel.ej.sorted$order[fuel.ej.sorted$Fuel=="Coal"]       =2
fuel.ej.sorted$order[fuel.ej.sorted$Fuel=="Oil"]        =1
fuel.ej.sorted$order[is.na(fuel.ej.sorted$Fuel)]        =0

fuel.nonej.sorted$order<-as.character(0)
fuel.nonej.sorted$order[fuel.nonej.sorted$Fuel=="Natural Gas"]=4
fuel.nonej.sorted$order[fuel.nonej.sorted$Fuel=="Biomass"]    =3
fuel.nonej.sorted$order[fuel.nonej.sorted$Fuel=="Coal"]       =2
fuel.nonej.sorted$order[fuel.nonej.sorted$Fuel=="Oil"]        =1
fuel.nonej.sorted$order[is.na(fuel.nonej.sorted$Fuel)]        =0

#as percentage
reporting.n.ej    <- doBy::summaryBy(freq~year, data = fuel.ej.sorted,    FUN=sum)
reporting.n.nonej <- doBy::summaryBy(freq~year, data = fuel.nonej.sorted, FUN=sum)
colnames(reporting.n.ej)       <- c("year", "total")
colnames(reporting.n.nonej)    <- c("year", "total")

fuel.nonej.sorted <- plyr::join(fuel.nonej.sorted, reporting.n.nonej, by="year", type="left")
fuel.ej.sorted    <- plyr::join(fuel.ej.sorted, reporting.n.ej, by="year", type="left")

fuel.nonej.sorted$percent <- fuel.nonej.sorted$freq/fuel.nonej.sorted$total * 100 
fuel.ej.sorted$percent    <- fuel.ej.sorted$freq/fuel.ej.sorted$total * 100
line.size <- 0.3
manual.colors.na <- c(na.color, oil.color, coal.color, ng.color)
fuel.labels      <- c("Not reported", "Oil", "Coal", "Natural Gas")

egu.ej.freq.by.year    <- doBy::summaryBy(freq ~year, data=fuel.ej.sorted,FUN=sum)
egu.nonej.freq.by.year <- doBy::summaryBy(freq ~year, data=fuel.nonej.sorted,FUN=sum)
max.egu.ej.n   <- max(egu.ej.freq.by.year$freq.sum,na.rm=T)
max.egu.nonej.n <- max(egu.nonej.freq.by.year$freq.sum,na.rm=T)

ej.secondary.axis.factor    <- 2.5
nonej.secondary.axis.factor <- 2.5

plot1.na <- ggplot(data=fuel.ej.sorted[order(fuel.ej.sorted$Fuel),],    aes(x=year, y=as.numeric(percent), fill=order)) +
  geom_bar(stat="identity") + 
  ylim(ylim) + 
  ggtitle(title.ej) +
  scale_fill_manual(values=manual.colors.na, labels=fuel.labels) + 
  labs(x="Year", y="Percent of EGUs")  +
  theme(legend.position="bottom") +
  geom_line(aes(y = total /ej.secondary.axis.factor),colour="black",size=line.size) +
  scale_y_continuous(sec.axis = sec_axis(~.*ej.secondary.axis.factor, name = "Total number of EGUs")) 
  
manual.colors.na <- c(na.color, oil.color, coal.color,biomass.color,  ng.color)
fuel.labels      <- c("Not reported", "Oil", "Coal", "Biomass", "Natural Gas")
plot2.na <- ggplot(data=fuel.nonej.sorted[order(fuel.nonej.sorted$Fuel),], aes(x=year, y=as.numeric(percent), fill=order)) + 
  geom_bar(stat="identity") +
  ylim(ylim) +
  ggtitle(title.nonej) + 
  scale_fill_manual(values=manual.colors.na, labels=fuel.labels) + 
  labs(x="Year", y="Percent of EGUs") +
  theme(legend.position="bottom") +
  geom_line(aes(y = total /nonej.secondary.axis.factor), colour = "black", size=line.size) +
  scale_y_continuous(sec.axis = sec_axis(~.*nonej.secondary.axis.factor, name = "Total number of EGUs"))
  
plot2.na$labels$fill <- "Fuel Type"

mylegend1<-g_legend(plot1.na) 
mylegend2<-g_legend(plot2.na) 

p3 <- grid.arrange(arrangeGrob(plot1.na + theme(legend.position="none"),
                               plot2.na + theme(legend.position="none"),
                               nrow=1),
                   mylegend2, nrow=2,heights=c(10, 1))
f <- "figure5.pdf"
ggsave(f,plot=p3, device="pdf", dpi=100, width=14, height=12, units="in")


# Generation --------------------------------------------------------------
#now same graph but for generation by fuel type
gen.all.ej    <-(lapply(year.list2,plants.ej,    FUN=get.generation.in.year.online))
gen.all.nonej <-(lapply(year.list2,plants.nonej, FUN=get.generation.in.year.online))
gen.ej        <- data.frame(Fuel=character,Generation=numeric,freq=integer,year=numeric, stringsAsFactors = F)
for(f in gen.all.ej)
{
  gen.ej <-rbind(gen.ej, f)
}
gen.nonej <- data.frame(Fuel=character,Generation=numeric, freq=integer,year=numeric, stringsAsFactors = F)
for(f in gen.all.nonej)
{
  gen.nonej <-rbind(gen.nonej, f)
}

gen.ej.sorted <- with(gen.ej, gen.ej[order(Fuel,Generation),])
gen.nonej.sorted <- with(gen.nonej, gen.nonej[order(Fuel,Generation),])

gen.ej.sorted$order<-as.character(0)
gen.ej.sorted$order[gen.ej.sorted$Fuel=="Natural Gas"]=4
gen.ej.sorted$order[gen.ej.sorted$Fuel=="Biomass"]    =3
gen.ej.sorted$order[gen.ej.sorted$Fuel=="Coal"]       =2
gen.ej.sorted$order[gen.ej.sorted$Fuel=="Oil"]        =1
gen.ej.sorted$order[is.na(gen.ej.sorted$Fuel)]        =0

gen.nonej.sorted$order<-as.character(0)
gen.nonej.sorted$order[gen.nonej.sorted$Fuel=="Natural Gas"]=4
gen.nonej.sorted$order[gen.nonej.sorted$Fuel=="Biomass"]    =3
gen.nonej.sorted$order[gen.nonej.sorted$Fuel=="Coal"]       =2
gen.nonej.sorted$order[gen.nonej.sorted$Fuel=="Oil"]        =1
gen.nonej.sorted$order[is.na(gen.nonej.sorted$Fuel)]        =0

#as percentage
total.gen.ej    <- doBy::summaryBy(Generation    ~year   ,    data = gen.ej.sorted,    FUN=sum, na.rm=T)
total.gen.nonej <- doBy::summaryBy(Generation ~year,    data = gen.nonej.sorted, FUN=sum, na.rm=T)
colnames(total.gen.ej)       <- c("year", "total.gen")
colnames(total.gen.nonej)    <- c("year", "total.gen")

gen.nonej.sorted <- plyr::join(gen.nonej.sorted, total.gen.nonej, by="year", type="left")
gen.ej.sorted    <- plyr::join(gen.ej.sorted,    total.gen.ej,    by="year", type="left")

gen.nonej.sorted$percent <- gen.nonej.sorted$Generation/gen.nonej.sorted$total.gen * 100 
gen.ej.sorted$percent    <- gen.ej.sorted$Generation   /gen.ej.sorted$total.gen    * 100

secondary.y.axis.scale.factor <- 2000

egu.ej.freq.by.year    <- doBy::summaryBy(freq ~year, data=gen.ej[!(gen.ej$Fuel=="NA"),],FUN=sum)
egu.nonej.freq.by.year <- doBy::summaryBy(freq ~year, data=gen.nonej[!(gen.nonej$Fuel=="NA"),],FUN=sum)
bar.width <- 0.8
annotate.text.size <- 4

manual.colors.na <- c(na.color, oil.color, coal.color, ng.color)
fuel.labels      <- c("Not reported", "Oil", "Coal", "Natural Gas")
plot1.na <- ggplot(data=gen.ej.sorted[order(gen.ej.sorted$Fuel),],    aes(x=year, y=as.numeric(percent), fill=order)) +
  geom_bar(stat="identity",  width = bar.width) + 
  ylim(ylim) + 
  ggtitle(title.ej) +
  scale_fill_manual(values=manual.colors.na, labels=fuel.labels) + 
  labs(x="Year", y="Percent of total net generation")  +
  #theme(legend.position="bottom") +
  theme(legend.position="bottom", 
        axis.text.x  = element_text(color = "grey20", size = 16, angle = 45, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y  = element_text(color = "grey20", size = 12, angle = 0,  hjust = 1,  vjust = 0,  face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, angle = 0,  hjust = .5, vjust = 0,  face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title   = element_text(lineheight=.8, size = 18, face="plain")) + 
  geom_line(aes(y = total.gen /secondary.y.axis.scale.factor),colour="black",size=line.size) +
  scale_y_continuous(sec.axis = sec_axis(~.*secondary.y.axis.scale.factor, name = "Total net generation (Gwh)"))   +
  annotate(geom="text", x=egu.ej.freq.by.year$year, y=(0.3), label=egu.ej.freq.by.year$freq.sum, color="black", size= annotate.text.size)

manual.colors.na <- c(na.color, oil.color, coal.color,biomass.color,  ng.color)
fuel.labels      <- c("Not reported", "Oil", "Coal", "Biomass", "Natural Gas")
plot2.na <- ggplot(data=gen.nonej.sorted[order(gen.nonej.sorted$Fuel),], aes(x=year, y=as.numeric(percent), fill=order)) + 
  geom_bar(stat="identity",  width = bar.width) +
  ylim(ylim) +
  ggtitle(title.nonej) + 
  scale_fill_manual(values=manual.colors.na, labels=fuel.labels) +
  labs(x="Year", y="Percent of total net generation") +
  #theme(legend.position="bottom") +
  theme(legend.position="bottom", 
        axis.text.x  = element_text(color = "grey20", size = 16, angle = 45, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y  = element_text(color = "grey20", size = 12, angle = 0,  hjust = 1,  vjust = 0,  face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, angle = 0,  hjust = .5, vjust = 0,  face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title   = element_text(lineheight=.8, size = 18, face="plain")) + 
  geom_line(aes(y = total.gen /secondary.y.axis.scale.factor),colour="black",size=line.size) +
  scale_y_continuous(sec.axis = sec_axis(~.*secondary.y.axis.scale.factor, name = "Total net generation (Gwh)")) + 
  annotate(geom="text", x=egu.ej.freq.by.year$year, y=(0.3), label=egu.ej.freq.by.year$freq.sum, color="black", size= annotate.text.size)

plot2.na$labels$fill <- "Fuel Type"

mylegend1<-g_legend(plot1.na) 
mylegend2<-g_legend(plot2.na) 

p3 <- grid.arrange(arrangeGrob(plot1.na + theme(legend.position="none"),
                               plot2.na + theme(legend.position="none"),
                               nrow=1),
                   mylegend2, nrow=2,heights=c(10, 1))
f <- "C:/Users/rama/OneDrive - Union of Concerned Scientists/EJ paper/PLoS/figure6.pdf"
ggsave(f,plot=p3, device="pdf", dpi=100, width=14, height=12, units="in")
dev.off()

# CF ----------------------------------------------------------------------
cf.all.ej    <-(lapply(year.list2,plants.ej, FUN=get.capacity.factor.in.year.online))
cf.all.nonej <-(lapply(year.list2,plants.nonej, FUN=get.capacity.factor.in.year.online))
cf.ej <- data.frame(Fuel=character,CF=integer,year=numeric, stringsAsFactors = F)
for(f in cf.all.ej)
{
  cf.ej <-rbind(cf.ej, f)
}
cf.nonej <- data.frame(Fuel=character,CF=integer,year=numeric, stringsAsFactors = F)
for(f in cf.all.nonej)
{
  cf.nonej <-rbind(cf.nonej, f)
}

biomass.color <- wes_palettes$Moonrise3[1]
coal.color    <- wes_palettes$Moonrise3[2]
ng.color      <- wes_palettes$Moonrise3[3]
oil.color     <- wes_palettes$Moonrise3[4]
na.color      <- wes_palettes$Moonrise3[5]

cf.ej.sorted      <- with(cf.ej, cf.ej[order(Fuel,CF),])
cf.ej.sorted.v    <- cf.ej.sorted[!cf.ej.sorted$Fuel=='NA',]
cf.nonej.sorted   <- with(cf.nonej, cf.nonej[order(Fuel,CF),])
cf.nonej.sorted.v <- cf.nonej.sorted[!cf.nonej.sorted$Fuel=='NA',]

cf.ej.sorted$order<-as.character(0)
cf.ej.sorted$order[cf.ej.sorted$Fuel=="Natural Gas"]=4
cf.ej.sorted$order[cf.ej.sorted$Fuel=="Biomass"]    =3
cf.ej.sorted$order[cf.ej.sorted$Fuel=="Coal"]       =2
cf.ej.sorted$order[cf.ej.sorted$Fuel=="Oil"]        =1
cf.ej.sorted$order[is.na(cf.ej.sorted$Fuel)]        =0

cf.nonej.sorted$order<-as.character(0)
cf.nonej.sorted$order[cf.nonej.sorted$Fuel=="Natural Gas"]=4
cf.nonej.sorted$order[cf.nonej.sorted$Fuel=="Biomass"]    =3
cf.nonej.sorted$order[cf.nonej.sorted$Fuel=="Coal"]       =2
cf.nonej.sorted$order[cf.nonej.sorted$Fuel=="Oil"]        =1
cf.nonej.sorted$order[is.na(cf.nonej.sorted$Fuel)]        =0

cf.ej.sorted    <- cf.ej.sorted[!cf.ej.sorted$Fuel=="NA",]
cf.nonej.sorted <- cf.nonej.sorted[!cf.nonej.sorted$Fuel=="NA",]

ylim <- c(0,70)
# Change point shapes by the levels of cyl
plot1 <- ggplot(cf.ej.sorted, aes(x=year, y=CF, color=Fuel)) +
  geom_line(size=1) + 
  ylim(ylim) +
  ggtitle(title.ej) +
  scale_color_manual(values=c(coal.color, ng.color,oil.color))+
  labs(x="Year", y="Capacity factor") +
  theme(legend.position="bottom")

plot2 <- ggplot(cf.nonej.sorted, aes(x=year, y=CF, color=Fuel)) +
  geom_line(size=1) + 
  ylim(ylim) +
  ggtitle(title.nonej) +
  scale_color_manual(values=c(biomass.color, coal.color, ng.color,oil.color))+
  labs(x="Year", y="Capacity factor") +
  theme(legend.position="bottom")

mylegend1<-g_legend(plot1) 
mylegend2<-g_legend(plot2) 
p3 <- grid.arrange(arrangeGrob(plot1 + theme(legend.position="none"),
                               plot2 + theme(legend.position="none"),
                               nrow=1),
                   mylegend2, nrow=2,heights=c(10, 1))
p3
f <- "C:/Users/rama/OneDrive - Union of Concerned Scientists/EJ paper/PLoS/figure7.tif"
ggsave(f,plot=p3, device="tiff")
dev.off()
