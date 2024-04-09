#PLANT AGE AND FUEL TYPE CROSSTABS

setwd("C:/Users/rama/OneDrive - Union of Concerned Scientists/EJ paper/PLoS/public_data")
source("function_definitions.R", local=TRUE)

title.ej  <- "a. EJCs"
title.nonej <- "b. non-EJCs"
ylim <- c(0,100)

get.co2.in.year.online <-function(year,plants)
{
  co2.column.index       <- grep(co2.columns[year-1994], colnames(plants))
  fuel.type.index        <- grep(fuel.type.cols[year-1994], colnames(plants))
  df      <- plants[plants$FIRST_YEAR_OF_OPERATION<=year & 
                      (plants$LAST_YEAR_OF_OPERATION>=year | is.na(plants$LAST_YEAR_OF_OPERATION)),
                    c(co2.column.index,fuel.type.index)]
  df.freq <- plyr::count(df)
  if(nrow(df.freq)==0)
  {
    df.freq[1,] <- c("NA",0)
  }
  df.freq$year <- year
  colnames(df.freq)[1] <- "CO2"
  colnames(df.freq)[2] <- "Fuel"
  
  return(df.freq)
}

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

year.list2 <- year.list[which(year.list !=2001 & year.list!=2002)]

fuel.all.ej<-(lapply(year.list2,plants.ej, FUN=get.co2.in.year.online))
fuel.all.nonej<-(lapply(year.list2,plants.nonej, FUN=get.co2.in.year.online))
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

unique(fuel.nonej$Fuel)

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

unique(fuel.ej.sorted$Fuel)
line.size <- 0.3
ylim <- c(0,7)
manual.colors.na <- c(na.color, oil.color, coal.color, ng.color)
fuel.labels      <- c("Not reported", "Oil", "Coal", "Natural Gas")
co2.fuel.ej.summary    <- doBy::summary_by(freq ~ year, FUN=sum, data=fuel.ej[complete.cases(fuel.ej$CO2),])
co2.fuel.nonej.summary <- doBy::summary_by(freq ~ year, FUN=sum, data=fuel.nonej[complete.cases(fuel.nonej$CO2),])
bar.width <- 0.7
coord.fixed.ratio  <- 5
annotate.text.size <- 4
plot1.na <- ggplot(data=fuel.ej.sorted[order(fuel.ej.sorted$Fuel),],    aes(x=year, y=CO2, fill=order)) +
  geom_bar(stat="summary", fun= "mean", width = bar.width) + 
  ylim(ylim) + 
  ggtitle(title.ej) +
  scale_fill_manual(values=manual.colors.na, labels=fuel.labels) + 
  labs(x="Year", y="CO2 million short tons (mean)")  +
  annotate(geom="text", x=co2.fuel.ej.summary$year, y=(0.1), label=co2.fuel.ej.summary$freq.sum, color="black", size= annotate.text.size) +
  theme(legend.position="bottom", 
        axis.text.x  = element_text(color = "grey20", size = 16, angle = 45, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y  = element_text(color = "grey20", size = 12, angle = 0,  hjust = 1,  vjust = 0,  face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, angle = 0,  hjust = .5, vjust = 0,  face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title   = element_text(lineheight=.8, size = 18, face="plain"))

manual.colors.na <- c(na.color, oil.color, coal.color,biomass.color,  ng.color)
fuel.labels      <- c("Not reported", "Oil", "Coal", "Biomass", "Natural Gas")
plot2.na <- ggplot(data=fuel.nonej.sorted[order(fuel.nonej.sorted$Fuel),], aes(x=year, y=as.numeric(CO2), fill=order)) + 
  geom_bar(stat="summary", fun= "mean", width = bar.width) + 
  ylim(ylim) +
  ggtitle(title.nonej) + 
  scale_fill_manual(values=manual.colors.na, labels=fuel.labels) + 
  labs(x="Year", y="CO2 million short tons (mean)") +
  annotate(geom="text", x=co2.fuel.nonej.summary$year, y=(0.1), label=co2.fuel.nonej.summary$freq.sum, color="black", size= annotate.text.size) +
  theme(legend.position="bottom", 
        axis.text.x  = element_text(color = "grey20", size = 16, angle = 45, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y  = element_text(color = "grey20", size = 12, angle = 0,  hjust = 1,  vjust = 0,  face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, angle = 0,  hjust = .5, vjust = 0,  face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title   = element_text(lineheight=.8, size = 18, face="plain"))

plot2.na$labels$fill <- "Fuel Type"

mylegend1<-g_legend(plot1.na) 
mylegend2<-g_legend(plot2.na) 

p3 <- grid.arrange(arrangeGrob(plot1.na + theme(legend.position="none"),
                               plot2.na + theme(legend.position="none"),
                               nrow=1),
                   mylegend2, nrow=2,heights=c(10, 1))
f <- "figure8.pdf"
ggsave(f,plot=p3, device="pdf", dpi=100, width=14, height=12, units="in")

