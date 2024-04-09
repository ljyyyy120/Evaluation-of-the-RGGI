library(rgdal)
library(ggplot2)
library(reshape)
library(doBy)
library(plyr)
library(sm)
library(data.table)
library(Hmisc)
library(xtable)
library(psych)
library(gtools)
library(lattice)
library(DescTools)	
library(scales)
require(gridExtra)
library(wesanderson)
library(DescTools)	
library(tidyverse)

states.lookup.table <- sf::st_read(   dsn = 'RGGI_and_EJ_analysis.gdb', layer = "states_lookup_table")

#FUNCTION DEFINITIONS
wssplot <- function(data, nc=10, seed=1234, main,sub){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",main=main,sub=sub,
       ylab="Within groups sum of squares")}

get.n <- function (column)
{
  return(length(which(column>0)))
}

# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mean.and.round <- function(x)
{
  #return(round(mean(x),0))
  return(floor(mean(x)))  
}


#variables
cap.cols              <- c("CAP_1995", "CAP_1996", "CAP_1997", "CAP_1998", "CAP_1999", "CAP_2000", "CAP_2001", "CAP_2002", "CAP_2003", "CAP_2004", "CAP_2005","CAP_2006", "CAP_2007", "CAP_2008", "CAP_2009", "CAP_2010", "CAP_2011", "CAP_2012", "CAP_2013", "CAP_2014", "CAP_2015")
gen.cols              <- c("GEN_1995", "GEN_1996", "GEN_1997", "GEN_1998", "GEN_1999", "GEN_2000", "GEN_2001", "GEN_2002", "GEN_2003", "GEN_2004", "GEN_2005","GEN_2006", "GEN_2007", "GEN_2008", "GEN_2009", "GEN_2010", "GEN_2011", "GEN_2012", "GEN_2013", "GEN_2014", "GEN_2015")
plant.detail.columnns <- c("ORSPL","PLANT_NAME","PRIME_MOVER","UNIT_SIZE","FUEL_TYPE","STATE","COUNTY","LATITUDE","LONGITUDE","FIRST_YEAR_OF_OPERATION","LAST_YEAR_OF_OPERATION","ENVIRONMENTAL_CONTROLS")
generation.columns    <- c("ORSPL","PLANT_NAME","PRIME_MOVER","UNIT_SIZE","FUEL_TYPE","STATE","GEN_1995","GEN_1996","GEN_1997","GEN_1998","GEN_1999","GEN_2000","GEN_2001","GEN_2002","GEN_2003","GEN_2004","GEN_2005","GEN_2006","GEN_2007","GEN_2008","GEN_2009","GEN_2010","GEN_2011","GEN_2012","GEN_2013","GEN_2014","GEN_2015","GEN_2016")
co2.columns           <- c("CO2_1995","CO2_1996","CO2_1997","CO2_1998","CO2_1999","CO2_2000","CO2_2001","CO2_2002","CO2_2003","CO2_2004","CO2_2005","CO2_2006","CO2_2007","CO2_2008","CO2_2009","CO2_2010","CO2_2011","CO2_2012","CO2_2013","CO2_2014","CO2_2015")
so2.columns           <- c("SO2_1995","SO2_1996","SO2_1997","SO2_1998","SO2_1999","SO2_2000","SO2_2001","SO2_2002","SO2_2003","SO2_2004","SO2_2005","SO2_2006","SO2_2007","SO2_2008","SO2_2009","SO2_2010","SO2_2011","SO2_2012","SO2_2013","SO2_2014","SO2_2015")
nox.columns           <- c("NOX_1995","NOX_1996","NOX_1997","NOX_1998","NOX_1999","NOX_2000","NOX_2001","NOX_2002","NOX_2003","NOX_2004","NOX_2005","NOX_2006","NOX_2007","NOX_2008","NOX_2009","NOX_2010","NOX_2011","NOX_2012","NOX_2013","NOX_2014","NOX_2015")
cf.columns            <- c("CF_1995","CF_1996","CF_1997","CF_1998","CF_1999","CF_2000","CF_2001","CF_2002","CF_2003","CF_2004","CF_2005","CF_2006","CF_2007","CF_2008","CF_2009","CF_2010","CF_2011","CF_2012","CF_2013","CF_2014","CF_2015")
year.list             <- seq(from=1995, to=2015)
fuel.type.cols        <- paste("MAJ_FUEL_", year.list, sep="")

pdf.basepath <- ""

states.fips <- c("09", "10", "25", "24","34","36","42","23","33","44","50")
#open and store in df

input_fgdb            <- 'Synapse.gdb'
plants.sp <- readOGR(dsn = input_fgdb, layer = "plant_detail_NAD83")
plants <- plants.sp@data
colnames(plants)[156]  <- "CLUSTER"

#Generate CF=Annual Generation/(24hr x 365 days x Capacity)
plants$CF_1995 <- ((plants$GEN_1995 * 1000)/ (24 * 365 * plants$CAP_1995)) * 100
plants$CF_1996 <- ((plants$GEN_1996 * 1000)/ (24 * 365 * plants$CAP_1996)) * 100
plants$CF_1997 <- ((plants$GEN_1997 * 1000)/ (24 * 365 * plants$CAP_1997)) * 100
plants$CF_1998 <- ((plants$GEN_1998 * 1000)/ (24 * 365 * plants$CAP_1998)) * 100
plants$CF_1999 <- ((plants$GEN_1999 * 1000)/ (24 * 365 * plants$CAP_1999)) * 100
plants$CF_2000 <- ((plants$GEN_2000 * 1000)/ (24 * 365 * plants$CAP_2000)) * 100
plants$CF_2001 <- ((plants$GEN_2001 * 1000)/ (24 * 365 * plants$CAP_2001)) * 100
plants$CF_2002 <- ((plants$GEN_2002 * 1000)/ (24 * 365 * plants$CAP_2002)) * 100
plants$CF_2003 <- ((plants$GEN_2003 * 1000)/ (24 * 365 * plants$CAP_2003)) * 100
plants$CF_2004 <- ((plants$GEN_2004 * 1000)/ (24 * 365 * plants$CAP_2004)) * 100
plants$CF_2005 <- ((plants$GEN_2005 * 1000)/ (24 * 365 * plants$CAP_2005)) * 100
plants$CF_2006 <- ((plants$GEN_2006 * 1000)/ (24 * 365 * plants$CAP_2006)) * 100
plants$CF_2007 <- ((plants$GEN_2007 * 1000)/ (24 * 365 * plants$CAP_2007)) * 100
plants$CF_2008 <- ((plants$GEN_2008 * 1000)/ (24 * 365 * plants$CAP_2008)) * 100
plants$CF_2009 <- ((plants$GEN_2009 * 1000)/ (24 * 365 * plants$CAP_2009)) * 100
plants$CF_2010 <- ((plants$GEN_2010 * 1000)/ (24 * 365 * plants$CAP_2010)) * 100
plants$CF_2011 <- ((plants$GEN_2011 * 1000)/ (24 * 365 * plants$CAP_2011)) * 100
plants$CF_2012 <- ((plants$GEN_2012 * 1000)/ (24 * 365 * plants$CAP_2012)) * 100
plants$CF_2013 <- ((plants$GEN_2013 * 1000)/ (24 * 365 * plants$CAP_2013)) * 100
plants$CF_2014 <- ((plants$GEN_2014 * 1000)/ (24 * 365 * plants$CAP_2014)) * 100
plants$CF_2015 <- ((plants$GEN_2015 * 1000)/ (24 * 365 * plants$CAP_2015)) * 100

plants2<- subset(plants,(  STATE =='CT' | 
                             STATE =='DE' |
                             STATE =='MA' |
                             STATE =='MD' |
                             STATE =='NJ' |
                             STATE =='NY' |
                             #STATE =='PA' |
                             STATE =='ME' |
                             STATE =='NH' |
                             STATE =='RI' |
                             STATE =='VT' ) & UNIT_SIZE == '>=25')


plants<- subset(plants,(  STATE =='CT' | 
                          STATE =='DE' |
                          STATE =='MA' |
                          STATE =='MD' |
                          STATE =='NJ' |
                          STATE =='NY' |
                          #STATE =='PA' |
                          STATE =='ME' |
                          STATE =='NH' |
                          STATE =='RI' |
                          STATE =='VT' ) & UNIT_SIZE == '>=25' & !is.na(LATITUDE))


rggi.plants.sp<- subset(plants.sp,(  STATE =='CT' | 
                            STATE =='DE' |
                            STATE =='MA' |
                            STATE =='MD' |
                            STATE =='NJ' |
                            STATE =='NY' |
                            STATE =='ME' |
                            STATE =='NH' |
                            STATE =='RI' |
                            STATE =='VT' ) & UNIT_SIZE == '>=25' & !is.na(LATITUDE))


#CLUSTER NAs recoded to 0
plants$CLUSTER[is.na(plants$CLUSTER)] = 0

plants.ej    <- subset(plants, CLUSTER==1 )
plants.nonej <- subset(plants, CLUSTER==0 )
nrow(plants.ej)
nrow(plants.nonej)
nrow(plants)
(table(plants$FUEL_TYPE))

biomass.color <- wes_palettes$Moonrise3[1]
coal.color    <- wes_palettes$Moonrise3[2]
ng.color      <- wes_palettes$Moonrise3[3]
oil.color     <- wes_palettes$Moonrise3[4]
na.color      <- wes_palettes$Moonrise3[5]

#all plants in RGGI
nrow(plants2)

#plants with valid latlon
nrow(plants)

#calculate nearest distance of EGUs to CTs
near.distance <- function(ct.sp, egus.sp, name="region", crs=CRS("EPSG:5070"))
{
  require(rgeos)
  meters.to.miles.conversion.factor <- 0.000621371
  
  #eliminate duplicates
  #ct.2010.ej <- ct.2010.ej[!duplicated(ct.2010.ej),]
  
  #1. calculate distance of nearest coal or oil EGU to Census Tract
  #first project to a projected coordinate system
  #CONUS areas will get CONUS Albers (NAD83, https://epsg.io/5070-1252)
  ct.sp   <- spTransform(ct.sp, CRSobj = crs)
  egus.sp <- spTransform(egus.sp, CRSobj = crs)
  
  #cast sp to sf
  #ct.region1.sf <- sf::st_as_sf(ct.region1.sp1)
  #get centroids
  ct.centroid <-  coordinates(ct.sp)
  x <- ct.centroid[,1]
  y <- ct.centroid[,2]
  ct.centroid.sp <- SpatialPointsDataFrame(cbind(x,y),proj4string=crs,data=ct.sp@data)
  
  ct.distance <- apply(gDistance(ct.centroid.sp,egus.sp,byid = T), 2,min)
  length(ct.distance)

  ct.centroid.sp$near_dist <- ct.distance * meters.to.miles.conversion.factor #convert to miles
  
  #writeOGR(obj=ct.centroid.sp, dsn="f:/tmp", layer="ct_centroid2", driver= "ESRI Shapefile", overwrite_layer = T)
  #writeOGR(obj=ct.sp, dsn="f:/tmp", layer="ctsp", driver= "ESRI Shapefile", overwrite_layer = T)
  #writeOGR(obj=egus.sp, dsn="f:/tmp", layer="egusp", driver= "ESRI Shapefile", overwrite_layer = T)
  
  
  return(ct.centroid.sp[,c("GEOID_Data", "GEOID","near_dist")])
}  




