
### paleo_field_db_theoCG : merging field data to create the general spatial database, adapted to Combe Grenal excavations
# https://github.com/ediscamps/paleo_field_db 
# E. Discamps emmanuel.discamps@cnrs.fr


setwd("C:/Boulot/MyCore/_Sites en cours/Combe Grenal/Combe Grenal MyCore/CG TERRAIN 2023/THEO")

baseWD <- getwd()


# importing new data from CSVs
setwd("./csv_to_import")
### SELECT THE PROPER FILE EXTENSION .csv or .txt
listCSV <- list.files(pattern= ".txt", full.names = "F")

extractDATA <- function(x){
  filenameCSV <- x
  ### SELECT THE PROPER SEPERATOR , or ;
  dataCSV <- read.table(x, header = T, sep = ",", dec = ".")
  data <- cbind(rep(filenameCSV, nrow(dataCSV)), dataCSV)
  return(data)
}

newdataLIST <- lapply(listCSV, extractDATA)
setwd(baseWD)


#creating dataframe
require(dplyr)

dataTOT <- bind_rows(newdataLIST)
colnames(dataTOT) <- c("Filename", "Point","X","Y","Z","Code")

#removing toponulle and REPERES
dataTOPO <- filter(dataTOT, Code == "TOPO")
dataCOIN <- filter(dataTOT, grepl("-", dataTOT$Point))

dataOK <- subset(dataTOT, Code != "toponulle")
dataOK <- subset(dataTOT, Code != "TOPONULLE")
dataOK <- subset(dataOK, Code != "REPERES")
dataOK <- subset(dataOK, Point != "CTRL6")
dataOK <- subset(dataOK, Point != "CTRL7")
dataOK <- subset(dataOK, Point != "st")
dataOK <- subset(dataOK, Point != "st2")
dataOK <- subset(dataOK, Point != "st3")
dataOK <- subset(dataOK, Code != "TOPO")
dataOK <- subset(dataOK, !grepl("-", dataOK$Point))


#removing .csv or .txt extension of Filename
### SELECT THE PROPER FILE EXTENSION .csv or .txt
dataOK$Filename <- strsplit(dataOK$Filename, ".txt")
dataTOPO$Filename <- strsplit(dataTOPO$Filename, ".txt")
dataCOIN$Filename <- strsplit(dataCOIN$Filename, ".txt")

#splitting the code
codeTMP <- as.data.frame(do.call(rbind,strsplit(dataOK$Code, "_")))
codeTMPCOIN <- as.data.frame(do.call(rbind,strsplit(dataCOIN$Code, "_")))
colnames(codeTMP) <- c("CarreTheo", "Code","Dec","USterrain")
colnames(codeTMPCOIN) <- c("CarreTheo", "Code","Dec","USterrain")
dataOK <- cbind(dataOK[,1:5],codeTMP)
dataCOIN <- cbind(dataCOIN[,1:5],codeTMPCOIN)

#CREATING TRENCHES
dataOK$Trench = case_when(
  (dataOK$CarreTheo == "G50A") ~ "50est",
  (dataOK$CarreTheo == "G50B") ~ "50ouest",
  (dataOK$CarreTheo == "G50C") ~ "50est",
  (dataOK$CarreTheo == "G50D") ~ "50ouest",
  (dataOK$CarreTheo == "H50A") ~ "50est",
  (dataOK$CarreTheo == "H50B") ~ "50ouest",
  (dataOK$CarreTheo == "H50C") ~ "50est",
  (dataOK$CarreTheo == "H50D") ~ "50ouest",
  (dataOK$CarreTheo == "I50A") ~ "50est",
  (dataOK$CarreTheo == "I50B") ~ "50ouest",
  (dataOK$CarreTheo == "I50C") ~ "50est",
  (dataOK$CarreTheo == "I50D") ~ "50ouest",
  (dataOK$CarreTheo == "J50A") ~ "50est",
  (dataOK$CarreTheo == "J50B") ~ "50ouest",
  (dataOK$CarreTheo == "J50C") ~ "50est",
  (dataOK$CarreTheo == "J50D") ~ "50ouest",
  (dataOK$CarreTheo == "G51A") ~ "51est",
  (dataOK$CarreTheo == "G51C") ~ "51est",
  (dataOK$CarreTheo == "H51A") ~ "51est",
  (dataOK$CarreTheo == "H51C") ~ "51est",  
  (dataOK$CarreTheo == "I51A") ~ "51est",
  (dataOK$CarreTheo == "I51C") ~ "51est",
  (dataOK$CarreTheo == "J51A") ~ "51est",
  (dataOK$CarreTheo == "J51C") ~ "51est",
  (dataOK$CarreTheo == "G51B") ~ "51ouest",
  (dataOK$CarreTheo == "G51D") ~ "51ouest",
  (dataOK$CarreTheo == "H51B") ~ "51ouest",
  (dataOK$CarreTheo == "H51D") ~ "51ouest",  
  (dataOK$CarreTheo == "I51B") ~ "51ouest",
  (dataOK$CarreTheo == "I51D") ~ "51ouest",
  (dataOK$CarreTheo == "J51B") ~ "51ouest",
  (dataOK$CarreTheo == "J51D") ~ "51ouest")

dataOK$Yminus <- -dataOK$Y
dataOK$Annee <- "2023"
dataOK$CarreVrai <- ""
dataOK$offsetcorr <- ""
dataOK$posapprox <- ""
dataOK$FabID <- ""
dataOK$Orient <- ""
dataOK$Pendage <- ""
dataOK$Notes <- ""
dataOK$UA <- ""

dataTOPO$Notes <- ""

#reorganizing column order and names
dataOK <- cbind(dataOK$Annee,dataOK[,1:4],dataOK$Yminus, dataOK$Z,dataOK$Code,
                 dataOK$CarreVrai, dataOK$CarreTheo, dataOK$Dec,
                 dataOK$US, dataOK$UA,
                 dataOK$offsetcorr, dataOK$posapprox, dataOK$Notes,
                 dataOK$FabID, dataOK$Orient, dataOK$Pendage,
                 dataOK$Trench)

colnames(dataOK) <- c("Year","Filename","Point","X","Y","Yminus","Z","Code",
"CarreVrai", "CarreTheo", "Dec",
"USfield", "UA","offsetcorr", "posapprox", "Notes",
"FabID", "Orient", "Pendage","Trench")


#moving CSVs
moveCSV <- function(x){
  file.rename( from = file.path("./csv_to_import", x) ,
               to = file.path("./csv_imported", x) )
}

lapply(listCSV, moveCSV)

#import data previously imported
if(file.exists("CG23_THEO.xlsx")){
  library(readxl)
  previous_dataOK <- read_excel("CG23_THEO.xlsx", sheet = "dataOK", 
                                col_types = c(rep("text",3),
                                              rep("numeric",4), 
                                              rep("text",10),
                                              rep("numeric",2),
                                              "text"))
  previous_dataTOPO <- read_excel("CG23_THEO.xlsx", sheet = "dataTOPO")
  previous_dataCOIN <- read_excel("CG23_THEO.xlsx", sheet = "dataCOIN")
  dataOK <- rbind(previous_dataOK, dataOK)
  dataTOPO <- rbind(previous_dataTOPO, dataTOPO)
  dataCOIN <- rbind(previous_dataCOIN, dataCOIN)
}

#writing Excel file
require(WriteXLS)
setwd(baseWD)
WriteXLS(c("dataOK","dataTOPO","dataCOIN"),"CG23_THEO.xlsx")

#exporting bucket points
require(WriteXLS)
WriteXLS(filter(dataOK, Code =="Seau"),"CG23_seaux.xlsx")

##RUNNING SEAHORS for checking
require("SEAHORS")
SEAHORS()
