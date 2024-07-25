
### paleo_field_db_theoCG : merging field data to create the general spatial database, adapted to Combe Grenal excavations
# https://github.com/ediscamps/paleo_field_db 
# E. Discamps emmanuel.discamps@cnrs.fr
# last edit: 25th July 2024

library("this.path")
setwd(here())

baseWD <- getwd()

## you should create two folders in your working directory : /csv_to_import and /csv_imported

# importing new data from CSVs
setwd("./csv_to_import")
### SELECT THE PROPER FILE EXTENSION .csv or .txt
listCSV <- list.files(pattern= ".csv", full.names = "F")

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

#extracting TOPO (topographic points taken in the field that should be kept) 
dataTOPO <- filter(dataTOT, Code == "TOPO")
dataTOPO <- filter(dataTOT, Code == "topo")

#extracting COIN (altitudes on the corners of a square taken at the end of a décapage)
dataCOIN <- filter(dataTOT, grepl("-", dataTOT$Point))

#creating dataOK (only archaeological objects and center of buckets)
#removing toponulle (topographic points taken in the field that should not be kept), REPERES, TOPO and COIN
dataOK <- subset(dataTOT, Code != "toponulle")
dataOK <- subset(dataOK, Code != "toponulle ")
dataOK <- subset(dataOK, Code != "TOPONULLE")
dataOK <- subset(dataOK, Code != "toonulle")
dataOK <- subset(dataOK, Code != "REPERES")
dataOK <- subset(dataOK, Point != "CTRL6")
dataOK <- subset(dataOK, Point != "CTRL7")
dataOK <- subset(dataOK, Point != "CTRL8")
dataOK <- subset(dataOK, Point != "st")
dataOK <- subset(dataOK, Point != "st2")
dataOK <- subset(dataOK, Point != "st3")
dataOK <- subset(dataOK, Code != "TOPO")
dataOK <- subset(dataOK, Code != "topo")
dataOK <- subset(dataOK, !grepl("-", dataOK$Point))

#extracting the filename in a new column (Filename), while removing .csv or .txt extension of Filename
### SELECT THE PROPER FILE EXTENSION .csv or .txt
dataOK$Filename <- strsplit(dataOK$Filename, ".csv")
dataTOPO$Filename <- strsplit(dataTOPO$Filename, ".csv")
dataCOIN$Filename <- strsplit(dataCOIN$Filename, ".csv")

#splitting the code
codeTMP <- as.data.frame(do.call(rbind,strsplit(dataOK$Code, "_")))
codeTMPCOIN <- as.data.frame(do.call(rbind,strsplit(dataCOIN$Code, "_")))
colnames(codeTMP) <- c("CarreTheo", "Code","Dec","USterrain")
colnames(codeTMPCOIN) <- c("CarreTheo", "Code","Dec","USterrain")
dataOK <- cbind(dataOK[,1:5],codeTMP)
dataCOIN <- cbind(dataCOIN[,1:5],codeTMPCOIN)

#CREATING TRENCHES
#TrenchVrai, based on the true coordinates
dataOK$TrenchVrai = case_when(
  (dataOK$X >= 51.5 & dataOK$X < 52) ~ "51ouest",
  (dataOK$X >= 51 & dataOK$X < 51.5) ~ "51est",
  (dataOK$X >= 50.5 & dataOK$X < 51) ~ "50ouest",
  (dataOK$X >= 50 & dataOK$X < 50.5) ~ "50est",
  .default = "Others")

#TrenchTheo, based on the square used in the field
dataOK$TrenchTheo = case_when(
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
  (dataOK$CarreTheo == "J51D") ~ "51ouest",
  .default = "Others")

#CALCULATING TRUE SQUARES
part1 <- case_when(
  (dataOK$Y >= 104 & dataOK$Y < 105) ~ "F",
  (dataOK$Y >= 103 & dataOK$Y < 104) ~ "G",
  (dataOK$Y >= 102 & dataOK$Y < 103) ~ "H",
  (dataOK$Y >= 101 & dataOK$Y < 102) ~ "I",
  (dataOK$Y >= 100 & dataOK$Y < 101) ~ "J",
  (dataOK$Y >= 99 & dataOK$Y < 100) ~ "K",
  (dataOK$Y >= 105 | dataOK$Y < 99) ~ "NA")

part2 <- case_when(
  (dataOK$X >= 52 & dataOK$X < 53) ~ "52",
  (dataOK$X >= 51 & dataOK$X < 52) ~ "51",
  (dataOK$X >= 50 & dataOK$X < 51) ~ "50",
  (dataOK$X >= 49 & dataOK$X < 50) ~ "49",
  (dataOK$X >= 53 | dataOK$X < 49) ~ "NA")

subX <- dataOK$X - floor(dataOK$X)
subY <- dataOK$Y - floor(dataOK$Y)

part3 <- case_when(
  (subX < 0.5 & subY >= 0.5) ~ "A",
  (subX >= 0.5 & subY >= 0.5) ~ "B",
  (subX < 0.5 & subY < 0.5) ~ "C",
  (subX >= 0.5 & subY < 0.5) ~ "D")

dataOK$CarreVrai <- paste0(part1, part2, part3)
rm(part1)
rm(part2)
rm(part3)
rm(subX)
rm(subY)

# calculating number of cases when CarreVrai correspond to CarreTheo
table(dataOK$CarreVrai == dataOK$CarreTheo)


#REPLACING CODES
dataOK$Code <- case_when(
  (dataOK$Code =="F") ~ "FAUNE",
  (dataOK$Code =="Si") ~ "SILEX",
  (dataOK$Code =="QZ") ~ "QUARTZ",
  (dataOK$Code =="Qz") ~ "QUARTZ",
  (dataOK$Code =="Seau") ~ "SEAU",
  (dataOK$Code =="seau") ~ "SEAU",
  (dataOK$Code =="Autre") ~ "AUTRE",
  .default = dataOK$Code)

#creating other columns in dataOK
dataOK$Yminus <- -dataOK$Y
dataOK$Annee <- "2024"
dataOK$offsetcorr <- ""
dataOK$posapprox <- ""
dataOK$FabID <- ""
dataOK$Orient <- ""
dataOK$Pendage <- ""
dataOK$Notes <- ""
dataOK$UA <- ""

#creating a "notes" column in dataTOPO
dataTOPO$Notes <- ""

#reorganizing column order and names
dataOK <- cbind(dataOK$Annee,dataOK[,1:4],dataOK$Yminus, dataOK$Z,dataOK$Code,
                dataOK$CarreVrai, dataOK$CarreTheo, dataOK$Dec,
                dataOK$US, dataOK$UA,
                dataOK$offsetcorr, dataOK$posapprox, dataOK$Notes,
                dataOK$FabID, dataOK$Orient, dataOK$Pendage,dataOK$TrenchVrai,
                dataOK$TrenchTheo)

colnames(dataOK) <- c("Year","Filename","Point","X","Y","Yminus","Z","Code",
                      "CarreVrai", "CarreTheo", "Dec",
                      "USfield", "UA","offsetcorr", "posapprox", "Notes",
                      "FabID", "Orient", "Pendage","TrenchVrai","TrenchTheo")


#moving CSVs
moveCSV <- function(x){
  file.rename( from = file.path("./csv_to_import", x) ,
               to = file.path("./csv_imported", x) )
}

lapply(listCSV, moveCSV)


#import data previously imported
#be sure to change the filename with the appropriate Excel file (full database)
#ATTENTION: in the process, some sheets of that Excel file will be deleted (the script returns which ones)
if(file.exists("CG24_THEO.xlsx")){
  library(readxl)
  previous_dataOK <- read_excel("CG24_THEO.xlsx", sheet = "dataOK", 
                                col_types = c(rep("text",3),
                                              rep("numeric",4), 
                                              rep("text",10),
                                              rep("numeric",2),
                                              rep("text",2)))
  previous_dataTOPO <- read_excel("CG24_THEO.xlsx", sheet = "dataTOPO")
  previous_dataCOIN <- read_excel("CG24_THEO.xlsx", sheet = "dataCOIN")
  list_sheets <- excel_sheets("CG24_THEO.xlsx")
  list_sheets <- list_sheets[list_sheets != "dataOK"]
  list_sheets <- list_sheets[list_sheets != "dataTOPO"]
  list_sheets <- list_sheets[list_sheets != "dataCOIN"]
  list_sheets <- list_sheets[list_sheets != "notes"]
  notes <- read_excel("CG24_THEO.xlsx", sheet = "notes")
  print("ATTENTION: the following sheets won't be imported:")
  print(list_sheets)
  dataOK <- rbind(previous_dataOK, dataOK)
  dataTOPO <- rbind(previous_dataTOPO, dataTOPO)
  dataCOIN <- rbind(previous_dataCOIN, dataCOIN)
}

#use the following lines if you are not importing new data, but want to edit your full database
# dataOK <- previous_dataOK
# dataTOPO <- previous_dataTOPO
# dataCOIN <- previous_dataCOIN
# rm(previous_dataOK)
# rm(previous_dataTOPO)
# rm(previous_dataCOIN)

#writing Excel file
require(WriteXLS)
setwd(baseWD)
WriteXLS(c("dataOK","dataTOPO","dataCOIN","notes"),"CG24_THEO.xlsx")

#exporting bucket points
require(WriteXLS)
WriteXLS(filter(dataOK, Code =="Seau"),"CG24_seaux.xlsx")


##RUNNING SEAHORS for checking
require("SEAHORS")
SEAHORS()
