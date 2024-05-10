###############################################################
# Impute data: Polar Bear 
# By: Kristin Eccles
# Written in R 4.02
###############################################################
# Load libraries

# Load data
library(readxl) 
library(missMDA)
library(FactoMineR)


read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

mysheets <- read_excel_allsheets("orig_data/PCA_tables.xlsx")

######################################################################################
#### Metals ####
#### Liver ####
Metals_liver <- mysheets$`Metals Liver`
Metals_liver <- as.data.frame(lapply(Metals_liver[-1], as.numeric))
sum(is.na(Metals_liver)) #0 missing

metal_liver_pca <- prcomp(Metals_liver[,4:ncol(Metals_liver)])

#### Fat ####
Metals_fat <- mysheets$`Metals Fat`
Metals_fat <- as.data.frame(lapply(Metals_fat[-1], as.numeric))
sum(is.na(Metals_fat)) #2 missing

## First the number of components has to be chosen
Metals_fat_nb <- estim_ncpPCA(Metals_fat, scale = TRUE, ncp.min=0, ncp.max=5, method.cv="Kfold") 
## Imputation
Metals_fat_complete <- imputePCA(Metals_fat, ncp=Metals_fat_nb$ncp)
write.csv(Metals_fat_complete$completeObs, "Metals_fat_complete.csv")

#### Feaces ####
Metals_feaces <- as.data.frame(mysheets$`Metals Feaces`)
Metals_feaces <- as.data.frame(lapply(Metals_feaces[-1], as.numeric))
sum(is.na(Metals_feaces)) #1 missing

## First the number of components has to be chosen
Metals_feaces_nb <- estim_ncpPCA(Metals_feaces, ncp.min=0, ncp.max=5, method.cv="Kfold") 
## Imputation
Metals_feaces_complete <- imputePCA(Metals_feaces, ncp=Metals_feaces_nb$ncp)
write.csv(Metals_feaces_complete$completeObs, "Metals_feaces_complete.csv")

Metals_feaces_pca <- PCA(Metals_feaces)
Metals_feaces_complete_pca <- PCA(abs(Metals_feaces_complete$completeObs))

#### Muscles ####
Metals_muscle <- mysheets$`Metals Muscle`
Metals_muscle <- as.data.frame(lapply(Metals_muscles[-1], as.numeric))
sum(is.na(Metals_muscle) )#0 missing

## First the number of components has to be chosen
Metals_muscle_nb <- estim_ncpPCA(Metals_muscle, ncp.min=0, ncp.max=5, method.cv="Kfold") 
## Imputation
Metals_muscle_complete <- imputePCA(Metals_muscle, ncp=Metals_muscle_nb$ncp)
write.csv(Metals_muscle_complete$completeObs, "Metals_muscle_complete.csv")

###############################################################
#### PAH ####

#### Liver ####
PAHs_liver <- mysheets$`PAHs Liver`
PAHs_liver <- as.data.frame(lapply(PAHs_liver[-1], as.numeric))
sum(is.na(PAHs_liver)) #2 missing

## First the number of components has to be chosen
PAHs_liver_nb <- estim_ncpPCA(PAHs_liver, ncp.min=0, ncp.max=5, method.cv="Kfold") 
## Imputation
PAHs_liver_complete <- imputePCA(PAHs_liver, ncp=PAHs_liver_nb$ncp)
write.csv(PAHs_liver_complete$completeObs, "PAHs_liver_complete.csv")

PAHs_liver_pca <- PCA(PAHs_liver)
PAHs_liver_complete_pca <- PCA(abs(PAHs_liver_complete$completeObs))

#### Fat ####
PAHs_fat <- mysheets$`PAHs Fat`
PAHs_fat <- as.data.frame(lapply(PAHs_fat[-1], as.numeric))
sum(is.na(PAHs_fat)) #0 missing

#### Feaces ####
PAHs_feaces <- mysheets$`PAHs Feaces`
PAHs_feaces <- as.data.frame(lapply(PAHs_feaces[-1], as.numeric))
sum(is.na(PAHs_feaces)) #0 missing

#### Muscle ###
PAHs_muscle <- mysheets$`PAHs Muscle`
PAHs_muscle <- as.data.frame(lapply(PAHs_muscle[-1], as.numeric))
sum(is.na(PAHs_muscle)) #0 missing

###############################################################
#### PCB ####

#### Liver ####
PCB_liver <- mysheets$`PCB Liver`
PCB_liver <- as.data.frame(lapply(PCB_liver[-1], as.numeric))
sum(is.na(PCB_liver)) #16 missing

## First the number of components has to be chosen
PCB_liver_nb <- estim_ncpPCA(PCB_liver, ncp.min=0, ncp.max=5, method.cv="Kfold") 
## Imputation
PCB_liver_complete <- imputePCA(PCB_liver, ncp=PCB_liver_nb$ncp)
write.csv(PCB_liver_complete$completeObs, "PCB_liver_complete.csv")

PCB_liver_pca <- PCA(PCB_liver)
PCB_liver_complete_pca <- PCA(abs(PCB_liver_complete$completeObs))

#### Feaces ####
PCB_feaces <- mysheets$`PCB Feaces`
PCB_feaces <- as.data.frame(lapply(PCB_feaces[-1], as.numeric))
sum(is.na(PCB_feaces)) #11 missing

## First the number of components has to be chosen
PCB_feaces_nb <- estim_ncpPCA(PCB_feaces, ncp.min=0, ncp.max=5, method.cv="Kfold") 
## Imputation
PCB_feaces_complete <- imputePCA(PCB_feaces, ncp=PCB_feaces_nb$ncp)
PCB_feaces_complete$completeObs[PCB_feaces_complete$completeObs < 0] <- 0  
write.csv(PCB_feaces_complete$completeObs, "PCB_feaces_complete.csv")

PCB_feaces_pca <- PCA(PCB_feaces)
PCB_feaces_complete_pca <- PCA(abs(PCB_feaces_complete$completeObs))

#### Fat ####
PCB_fat <- mysheets$`PCB Fat`
PCB_fat <- as.data.frame(lapply(PCB_fat[-1], as.numeric))
sum(is.na(PCB_fat)) #16 missing

## First the number of components has to be chosen
PCB_fat_nb <- estim_ncpPCA(PCB_fat, ncp.min=0, ncp.max=5, method.cv="Kfold") 
## Imputation
PCB_fat_complete <- imputePCA(PCB_fat, ncp=PCB_fat_nb$ncp)
PCB_fat_complete$completeObs[PCB_fat_complete$completeObs < 0] <- 0  
write.csv(PCB_fat_complete$completeObs, "PCB_fat_complete.csv")

PCB_fat_pca <- PCA(PCB_fat)
PCB_fat_complete_pca <- PCA(abs(PCB_fat_complete$completeObs))

#### muscle ####
PCB_muscle <- mysheets$`PCB Muscle`
PCB_muscle <- as.data.frame(lapply(PCB_muscle[-1], as.numeric))
sum(is.na(PCB_muscle)) #11 missing

## First the number of components has to be chosen
PCB_muscle_nb <- estim_ncpPCA(PCB_muscle, ncp.min=0, ncp.max=5, method.cv="Kfold") 
## Imputation
PCB_muscle_complete <- imputePCA(PCB_muscle, ncp=PCB_muscle_nb$ncp)
PCB_muscle_complete$completeObs[PCB_muscle_complete$completeObs < 0] <- 0  
write.csv(((PCB_muscle_complete$completeObs)), "PCB_muscle_complete.csv")

PCB_muscle_pca <- PCA(PCB_muscle)
PCB_muscle_complete_pca <- PCA(abs(PCB_muscle_complete$completeObs))

#### PCB ALL ####
PCB_all<- cbind(PCB_muscle, PCB_fat, PCB_liver,PCB_feaces)
## First the number of components has to be chosen
PCB_all_nb <- estim_ncpPCA(PCB_all, ncp.min=0, ncp.max=5, method.cv="Kfold") 
## Imputation
PCB_all_complete <- imputePCA(PCB_all, ncp=PCB_all_nb$ncp)
PCB_all_complete$completeObs[PCB_all_complete$completeObs < 0] <- 0  
write.csv(((PCB_all_complete$completeObs)), "PCB_all_complete.csv")

PCB_all_pca <- PCA(PCB_all)
PCB_all_complete_pca <- PCA(abs(PCB_all_complete$completeObs))
#############################################################################################
#### Chlordane ####

#### Liver ####
Chlordanes_liver <- mysheets$`Chlordanes Liver`
Chlordanes_liver <- as.data.frame(lapply(Chlordanes_liver[-1], as.numeric))
sum(is.na(Chlordanes_liver)) #64 missing

## First the number of components has to be chosen
Chlordanes_liver_nb <- estim_ncpPCA(Chlordanes_liver, ncp.min=0, ncp.max=5, method.cv="Kfold") 
## Imputation
Chlordanes_liver_complete <- imputePCA(Chlordanes_liver, ncp=Chlordanes_liver_nb$ncp)
write.csv((abs(Chlordanes_liver_complete$completeObs)), "Chlordanes_liver_complete.csv")

Chlordanes_liver_pca <- PCA(Chlordanes_liver)
Chlordanes_liver_complete_pca <- PCA(abs(Chlordanes_liver_complete$completeObs))

#### Feaces ####
Chlordanes_feaces <- mysheets$`Chlordanes Feaces`
Chlordanes_feaces <- as.data.frame(lapply(Chlordanes_feaces[-1], as.numeric))
sum(is.na(Chlordanes_feaces)) #122 missing

## First the number of components has to be chosen
Chlordanes_feaces_nb <- estim_ncpPCA(Chlordanes_feaces, ncp.min=0, ncp.max=5, method.cv="Kfold") 
## Imputation
Chlordanes_feaces_complete <- imputePCA(Chlordanes_feaces, ncp=Chlordanes_feaces_nb$ncp)
write.csv((abs(Chlordanes_feaces_complete$completeObs)), "Chlordanes_feaces_complete.csv")

Chlordanes_feaces_pca <- PCA(Chlordanes_feaces)
Chlordanes_feaces_complete_pca <- PCA(abs(Chlordanes_feaces_complete$completeObs))

#### Fat ####
Chlordanes_fat <- mysheets$`Chlordanes Fat`
Chlordanes_fat <- as.data.frame(lapply(Chlordanes_fat[-1], as.numeric))
sum(is.na(Chlordanes_fat)) #78 missing

## First the number of components has to be chosen
Chlordanes_fat_nb <- estim_ncpPCA(Chlordanes_fat, ncp.min=0, ncp.max=5, method.cv="Kfold") 
## Imputation
Chlordanes_fat_complete <- imputePCA(Chlordanes_fat, ncp=Chlordanes_fat_nb$ncp)
write.csv((abs(Chlordanes_fat_complete$completeObs)), "Chlordanes_fat_complete.csv")

Chlordanes_fat_pca <- PCA(Chlordanes_fat)
Chlordanes_fat_complete_pca <- PCA(abs(Chlordanes_fat_complete$completeObs))

#### Muscle ####
Chlordanes_muscle <- mysheets$`Chlordanes Muscle`
Chlordanes_muscle <- as.data.frame(lapply(Chlordanes_muscle[-1], as.numeric))
sum(is.na(Chlordanes_muscle)) #117 missing

## First the number of components has to be chosen
Chlordanes_muscle_nb <- estim_ncpPCA(Chlordanes_muscle, ncp.min=0, ncp.max=5, method.cv="Kfold") 
## Imputation
Chlordanes_muscle_complete <- imputePCA(Chlordanes_muscle, ncp=Chlordanes_muscle_nb$ncp)
write.csv((abs(Chlordanes_muscle_complete$completeObs)), "Chlordanes_muscle_complete.csv")

Chlordanes_muscle_pca <- PCA(Chlordanes_muscle)
Chlordanes_muscle_complete_pca <- PCA(abs(Chlordanes_muscle_complete$completeObs))

#### ALL ####
chlordanes_all<- cbind(Chlordanes_muscle, Chlordanes_fat, Chlordanes_liver,Chlordanes_feaces)
## First the number of components has to be chosen
chlordanes_all_nb <- estim_ncpPCA(chlordanes_all, ncp.min=0, ncp.max=5, method.cv="Kfold") 
## Imputation
chlordanes_all_complete <- imputePCA(chlordanes_all, ncp=chlordanes_all_nb$ncp)
write.csv((abs(chlordanes_all_complete$completeObs)), "chlordanes_all_complete.csv")

chlordanes_all_pca <- PCA(chlordanes_all)
chlordanes_all_complete_pca <- PCA(abs(chlordanes_all_complete$completeObs))
