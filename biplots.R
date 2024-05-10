###############################################################
# PCA data: Polar Bear 
# By: Kristin Eccles
# Written in R 4.02
###############################################################
# Load libraries

# Load data
library(readxl) 
library(missMDA)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(ggbiplot)
library(dplyr)
library(ggpubr)
library(sjPlot)

library(devtools)
install_github("vqv/ggbiplot")

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

mysheets <- read_excel_allsheets("boutet_manuscript/PCA_tables_complete_edit.xlsx")

############################################################
#### Metal: Liver ####
Metals_liver <- mysheets$`Metals Liver`
Metals_liver$age_code <-recode(Metals_liver$Age,
                               AD= 3,
                               SUB = 1)
metal_liver_pca <- prcomp(Metals_liver[,5:36], scale = TRUE, center= TRUE)
write.csv(metal_liver_pca$rotation, "metal_liver_loadings.csv")
summary_metal_liver <-summary(metal_liver_pca)
write.csv(summary_metal_liver$importance, "summary_metal_liver.csv")

metal_liver_pca$sdev
 # Extract PC axes for plotting
 PCAvalues <- data.frame(metal_liver_pca$x)

 # Extract loadings of the variables
 PCAloadings <- data.frame(Variables = rownames(metal_liver_pca$rotation), metal_liver_pca$rotation)
 
 # Plot
 metal_liver_plot1<- ggplot(PCAvalues, aes(x = PC1, y = PC2)) +
   geom_point(aes(color = Metals_liver$Location, shape = Metals_liver$Sex, size = Metals_liver$age_code)) +
   geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC1*10),
                                        yend = (PC2*10)), arrow = arrow(length = unit(1/2, "picas")),
                color = "#939799", alpha=0.5) +
   scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                                "#FDDF50","#a50026","#f46d43"))+
   scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
   scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
   geom_text_repel(data = PCAloadings, mapping = aes(x = (PC1*10), y = (PC2*10), label = rownames(PCAloadings)))+
   theme(legend.text=element_text(size=10),text = element_text(size = 20))+ theme_bw(base_size=15)+    
   guides(shape = guide_legend(override.aes = list(size = 5)))+
   guides(colour = guide_legend(override.aes = list(size=5)))
 metal_liver_plot1
 
 metal_liver_plot2<- ggplot(PCAvalues, aes(x = PC3, y = PC4)) +
   geom_point(aes(color = Metals_liver$Location, shape = Metals_liver$Sex, size = Metals_liver$age_code)) +
   scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                                "#FDDF50","#a50026","#f46d43"))+
   geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC3*10),
                                        yend = (PC4*10)), arrow = arrow(length = unit(1/2, "picas")),
                color = "#939799", alpha=0.5) +
   scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
   scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
   geom_text_repel(data = PCAloadings, mapping = aes(x = (PC3*10), y = (PC4*10), label = rownames(PCAloadings)))+
   theme(legend.text=element_text(size=10),
         text = element_text(size = 20))+
      theme_bw(base_size=15)+    
   guides(shape = guide_legend(override.aes = list(size = 5)))+    
   guides(colour = guide_legend(override.aes = list(size=5)))
 metal_liver_plot2
 
 #### Metal: Muscle ####
 Metals_muscle <- mysheets$`Metals Muscle`
 Metals_muscle$age_code <-recode(Metals_muscle$Age,
                                AD= 3,
                                SUB = 1)
 metal_muscle_pca <- prcomp(Metals_muscle[,5:36], scale = TRUE, center= TRUE)
 write.csv(metal_muscle_pca$rotation, "metal_muscle_loadings.csv")
 
 summary_metal_muscle <-summary(metal_muscle_pca)
 write.csv(summary_metal_muscle$importance, "summary_metal_muscle.csv")
 
 
 # Extract PC axes for plotting
 PCAvalues <- data.frame(metal_muscle_pca$x)
 
 # Extract loadings of the variables
 PCAloadings <- data.frame(Variables = rownames(metal_muscle_pca$rotation), metal_muscle_pca$rotation)
 
 # Plot
 metal_muscle_plot1<- ggplot(PCAvalues, aes(x = PC1, y = PC2)) +
   geom_point(aes(color = Metals_muscle$Location, shape = Metals_muscle$Sex, size = Metals_muscle$age_code)) +
   geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC1*10),
                                        yend = (PC2*10)), arrow = arrow(length = unit(1/2, "picas")),
                color = "#939799", alpha=0.5) +
   scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                                "#FDDF50","#a50026","#f46d43"))+
   scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
   scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
   geom_text_repel(data = PCAloadings, mapping = aes(x = (PC1*10), y = (PC2*10), label = rownames(PCAloadings)))+
   theme(legend.text=element_text(size=10),
         text = element_text(size = 20))+
      theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+    guides(colour = guide_legend(override.aes = list(size=5)))
 metal_muscle_plot1
 
 metal_muscle_plot2<- ggplot(PCAvalues, aes(x = PC3, y = PC4)) +
   geom_point(aes(color = Metals_muscle$Location, shape = Metals_muscle$Sex, size = Metals_muscle$age_code)) +
   scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                                "#FDDF50","#a50026","#f46d43"))+
   scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
   scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
   geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC3*9),
                                        yend = (PC4*9)), arrow = arrow(length = unit(1/2, "picas")),color = "#939799", alpha=0.5) +
   geom_text_repel(data = PCAloadings, mapping = aes(x = (PC3*10), y = (PC4*10), label = rownames(PCAloadings)))+
   theme(legend.text=element_text(size=10),
         text = element_text(size = 20))+
      theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+    guides(colour = guide_legend(override.aes = list(size=5)))
 metal_muscle_plot2
 
 #### Metal: fat ####
 Metals_fat <- mysheets$`Metals Fat`
 Metals_fat$age_code <-recode(Metals_fat$Age,
                                 AD= 3,
                                 SUB = 1)
 metal_fat_pca <- prcomp(Metals_fat[,5:36], scale = TRUE, center= TRUE)
 write.csv(metal_fat_pca$rotation, "metal_fat_pca_loadings.csv")
 
 summary_metal_fat <-summary(metal_fat_pca)
 write.csv(summary_metal_fat$importance, "metal_fat_pca.csv")

 
 summary_metal_muscle
 
 # Extract PC axes for plotting
 PCAvalues <- data.frame(metal_fat_pca$x)
 
 # Extract loadings of the variables
 PCAloadings <- data.frame(Variables = rownames(metal_fat_pca$rotation), metal_fat_pca$rotation)
 
 # Plot
 metal_fat_plot1<- ggplot(PCAvalues, aes(x = PC1, y = PC2)) +
   geom_point(aes(color = Metals_fat$Location, shape = Metals_fat$Sex, size = Metals_fat$age_code)) +
   geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC1*10),
                                        yend = (PC2*10)), arrow = arrow(length = unit(1/2, "picas")),
                color = "#939799", alpha=0.5) +
   scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                                "#FDDF50","#a50026","#f46d43"))+
   scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
   scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
   geom_text_repel(data = PCAloadings, mapping = aes(x = (PC1*10), y = (PC2*10), label = rownames(PCAloadings)))+
   theme(legend.text=element_text(size=10),
         text = element_text(size = 20))+
      theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+    guides(colour = guide_legend(override.aes = list(size=5)))
 metal_fat_plot1
 
 metal_fat_plot2<- ggplot(PCAvalues, aes(x = PC3, y = PC4)) +
   geom_point(aes(color = Metals_fat$Location, shape = Metals_fat$Sex, size = Metals_fat$age_code)) +
   scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                                "#FDDF50","#a50026","#f46d43"))+
   scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
   scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
   geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC3*10),
                                        yend = (PC4*10)), arrow = arrow(length = unit(1/2, "picas")),color = "#939799", alpha=0.5) +
   geom_text_repel(data = PCAloadings, mapping = aes(x = (PC3*10), y = (PC4*10), label = rownames(PCAloadings)))+
   theme(legend.text=element_text(size=10),
         text = element_text(size = 20))+
      theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+    guides(colour = guide_legend(override.aes = list(size=5)))
 metal_fat_plot2
 
# Pub quality figures
metals=ggarrange(metal_liver_plot1, metal_liver_plot2,
                metal_muscle_plot1, metal_muscle_plot2,
                metal_fat_plot1, metal_fat_plot2,
                ncol = 2,
                nrow = 3,
                labels = c("A","B ", 
                           "C", "D", 
                           "E", "F"),
                common.legend = TRUE,
                font.label = list(size = 20),
               legend = "bottom")
metals

#Plot figures with dpi=300
save_plot("metals.tif", metals, width = 30, height =30, dpi = 300)

########################################################################################
#### PAH: Liver ####
PAHs_liver <- mysheets$`PAHs Liver`
PAHs_liver$age_code <-recode(PAHs_liver$Age,
                               AD= 3,
                               SUB = 1)
PAH_liver_pca <- prcomp(PAHs_liver[,5:15], scale = TRUE, center= TRUE)
write.csv(PAH_liver_pca$rotation, "PAH_liver_pca_loadings.csv")

summary_PAH_liver <-summary(PAH_liver_pca)
write.csv(summary_PAH_liver$importance, "summary_PAH_liver.csv")

# Extract PC axes for plotting
PCAvalues <- data.frame(PAH_liver_pca$x)

# Extract loadings of the variables
PCAloadings <- data.frame(Variables = rownames(PAH_liver_pca$rotation), PAH_liver_pca$rotation)

# Plot
PAH_liver_plot1<- ggplot(PCAvalues, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = PAHs_liver$Location, shape = PAHs_liver$Sex, size = PAHs_liver$age_code)) +
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC1*10),
                                       yend = (PC2*10)), arrow = arrow(length = unit(1/2, "picas")),
               color = "#939799", alpha=0.5) +
  scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                               "#FDDF50","#a50026","#f46d43"))+
  scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
  scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
  geom_text_repel(data = PCAloadings, mapping = aes(x = (PC1*10), y = (PC2*10), label = rownames(PCAloadings)))+
  theme(legend.text=element_text(size=10),
        text = element_text(size = 20))+
     theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+  
  guides(color = guide_legend(override.aes = list(size = 10)))+   
  guides(colour = guide_legend(override.aes = list(size=5)))
PAH_liver_plot1

PAH_liver_plot2<- ggplot(PCAvalues, aes(x = PC3, y = PC4)) +
  geom_point(aes(color = PAHs_liver$Location, shape = PAHs_liver$Sex, size = PAHs_liver$age_code)) +
  scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                               "#FDDF50","#a50026","#f46d43"))+
  scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
  scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC3*10),
                                       yend = (PC4*10)), arrow = arrow(length = unit(1/2, "picas")),color = "#939799", alpha=0.5) +
  geom_text_repel(data = PCAloadings, mapping = aes(x = (PC3*10), y = (PC4*10), label = rownames(PCAloadings)))+
  theme(legend.text=element_text(size=10),
        text = element_text(size = 10))+
     theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+    
  guides(colour = guide_legend(override.aes = list(size=5)))
PAH_liver_plot2

#### PAH: Muscle ####
PAHs_muscle <- mysheets$`PAHs Muscle`
PAHs_muscle$age_code <-recode(PAHs_muscle$Age,
                                AD= 3,
                                SUB = 1)
PAH_muscle_pca <- prcomp(PAHs_muscle[,5:15], scale = TRUE, center= TRUE)
write.csv(PAH_muscle_pca $rotation, "PAH_muscle_pca_loadings.csv")

summary_PAH_muscle <-summary(PAH_muscle_pca)
write.csv(summary_PAH_muscle$importance, "summary_PAH_muscle.csv")


# Extract PC axes for plotting
PCAvalues <- data.frame(PAH_muscle_pca$x)

# Extract loadings of the variables
PCAloadings <- data.frame(Variables = rownames(PAH_muscle_pca$rotation), PAH_muscle_pca$rotation)

# Plot
PAH_muscle_plot1<- ggplot(PCAvalues, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = PAHs_muscle$Location, shape = PAHs_muscle$Sex, size = PAHs_muscle$age_code)) +
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC1*10),
                                       yend = (PC2*10)), arrow = arrow(length = unit(1/2, "picas")),
               color = "#939799", alpha=0.5) +
  scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                               "#FDDF50","#a50026","#f46d43"))+
  scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
  scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
  geom_text_repel(data = PCAloadings, mapping = aes(x = (PC1*10), y = (PC2*10), label = rownames(PCAloadings)))+
  theme(legend.text=element_text(size=10),
        text = element_text(size = 20))+
     theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+    guides(colour = guide_legend(override.aes = list(size=5)))
PAH_muscle_plot1

PAH_muscle_plot2<- ggplot(PCAvalues, aes(x = PC3, y = PC4)) +
  geom_point(aes(color = PAHs_muscle$Location, shape = PAHs_muscle$Sex, size = PAHs_muscle$age_code)) +
  scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                               "#FDDF50","#a50026","#f46d43"))+
  scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
  scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC3*10),
                                       yend = (PC4*10)), arrow = arrow(length = unit(1/2, "picas")),color = "#939799", alpha=0.5) +
  geom_text_repel(data = PCAloadings, mapping = aes(x = (PC3*10), y = (PC4*10), label = rownames(PCAloadings)))+
  theme(legend.text=element_text(size=10),
        text = element_text(size = 20))+
     theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+    guides(colour = guide_legend(override.aes = list(size=5)))
PAH_muscle_plot2

#### PAH: fat ####
PAHs_fat <- mysheets$`PAHs Fat`
PAHs_fat$age_code <-recode(PAHs_fat$Age,
                             AD= 3,
                             SUB = 1)
PAH_fat_pca <- prcomp(PAHs_fat[,5:15], scale = TRUE, center= TRUE)
write.csv(PAH_fat_pca$rotation, "PAH_fat_pca_loadings.csv")

summary_PAH_fat <-summary(PAH_fat_pca)
write.csv(summary_PAH_fat$importance, "summary_PAH_fat.csv")


# Extract PC axes for plotting
PCAvalues <- data.frame(PAH_fat_pca$x)

# Extract loadings of the variables
PCAloadings <- data.frame(Variables = rownames(PAH_fat_pca$rotation), PAH_fat_pca$rotation)

# Plot
PAH_fat_plot1<- ggplot(PCAvalues, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = PAHs_fat$Location, shape = PAHs_fat$Sex, size = PAHs_fat$age_code)) +
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC1*10),
                                       yend = (PC2*10)), arrow = arrow(length = unit(1/2, "picas")),
               color = "#939799", alpha=0.5) +
  scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                               "#FDDF50","#a50026","#f46d43"))+
  scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
  scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
  geom_text_repel(data = PCAloadings, mapping = aes(x = (PC1*10), y = (PC2*10), label = rownames(PCAloadings)))+
  theme(legend.text=element_text(size=10),
        text = element_text(size = 20))+
     theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+    guides(colour = guide_legend(override.aes = list(size=5)))
PAH_fat_plot1

PAH_fat_plot2<- ggplot(PCAvalues, aes(x = PC3, y = PC4)) +
  geom_point(aes(color = PAHs_fat$Location, shape = PAHs_fat$Sex, size = PAHs_fat$age_code)) +
  scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                               "#FDDF50","#a50026","#f46d43"))+
  scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
  scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC3*10),
                                       yend = (PC4*10)), arrow = arrow(length = unit(1/2, "picas")),color = "#939799", alpha=0.5) +
  geom_text_repel(data = PCAloadings, mapping = aes(x = (PC3*10), y = (PC4*10), label = rownames(PCAloadings)))+
  theme(legend.text=element_text(size=10),
        text = element_text(size = 20))+
     theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+    guides(colour = guide_legend(override.aes = list(size=5)))
PAH_fat_plot2

# Pub quality figures
PAHs=ggarrange(PAH_liver_plot1, PAH_liver_plot2,
                 PAH_muscle_plot1, PAH_muscle_plot2,
                 PAH_fat_plot1, PAH_fat_plot2,
               ncol = 2,
               nrow = 3,
               labels = c("A","B ", 
                          "C", "D", 
                          "E", "F"),
              common.legend = TRUE,
              legend = "bottom",
              font.label = list(size = 20))
PAHs

#Plot figures with dpi=300
save_plot("PAHs.tif", PAHs, width = 30, height =30, dpi = 300)

########################################################################################
#### Chlordanes: Liver ####
Chlordanes_liver <- mysheets$`Chlordanes Liver`
Chlordanes_liver$age_code <-recode(Chlordanes_liver$Age,
                             AD= 3,
                             SUB = 1)
Chlordanes_liver_pca <- prcomp(Chlordanes_liver[,5:10], scale = TRUE, center= TRUE)
write.csv(Chlordanes_liver_pca$rotation, "Chlordanes_liver_pca_loadings.csv")

summary_Chlordanes_liver_pca <-summary(Chlordanes_liver_pca)
write.csv(summary_Chlordanes_liver_pca$importance, "summary_Chlordanes_liver_pca.csv")

# Extract PC axes for plotting
PCAvalues <- data.frame(Chlordanes_liver_pca$x)

# Extract loadings of the variables
PCAloadings <- data.frame(Variables = rownames(Chlordanes_liver_pca$rotation), Chlordanes_liver_pca$rotation)

# Plot
Chlordanes_liver_plot1<- ggplot(PCAvalues, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Chlordanes_liver$Location, shape = Chlordanes_liver$Sex, size = Chlordanes_liver$age_code)) +
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC1*5),
                                       yend = (PC2*5)), arrow = arrow(length = unit(1/2, "picas")),
               color = "#939799", alpha=0.5) +
  scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                               "#FDDF50","#a50026","#f46d43"))+
  scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
  scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
  geom_text_repel(data = PCAloadings, mapping = aes(x = (PC1*5), y = (PC2*5), label = rownames(PCAloadings)))+
  theme(legend.text=element_text(size=10),
        text = element_text(size = 20))+
     theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+    guides(colour = guide_legend(override.aes = list(size=5)))
Chlordanes_liver_plot1

Chlordanes_liver_plot2<- ggplot(PCAvalues, aes(x = PC3, y = PC4)) +
  geom_point(aes(color = Chlordanes_liver$Location, shape = Chlordanes_liver$Sex, size = Chlordanes_liver$age_code)) +
  scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                               "#FDDF50","#a50026","#f46d43"))+
  scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
  scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC3*5),
                                       yend = (PC4*5)), arrow = arrow(length = unit(1/2, "picas")),color = "#939799", alpha=0.5) +
  geom_text_repel(data = PCAloadings, mapping = aes(x = (PC3*5), y = (PC4*5), label = rownames(PCAloadings)))+
  theme(legend.text=element_text(size=10),
        text = element_text(size = 20))+
     theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+    guides(colour = guide_legend(override.aes = list(size=5)))
Chlordanes_liver_plot2

#### Chlordanes: Muscle ####
Chlordanes_muscle <- mysheets$`Chlordanes Muscle`
Chlordanes_muscle$age_code <-recode(Chlordanes_muscle$Age,
                              AD= 3,
                              SUB = 1)
Chlordanes_muscle_pca <- prcomp(Chlordanes_muscle[,5:10], scale = TRUE, center= TRUE)
write.csv(Chlordanes_muscle_pca$rotation, "Chlordanes_muscle_pca_loadings.csv")

summary_Chlordanes_muscle_pca <-summary(Chlordanes_muscle_pca)
write.csv(summary_Chlordanes_muscle_pca$importance, "summary_Chlordanes_muscle_pca.csv")

# Extract PC axes for plotting
PCAvalues <- data.frame(Chlordanes_muscle_pca$x)

# Extract loadings of the variables
PCAloadings <- data.frame(Variables = rownames(Chlordanes_muscle_pca$rotation), Chlordanes_muscle_pca$rotation)

# Plot
Chlordanes_muscle_plot1<- ggplot(PCAvalues, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Chlordanes_muscle$Location, shape = Chlordanes_muscle$Sex, size = Chlordanes_muscle$age_code)) +
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC1*5),
                                       yend = (PC2*5)), arrow = arrow(length = unit(1/2, "picas")),
               color = "#939799", alpha=0.5) +
  scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                               "#FDDF50","#a50026","#f46d43"))+
  scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
  scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
  geom_text_repel(data = PCAloadings, mapping = aes(x = (PC1*5), y = (PC2*5), label = rownames(PCAloadings)))+
  theme(legend.text=element_text(size=10),
        text = element_text(size = 20))+
     theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+    guides(colour = guide_legend(override.aes = list(size=5)))
Chlordanes_muscle_plot1

Chlordanes_muscle_plot2<- ggplot(PCAvalues, aes(x = PC3, y = PC4)) +
  geom_point(aes(color = Chlordanes_muscle$Location, shape = Chlordanes_muscle$Sex, size = Chlordanes_muscle$age_code)) +
  scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                               "#FDDF50","#a50026","#f46d43"))+
  scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
  scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC3*5),
                                       yend = (PC4*5)), arrow = arrow(length = unit(1/2, "picas")),color = "#939799", alpha=0.5) +
  geom_text_repel(data = PCAloadings, mapping = aes(x = (PC3*5), y = (PC4*5), label = rownames(PCAloadings)))+
  theme(legend.text=element_text(size=10),
        text = element_text(size = 20))+
     theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+    guides(colour = guide_legend(override.aes = list(size=5)))
Chlordanes_muscle_plot2

#### Chlordanes: fat ####
Chlordanes_fat <- mysheets$`Chlordanes Fat`
Chlordanes_fat$age_code <-recode(Chlordanes_fat$Age,
                           AD= 3,
                           SUB = 1)
Chlordanes_fat_pca <- prcomp(Chlordanes_fat[,5:10], scale = TRUE, center= TRUE)
write.csv(Chlordanes_fat_pca$rotation, "Chlordanes_fat_pca_loadings.csv")

summary_Chlordanes_fat_pca <-summary(Chlordanes_fat_pca)
write.csv(summary_Chlordanes_fat_pca$importance, "summary_Chlordanes_fat_pca.csv")


# Extract PC axes for plotting
PCAvalues <- data.frame(Chlordanes_fat_pca$x)

# Extract loadings of the variables
PCAloadings <- data.frame(Variables = rownames(Chlordanes_fat_pca$rotation), Chlordanes_fat_pca$rotation)

# Plot
Chlordanes_fat_plot1<- ggplot(PCAvalues, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Chlordanes_fat$Location, shape = Chlordanes_fat$Sex, size = Chlordanes_fat$age_code)) +
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC1*5),
                                       yend = (PC2*5)), arrow = arrow(length = unit(1/2, "picas")),
               color = "#939799", alpha=0.5) +
  scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                               "#FDDF50","#a50026","#f46d43"))+
  scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
  scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
  geom_text_repel(data = PCAloadings, mapping = aes(x = (PC1*5), y = (PC2*5), label = rownames(PCAloadings)))+
  theme(legend.text=element_text(size=10),
        text = element_text(size = 20))+
     theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+    guides(colour = guide_legend(override.aes = list(size=5)))
Chlordanes_fat_plot1

Chlordanes_fat_plot2<- ggplot(PCAvalues, aes(x = PC3, y = PC4)) +
  geom_point(aes(color = Chlordanes_fat$Location, shape = Chlordanes_fat$Sex, size = Chlordanes_fat$age_code)) +
  scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                               "#FDDF50","#a50026","#f46d43"))+
  scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
  scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC3*5),
                                       yend = (PC4*5)), arrow = arrow(length = unit(1/2, "picas")),color = "#939799", alpha=0.5) +
  geom_text_repel(data = PCAloadings, mapping = aes(x = (PC3*5), y = (PC4*5), label = rownames(PCAloadings)))+
  theme(legend.text=element_text(size=10),
        text = element_text(size = 20))+
     theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+    guides(colour = guide_legend(override.aes = list(size=5)))
Chlordanes_fat_plot2

# Pub quality figures
Chlordanes=ggarrange(Chlordanes_liver_plot1, Chlordanes_liver_plot2,
               Chlordanes_muscle_plot1, Chlordanes_muscle_plot2,
               Chlordanes_fat_plot1, Chlordanes_fat_plot2,
               ncol = 2,
               nrow = 3,
               labels = c("A","B", 
                          "C", "D", 
                          "E", "F"),
               common.legend = TRUE,
               font.label = list(size = 20),
              legend = "bottom")
Chlordanes

#Plot figures with dpi=300
save_plot("Chlordanes.tif", Chlordanes, width = 30, height =30, dpi = 300)

########################################################################################
#### PCB: Liver ####
PCB_liver <- mysheets$`PCB Liver`
PCB_liver$age_code <-recode(PCB_liver$Age,
                             AD= 3,
                             SUB = 1)
PCB_liver_pca <- prcomp(PCB_liver[,5:24], scale = TRUE, center= TRUE)
write.csv(PCB_liver_pca$rotation, "PCB_liver_pca_loadings.csv")


summary_PCB_liver <-summary(PCB_liver_pca)
write.csv(summary_PCB_liver$importance, "summary_PCB_liver.csv")

# Extract PC axes for plotting
PCAvalues <- data.frame(PCB_liver_pca$x)

# Extract loadings of the variables
PCAloadings <- data.frame(Variables = rownames(PCB_liver_pca$rotation), PCB_liver_pca$rotation)

# Plot
PCB_liver_plot1<- ggplot(PCAvalues, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = PCB_liver$Location, shape = PCB_liver$Sex, size = PCB_liver$age_code)) +
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC1*10),
                                       yend = (PC2*10)), arrow = arrow(length = unit(1/2, "picas")),
               color = "#939799", alpha=0.5) +
  scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                               "#FDDF50","#a50026","#f46d43"))+
  scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
  scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
  geom_text_repel(data = PCAloadings, mapping = aes(x = (PC1*9), y = (PC2*9), label = rownames(PCAloadings)))+
  theme(legend.text=element_text(size=25),
        text = element_text(size = 25))+
     theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+    guides(colour = guide_legend(override.aes = list(size=5)))
PCB_liver_plot1

PCB_liver_plot2<- ggplot(PCAvalues, aes(x = PC3, y = PC4)) +
  geom_point(aes(color = PCB_liver$Location, shape = PCB_liver$Sex, size = PCB_liver$age_code)) +
  scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                               "#FDDF50","#a50026","#f46d43"))+
  scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
  scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC3*10),
                                       yend = (PC4*10)), arrow = arrow(length = unit(1/2, "picas")),color = "#939799", alpha=0.5) +
  geom_text_repel(data = PCAloadings, mapping = aes(x = (PC3*9), y = (PC4*9), label = rownames(PCAloadings)))+
  theme(legend.text=element_text(size=10),
        text = element_text(size = 20))+
     theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+    guides(colour = guide_legend(override.aes = list(size=5)))
PCB_liver_plot2

#### PCB: Muscle ####
PCB_muscle <- mysheets$`PCB Muscle`
PCB_muscle$age_code <-recode(PCB_muscle$Age,
                              AD= 3,
                              SUB = 1)
PCB_muscle_pca <- prcomp(PCB_muscle[,5:24], scale = TRUE, center= TRUE)
write.csv(PCB_muscle_pca$rotation, "PCB_muscle_pca_loadings.csv")

summary_PCB_muscle <-summary(PCB_muscle_pca)
write.csv(summary_PCB_muscle$importance, "summary_PCB_muscle.csv")

# Extract PC axes for plotting
PCAvalues <- data.frame(PCB_muscle_pca$x)

# Extract loadings of the variables
PCAloadings <- data.frame(Variables = rownames(PCB_muscle_pca$rotation), PCB_muscle_pca$rotation)

# Plot
PCB_muscle_plot1<- ggplot(PCAvalues, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = PCB_muscle$Location, shape = PCB_muscle$Sex, size = PCB_muscle$age_code)) +
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC1*10),
                                       yend = (PC2*10)), arrow = arrow(length = unit(1/2, "picas")),
               color = "#939799", alpha=0.5) +
  scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                               "#FDDF50","#a50026","#f46d43"))+
  scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
  scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
  geom_text_repel(data = PCAloadings, mapping = aes(x = (PC1*9), y = (PC2*9), label = rownames(PCAloadings)))+
  theme(legend.text=element_text(size=10),
        text = element_text(size = 20))+
     theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+    guides(colour = guide_legend(override.aes = list(size=5)))
PCB_muscle_plot1

PCB_muscle_plot2<- ggplot(PCAvalues, aes(x = PC3, y = PC4)) +
  geom_point(aes(color = PCB_muscle$Location, shape = PCB_muscle$Sex, size = PCB_muscle$age_code)) +
  scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                               "#FDDF50","#a50026","#f46d43"))+
  scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
  scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC3*10),
                                       yend = (PC4*10)), arrow = arrow(length = unit(1/2, "picas")),color = "#939799", alpha=0.5) +
  geom_text_repel(data = PCAloadings, mapping = aes(x = (PC3*9), y = (PC4*9), label = rownames(PCAloadings)))+
  theme(legend.text=element_text(size=10),
        text = element_text(size = 20))+
     theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+    guides(colour = guide_legend(override.aes = list(size=5)))
PCB_muscle_plot2

#### PCB: fat ####
PCB_fat <- mysheets$`PCB Fat`
PCB_fat$age_code <-recode(PCB_fat$Age,
                           AD= 3,
                           SUB = 1)
PCB_fat_pca <- prcomp(PCB_fat[,5:24], scale = TRUE, center= TRUE)
write.csv(PCB_fat_pca$rotation, "PCB_fat_pca_loadings.csv")

summary_PCB_fat <-summary(PCB_fat_pca)
write.csv(summary_PCB_fat$importance, "summary_PCB_fat.csv")

# Extract PC axes for plotting
PCAvalues <- data.frame(PCB_fat_pca$x)

# Extract loadings of the variables
PCAloadings <- data.frame(Variables = rownames(PCB_fat_pca$rotation), PCB_fat_pca$rotation)

# Plot
PCB_fat_plot1<- ggplot(PCAvalues, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = PCB_fat$Location, shape = PCB_fat$Sex, size = PCB_fat$age_code)) +
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC1*10),
                                       yend = (PC2*10)), arrow = arrow(length = unit(1/2, "picas")),
               color = "#939799", alpha=0.5) +
  scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                               "#FDDF50","#a50026","#f46d43"))+
  scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
  scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
  geom_text_repel(data = PCAloadings, mapping = aes(x = (PC1*9), y = (PC2*9), label = rownames(PCAloadings)))+
  theme(legend.text=element_text(size=10),
        text = element_text(size = 20))+
     theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+    guides(colour = guide_legend(override.aes = list(size=5)))
PCB_fat_plot1

PCB_fat_plot2<- ggplot(PCAvalues, aes(x = PC3, y = PC4)) +
  geom_point(aes(color = PCB_fat$Location, shape = PCB_fat$Sex, size = PCB_fat$age_code)) +
  scale_color_manual(name="Location", values=c("#3288bd","#313695","#74add1",
                                               "#FDDF50","#a50026","#f46d43"))+
  scale_shape_manual(name="Sex", values=c(16, 17), labels = c("Female", "Male"))+
  scale_size_area(name="Age", breaks = c(1, 2), max_size = 5, labels = c("Sub Adult", "Adult"))+
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC3*10),
                                       yend = (PC4*10)), arrow = arrow(length = unit(1/2, "picas")),color = "#939799", alpha=0.5) +
  geom_text_repel(data = PCAloadings, mapping = aes(x = (PC3*9), y = (PC4*9), label = rownames(PCAloadings)))+
  theme(legend.text=element_text(size=10),
        text = element_text(size = 20))+
     theme_bw(base_size=15)+    guides(shape = guide_legend(override.aes = list(size = 5)))+    guides(colour = guide_legend(override.aes = list(size=5)))
PCB_fat_plot2

# Pub quality figures
PCB=ggarrange(PCB_liver_plot1, PCB_liver_plot2,
               PCB_muscle_plot1, PCB_muscle_plot2,
               PCB_fat_plot1, PCB_fat_plot2,
              ncol = 2,
              nrow = 3,
              labels = c("A","B ", 
                         "C", "D", 
                         "E", "F"),
              common.legend = TRUE,
              font.label = list(size = 20),
             legend = "bottom")
PCB

#Plot figures with dpi=300
save_plot("PCB.tif", PCB, width = 30, height =30, dpi = 300)





