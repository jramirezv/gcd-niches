#Organize crop landrace data for niche analyses for Crop Trust-GIZ study
#Julian Ramirez-Villegas
#Nov, 2024

#load packages
library(readxl)
library(tidyverse)

#working directory
wd <- "~/common_data/gcdt_niche_gaps"
data_dir <- paste0(wd, "/accession_data")
out_dir <- paste0(data_dir, "/landrace_processed")
if (!file.exists(out_dir)) {dir.create(out_dir)}

#list of crops
crop_list <- c("Rice", "African rice", "Maize", "Wheat", "Potato", "Sorghum", 
               "Common bean", "Chickpea", "Pigeonpea", "Sweet potato", "Cowpea")

#get occurrence data
occ_data <- read_xlsx(paste0(data_dir, "/BD_Coregidas_v2.xlsx"), sheet = "Sheet1")

#wrangle to select status == G, and keep only key fields, and those groups that
#we really analyzed
occ_sel <- occ_data %>%
  dplyr::select(-(access:yield)) %>%
  dplyr::filter(!crop == "Bread Wheat (environmental clusters)") %>%
  dplyr::filter(!crop == "Durum Wheat (environmental clusters)") %>%
  dplyr::filter(!crop == "potato_hawkes") %>%
  dplyr::filter(status == "G")

#crop name table
crop_tb <- data.frame(Crop_Name=c(crop_list, "Wheat_durum"), 
                      crop=c("Rice (Asia)", "Rice (Africa-glaberrima)", "Maize",
                             "Bread Wheat (genetic clusters)", "potato_spooner",
                             "Sorghum", "Common bean", "Chickpea", "Pigeonpea",
                             "Sweetpotato", "Cowpea", "Durum Wheat (genetic clusters)"))

#join crop_tb with occ_sel
occ_sel <- occ_sel %>%
  dplyr::left_join(., crop_tb, by="crop") %>%
  dplyr::select(Crop_Name, crop:used)
occ_sel$Crop_Name[which(occ_sel$crop == "Durum Wheat (genetic clusters)")] <- "Wheat"

#loop through crops
for (cname in crop_list) {
  #cname <- crop_list[11]
  
  #select crop occurrences from larger table
  occ_crp <- occ_sel %>%
    dplyr::filter(Crop_Name == cname)
  
  #write table
  write.csv(occ_crp, paste0(out_dir,"/", cname, ".csv"), row.names=FALSE, quote=TRUE)
}

# plot(occ_crp$Longitude, occ_crp$Latitude, pch=20, col="red")
# library(maptools)
# data(wrld_simpl)
# plot(wrld_simpl, add=TRUE)


