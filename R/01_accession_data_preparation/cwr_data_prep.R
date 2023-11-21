#Organize CWR data for niche analyses for Crop Trust-GIZ study
#Julian Ramirez-Villegas
#Nov, 2024

#load packages
library(xlsx)
library(tidyverse)

#working directory
wd <- "~/common_data/gcdt_niche_gaps"
data_dir <- paste0(wd, "/accession_data")
out_dir <- paste0(data_dir, "/cwr_processed")
if (!file.exists(out_dir)) {dir.create(out_dir)}

#read base files
crop_names <- read.csv(paste0(data_dir, "/crop.csv"))
occ_data <- read.csv(paste0(data_dir, "/occurrences_g.csv"))
spp_names <- read.xlsx(paste0(data_dir, "/41477_2016_BFnplants201622_MOESM3_ESM.xlsx"), 
                       sheetName = "Supplementary Table 2")
gpool_names <- read.csv(paste0(data_dir, "/genepool.csv"))

#list of crops
crop_list <- c("Rice", "African rice", "Maize", "Wheat", "Potato", "Sorghum", 
               "Common bean", "Chickpea", "Pigeonpea", "Sweet potato", "Cowpea")

#first merge crop_names and gpool_names
#add musa balbisiana
crop_names <- crop_names %>%
  dplyr::bind_rows(., data.frame(Crop_ID=34, 
                                 Crop_Scientific_Name="Musa balbisiana", 
                                 Crop_Common_Name="Plantain"))

#join tables
cwr_list <- gpool_names %>%
  dplyr::mutate(Crop_ID = Crop_VAlid_Taxon_ID) %>%
  dplyr::left_join(., crop_names, by="Crop_ID") %>%
  dplyr::select(-Crop_Scientific_Name.y) %>%
  dplyr::rename(Crop_Scientific_Name = Crop_Scientific_Name.x)

#create the taxon_final field in cwr_list, and filter crops i need
cwr_list <- cwr_list %>%
  dplyr::mutate(taxon_final = sapply(as.list(cwr_list$Scientific_Name), FUN=function(x) {gsub(" ", "_", x)})) %>%
  dplyr::filter(Crop_Common_Name %in% crop_list)

#given that for crops sometimes cwr overlap, loop by crops and then
#filter the cwr based on taxon_final
for (cname in crop_list) {
  #cname <- crop_list[3]
  #select cwr from cwr_list, note removal of "Crop Taxa" following CWR paper methods
  cwr_sel <- cwr_list %>%
    dplyr::filter(Crop_Common_Name == cname) %>%
    dplyr::filter(concept_level != "Crop taxa") %>%
    dplyr::select(taxon_final, concept_type, concept_level, Crop_Common_Name) %>%
    dplyr::distinct()
  
  #select occ from occ_data, based onn taxon_final
  occ_sel <- occ_data %>%
    dplyr::filter(taxon_final %in% cwr_sel$taxon_final) %>%
    dplyr::mutate(hascoord = "YES") %>%
    dplyr::left_join(., cwr_sel, by="taxon_final")
  occ_sel$hascoord[which(occ_sel$final_lat == "\\N")] <- "NO"
  occ_sel$hascoord[which(occ_sel$final_lon == "\\N")] <- "NO"
  
  #write sample file
  write.csv(occ_sel, paste0(out_dir,"/", cname, ".csv"), row.names=FALSE, quote=TRUE)
}



