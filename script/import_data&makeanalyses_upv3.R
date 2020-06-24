# Create the folders in the hpc 

# dir.create(paste0("/hpcnfs/scratch/EO/covid19_cellend/",as.character(Sys.Date())))

# setwd(paste0("/hpcnfs/scratch/EO/covid19_cellend/",as.character(Sys.Date())))

# Create the folders 

dir.create("script")
dir.create("data")
dir.create("fig")
dir.create("tab")

# load the library
library(readxl)
library(dplyr)
library(openxlsx)


# Load the Functions 

source("script/functions_2_an&plots.R")

pz_dat <- read_xlsx("data/11.5.2020 pz_Covid19_hemdata_ar_v2.xlsx", na = "NA")

# Convert the name in english
labels <- c("IEO Code", "Sex", "Age", "Group", 
            "RCP at entrance", "RCP at test day","Hematocrit", "Leucocytes", 
            "Neutrophils", "Lymphocytes","Monocytes", "Eosinophils", 
            "Basophils","Erythrocytes", "Hemoglobin", "MCV", 
            "MCH", "Platelets", "D dimer", "Creatinine",
            "IL6", "Ferritin", "Total CEP/mL", "Total CEC/mL", 
            "% apoptotic CEP", "% apoptotic CEC","Apoptotic CEP/mL", "Viable CEP/mL", 
            "Apoptotic CEC/mL", "Viable CEC/mL", "IEO code gs","Pellet viremia", 
            "Plasma viremia", "RCP grade", "Viral RNA copies \n(cellular fraction)","Viral RNA copies \n(plasma)")
names(labels) <- colnames(pz_dat)

# Change names provided
pz_dat$gruppo[grep("_B",pz_dat$Codifica_IEO)] <- "recovered_b"

colnames(pz_dat)

# Exclude the rec_b they will be usefull for other analyses
pz_data_norecb <- pz_dat[pz_dat$gruppo!="recovered_b",]

# Create the info for sex using the first dataset
perc_male = length(which(pz_data_norecb$sesso[which(!is.na(pz_data_norecb$sesso))] == "M")) / length(pz_data_norecb$sesso[which(!is.na(pz_data_norecb$sesso))])

tab_sex <- group_by(pz_data_norecb, sesso) %>%
  summarise(
    count = n())

tab_sex <-  pz_data_norecb %>% 
  filter(!is.na(sesso)) %>%
  group_by(gruppo, sesso) %>%
  summarise(count = n())

colnames(tab_sex) <- c(na.omit(labels[match(colnames(tab_sex),names(labels))]), "Count")

# Test which is not numeric in all the columns and modify it accodingly

noNum <- list()
for(c in colnames(pz_data_norecb)){
  noNum[c] <- with(pz_data_norecb, is.numeric(get(c)))
}

names(which(unlist(lapply(noNum, function(x) x==F))))

# Correct two variable and run again to be sure they are numeric I manually check what was non numeric

# Info about age per group
tab.age <- summ_info(tab = pz_data_norecb, field = "Age")

colnames(tab.age)[colnames(tab.age)=="gruppo"] <- "group"

# Create the summary for each category beside sex

for(c in colnames(pz_data_norecb[,5:(ncol(pz_data_norecb)-6)])){
  
  assign(paste0("tabSumm.", c),  summ_info(tab = pz_data_norecb, field = c))

}

# Single comparisons test if the categories had just two groups while pairwise comparison if the categories had three groups. Details on the function can be found in function_2_an&plot.R 
for(c in colnames(pz_data_norecb[,5:(ncol(pz_data_norecb)-6)])){
  
  c= c
  
  if(nrow(get(paste0("tabSumm.", c)))== 2){
    
    assign(paste0("temp1.tab_single_p_",c), test_single_pairs(pz_data_norecb, field = c, g1 = "mild", g2 = "severe"))
    
    assign(paste0("tab_single_p_",c) ,melt(get(paste0("temp1.tab_single_p_",c))$p.value, na.rm = T))

  } else if(nrow(get(paste0("tabSumm.", c)))> 2) {
    
    assign(paste0("temp1.tab_multi_p_",c), test_multi_pairs(pz_data_norecb, field = c, g1 = "mild", g2 = "severe", g3 = "recovered"))
    
    assign(paste0("tab_multi_p_",c) ,melt(get(paste0("temp1.tab_multi_p_",c))$p.value, na.rm = T))
    
  }
}

# Pairwise comparison if the categories had four groups. Details on the functions in script/function_2_an&plot.R 
for(c in colnames(pz_data_norecb[,5:(ncol(pz_data_norecb)-6)])){
  
  if(nrow(get(paste0("tabSumm.", c)))== 4){
    
    # c <- "perc_CEP_Apoptosis"
    
    assign(paste0("temp1.tab_multi4_p_",c), test_multi_pairs_4(pz_data_norecb, field = c, g1 = "mild", g2 = "severe", g3 = "recovered", g4= "control"))
    
    assign(paste0("tab_multi4_p_",c) ,melt(get(paste0("temp1.tab_multi4_p_",c))$p.value, na.rm = T))
    
  }
  
}


# Combine all the summary
tot_table_Summ <- tabSumm.PCR_arruolamento


for(c in colnames(pz_data_norecb[,6:(ncol(pz_data_norecb)-6)])){
  
  tot_table_Summ <- rbind(tot_table_Summ, get(paste0("tabSumm.", c )))
  
}

# Give the names to the last column
names_var <- list()

for(c in colnames(pz_data_norecb[,5:(ncol(pz_data_norecb)-6)])){
  
  names_var[[c]] <- rep(c, nrow(get(paste0("tabSumm.", c ))))
  
}

tot_table_Summ$categories <- unlist(names_var)
colnames(tot_table_Summ)[colnames(tot_table_Summ)=="gruppo"] <- "group"

tot_table_Summ$categories <- labels[match(tot_table_Summ$categories, names(labels))]

# Combine the single comparison

tot_table_SingleTest <- tab_single_p_basofili

for(f in ls(pattern = "^tab_single_p_")[2:length(ls(pattern = "^tab_single_p_"))]){
  
  tot_table_SingleTest <- rbind(tot_table_SingleTest, get(f))
  
}


# Assign the names to the categories

tot_table_SingleTest$cat <- gsub("tab_single_p_", "",ls(pattern = "^tab_single_p_"))

tot_table_SingleTest$g1 <- "mild"
tot_table_SingleTest$g2 <- "severe"

colnames(tot_table_SingleTest) <- c("p-value", "categories", "group1", "group2")
tot_table_SingleTest$categories <- labels[match(tot_table_SingleTest$categories, names(labels))]

# Combine the multi

tot_table_Multi <- tab_multi_p_apoptotic_CEC_mL

for(f in ls(pattern = "^tab_multi_p_")[2:length(ls(pattern = "^tab_multi_p_"))]){
  
  tot_table_Multi <- rbind(tot_table_Multi, get(f))
  
}

tot_table_Multi$cat <- rep(gsub("tab_multi_p_", "",ls(pattern = "^tab_multi_p_")), each=3)

colnames(tot_table_Multi) <- c("group1","group2","p-value", "categories")

tot_table_Multi$categories <- labels[match(tot_table_Multi$categories, names(labels))]

# Combine the multi 4

tot_table_4Multi <- tab_multi4_p_apoptotic_CEC_mL

for(f in ls(pattern = "^tab_multi4_p_")[2:length(ls(pattern = "^tab_multi4_p_"))]){
  
  tot_table_4Multi <- rbind(tot_table_4Multi, get(f))
  
}

tot_table_4Multi$cat <- rep(gsub("tab_multi4_p_", "",ls(pattern = "^tab_multi4_p_")), each=6)

colnames(tot_table_4Multi) <- c("group1","group2","p-value", "categories")

tot_table_4Multi$categories <- labels[match(tot_table_4Multi$categories, names(labels))]

# Save the Tables
write.table(tot_table_Summ, "tab/tableSumm_upv3.csv", quote = F, sep = "\t", row.names = F)
write.table(tot_table_SingleTest, "tab/tableSingle_upv3.csv", quote = F, sep = "\t", row.names = F)
write.table(tot_table_Multi, "tab/tableMulti_upv3.csv", quote = F, sep = "\t", row.names = F)
write.table(tot_table_4Multi, "tab/table4Multi_upv3.csv", quote = F, sep = "\t", row.names = F)

# Save the fifth table

write.table(tab.age, "tab/tableAge_upv3.csv", quote = F, sep = "\t", row.names = F)
write.table(tab_sex, "tab/tableSex_upv3.csv", quote = F, sep = "\t", row.names = F)

# Make the analysis paired
comp_tab <- pz_dat[grep("_B", pz_dat$Codifica_IEO),]

fields_paired.tmp1 <- names(which(apply(comp_tab, 2, function(x) all(!is.na(x)))))[5:18]

fields_paired2rm <- names(which(apply(comp_tab, 2, function(x) length(unique(x))==1)))[grep("CE",names(which(apply(comp_tab, 2, function(x) length(unique(x))==1))))]

fields_paired <- fields_paired.tmp1[-match(fields_paired2rm, fields_paired.tmp1)]

field_CEC <- fields_paired[grep("CEC", fields_paired)]

field_CEP <- fields_paired[grep("CEP", fields_paired)]

for (f in field_CEC) {
  
  f= f
  
  assign(paste0("temp1.tab_s_paired_", f), test_single_pairs_Paired(tab = pz_dat, field = f, g1 = "recovered",g2 = "recovered_b", alt = "less"))
  
  assign(paste0("tab_s_paired_",f) ,melt(get(paste0("temp1.tab_s_paired_",f))$p.value, na.rm = T))
  
}

for (f in field_CEP) {
  
  f= f
  
  assign(paste0("temp1.tab_s_paired_", f), test_single_pairs_Paired(tab = pz_dat, field = f, g1 = "recovered",g2 = "recovered_b", alt = "greater"))
  
  assign(paste0("tab_s_paired_",f) ,melt(get(paste0("temp1.tab_s_paired_",f))$p.value, na.rm = T))
  
}

otherfields_paired <- fields_paired[-match(c(field_CEP, field_CEC), fields_paired)]

for (f in otherfields_paired) {
  
  f= f
  
  assign(paste0("temp1.tab_s_paired_", f), test_single_pairs_Paired(tab = pz_dat, field = f, g1 = "recovered",g2 = "recovered_b", alt = "two.sided"))
  
  assign(paste0("tab_s_paired_",f) ,melt(get(paste0("temp1.tab_s_paired_",f))$p.value, na.rm = T))
  
}


# Combine the single comparison
tot_table_S_Paired <- tab_s_paired_Leucociti

for(f in ls(pattern = "^tab_s_paired_")[2:length(ls(pattern = "^tab_s_paired_"))]){
  
  tot_table_S_Paired <- rbind(tot_table_S_Paired, get(f))
  
}

# Assign the names to the categories
tot_table_S_Paired$cat <- gsub("tab_s_paired_", "",ls(pattern = "^tab_s_paired_"))

tot_table_S_Paired$g1 <- "recovered"
tot_table_S_Paired$g2 <- "recovered_b"

colnames(tot_table_S_Paired) <- c("p-value", "categories", "group1", "group2")

tot_table_S_Paired$categories <- labels[match(tot_table_S_Paired$categories, names(labels))]

# Save the Tables
write.table(tot_table_S_Paired, "tab/tableS_Paired_upv3.csv", quote = F, sep = "\t", row.names = F)

# save Multiexcel

of= "Summary_Stat_Covid19.xlsx"
OUT<- createWorkbook()

lis_tab <- ls(pattern = "tot_table")
names(lis_tab) <- 


for( i in ls(pattern = "tot_table")){
  addWorksheet(OUT, i)
  writeData(OUT, sheet = i, x= get(i))
  }

saveWorkbook(OUT, paste0("tab/",of))

# Save table patiens 

pz_dat_table1 <- pz_dat
colnames(pz_dat_table1) <- labels[match(colnames(pz_dat_table1), names(labels))]

write.xlsx(pz_dat_table1, file= "tab/Table1_patient_cat.xlsx")



