# Create the folders in the hpc 

# Create the folders 

dir.create("script")
dir.create("data")
dir.create("fig")
dir.create("tab")

# load the library
library(readxl)
library(dplyr)

# Load the Functions 

source("script/functions_2_an&plots.R")

# Load the excel

pz_dat <- read_excel("data/db_pz_22042020_PM_modAR.xlsx")

# Change names provided

pz_dat$gruppo[grep("DON", pz_dat$`Codifica IEO`)] <- "control"
pz_dat$gruppo[pz_dat$gruppo=="grave"] <- "severe"
pz_dat$gruppo[pz_dat$gruppo=="lieve" | pz_dat$gruppo=="Lieve" ] <- "mild"
pz_dat$gruppo[pz_dat$gruppo=="guarito"] <- "recovered"

colnames(pz_dat)

# Change colnames for better use later

colnames(pz_dat) <- gsub("-", "_", gsub("età", "Age",gsub("/", "", gsub("%", "perc",gsub(" ", "_",colnames(pz_dat))))))

# Create the info for sex using the first dataset

perc_male = length(which(pz_dat$sesso[which(!is.na(pz_dat$sesso))] == "M")) / length(pz_dat$sesso[which(!is.na(pz_dat$sesso))])

tab_sex <- group_by(pz_dat, sesso) %>%
  summarise(
    count = n())

tab_sex <-  pz_dat %>% 
  filter(!is.na(sesso)) %>%
  group_by(gruppo, sesso) %>%
  summarise(count = n())

# Test which is not numeric in all the columns and modify it accodingly

noNum <- list()
for(c in colnames(pz_dat)){
  noNum[c] <- with(pz_dat, is.numeric(get(c)))
}

names(which(unlist(lapply(noNum, function(x) x==F))))

# Correct two variable and run again to be sure they are numeric I manually check what was non numeric

pz_dat$D_Dimero[grep("<200", pz_dat$D_Dimero)] <- 199

pz_dat$D_Dimero <-  as.numeric(pz_dat$D_Dimero)

pz_dat$PCR_giorno_esame[grep("<0,4", pz_dat$PCR_giorno_esame)] <- 0.39

pz_dat$PCR_giorno_esame <-  as.numeric(pz_dat$PCR_giorno_esame)


# Info about age per group

tab.age <- summ_info(tab = pz_dat, field = "Age")


# Create the summary for each category beside sex

for(c in colnames(pz_dat[,5:ncol(pz_dat)])){
  
  assign(paste0("tabSumm.", c),  summ_info(tab = pz_dat, field = c))

}

# Single comparisons test if the categories had just two groups while pairwise comparison if the categories had three groups. Details on the function can be found in function_2_an&plot.R 

for(c in colnames(pz_dat[,5:ncol(pz_dat)])){
  
  c= c
  
  if(nrow(get(paste0("tabSumm.", c)))== 2){
    
    assign(paste0("temp1.tab_single_p_",c), test_single_pairs(pz_dat, field = c, g1 = "mild", g2 = "severe"))
    
    assign(paste0("tab_single_p_",c) ,melt(get(paste0("temp1.tab_single_p_",c))$p.value, na.rm = T))

  } else if(nrow(get(paste0("tabSumm.", c)))> 2) {
    
    assign(paste0("temp1.tab_multi_p_",c), test_multi_pairs(pz_dat, field = c, g1 = "mild", g2 = "severe", g3 = "recovered"))
    
    assign(paste0("tab_multi_p_",c) ,melt(get(paste0("temp1.tab_multi_p_",c))$p.value, na.rm = T))
    
  }
}

# Pairwise comparison if the categories had four groups. Details on the functions in script/function_2_an&plot.R 

for(c in colnames(pz_dat[,5:ncol(pz_dat)])){
  
  if(nrow(get(paste0("tabSumm.", c)))== 4){
    
    # c <- "perc_CEP_Apoptosis"
    
    assign(paste0("temp1.tab_multi4_p_",c), test_multi_pairs_4(pz_dat, field = c, g1 = "mild", g2 = "severe", g3 = "recovered", g4= "control"))
    
    assign(paste0("tab_multi4_p_",c) ,melt(get(paste0("temp1.tab_multi4_p_",c))$p.value, na.rm = T))
    
  }
  
}


# Combine all the summary

tot_table_Summ <- tabSumm.PCR_arruolamento


for(c in colnames(pz_dat[,6:ncol(pz_dat)])){
  
  tot_table_Summ <- rbind(tot_table_Summ, get(paste0("tabSumm.", c )))
  
}

# Give the names to the last column
names_var <- list()

for(c in colnames(pz_dat[,5:ncol(pz_dat)])){
  
  names_var[[c]] <- rep(c, nrow(get(paste0("tabSumm.", c ))))
  
}

tot_table_Summ$categories <- unlist(names_var)


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

# Combine the multi

tot_table_Multi <- tab_multi_p_apoptotic_CEC_mL

for(f in ls(pattern = "^tab_multi_p_")[2:length(ls(pattern = "^tab_multi_p_"))]){
  
  tot_table_Multi <- rbind(tot_table_Multi, get(f))
  
}

tot_table_Multi$cat <- rep(gsub("tab_multi_p_", "",ls(pattern = "^tab_multi_p_")), each=3)

colnames(tot_table_Multi) <- c("group1","group2","p-value", "categories")

# Combine the multi 4

tot_table_4Multi <- tab_multi4_p_apoptotic_CEC_mL

for(f in ls(pattern = "^tab_multi4_p_")[2:length(ls(pattern = "^tab_multi4_p_"))]){
  
  tot_table_4Multi <- rbind(tot_table_4Multi, get(f))
  
}

tot_table_4Multi$cat <- rep(gsub("tab_multi4_p_", "",ls(pattern = "^tab_multi4_p_")), each=6)

colnames(tot_table_4Multi) <- c("group1","group2","p-value", "categories")


# Save the Tables

write.table(tot_table_Summ, "tab/tableSumm.csv", quote = F, sep = "\t", row.names = F)
write.table(tot_table_SingleTest, "tab/tableSingle.csv", quote = F, sep = "\t", row.names = F)
write.table(tot_table_Multi, "tab/tableMulti.csv", quote = F, sep = "\t", row.names = F)
write.table(tot_table_4Multi, "tab/table4Multi.csv", quote = F, sep = "\t", row.names = F)

# Save the fifth table

write.table(tab.age, "tab/tableAge", quote = F, sep = "\t", row.names = F)
write.table(tab_sex, "tab/tableSex", quote = F, sep = "\t", row.names = F)




