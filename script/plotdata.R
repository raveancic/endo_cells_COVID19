# Load the Functions & Libraries

library(cowplot)

source("script/functions_2_an&plots.R")

# Load a new table
# Introduce the viral RNA data given another time for the correlation

rnavirpz <- read_excel("data/RNAvirale_AR.xlsx")

colnames(rnavirpz) <- gsub(" ", "_", colnames(rnavirpz))

# Put toghether the tables

rnavirpz_nword <-rnavirpz[match(pz_dat$Codifica_IEO,as.character(rnavirpz$Codice_IEO)),1:ncol(rnavirpz)-1]

pz_dat_AddRNAvir <- cbind(pz_dat,rnavirpz_nword)

# Chose colors 

colbp <- c("#1aafd0", "#6a67ce", "#ffb900", "#fc636b")
names(colbp) <- c("control", "recovered", "mild", "severe")

# Convert the labels in englisgh

labels <- c("IEO Code", "Sex", "Age", "Group", 
            "RCP at entrance", "RCP at test day","Hematocrit", "Leucocytes", 
            "Neutrophils", "Lymphocytes","Monocytes", "Eosinophils", 
            "Basophils","Erythrocytes", "Hemoglobin", "MCV", 
            "MCH", "Platelets", "D dimer", "Creatinine",
            "IL6", "Ferritin", "Total CEP/mL", "Total CEC/mL", 
            "% apoptotic CEP", "% apoptotic CEC","Apoptotic CEP/mL", "Viable CEP/mL", 
            "Apoptotic CEC/mL", "Viable CEC/mL", "IEO code gs","Pellet viremia", 
            "Plasma viremia", "RCP grade", "Viral RNA copies \n(cellular fraction)","Viral RNA copies \n(plasma)")
names(labels) <- colnames(pz_dat_AddRNAvir)

# Plot all the significative values for CEP and CEC

# Plot the boxplot 8 of them with the control

# Plot 1  "Total_CEPmL"

# How many sign to plot? 

sign_tab <- tab_multi4_p_Total_CEPmL[tab_multi4_p_Total_CEPmL$value < 0.05,]

nrow(sign_tab)

# Plot the sign looking at the groups in sign tabs

plotTot_CEPmL <- plot_half_bp_4_sign(pz_dat, "Total_CEPmL") +
  ggplot_sign(tab = pz_dat, c = "Total_CEPmL", 
              g1sign = as.character(sign_tab$Var1[1]),
              g2sign = as.character(sign_tab$Var2[1]),
              h= 10
                ) +
  ggplot_sign(tab = pz_dat, c = "Total_CEPmL", 
              g1sign = as.character(sign_tab$Var1[2]),
              g2sign = as.character(sign_tab$Var2[2]),
              h= 20
  ) +
  ggplot_sign(tab = pz_dat, c = "Total_CEPmL", 
              g1sign = as.character(sign_tab$Var1[3]),
              g2sign = as.character(sign_tab$Var2[3]),
              h= 0
  )


# Plot 2  "Total_CECmL"

# How many sign to plot? 

sign_tab <- tab_multi4_p_Total_CEC_mL[tab_multi4_p_Total_CEC_mL$value < 0.05,]

nrow(sign_tab)

plotTotal_CEC_mL <- plot_half_bp_4_sign(pz_dat, "Total_CEC_mL") +
  ggplot_sign(tab = pz_dat, c = "Total_CEC_mL", 
              g1sign = as.character(sign_tab$Var1[1]),
              g2sign = as.character(sign_tab$Var2[1]),
              h= 0
  ) +
  ggplot_sign(tab = pz_dat, c = "Total_CEC_mL", 
              g1sign = as.character(sign_tab$Var1[2]),
              g2sign = as.character(sign_tab$Var2[2]),
              h= 16
  ) +
  ggplot_sign(tab = pz_dat, c = "Total_CEC_mL", 
              g1sign = as.character(sign_tab$Var1[3]),
              g2sign = as.character(sign_tab$Var2[3]),
              h= -16
  )


# Plot 3 "perc_CEP_Apoptosis" No sign

# How many sign to plot? 

sign_tab <- tab_multi4_p_perc_CEP_Apoptosis[tab_multi4_p_perc_CEP_Apoptosis$value < 0.05,]

nrow(sign_tab)

# Plot 4 perc_CEC_Apoptosis

# How many sign to plot? 

sign_tab <- tab_multi4_p_perc_CEC_Apoptosis[tab_multi4_p_perc_CEC_Apoptosis$value < 0.05,]

nrow(sign_tab)


plotTotal_perc_CEC_Apoptosis <- plot_half_bp_4_sign(pz_dat, "perc_CEC_Apoptosis") +
  ggplot_sign(tab = pz_dat, c = "perc_CEC_Apoptosis", 
              g1sign = as.character(sign_tab$Var1[1]),
              g2sign = as.character(sign_tab$Var2[1]),
              h= 16
  ) +
  ggplot_sign(tab = pz_dat, c = "perc_CEC_Apoptosis", 
              g1sign = as.character(sign_tab$Var1[2]),
              g2sign = as.character(sign_tab$Var2[2]),
              h= 24
  ) +
  ggplot_sign(tab = pz_dat, c = "perc_CEC_Apoptosis", 
              g1sign = as.character(sign_tab$Var1[3]),
              g2sign = as.character(sign_tab$Var2[3]),
              h= 8
  )


# Plot 5 apototic_CEP_mL

# How many sign to plot? 

sign_tab <- tab_multi4_p_apototic_CEP_mL[tab_multi4_p_apototic_CEP_mL$value < 0.05,]

nrow(sign_tab)

plotTotal_apototic_CEP_mL <- plot_half_bp_4_sign(pz_dat, "apototic_CEP_mL") +
  ggplot_sign(tab = pz_dat, c = "apototic_CEP_mL", 
              g1sign = as.character(sign_tab$Var1[1]),
              g2sign = as.character(sign_tab$Var2[1]),
              h= 10
  ) +
  ggplot_sign(tab = pz_dat, c = "apototic_CEP_mL", 
              g1sign = as.character(sign_tab$Var1[2]),
              g2sign = as.character(sign_tab$Var2[2]),
              h= 20
  ) +
  ggplot_sign(tab = pz_dat, c = "apototic_CEP_mL", 
              g1sign = as.character(sign_tab$Var1[3]),
              g2sign = as.character(sign_tab$Var2[3]),
              h= 0
  )


# Plot 6 apoptotic_CEC_mL

# How many sign to plot? 

sign_tab <- tab_multi4_p_apoptotic_CEC_mL[tab_multi4_p_apoptotic_CEC_mL$value < 0.05,]

nrow(sign_tab)

plotTotal_apoptotic_CEC_mL <- plot_half_bp_4_sign(pz_dat, "apoptotic_CEC_mL") +
  ylim(0, max(pz_dat[,"apoptotic_CEC_mL"]) + 34) +
  
  
  ggplot_sign(tab = pz_dat, c = "apoptotic_CEC_mL", 
              g1sign = as.character(sign_tab$Var1[1]),
              g2sign = as.character(sign_tab$Var2[1]),
              h= -4
  ) +
  ggplot_sign(tab = pz_dat, c = "apoptotic_CEC_mL", 
              g1sign = as.character(sign_tab$Var1[2]),
              g2sign = as.character(sign_tab$Var2[2]),
              h= 20
  ) +
  ggplot_sign(tab = pz_dat, c = "apoptotic_CEC_mL", 
              g1sign = as.character(sign_tab$Var1[3]),
              g2sign = as.character(sign_tab$Var2[3]),
              h= 8
  ) +
  ggplot_sign(tab = pz_dat, c = "apoptotic_CEC_mL", 
              g1sign = as.character(sign_tab$Var1[4]),
              g2sign = as.character(sign_tab$Var2[4]),
              h= 32
  ) +
  ggplot_sign(tab = pz_dat, c = "apoptotic_CEC_mL", 
              g1sign = as.character(sign_tab$Var1[5]),
              g2sign = as.character(sign_tab$Var2[5]),
              h= 8
  )


# Plot 7 viable_CEP__mL

# How many sign to plot? 

sign_tab <- tab_multi4_p_viable_CEP__mL[tab_multi4_p_viable_CEP__mL$value < 0.05,]

nrow(sign_tab)

plotTotal_viable_CEP__mL <- plot_half_bp_4_sign(pz_dat, "viable_CEP__mL") +
  ggplot_sign(tab = pz_dat, c = "viable_CEP__mL", 
              g1sign = as.character(sign_tab$Var1[1]),
              g2sign = as.character(sign_tab$Var2[1]),
              h= 10
  ) +
  ggplot_sign(tab = pz_dat, c = "viable_CEP__mL", 
              g1sign = as.character(sign_tab$Var1[2]),
              g2sign = as.character(sign_tab$Var2[2]),
              h= 20
  ) +
  ggplot_sign(tab = pz_dat, c = "viable_CEP__mL", 
              g1sign = as.character(sign_tab$Var1[3]),
              g2sign = as.character(sign_tab$Var2[3]),
              h= 0
  )


# Plot 8 viable_CEC_Ml

# How many sign to plot? 

sign_tab <- tab_multi4_p_viable_CEC_Ml[tab_multi4_p_viable_CEC_Ml$value < 0.05,]

nrow(sign_tab)

plotTotal_viable_CEC_Ml <- plot_half_bp_4_sign(pz_dat, "viable_CEC_Ml") +
  ggplot_sign(tab = pz_dat, c = "viable_CEC_Ml", 
              g1sign = as.character(sign_tab$Var1[1]),
              g2sign = as.character(sign_tab$Var2[1]),
              h= 8
  ) 


# Plot and make the correlation 

# Create the vector for to test, they are the one containing CEP and CEC

vector_g1 <- colnames(pz_dat_AddRNAvir)[grep("_CE",colnames(pz_dat_AddRNAvir))]
vector_g2 <- colnames(pz_dat_AddRNAvir)[6:grep("_CE",colnames(pz_dat_AddRNAvir))[1]-1]

# Vector two change sinse in severe we have also the viral RNA 

vector_g2_sev <-  c(colnames(pz_dat_AddRNAvir)[6:grep("_CE",colnames(pz_dat_AddRNAvir))[1]-1], colnames(pz_dat_AddRNAvir)[grep("Copie",colnames(pz_dat_AddRNAvir))]) 

# Vector two change in recovered since we do not have some blood parameters

rec.tabcol.tmp1 <- pz_dat_AddRNAvir[pz_dat_AddRNAvir$gruppo=="recovered",6:grep("_CE",colnames(pz_dat_AddRNAvir))[1]-1]
vector_g2_rec <- names(which(!is.na(colSums(rec.tabcol.tmp1))))


# Run the correlations function, 4 details see function_2_an&plot.R

for(g1 in vector_g1){
  for(g2 in vector_g2){
    
    assign(paste0("lstTest","_mild_",g1,"vs",g2) , corr_pairs(tab= pz_dat, cat = "mild", g1= g1, g2= g2))
    
  }
}

# Save multiple plot of mild in the list of plot you have to make 3 of them

lPlot_mild <- list()

for(l in ls(pattern="lstTest_mild_")){
  
  if(any(names(get(l)) == "Plot")){
    
    lPlot_mild[[l]] <- get(l)[["Plot"]]
    
  }
  
}


# Severe

for(g1 in vector_g1){
  for(g2 in vector_g2_sev){
    
    assign(paste0("lstTest","_sev_",g1,"vs",g2) , corr_pairs(tab= pz_dat_AddRNAvir, cat = "severe", g1= g1, g2= g2))
    
  }
}

# Save multiple plot of mild in the list of plot you have to make 3 of them

lPlot_sev <- list()

for(l in ls(pattern="lstTest_sev_")){
  
  if(any(names(get(l)) == "Plot")){
    
    lPlot_sev[[l]] <- get(l)[["Plot"]]
    
  }
  
}

# Recovered

for(g1 in vector_g1){
  for(g2 in vector_g2_rec){
    
    assign(paste0("lstTest","_rec_",g1,"vs",g2) , corr_pairs(tab= pz_dat_AddRNAvir, cat = "recovered", g1= g1, g2= g2))
    
  }
}

# Save multiple plot of mild in the list of plot you have to make 3 of them

lPlot_rec <- list()

for(l in ls(pattern="lstTest_rec_")){
  
  if(any(names(get(l)) == "Plot")){
    
    lPlot_rec[[l]] <- get(l)[["Plot"]]
    
  }
  
}



# Explorative to make the plots but not used for the article images


# Used at the beginning but now not anymore used
# 
# 
# p2save <- plot_grid(plotTot_CEPmL, plotTotal_CEC_mL,
#           plotTotal_apototic_CEP_mL,plotTotal_apoptotic_CEC_mL,
#           plotTotal_viable_CEP__mL, plotTotal_viable_CEC_Ml,
#           plotTotal_perc_CEC_Apoptosis
#           , labels = "AUTO", ncol = 2)
# 
# 
# ggsave("fig/bp_CEP_CEC_multi4.png", p2save,height = 10, width = 7)
# ggsave("fig/bp_CEP_CEC_multi4.pdf", p2save,height = 11, width = 6.8)

# 
# # Save the three plots
# 
# library(gridExtra)
# 
# #1
# 
# n <- length(lPlot_mild)
# nCol <-  floor(sqrt(n))
# 
# do.call("grid.arrange",  c(lPlot_mild, ncol= nCol))
# 
# ggsave("fig/corr_mild.pdf", arrangeGrob(grobs = lPlot_mild), height = 12, width = 16)
# ggsave("fig/corr_mild.png", arrangeGrob(grobs = lPlot_mild), height = 12, width = 16)
# 
# 
# #2
# 
# n <- length(lPlot_sev)
# nCol <-  floor(sqrt(n))
# 
# do.call("grid.arrange",  c(lPlot_sev, ncol= nCol))
# 
# ggsave("fig/corr_sev.pdf", arrangeGrob(grobs = lPlot_sev), height = 13, width = 18)
# ggsave("fig/corr_sev.png", arrangeGrob(grobs = lPlot_sev), height = 13, width = 18)
# 
# 
# #3
# 
# n <- length(lPlot_rec)
# nCol <-  floor(sqrt(n))
# 
# do.call("grid.arrange",  c(lPlot_rec, ncol= nCol))
# 
# ggsave("fig/corr_rec.pdf", arrangeGrob(grobs = lPlot_rec), height = 6, width = 10)
# ggsave("fig/corr_rec.png", arrangeGrob(grobs = lPlot_rec), height = 6, width = 10)
# 
# 
# 
# ###
































