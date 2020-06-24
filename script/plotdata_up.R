# Load the Functions & Libraries

library(cowplot)

source("script/functions_2_an&plots.R")

# Load a new table
# Introduce the viral RNA data given another time for the correlation
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
names(labels) <- colnames(pz_dat)

# Plot all the significative values for CEP and CEC

# Plot the boxplot 8 of them with the control

# Plot 1  "Total_CEPmL"

# How many sign to plot? 

sign_tab <- tab_multi4_p_Total_CEPmL[tab_multi4_p_Total_CEPmL$value < 0.05,]

nrow(sign_tab)

# Plot the sign looking at the groups in sign tabs

plotTot_CEPmL <- plot_half_bp_4_sign(pz_data_norecb, "Total_CEPmL") +
  ggplot_sign(tab = pz_data_norecb, field = "Total_CEPmL", 
              g1sign = as.character(sign_tab$Var1[1]),
              g2sign = as.character(sign_tab$Var2[1]),
              h= 10
                ) +
  ggplot_sign(tab = pz_data_norecb, field = "Total_CEPmL", 
              g1sign = as.character(sign_tab$Var1[2]),
              g2sign = as.character(sign_tab$Var2[2]),
              h= 20
  ) +
  ggplot_sign(tab = pz_data_norecb, field = "Total_CEPmL", 
              g1sign = as.character(sign_tab$Var1[3]),
              g2sign = as.character(sign_tab$Var2[3]),
              h= 0
  )


# Plot 2  "Total_CECmL"

# How many sign to plot? 

sign_tab <- tab_multi4_p_Total_CEC_mL[tab_multi4_p_Total_CEC_mL$value < 0.05,]

nrow(sign_tab)

plotTotal_CEC_mL <- plot_half_bp_4_sign(pz_data_norecb, "Total_CEC_mL") +
  ggplot_sign(tab = pz_data_norecb, field = "Total_CEC_mL", 
              g1sign = as.character(sign_tab$Var1[1]),
              g2sign = as.character(sign_tab$Var2[1]),
              h= 0
  ) +
  ggplot_sign(tab = pz_data_norecb, field = "Total_CEC_mL", 
              g1sign = as.character(sign_tab$Var1[2]),
              g2sign = as.character(sign_tab$Var2[2]),
              h= 16
  ) +
  ggplot_sign(tab = pz_data_norecb, field = "Total_CEC_mL", 
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


plotTotal_perc_CEC_Apoptosis <- plot_half_bp_4_sign(pz_data_norecb, "perc_CEC_Apoptosis") +
  ggplot_sign(tab = pz_data_norecb, field = "perc_CEC_Apoptosis", 
              g1sign = as.character(sign_tab$Var1[1]),
              g2sign = as.character(sign_tab$Var2[1]),
              h= 16
  ) +
  ggplot_sign(tab = pz_data_norecb, field = "perc_CEC_Apoptosis", 
              g1sign = as.character(sign_tab$Var1[2]),
              g2sign = as.character(sign_tab$Var2[2]),
              h= 24
  ) +
  ggplot_sign(tab = pz_data_norecb, field = "perc_CEC_Apoptosis", 
              g1sign = as.character(sign_tab$Var1[3]),
              g2sign = as.character(sign_tab$Var2[3]),
              h= 8
  )


# Plot 5 apototic_CEP_mL

# How many sign to plot? 

sign_tab <- tab_multi4_p_apototic_CEP_mL[tab_multi4_p_apototic_CEP_mL$value < 0.05,]

nrow(sign_tab)

plotTotal_apototic_CEP_mL <- plot_half_bp_4_sign(pz_data_norecb, "apototic_CEP_mL") +
  ggplot_sign(tab = pz_data_norecb, field = "apototic_CEP_mL", 
              g1sign = as.character(sign_tab$Var1[1]),
              g2sign = as.character(sign_tab$Var2[1]),
              h= 10
  ) +
  ggplot_sign(tab = pz_data_norecb, field = "apototic_CEP_mL", 
              g1sign = as.character(sign_tab$Var1[2]),
              g2sign = as.character(sign_tab$Var2[2]),
              h= 20
  ) +
  ggplot_sign(tab = pz_data_norecb, field = "apototic_CEP_mL", 
              g1sign = as.character(sign_tab$Var1[3]),
              g2sign = as.character(sign_tab$Var2[3]),
              h= 0
  )


# Plot 6 apoptotic_CEC_mL

# How many sign to plot? 

sign_tab <- tab_multi4_p_apoptotic_CEC_mL[tab_multi4_p_apoptotic_CEC_mL$value < 0.05,]

nrow(sign_tab)

plotTotal_apoptotic_CEC_mL <- plot_half_bp_4_sign(pz_data_norecb, "apoptotic_CEC_mL") +
  ylim(0, max(pz_data_norecb[,"apoptotic_CEC_mL"]) + 34) +
  
  
  ggplot_sign(tab = pz_data_norecb, field = "apoptotic_CEC_mL", 
              g1sign = as.character(sign_tab$Var1[1]),
              g2sign = as.character(sign_tab$Var2[1]),
              h= -4
  ) +
  ggplot_sign(tab = pz_data_norecb, field = "apoptotic_CEC_mL", 
              g1sign = as.character(sign_tab$Var1[2]),
              g2sign = as.character(sign_tab$Var2[2]),
              h= 20
  ) +
  ggplot_sign(tab = pz_data_norecb, field = "apoptotic_CEC_mL", 
              g1sign = as.character(sign_tab$Var1[3]),
              g2sign = as.character(sign_tab$Var2[3]),
              h= 8
  ) +
  ggplot_sign(tab = pz_data_norecb, field = "apoptotic_CEC_mL", 
              g1sign = as.character(sign_tab$Var1[4]),
              g2sign = as.character(sign_tab$Var2[4]),
              h= 32
  ) +
  ggplot_sign(tab = pz_data_norecb, field = "apoptotic_CEC_mL", 
              g1sign = as.character(sign_tab$Var1[5]),
              g2sign = as.character(sign_tab$Var2[5]),
              h= 8
  )


# Plot 7 viable_CEP__mL

# How many sign to plot? 

sign_tab <- tab_multi4_p_viable_CEP__mL[tab_multi4_p_viable_CEP__mL$value < 0.05,]

nrow(sign_tab)

plotTotal_viable_CEP__mL <- plot_half_bp_4_sign(pz_data_norecb, "viable_CEP__mL") +
  ggplot_sign(tab = pz_data_norecb, field = "viable_CEP__mL", 
              g1sign = as.character(sign_tab$Var1[1]),
              g2sign = as.character(sign_tab$Var2[1]),
              h= 10
  ) +
  ggplot_sign(tab = pz_data_norecb, field = "viable_CEP__mL", 
              g1sign = as.character(sign_tab$Var1[2]),
              g2sign = as.character(sign_tab$Var2[2]),
              h= 20
  ) +
  ggplot_sign(tab = pz_data_norecb, field = "viable_CEP__mL", 
              g1sign = as.character(sign_tab$Var1[3]),
              g2sign = as.character(sign_tab$Var2[3]),
              h= 0
  )


# Plot 8 viable_CEC_Ml

# How many sign to plot? 

sign_tab <- tab_multi4_p_viable_CEC_Ml[tab_multi4_p_viable_CEC_Ml$value < 0.05,]

nrow(sign_tab)

plotTotal_viable_CEC_Ml <- plot_half_bp_4_sign(pz_data_norecb, "viable_CEC_Ml") +
  ggplot_sign(tab = pz_data_norecb, field = "viable_CEC_Ml", 
              g1sign = as.character(sign_tab$Var1[1]),
              g2sign = as.character(sign_tab$Var2[1]),
              h= 8
  ) +
  ggplot_sign(tab = pz_data_norecb, field = "viable_CEC_Ml", 
              g1sign = as.character(sign_tab$Var1[2]),
              g2sign = as.character(sign_tab$Var2[2]),
              h= 16
  ) 


# Plot and make the correlation 

# Create the vector for to test, they are the one containing CEP and CEC

vector_g1 <- colnames(pz_data_norecb)[grep("_CE",colnames(pz_data_norecb))]
vector_g2 <- colnames(pz_data_norecb)[6:grep("_CE",colnames(pz_data_norecb))[1]-1]

# Vector two change sinse in severe we have also the viral RNA 

vector_g2_sev <-  c(colnames(pz_data_norecb)[6:grep("_CE",colnames(pz_data_norecb))[1]-1], colnames(pz_data_norecb)[grep("Copie",colnames(pz_data_norecb))]) 
vector_g2_mild <- vector_g2_sev


# Vector two change in recovered since we do not have some blood parameters

rec.tabcol.tmp1 <- pz_data_norecb[pz_data_norecb$gruppo=="recovered",6:grep("_CE",colnames(pz_data_norecb))[1]-1]

vector_g2_rec <- names(which(!is.na(colSums(rec.tabcol.tmp1))))
# Just to be sure I do not exclude fields only coz they have some missing
vector_g2_rec_alt <- names(which(!apply(rec.tabcol.tmp1, 2, function(x) all(is.na(x)))))


# Run the correlations function, 4 details see function_2_an&plot.R

for(g1 in vector_g1){
  for(g2 in vector_g2_mild){
    
    assign(paste0("lstTest","_mild_",g1,"vs",g2) , corr_pairs(tab= pz_data_norecb, cat = "mild", g1= g1, g2= g2))
    
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
    
    assign(paste0("lstTest","_sev_",g1,"vs",g2) , corr_pairs(tab= pz_data_norecb, cat = "severe", g1= g1, g2= g2))
    
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
    
    assign(paste0("lstTest","_rec_",g1,"vs",g2) , corr_pairs(tab= pz_data_norecb, cat = "recovered", g1= g1, g2= g2))
    
  }
}

# Save multiple plot of mild in the list of plot you have to make 3 of them

lPlot_rec <- list()

for(l in ls(pattern="lstTest_rec_")){
  
  if(any(names(get(l)) == "Plot")){
    
    lPlot_rec[[l]] <- get(l)[["Plot"]]
    
  }
  
}

# Make the table with the data 
# 
# write.table(pz_data_norecb, col.names = names(labels), row.names = F, quote = F, sep = ",", file = "tab/pz_Covid19_hemdata_upv2.csv")

# Make the plot paired #1 

# How many sign to plot? 

sign_tab <- tab_s_paired_Total_CEC_mL[tab_s_paired_Total_CEC_mL$value < 0.05,]

nrow(sign_tab)

Paired_Total_CEC_mL_p <- plot_two_half_bp(tab = pz_dat, field = "Total_CEC_mL", g1="recovered", g2= "recovered_b")


# Make the plot paired #2

sign_tab <- tab_s_paired_Total_CEPmL[tab_s_paired_Total_CEPmL$value < 0.05,]

nrow(sign_tab)

Paired_Total_CEP_mL_p <- plot_two_half_bp(tab = pz_dat, field = "Total_CEPmL", g1="recovered", g2= "recovered_b")

# Make the plot paired #3

sign_tab <- tab_s_paired_viable_CEC_Ml[tab_s_paired_viable_CEC_Ml$value < 0.05,]

nrow(sign_tab)

Paired_viable_CEC_Ml_p <- plot_two_half_bp(tab = pz_dat, field = "viable_CEC_Ml", g1="recovered", g2= "recovered_b")


# Make the plot paired #4

sign_tab <- tab_s_paired_viable_CEP__mL[tab_s_paired_viable_CEP__mL$value < 0.05,]

nrow(sign_tab)

Paired_viable_CEP_Ml_p <- plot_two_half_bp(tab = pz_dat, field = "viable_CEP__mL", g1="recovered", g2= "recovered_b") 

