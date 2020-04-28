#Ths script organize the figures and the plot generated with the script/plotdata.R

library(cowplot)
library(gridExtra)

# Figure 1 

p2save_fig1 <- plot_grid(plotTotal_CEC_mL,
                    plotTotal_viable_CEC_Ml,
                    plotTotal_apoptotic_CEC_mL,
                    plotTotal_perc_CEC_Apoptosis, 
                    plotTot_CEPmL,
                    plotTotal_viable_CEP__mL,
                    plotTotal_apototic_CEP_mL,
                    lPlot_sev$lstTest_sev_apototic_CEP_mLvsCopie_Pellet,
                    labels = "AUTO", ncol = 4)

ggsave("fig/Fig1_v3.pdf", p2save_fig1,height = 5, width = 12)
ggsave("fig/Fig1_v3.png", p2save_fig1,height = 5, width = 12)                    

# Figure 2 
# remove the figure I included before
lPlot_sev_mod <- lPlot_sev[-grep("lstTest_sev_apototic_CEP_mLvsCopie_Pellet", names(lPlot_sev))]

n <- length(lPlot_sev_mod)
nCol <-  floor(sqrt(n))

p2save_fig2 <- plot_grid(plotlist = lPlot_sev_mod, ncol= nCol,
          labels = "AUTO")

ggsave("fig/Fig2_v2.pdf", p2save_fig2,height = 11, width = 18.1)
ggsave("fig/Fig2_v2.png", p2save_fig2,height = 11, width = 18.1)                    

# Figure 3

n <- length(lPlot_rec)
nCol <-  floor(sqrt(n))

p2save_fig3 <- plot_grid(plotlist = lPlot_rec, ncol= nCol,
                         labels = "AUTO")

ggsave("fig/Fig3.pdf", p2save_fig3,height = 5, width = 10)
ggsave("fig/Fig3.png", p2save_fig3,height = 5, width = 10)                    

# Supp Fig Mild

n <- length(lPlot_mild)
nCol <-  floor(sqrt(n))

p2save_Sfig <- plot_grid(plotlist = lPlot_mild, ncol= nCol,
                         labels = "AUTO")

ggsave("fig/SFigx.pdf", p2save_Sfig, height = 9, width = 15)
ggsave("fig/SFigx.png", p2save_Sfig, height = 9, width = 15)                    



                    



