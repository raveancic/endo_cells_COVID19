# Load the libraries

library(scales)
library(reshape2)
library(ggpubr)
library(ggsignif)
library(ggplot2)
library(gghalves)
library(dplyr)
# Function to apply for single comparison

# In the table the groups need to be nder a column with the name gruppo

# Summary Information using dplyr

summ_info <- function(tab, field){
  
  sub_tab <- tab[which(!is.na(tab[,field])),]
  
  # Mean and sd for the field of interest
  res <- group_by(sub_tab, gruppo) %>%
    summarise(
      count = n(),
      mean = mean(get(field), na.rm = TRUE),
      sd = sd(get(field), na.rm = TRUE)
    )
  
  return(res)
}


# It does t test or wilcoxon according to the normality moreover apply either one or another t test if the homogeneity of the variance is true or not

# The field is the ctaegory while g1 and g2 are the groups under the column gruppo

test_single_pairs <- function(tab, field, g1, g2){
  
  # remove NA
  sub_tab.tmp1 <- tab[which(!is.na(tab[,field])),]
  
  # filter the two groups
  sub_tab <- sub_tab.tmp1[sub_tab.tmp1$gruppo== g1 | sub_tab.tmp1$gruppo== g2,]
  
  # Shapiro_Wilk normality on first group
  p.sh1 <- with(sub_tab, shapiro.test(get(field)[gruppo== g1]))$p.value 
  
  
  # Shapiro_Wilk normality on second group
  p.sh2 <- with(sub_tab, shapiro.test(get(field)[gruppo== g2]))$p.value 
  
  #  F-test to test the homogeneity in variances 
  res.ftest <- var.test(get(field) ~ gruppo, data = sub_tab)$p.value
  
  # If the three are not sign we have normality and homogeneity
  if(p.sh1 > 0.05 & p.sh2 > 0.05 & res.ftest > 0.05){
    
    res <- with(sub_tab, t.test(get(field)[gruppo== g1],get(field)[gruppo== g2], var.equal = T))
    
  } 
  # If the shap are not sign while the f is sign we have normality but no homogeneity therefore var.equal= F
  else if (p.sh1 > 0.05 & p.sh2 > 0.05 & res.ftest < 0.05){
    res <-     with(sub_tab, t.test(get(field)[gruppo== g1],get(field)[gruppo== g2], var.equal = F))
    
  } 
  
  # If the shap are sign or one we have no normality and therefore wilcoxon non parametric
  else if(p.sh1 < 0.05 | p.sh2 < 0.05){
    res <- with(sub_tab, wilcox.test(get(field)[gruppo== g1],get(field)[gruppo== g2]))
  }
  return(res)
}



test_single_pairs_Paired <- function(tab, field, g1, g2, alt){
  
  # tab= pz_dat
  # 
  # field = "Total_CEPmL"

  # remove NA
  sub_tab.tmp1 <- tab[which(!is.na(tab[,field])),]

  #
  # g1= "recovered"
  # g2= "recovered_b"

  # filter the two groups
  sub_tab.tmp2 <- sub_tab.tmp1[sub_tab.tmp1$gruppo== g1 | sub_tab.tmp1$gruppo== g2,]
  
  sub_tab <- rbind(sub_tab.tmp2[match(gsub("_B", "",sub_tab.tmp2$Codifica_IEO[sub_tab.tmp2$gruppo==g2]),
                                 sub_tab.tmp2$Codifica_IEO[sub_tab.tmp2$gruppo==g1]),],
                   sub_tab.tmp2[sub_tab.tmp2$gruppo==g2,])
  
  
  
  # Shapiro_Wilk normality on first group
  p.sh1 <- with(sub_tab, shapiro.test(get(field)[gruppo== g1]))$p.value 
  
  
  # Shapiro_Wilk normality on second group
  p.sh2 <- with(sub_tab, shapiro.test(get(field)[gruppo== g2]))$p.value 
  
  #  F-test to test the homogeneity in variances 
  res.ftest <- var.test(get(field) ~ gruppo, data = sub_tab)$p.value
  
  # If the three are not sign we have normality and homogeneity
  if(p.sh1 > 0.05 & p.sh2 > 0.05 & res.ftest > 0.05){
    
    res <- with(sub_tab, t.test(get(field)[gruppo== g1],get(field)[gruppo== g2], var.equal = T, paired = T,  alternative = alt))
    
  } 
  # If the shap are not sign while the f is sign we have normality but no homogeneity therefore var.equal= F
  else if (p.sh1 > 0.05 & p.sh2 > 0.05 & res.ftest < 0.05){
    res <-     with(sub_tab, t.test(get(field)[gruppo== g1],get(field)[gruppo== g2], var.equal = F, paired = T, alternative = alt))
    
  } 
  
  # If the shap are sign or one we have no normality and therefore wilcoxon non parametric
  else if(p.sh1 < 0.05 | p.sh2 < 0.05){
    res <- with(sub_tab, wilcox.test(get(field)[gruppo== g1],get(field)[gruppo== g2], paired = T, 
                                     alternative = alt))
  }
  return(res)
}


# It does pairwise t test if the multi groups are normal and if the variance is homogeneous consider it as above; otherwise it does Wilcoxon pairwise all the methods have the p-value adjusted for BH

test_multi_pairs <- function(tab, field, g1, g2, g3) {
  
  # Exclude the NA
  sub_tab.tmp1 <- tab[which(!is.na(tab[,field])),]
  
  # Filter the groups
  sub_tab <- sub_tab.tmp1[sub_tab.tmp1$gruppo== g1 | sub_tab.tmp1$gruppo== g2 | sub_tab.tmp1$gruppo== g3,]
  
  #Order the level
  sub_tab$gruppo <- ordered(sub_tab$gruppo,levels = c(g1, g2, g3))
  
  # Shapiro_Wilk normality on first group
  p.sh1 <- with(sub_tab, shapiro.test(get(field)[gruppo== g1]))$p.value 
  
  # Shapiro_Wilk normality on second group
  p.sh2 <- with(sub_tab, shapiro.test(get(field)[gruppo== g2]))$p.value 
  
  # Shapiro_Wilk normality on third group
  p.sh3 <- with(sub_tab, shapiro.test(get(field)[gruppo== g3]))$p.value
  
  # bartlett test to verify the variance homogeneity
  pvarv <- bartlett.test(get(field) ~ gruppo, data = sub_tab)$p.value
  
  # As above but with the three groups
  
  if(p.sh1 > 0.05 & p.sh2 > 0.05 & p.sh3 > 0.05 & pvarv > 0.05){
    
    res <- with(sub_tab, pairwise.t.test(get(field), gruppo, p.adjust.method = "BH", pool.sd = T))
    
  } else if(p.sh1 > 0.05 & p.sh2 > 0.05 & p.sh3 > 0.05 & pvarv < 0.05){
    
    res <- with(sub_tab, pairwise.t.test(get(field), gruppo, p.adjust.method = "BH", pool.sd = F))
    
  } else if(p.sh1 < 0.05 | p.sh2 < 0.05 | p.sh3 < 0.05 ){
    
    res <- with(sub_tab, pairwise.wilcox.test(get(field), gruppo, p.adjust.method = "BH"))
    
  }
  return(res)
  
}

# It does pairwise t test if the multi groups are normal and if the variance is homogeneous consider it as above; otherwise it does Wilcoxon pairwise all the methods have the p-value adjusted for BH, dfferently from the function above it does it for 4 groups

test_multi_pairs_4 <- function(tab, field, g1, g2, g3, g4) {
  
  sub_tab.tmp1 <- tab[which(!is.na(tab[,field])),]
  
  sub_tab <- sub_tab.tmp1[sub_tab.tmp1$gruppo== g1 | sub_tab.tmp1$gruppo== g2 | sub_tab.tmp1$gruppo== g3| sub_tab.tmp1$gruppo== g4,]
  
  #Order the level
  
  sub_tab$gruppo <- ordered(sub_tab$gruppo,levels = c(g1, g2, g3, g4))
  
  # Shapiro_Wilk normality on first group
  
  p.sh1 <- with(sub_tab, shapiro.test(get(field)[gruppo== g1]))$p.value 
  
  # Shapiro_Wilk normality on second group
  
  p.sh2 <- with(sub_tab, shapiro.test(get(field)[gruppo== g2]))$p.value 
  
  # Shapiro_Wilk normality on third group
  
  p.sh3 <- with(sub_tab, shapiro.test(get(field)[gruppo== g3]))$p.value
  
  # Shapiro_Wilk normality on fourth group
  
  p.sh4 <- with(sub_tab, shapiro.test(get(field)[gruppo== g4]))$p.value
  
  
  pvarv <- bartlett.test(get(field) ~ gruppo, data = sub_tab)$p.value
  
  if(p.sh1 > 0.05 & p.sh2 > 0.05 & p.sh3 > 0.05 & p.sh4 > 0.05 & pvarv > 0.05){
    
    res <- with(sub_tab, pairwise.t.test(get(field), gruppo, p.adjust.method = "BH", pool.sd = T))
    
  } else if(p.sh1 > 0.05 & p.sh2 > 0.05 & p.sh3 > 0.05 & p.sh4 > 0.05 & pvarv < 0.05){
    
    res <- with(sub_tab, pairwise.t.test(get(field), gruppo, p.adjust.method = "BH", pool.sd = F))
    
  } else if(p.sh1 < 0.05 | p.sh2 < 0.05 | p.sh3 < 0.05 | p.sh4 < 0.05){
    
    res <- with(sub_tab, pairwise.wilcox.test(get(field), gruppo, p.adjust.method = "BH"))
    
  }
  return(res)
  
}

# This script gives you the significance in term of * which stand for *** < 0.001, ** < 0.01, * < 0.05

getSigbp <- function(table, field, g1, g2) {
  
  # It retrieve the table t calling the function multi pairs 4
  
  t <- melt(test_multi_pairs_4(table, field, g1= "control", g2= "recovered", g3= "mild", g4= "severe")$p.value, na.rm = T)
  
  # It gives you the pvalue on the two groups of interest
  
  p_value <- t[t$Var1==g2 & t$Var2==g1,"value"]
  
  if ( p_value < 0.001 ) {
    return('***')
  } else if ( p_value < 0.01 ) {
    return('**')
  } else if ( p_value < 0.05 ) {
    return('*')
  } else {
    return('n.s.')
  }
}


# Functions for plot half, it uses gghalves but before there is the preparation of the table

plot_half_bp_4_sign <- function(tab, field){
  
  # filter gruppo and the field of interest
  tab_ggplot <- as.data.frame(tab[,c("gruppo",field)])
  
  # Order the level
  tab_ggplot$gruppo <- ordered(tab_ggplot$gruppo,levels =  c("control", "recovered", "mild", "severe"))
  
  # Use gghalves function
  ggplot(tab_ggplot, aes(x= gruppo, y = get(field)) ) + 
    geom_half_boxplot(aes(fill = gruppo), side = "r", errorbar.draw = TRUE,
                      outlier.color = NA) +
    geom_half_dotplot(aes(fill = gruppo),
                      dotsize= 0.9,
                      method="histodot", stackdir="down")+
    scale_fill_manual(values = colbp) +
    scale_color_manual(values = colbp) +
    theme_classic()+
    theme(legend.position = "none", 
          axis.text.x = element_text(angle=45, hjust = 1))+
    ylab(label = labels[field])+
    xlab(label = "") +
    ylim(0, max(tab_ggplot[,field]) + 25)+
    scale_x_discrete(labels= c("control"= paste0("control (",length(which(tab_ggplot$gruppo=="control")) ,")"),
                               "recovered"= paste0("recovered (",length(which(tab_ggplot$gruppo=="recovered")) ,")"),
                               "mild"= paste0("mild (",length(which(tab_ggplot$gruppo=="mild")) ,")"),
                               "severe"= paste0("severe (",length(which(tab_ggplot$gruppo=="severe")) ,")")
                               
                               
                                 ), )
    
  
}

# Function to plot sign

# Use the function of ggsignif

ggplot_sign <- function(tab, field, g1sign, g2sign, h){
  
  # Select the field of interest
  tab_ggplot <- as.data.frame(tab[,c("gruppo",field)])
  
  # Order the groups
  tab_ggplot$gruppo <- ordered(tab_ggplot$gruppo,levels =  c("control", "recovered", "mild", "severe"))
  
  # Apply the function of ggsinf
  geom_signif(y_position = c(max(tab_ggplot[,field]) + h ) , 
              tip_length = 0.01, 
              # textsize = 10,
              vjust = .7,
              xmin = which(names(colbp)== g1sign)+.05, 
              xmax = which(names(colbp)== g2sign)-.05,
              #Run the function above
              annotation = getSigbp(table = tab_ggplot, 
                                    field = field, 
                                    g1= g1sign,
                                    g2= g2sign 
              )  
  ) 
  
  
  
}

# Function for the correlation in this case cat is equal to field of before, here the shapiro test has been employed to test the normality again and if they were normal the pearson was used otherwise the spearman, then if the p-value of the correlation was sign so the plot was produced and inserted in a list

corr_pairs <- function(tab, cat, g1, g2){
  
  l.res <- list()
  
  tbl_test= tab[tab$gruppo==cat, c(g1,g2)]
  
  # Test if the two distributions are normally distribute
  
  p.sh1 <- with(tbl_test, shapiro.test(get(g1)))$p.value
  
  p.sh2 <- with(tbl_test, shapiro.test(get(g2)))$p.value
  
  if(p.sh1 > 0.05 & p.sh2 > 0.05){
    
    p.corr <- with(tbl_test, cor.test(get(g1), get(g2),  method = "pearson"))
    
    # If pearson sign the plot is produced
    if(p.corr$p.value < 0.05){
      
      p <- ggscatter(tbl_test, 
                     x =g1, 
                     y = g2,
                     add = "reg.line",
                     conf.int = TRUE,
                     cor.coef = F, cor.method = "pearson",
                     xlab = labels[g1], ylab = labels[g2],
                     add.params = list(color = "brown1", fill = "indianred1"), ellipse.alpha=0.9,
                     shape= 21,
                     color = "black",
                     fill= colbp[cat],
                     # title = "Pearson",
                     # cor.coeff.args = list(method= "pearson",
                     #                       label.x.npc= "center",
                     #                       label.y= max(tbl_test[,g2]) + 5,
                     #                       label.sep= "\n"
                     #                       )
                     ) +
        theme( plot.margin = unit(c(.5,.5,.8,.7), "cm"),
               plot.background = element_rect(color = "black"),
             )+
        # This stat_cor allow to place the p-value where you want and then also the number of digits to be indicated 
        stat_cor(method= "pearson",
                 label.x.npc= ifelse(p.corr$estimate > 0, "left", "center"),
                 label.y.npc = "top" ,
                 label.sep= ",",
                 digits = 3)
    }
    
  } 
  
  # If the data are not normally distributed
  else{
    
    p.corr <- with(tbl_test, cor.test(get(g1), get(g2),  method = "spearman"))
    
    # If the pval of the spearman was sign the plot will be generated
    if(p.corr$p.value < 0.05){
      
      p <- ggscatter(tbl_test, 
                     x =g1, 
                     y = g2,
                     add = "reg.line",
                     conf.int = TRUE,
                     cor.coef = F, cor.method = "spearman",
                     xlab = labels[g1], ylab = labels[g2],
                     add.params = list(color = "brown1", fill =  "indianred1"), ellipse.alpha=0.9,
                     shape= 21,
                     color = "black",
                     fill= colbp[cat],
                     # title = "Spearman",
                     # cor.coeff.args = list(method= "spearman",
                     #                       label.x.npc= "center",
                     #                       label.y= max(tbl_test[,g2]) + 5,
                     #                       label.sep= "\n"
                     #                       )
                     ) +
        theme( plot.margin = unit(c(.7,.7,.8,.7), "cm"),
               plot.background = element_rect(color = "black"),
        )+
        stat_cor(method="spearman",
          label.x.npc= ifelse(p.corr$estimate > 0, "left", "center"),
                 label.y.npc = "top",
                 label.sep= ",",
                 digits = 3)
    }
    
    
  }
  
  # At this point the list of the result can be filled with the p-valye abd with the plot if the p-value was significative
  
  l.res[["TestCorr"]] <- p.corr
  if(l.res$TestCorr$p.value   < 0.05){l.res[["Plot"]] <- p
  }


  return(l.res)
  
}


# Functions for plot half, it uses gghalves but before there is the preparation of the table

plot_two_half_bp <- function(tab, field, g1, g2){
  
  # tab= pz_dat
  # field= "Total_CEPmL"
  # g1= "recovered"
  # g2= "recovered_b"
  # 
  
  # filter gruppo and the field of interest
  tab_ggplot.tmp1 <- as.data.frame(tab[tab$gruppo==g1 | tab$gruppo==g2,c("Codifica_IEO","gruppo",field)])
  
  tab_ggplot <- rbind(tab_ggplot.tmp1[match(gsub("_B", "",tab_ggplot.tmp1$Codifica_IEO[grep("_b", tab_ggplot.tmp1$gruppo)]),
                                            tab_ggplot.tmp1$Codifica_IEO),],
                      tab_ggplot.tmp1[grep("_b",tab_ggplot.tmp1$gruppo),])
  
  
  # Order the level
  tab_ggplot$gruppo <- ordered(tab_ggplot$gruppo,levels =  c(g1,g2))
  
  # Use gghalves function
  ggplot(tab_ggplot, aes(x= gruppo, y = get(field)) ) + 
    geom_half_boxplot(aes(alpha = gruppo), fill= colbp[match(g1, names(colbp))], side = "l", errorbar.draw = TRUE,
                      outlier.color = NA) +
    geom_half_dotplot(aes(alpha = gruppo),
                      fill= colbp[match(g1, names(colbp))],
                      dotsize= 0.9,
                      method="histodot", stackdir="up")+
    # scale_fill_manual(values = colbp[match(g1, names(colbp))]) +
    scale_alpha_manual(values = c(1,.3)) +
    # scale_color_manual(values = colbp[match(g1, names(colbp))]) +
    theme_classic()+
    theme(legend.position = "none", 
          axis.text.x = element_text(angle=45, hjust = 1))+
    ylab(label = labels[field])+
    xlab(label = "") +
    ylim(0, max(tab_ggplot[,field]) + 25)
  # +
    # theme(plot.background = element_rect(color = "black"))
  
}








