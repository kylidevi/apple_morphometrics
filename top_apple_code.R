# libraries
library(tidyverse)
library(dplyr)

# load in data
setwd("C:/Users/User/Desktop/COOP02/code/apple_shape")
MyData <- read.csv("apple_metadata_0905.csv") %>% dplyr::select(6, 23:35)

#Then the colour trait distributions for the nine most commercially popular accessions, of which only six were found in the data (hereby called the “top six”), in the USA were overlayed onto the distributions for the 565 samples. The top six consists of the Honeycrisp, Granny Smith, Empire, Gala, Red Delicious, and McIntosh accessions.

#https://usapple.org/apple-varieties

#list of top 9 apple types, only 5 found (Red Delicious, Honeycrisp, McIntosh, Empire, Gala)
top_nine <- MyData %>% filter(apple_id %in% c(48, 205, 160, 167, 1223, 222, 57, 235, 171))

# top 6 find out which ones create table for min and max vaules of the top 6
MinMaxTable <- top_nine %>%  
  pivot_longer((!apple_id & !PLANTID),names_to = "pheno", values_to = "value") 

#order the labels 
factor_order_mean <- c("width_reml_lsmeans_predict", "length_reml_lsmeans_predict", "area_reml_lsmeans_predict", "solidity_reml_lsmeans_predict", "asymmetry_reml_lsmeans_predict", "aspect_ratio","PC1_reml_lsmeans_predict", "PC2_reml_lsmeans_predict","PC3_reml_lsmeans_predict", "PC4_reml_lsmeans_predict", "PC5_reml_lsmeans_predict", "PC6_reml_lsmeans_predict")

#Mean vaule data table
MyDataTable_mean <- MyData %>%  
  pivot_longer((!apple_id & !PLANTID),names_to = "pheno", values_to = "value") %>% 
  filter(pheno %in% factor_order_mean )

# tables for mins and maxs 
MinMaxTable_mean <- MinMaxTable %>% 
  filter(pheno %in% factor_order_mean )


#caluclate min of each trait 
min_top_nine_mean <- MinMaxTable_mean %>% group_by(pheno) %>% slice(which.min(value))

#caluclate max of each trait 
max_top_nine_mean <- MinMaxTable_mean %>% group_by(pheno) %>% slice(which.max(value))

#calculate the mean of the top nine 
mean_top_nine_mean <- MinMaxTable_mean %>% group_by(pheno) %>% summarise(mean = mean(value, na.rm=T)) 

#mean table
min_max_top_nine_mean <- left_join(min_top_nine_mean, max_top_nine_mean, by = "pheno") 
min_max_top_nine_mean <- left_join(min_max_top_nine_mean, mean_top_nine_mean, by = "pheno") 

#remove apple id
min_max_top_nine_mean <- min_max_top_nine_mean %>% dplyr::select(-c(apple_id.x, apple_id.y))

#rename columns 
colnames(min_max_top_nine_mean)[colnames(min_max_top_nine_mean)=="value.x"] <- "min_top" 
colnames(min_max_top_nine_mean)[colnames(min_max_top_nine_mean)=="value.y"] <- "max_top" 
colnames(min_max_top_nine_mean)[colnames(min_max_top_nine_mean)=="mean"] <- "mean_top"  


#make histograms for figure 2 
figure_2 <- MyDataTable_mean %>% 
  
  ggplot(aes(x=value)) + 
  
  geom_rect(data = min_max_top_nine_mean, aes(ymin = 0, ymax = Inf, xmin = min_top, xmax = max_top), fill = "lightgrey", inherit.aes = FALSE) + # this is the top apples background
  
  geom_histogram(bins=30, size = 0.8) + # bin sizes may be good 
  
  ylab("Count") + 
  
  scale_x_continuous(expand = c(0,0)) + 
  
  scale_y_continuous(expand = c(0,0)) + 
  
  facet_wrap(~factor(pheno, c("width_reml_lsmeans_predict", "length_reml_lsmeans_predict", "area_reml_lsmeans_predict", "solidity_reml_lsmeans_predict", "asymmetry_reml_lsmeans_predict", "aspect_ratio","PC1_reml_lsmeans_predict", "PC2_reml_lsmeans_predict","PC3_reml_lsmeans_predict", "PC4_reml_lsmeans_predict", "PC5_reml_lsmeans_predict", "PC6_reml_lsmeans_predict")), scale='free', nrow=4, strip.position = "bottom") +
  
  theme(strip.placement = "outside", strip.background = element_blank(), panel.spacing = unit(2, "lines"), axis.title.x = element_blank(), strip.text.x = element_text(size = 8, face = "bold"), axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) 

print(figure_2)

# getting the mean from each top commercial apple
top_five <- MinMaxTable_mean %>% group_by(apple_id) %>% 
  filter(row_number() %in% c(1:12))

top_five$apple_id <- factor(top_five$apple_id)

# adding the mean line to the plot
figure_3 <- figure_2 + geom_vline(aes(colour = PLANTID, xintercept = value), top_five)

print(figure_3)
ggsave("distributions.png", plot = figure_3, units = "in", width = 12, height = 15)



#-----------------------

### Some code on correlation with a loop to test significance using bonferroni and Spearman's 

### make corlation matxix for colour traits https://github.com/zoemigicovsky/grapevine_rootstocks/blob/master/figure4.R

ColourCor <- MyData %>% 
  select(-apple_id)

#create an empty matrix for correlations, make matrix with no data and just col and rows from final pheno table.
pairwise_pheno_correlations=matrix(,ncol(ColourCor), ncol(ColourCor))

rownames(pairwise_pheno_correlations)=colnames(ColourCor)

colnames(pairwise_pheno_correlations)=colnames(ColourCor)

#marix for pvalues.
pairwise_pheno_correlations_pval=matrix(,ncol(ColourCor), ncol(ColourCor))

rownames(pairwise_pheno_correlations_pval)=colnames(ColourCor)

colnames(pairwise_pheno_correlations_pval)=colnames(ColourCor)

#Treat the data as all quantitative data and run pearson's correlation.
for (i in 1:ncol(ColourCor)) {
  phenoname_x = colnames(ColourCor)[i]
  for (j in 1:ncol(ColourCor)) {
    phenoname_y = colnames(ColourCor)[j]
    pairwise_pheno_correlations[j,i]=cor.test(ColourCor[,i], ColourCor[,j], method = "spearman")$estimate
    pairwise_pheno_correlations_pval[j,i]= cor.test(ColourCor[,i], ColourCor[,j], method = "spearman")$p.value
  }
}

#Bonferroni correct.
pairwise_pheno_correlations_pval[lower.tri(pairwise_pheno_correlations_pval)] = NA

pairwise_pheno_correlations_pval[upper.tri(pairwise_pheno_correlations_pval)] = p.adjust(pairwise_pheno_correlations_pval[upper.tri(pairwise_pheno_correlations_pval)], method = "bonferroni")

#How many comparisons?
length(pairwise_pheno_correlations_pval[lower.tri(pairwise_pheno_correlations_pval)] )
#66


#how many are significant?
table(pairwise_pheno_correlations_pval[upper.tri(pairwise_pheno_correlations_pval)] <0.05)

#FALSE  TRUE 
#26     40 
#40 / 66 are significant 