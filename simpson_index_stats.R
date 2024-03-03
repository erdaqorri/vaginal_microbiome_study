### Alpha Diversity: Simpson Index ###

library(readxl)
library(vegan)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plyr)
library(tseries)
library(rstatix)


abundancies_form_cleaned <- read_excel("input_file.xsxl")
abundancies_form_cleaned.df <- as.data.frame(abundancies_form_cleaned)
rownames(abundancies_form_cleaned.df) <- abundancies_form_cleaned.df$index
abundancies_form_cleaned.df <- abundancies_form_cleaned.df[, -1]

# Convert to matrix
abundancies_form_cleaned.mtx <- as.matrix(abundancies_form_cleaned.df)


## Calculate Simpson Index for all samples

simpson_index_all <- diversity(abundancies_form_cleaned_final_num.df, index = "simpson")
simpson_index_df <- data.frame(sample_id = rownames(abundancies_form_cleaned_final.mtx),
                               simpson_idx = simpson_index_all)

## Read in the samples positive for the presence of vaginal bacteria and the negative ones

pos_neg_simpson_index <- read_excel("pos_neg_input.xsxl")

pos_neg_simpson_idx_plot <- ggplot(pos_neg_simpson_index, aes(x=Group, y=simpson_idx, fill=Group )) + 
  # stat_boxplot(aes(fill=Group), geom = 'errorbar', width = 0.5, height = 0.4, position = position_dodge(width = 0.5)) +
  geom_jitter(aes(fill = Group), color = "black", size = 1, shape = 21, alpha = 0.3) +
  geom_boxplot(aes(fill = Group), width = 0.9, outlier.shape = 1)  +
  scale_fill_brewer(palette = 'Set2') +
  # geom_jitter(aes(fill = Group), color = "black", size = 1, shape = 21, alpha = 0.4) +
  # geom_boxplot(notch = F, outlier.shape = 22, fill = "transparent", color = c("pink", "blue"),
  #              size = 2) +
  # geom_boxplot(aes(fill = Group), position = position_dodge(width=0.8), width = 0.6, outlier.shape = NA) +
  stat_boxplot(geom = "errorbar",
               width = 0.2,
               size=0.5,
               color = c("darkgreen", "pink4")) +
  ggtitle("Simpson Index") +
  ylab("Simpson Index Values") + 
  xlab("") +
  # scale_fill_manual(values = c("blue", "pink")) +
  theme_classic() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "plain",
      size = 15,
      color = "black",
    ),
    axis.text.x = element_text(
      hjust = 1,
      size = 13,
      angle = 30,
      vjust = 1,
      color = "black"
    ),
    axis.title.x = element_text(
      hjust = 0.5,
      size = 12,
      vjust = 0.2
    ),
    axis.text.y = element_text(
      hjust = 0.8,
      size = 13,
      angle = 0,
      color = "black"),
    axis.title.y = element_text(
      hjust = 0.5,
      size = 13.5,
      vjust = 1.5
    ),
    legend.position =  "none",
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 0.5
    ))

pos_neg_simpson_idx_plot



### Part 2: Statistical Analysis ###

set.seed(15568875)

pos_neg_simpson_index <- pos_neg_simpson_index %>%
  reorder_levels(Group, order = c("Negative", "Positive"))

# Kruskal-Wallis Test

simpson_kw <- pos_neg_simpson_index %>% kruskal_test(simpson_idx ~ Group)

# Multiple testing correction 
simpson_dunn <- pos_neg_simpson_index %>% 
  dunn_test(simpson_idx ~ Group, p.adjust.method = "bonferroni") 

