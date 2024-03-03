### Alpha Diversity: Shannon Index ###

library(readxl)
library(vegan)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plyr)
library(tseries)
library(rstatix)

### Part 1: Calculate Shannon Index ###

# Read in the cleaned abundances form

abundancies_form_cleaned <- read_excel("input_file.xsxl")
abundancies_form_cleaned.df <- as.data.frame(abundancies_form_cleaned)
rownames(abundancies_form_cleaned.df) <- abundancies_form_cleaned.df$index
abundancies_form_cleaned.df <- abundancies_form_cleaned.df[, -1]

# Convert to matrix
abundancies_form_cleaned.mtx <- as.matrix(abundancies_form_cleaned.df)

## Calculate Shannon Index for all samples

shannon_index_all <- diversity(abundancies_form_cleaned_num.df, index = "shannon")
shannon_index_df <- data.frame(sample_id = rownames(abundancies_form_cleaned.mtx),
                              shannon_idx = shannon_index_all)


## Read in the samples positive for the presence of vaginal bacteria and the negative ones

pos_neg_shannon_idx <- read_excel("pos_neg_shannon_02-03-2024.xlsx")

pos_neg_shannon_idx_plot <- ggplot(pos_neg_shannon_idx, aes(x=Group, y=shannon_idx, fill=Group )) + 
  geom_jitter(aes(fill = Group), color = "black", size = 1, shape = 21, alpha = 0.3) +
  geom_boxplot(aes(fill = Group), width = 0.5, outlier.shape = 1)  +
  scale_fill_brewer(palette = 'Set1') +
  stat_boxplot(geom = "errorbar",
               width = 0.2,
               color = c("pink4", "blue4")) +
  ggtitle("Shannon Index") +
  ylab("Shannon Index Values") + 
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


### Part 2: Statistical Analysis ###

set.seed(1554552)

# Kruskal-Wallis Test

pos_neg_shannon_idx <- pos_neg_shannon_idx %>%
  reorder_levels(Group, order = c("Negative", "Positive"))

shannon_kw <- pos_neg_shannon_idx %>% kruskal_test(shannon_idx ~ Group)

# Dunn's Test
dunn_test_shannon <- pos_neg_shannon_idx %>% 
  dunn_test(shannon_idx ~ Group, p.adjust.method = "bonferroni")

