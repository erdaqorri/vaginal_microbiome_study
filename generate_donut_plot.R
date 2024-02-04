
# load libraries
library(ggplot2)
library(tidyverse)
library(dplyr)
library(extrafont)
library(grid)
library(gridExtra)
library(cowplot)



##### Figure 1: Total vaginal species identified across samples #####


# Vaginal samples absence/presence ----
identified_samples <- data.frame(sample_name = c("Present", "Absent"),
                                 total_identified = c("2118", "2286"))


identified_samples$total_identified <- as.numeric(identified_samples$total_identified)
identified_samples <- identified_samples %>% mutate(percentage = round(total_identified/sum(total_identified),4)*100)
identified_samples$midpoint <- cumsum(identified_samples$total_identified) - 0.5 * identified_samples$total_identified

final_plot_vaginal <- ggplot(identified_samples, aes(x = "", y = percentage, fill = sample_name)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  geom_text(aes(label = paste0("n = ", total_identified, " \n", percentage, "%")),
            position = position_stack(vjust = 0.5),  color = "white", size = 7) +
  scale_fill_manual(values = c("Present" = "#835d74", "Absent" = "#718e88")) +
  ggtitle("Vaginal bacteria presence in sample") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    # plot.margin = unit(rep(-2, 4), "cm")
  ) +
  annotate("text", x = 0, y = 0, label = "Total = 4404", vjust = 0.5, hjust = 0.5, color = "black", size = 7) + 
  guides(fill = guide_legend(title = "")) +
  theme(legend.key.size = unit(1.5, 'cm'),
        # legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.position = "bottom",
        legend.justification="center",
        legend.spacing.x = unit(0.5, 'cm'),
        legend.margin=margin(-30,30, 30, 30),
        # legend.box.margin=margin(70,70, 70, 70),
        plot.title = element_text(size = 26, family = "Arial",
                                  hjust = 0.5, vjust = -2, margin = margin(b = -10)))




##### Figure 2: Distribution of positive samples across genders #####

# Gender plot ----
gender_plots_df <- data.frame(
  sample_name = c("Female", "Male"),
  gender_identified = c(1384, 687),
  percentage = c(66.8, 33.2),
  midpoint = c(2295.0, 2475.4)
)

gender_plots <- ggplot(df, aes(x = "", y = percentage, fill = sample_name)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  geom_text(aes(label = paste0("n = ", gender_identified, " \n", percentage, "%")),
            position = position_stack(vjust = 0.5),  color = "white", size = 7) +
  scale_fill_manual(values = c("Female" = "#efa8b8", "Male" = "#45818e")) +
  ggtitle("Prevalence among sexes") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    # plot.margin = unit(rep(-2, 4), "cm")
  ) +
  annotate("text", x = 0, y = 0, label = "Total = 2071", vjust = 0.5, hjust = 0.5, color = "black", size = 7) 

gender_plots <- gender_plots + guides(fill = guide_legend(title = "")) +
  theme(legend.key.size = unit(1.5, 'cm'),
        # legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.position = "bottom",
        legend.justification="center",
        legend.margin=margin(-25,30, 30, 30),
        legend.spacing.x = unit(0.5, 'cm'),
        # legend.box.margin=margin(70,70, 70, 70),
        plot.title = element_text(size = 26, family = "Arial",
                                  hjust = 0.5, vjust = -2, margin = margin(b = -10)))