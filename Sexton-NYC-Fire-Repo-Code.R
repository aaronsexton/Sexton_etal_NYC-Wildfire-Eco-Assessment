# Figures ----


### Decriptive Boxs ----

library(tidyverse)
library(vegan)
library(ggpubr)

# First need to combine all rounds into one.
# First by creating a season-long species list with nativeness status

# Read in all the species lists from each round
rd1_sp <- read.csv("../Data/Fire Inwood Prospect/fire_rd1_specieslist.csv")
rd2_sp <- read.csv("../Data/Fire Inwood Prospect/fire_rd2_specieslist.csv")
rd3_sp <- read.csv("../Data/Fire Inwood Prospect/fire_rd3_specieslist.csv")
rd4_sp <- read.csv("../Data/Fire Inwood Prospect/fire_rd4_specieslist.csv")


# Combine all four data frames, adding a round identifier
master <- bind_rows(
  rd1_sp %>% mutate(round = 1),
  rd2_sp %>% mutate(round = 2),
  rd3_sp %>% mutate(round = 3),
  rd4_sp %>% mutate(round = 4)
) %>%
  group_by(species, status) %>%
  summarise(
    rounds_detected = paste(sort(unique(round)), collapse = ", "),
    n_rounds         = n_distinct(round),
    .groups = "drop"
  )




# Okay now load in season long veg data 

veg_long <- read.csv("../Data/Fire Inwood Prospect/fire_25_veglong_fullseason.csv")
str(veg_long)

# Clean up burn treatment
veg_long$Treatment_Burn
unique(veg_long$Treatment_Burn)

# Lets remove burned_restored rows for now
veg_long <- dplyr::filter(veg_long, Treatment_Burn != "Burned_Restored")

# And create a new column to be clean
veg_long$Burn <- veg_long$Treatment_Burn

# Now clean up treatment names
veg_long$Burn <- gsub("Burned_Unrestored", "Burned", veg_long$Burn)
veg_long$Burn <- gsub("Burned1",   "Burned",         veg_long$Burn)
veg_long$Burn <- gsub("Unburned1", "Unburned",       veg_long$Burn)
veg_long$Burn <- gsub("Burned2",   "Burned",         veg_long$Burn)
veg_long$Burn <- gsub("Unburned2", "Unburned",       veg_long$Burn)
veg_long$Burn <- gsub("Uburned2", "Unburned",        veg_long$Burn)

# Remove these weird rows with 0 cover
veg_long <- veg_long %>% filter(Percent_Cover != 0)

# Add a nativeness column
master_nat <- select(master, species, status)
master_nat <- rename(master_nat, Species = species)
veg_long <- left_join(veg_long, master_nat)
head(veg_long)



subplot_metrics <- veg_long %>%
  group_by(Round, Park, Burn, Suplot_ID) %>%
  summarise(
    species_richness   = n_distinct(Species),
    total_cover        = sum(Percent_Cover, na.rm = TRUE),
    prop_native_cover  = sum(Percent_Cover[status == "native"], na.rm = TRUE) / 
      sum(Percent_Cover, na.rm = TRUE),
    .groups = "drop"
  )

burn_round_means <- subplot_metrics %>%
  group_by(Round, Burn) %>%
  summarise(
    mean_richness          = mean(species_richness, na.rm = TRUE),
    mean_total_cover       = mean(total_cover, na.rm = TRUE),
    mean_prop_native_cover = mean(prop_native_cover, na.rm = TRUE),
    n_subplots             = n(),
    .groups = "drop"
  )



burn_round_summary <- subplot_metrics %>%
  group_by(Round, Burn) %>%
  summarise(
    mean_prop_native = mean(prop_native_cover, na.rm = TRUE),
    se_prop_native   = sd(prop_native_cover, na.rm = TRUE) / sqrt(n()),
    mean_richness    = mean(species_richness, na.rm = TRUE),
    se_richness      = sd(species_richness, na.rm = TRUE) / sqrt(n()),
    mean_total_cover = mean(total_cover, na.rm = TRUE),
    se_total_cover   = sd(total_cover, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

a <- ggplot(burn_round_summary, aes(x = Round, y = mean_prop_native, color = Burn, group = Burn)) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5), color = "grey60", linewidth = 0.6, linetype = "dotted") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean_prop_native - se_prop_native,
                    ymax = mean_prop_native + se_prop_native),
                width = 0.1) +
  scale_color_manual(values = c("Burned" = "tomato", "Unburned" = "royalblue")) +
  scale_x_continuous(breaks = 1:4,
                     labels = c("April", "June", "August", "October")) +
  labs(x = "Round", y = "Proportion native cover (%)", color = NULL) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y     = element_text(size = 24, face = "bold"),
    axis.title.x     = element_blank(),
    legend.text      = element_text(size = 28),
    axis.text.y      = element_text(size = 16),
    axis.text.x      = element_text(size = 22, face = "bold"),
    plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt"))
a

b <- ggplot(burn_round_summary, aes(x = Round, y = mean_richness, color = Burn, group = Burn)) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5), color = "grey60", linewidth = 0.6, linetype = "dotted") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean_richness - se_richness,
                    ymax = mean_richness + se_richness),
                width = 0.1) +
  scale_color_manual(values = c("Burned" = "tomato", "Unburned" = "royalblue")) +
  scale_x_continuous(breaks = 1:4,
                     labels = c("April", "June", "August", "October")) +
  labs(x = "Round", y = "Average Species Richness", color = NULL) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y     = element_text(size = 24, face = "bold"),
    axis.title.x     = element_blank(),
    legend.text      = element_text(size = 28),
    axis.text.y      = element_text(size = 16),
    axis.text.x      = element_text(size = 22, face = "bold"))
b


figure <- ggpubr::ggarrange(a, b, 
                            ncol = 2,
                            common.legend = T,
                            legend = "right",
                            labels = c("A", "B"),
                            font.label = list(size = 28, face = "bold"),
                            label.x = 0.02,
                            label.y = 0.98)

figure <- annotate_figure(figure,
                          top = text_grob(
                            "Understory Plant Community",
                            face = "bold",
                            size = 30))

figure


##


c <- ggplot(burn_round_summary, aes(x = Round, y = mean_total_cover, color = Burn, group = Burn)) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5), color = "grey60", linewidth = 0.6, linetype = "dotted") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean_total_cover - se_total_cover,
                    ymax = mean_total_cover + se_total_cover),
                width = 0.1) +
  scale_color_manual(values = c("Burned" = "tomato", "Unburned" = "royalblue")) +
  scale_x_continuous(breaks = 1:4,
                     labels = c("April", "June", "August", "October")) +
  labs(x = "Round", y = "Mean total cover (%)", color = NULL) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title       = element_text(size = 14),
    legend.text      = element_text(size = 13),
    legend.title     = element_text(size = 13)
  )
c










# NMDS - Full Year ----

library(vegan)
library(tidyverse)
library(ggpubr)


# Veg data
year_long <- read.csv("../Data/Fire Inwood Prospect/fire_25_veglong_fullseason.csv")
str(year_long)


# First we'll do it at the subplot level
# Need to update subplotid to include round
year_long$subplot_id <- paste(year_long$Suplot_ID, year_long$Round, sep = "_")

# First remove extra columns
year_long_trim <- select(year_long, subplot_id, Species, Percent_Cover)


year_wide <- pivot_wider(year_long_trim,
                         names_from = Species,
                         values_from = Percent_Cover)

year_wide <- select(year_wide, -`NA`)
year_wide[is.na(year_wide)] <- 0

# Add back in treatment columns
# First fix stupid burn treatment column
year_long <- year_long %>%
  mutate(Burn = case_when(
    grepl("^Burned", Treatment_Burn) ~ "Burned",
    grepl("^Unburned", Treatment_Burn) ~ "Unburned",
    TRUE ~ NA_character_   # catches anything unexpected
  ))
trts <- select(year_long, subplot_id, Park, Burn)
trts <- trts[!duplicated(trts$subplot_id), ]

year_wide <- left_join(year_wide, trts)
str(year_wide)
year_wide <- select(year_wide, Park, Burn, subplot_id, everything())


# Run distance matrix
year_dist <- vegdist(decostand(year_wide[4:108], "norm"), "euclidean")
year_dist
summary(as.matrix(year_dist))

# NMDS
NMDS <- metaMDS(year_dist, k = 3, maxit = 999,
                try = 5, trymax = 250, wascores = T)
ordiplot (NMDS, cex = 1.5, type = 't')
stressplot (NMDS)



# Preds
preds <- year_wide[,2]
str(preds)

# Vector analysis
ef <- envfit(NMDS, preds, na.rm = T)
ordiplot(NMDS, cex = 1.5, type = "t")
plot(ef)
summary(ef)
ef$factors

# Permdisp
burn <- year_wide$Burn
park <- year_wide$Park


burn_disp <- betadisper(year_dist, burn)
burn_disp$group.distances
hist(burn_disp$group.distances)
plot(burn_disp)
anova(burn_disp)


park_disp <- betadisper(year_dist, park)
park_disp$group.distances
hist(park_disp$group.distances)
plot(park_disp)
anova(park_disp)




## Plot it

# Save subplot scores for ggplotting

site.scrs <- as.data.frame(scores(NMDS, display = "sites")) #save NMDS results into dataframe
site.scrs <- cbind(site.scrs, burn <- year_wide$Burn)
site.scrs <- cbind(site.scrs, park <- year_wide$Park)
site.scrs$`burn <- year_wide$Burn`

site.scrs <- rename(site.scrs, Burn = `burn <- year_wide$Burn`)
site.scrs <- rename(site.scrs, Park = `park <- year_wide$Park`)
site.scrs <- na.omit(site.scrs)



#  ordination graph
ggplot(site.scrs, aes(x=NMDS1, y=NMDS2))+
  geom_point(aes(NMDS1, NMDS2, colour = factor(Burn), 
                 shape = factor(Park)), size = 6, alpha = 0.7) +
  stat_ellipse(aes(color = factor(Burn)), linetype = 2)+
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, 
                                        linetype = "solid")) +
  labs(colour = "Burn", shape = "Park",
       title = "Herbaceous Community Composition") +
  scale_color_manual(values = c("Burned" = "tomato3",
                                "Unburned" = "steelblue")) +
  annotate("text", x = 0.7, y = -0.6, 
           label = expression(bolditalic("p < 0.001")), 
           color = "grey40", size = 8) +
  theme(plot.title = element_text(hjust = 0.5, size = 40))







# Now need to do it at the survey level
# Create survey id
str(year_long)
year_long$survey_id <- paste(year_long$Park, year_long$Burn, year_long$Round, sep = "_")
year_long_survey <- select(year_long, survey_id, subplot_id, Species, Percent_Cover)


# Send it wide
year_wide_survey <- year_long_survey %>%
  group_by(survey_id, Species) %>%
  summarise(mean_cover = mean(Percent_Cover, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Species,
    values_from = mean_cover
  )


year_wide_survey <- select(year_wide_survey, -`NA`)
year_wide_survey[is.na(year_wide_survey)] <- 0

# Add back in treatment columns
# First fix stupid burn treatment column
str(year_long)
trts_survey <- select(year_long, survey_id, Burn, Park)
trts_survey <- trts_survey[!duplicated(trts_survey$survey_id), ]

year_wide_survey <- left_join(year_wide_survey, trts_survey)
str(year_wide_survey)
year_wide_survey <- select(year_wide_survey, Park, Burn, survey_id, everything())


# Run distance matrix
year_dist_survey <- vegdist(decostand(year_wide_survey[4:108], "norm"), "euclidean")
year_dist_survey
summary(as.matrix(year_dist_survey))

# NMDS
NMDS_survey <- metaMDS(year_dist_survey, k = 3, maxit = 999,
                       try = 5, trymax = 250, wascores = T)
ordiplot(NMDS_survey, cex = 1.5, type = 't')
stressplot(NMDS_survey)



# Preds
preds_survey <- year_wide_survey[,2]
str(preds_survey)

# Vector analysis
ef_survey <- envfit(NMDS_survey, preds_survey, na.rm = T)
ordiplot(NMDS_survey, cex = 1.5, type = "t")
plot(ef_survey)
summary(ef_survey)
ef_survey$factors

# Permdisp
burn_survey <- year_wide_survey$Burn
park_survey <- year_wide_survey$Park


burn_disp_survey <- betadisper(year_dist_survey, burn_survey)
burn_disp_survey$group.distances
hist(burn_disp_survey$group.distances)
plot(burn_disp_survey)
anova(burn_disp_survey)


park_disp_survey <- betadisper(year_dist_survey, park_survey)
park_disp_survey$group.distances
hist(park_disp_survey$group.distances)
plot(park_disp_survey)
anova(park_disp_survey)




## Plot it

# Save subplot scores for ggplotting

site.scrs_survey <- as.data.frame(scores(NMDS_survey, display = "sites")) #save NMDS results into dataframe
site.scrs_survey <- cbind(site.scrs_survey, burn_survey <- year_wide_survey$Burn)
site.scrs_survey <- cbind(site.scrs_survey, park_survey <- year_wide_survey$Park)

site.scrs_survey$`burn_survey <- year_wide_survey$Burn`
site.scrs_survey$`park_survey <- year_wide_survey$Park`

site.scrs_survey <- rename(site.scrs_survey, Burn_survey = `burn_survey <- year_wide_survey$Burn`)
site.scrs_survey <- rename(site.scrs_survey, Park_survey = `park_survey <- year_wide_survey$Park`)
site.scrs_survey <- na.omit(site.scrs_survey)



#  ordination graph
ggplot(site.scrs_survey, aes(x=NMDS1, y=NMDS2))+
  geom_point(aes(NMDS1, NMDS2, colour = factor(Burn_survey), 
                 shape = factor(Park_survey)), size = 6, alpha = 0.7) +
  stat_ellipse(aes(color = factor(Burn_survey)), linetype = 2)+
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, 
                                        linetype = "solid")) +
  labs(colour = "Burn", shape = "Park",
       title = "Herbaceous Community Composition") +
  scale_color_manual(values = c("Burned" = "tomato3",
                                "Unburned" = "steelblue")) +
  annotate("text", x = 0.7, y = -0.6, 
           label = expression(bolditalic("p < 0.001")), 
           color = "grey40", size = 8) +
  theme(plot.title = element_text(hjust = 0.5, size = 40))
















### Ordination figures ----

# Subplot-level
nmds_p_subplot <- ggplot(site.scrs, aes(x=NMDS1, y=NMDS2))+
  geom_point(aes(NMDS1, NMDS2, colour = factor(Burn), 
                 shape = factor(Park)), size = 6, alpha = 0.7) +
  stat_ellipse(aes(color = factor(Burn)), linetype = 2)+
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, 
                                        linetype = "solid")) +
  labs(colour = "Burn", shape = "Park",
       title = "Subplot-Level") +
  scale_color_manual(values = c("Burned" = "tomato3",
                                "Unburned" = "steelblue")) +
  annotate("text", x = 0.7, y = -0.6, 
           label = expression(bolditalic("p < 0.001")), 
           color = "grey40", size = 8) +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24, face = "bold"))


# Survey-level
nmds_p_survey <- ggplot(site.scrs_survey, aes(x=NMDS1, y=NMDS2))+
  geom_point(aes(NMDS1, NMDS2, colour = factor(Burn_survey), 
                 shape = factor(Park_survey)), size = 6, alpha = 0.7) +
  stat_ellipse(aes(color = factor(Burn_survey)), linetype = 2)+
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, 
                                        linetype = "solid")) +
  labs(colour = "Burn", shape = "Park",
       title = "Survey-Level") +
  scale_color_manual(values = c("Burned" = "tomato3",
                                "Unburned" = "steelblue")) +
  annotate("text", x = 0.7, y = -0.6, 
           label = expression(bolditalic("p < 0.001")), 
           color = "grey40", size = 8) +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24, face = "bold"))



figure3 <- ggpubr::ggarrange(nmds_p_subplot, nmds_p_survey, 
                             ncol = 2,
                             common.legend = T,
                             legend = "right",
                             labels = c("A", "B"),
                             font.label = list(size = 28, face = "bold"),
                             label.x = 0.02,
                             label.y = 0.98)
figure3
figure3 <- annotate_figure(figure3,
                           top = text_grob(
                             "Understory Plant Community Composition",
                             face = "bold",
                             size = 40))

figure3

##









#### Trees ----

library(vegan)
library(tidyverse)
library(ggpubr)
library(scales)


# Inwood 

fire_trees <- read.csv("../Data/Fire Inwood Prospect/fire_25_trees.csv")
str(fire_trees)


# Stacked bar graphs
ggplot(fire_trees, aes(x = Treatment, y = Abundance, fill = Scientific.name)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Tree Species Abundance by Treatment",
       x = "Treatment",
       y = "Abundance",
       fill = "Species")



# Ensure factor levels with "Standing Dead" at the bottom of the stack (last level)
fire_trees$Scientific.name <- factor(fire_trees$Scientific.name)

# Reorder factor levels so "Standing Dead" is plotted last (i.e., bottom of stack)
fire_trees$Scientific.name <- fct_relevel(fire_trees$Scientific.name, "Standing Dead", after = Inf)

# Get levels
species_levels <- levels(fire_trees$Scientific.name)
n_species <- length(species_levels)

# Assign colors: black for "Standing Dead", distinct for others
# Use consistent ordering for color assignment
base_colors <- "dimgrey"
other_colors <- hue_pal()(n_species - 1)
custom_colors <- setNames(c(other_colors, base_colors), species_levels)

# Create plot
ggplot(fire_trees, aes(x = Treatment, y = Abundance, fill = Scientific.name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  labs(title = "Understory Tree Community Composition",
       x = "Treatment",
       y = "Abundance",
       fill = "Species") +
  theme(plot.title   = element_text(size = 40),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.title = element_blank())


# Calculate % standing dead
dead_summary <- fire_trees %>%
  group_by(Year, Park, Treatment) %>%
  summarise(
    total_abundance = sum(Abundance),
    dead_abundance = sum(Abundance[Common.name == "Standing Dead"]),
    pct_dead = dead_abundance / total_abundance * 100
  )
dead_summary



# Split by Park
fire_trees_p <- filter(fire_trees, Park == "Prospect")
fire_trees_i <- filter(fire_trees, Park == "Inwood")

str(fire_trees_i)



# Prospect
pp_t_p <- ggplot(fire_trees_p, aes(x = Treatment, y = Abundance, fill = Scientific.name)) +
  geom_vline(xintercept = c(1.5), color = "grey60", linewidth = 0.9, linetype = "dotted") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  ylim(0,325) +
  labs(title = "Prospect Park",
       x = "Treatment",
       y = "Abundance",
       fill = "Species")  +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title   = element_text(size = 24, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size = 20),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.title = element_blank())
pp_t_p


# Inwood
inwood_t_p <- ggplot(fire_trees_i, aes(x = Treatment, y = Abundance, fill = Scientific.name)) +
  geom_vline(xintercept = c(1.5), color = "grey60", linewidth = 0.9, linetype = "dotted") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  labs(title = "Inwood Hill",
       x = "Treatment",
       y = "Abundance",
       fill = "Species") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title   = element_text(size = 24, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size = 20),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.title = element_blank())




ggpubr::ggarrange(pp_t_p, inwood_t_p, 
                  ncol = 2, nrow = 1, common.legend = T, legend = "right")

combined <- ggarrange(pp_t_p, inwood_t_p,
                      ncol = 2, common.legend = TRUE, legend = "right")
annotate_figure(combined, top = text_grob("Understory Tree Community", 
                                          size = 36, face = "bold"))





# Need to split the two Inwood fires up

fire_trees_i$site <- "1"
fire_trees_i[32:51, 8] <- "2"

fire_trees_i_1 <- filter(fire_trees_i, site == "1")
fire_trees_i_2 <- filter(fire_trees_i, site == "2")




# Inwood
inwood_1_t_p <- ggplot(fire_trees_i_1, aes(x = Treatment, y = Abundance, fill = Scientific.name)) +
  geom_vline(xintercept = c(1.5), color = "grey60", linewidth = 0.9, linetype = "dotted") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  ylim(0,325) +
  labs(title = "Inwood Hill Burn 1",
       x = "Treatment",
       y = "Abundance",
       fill = "Species") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title   = element_text(size = 24, hjust = 0.5),
        axis.title = element_blank(),
        axis.text.x  = element_text(size = 20),
        legend.title = element_blank()) +
  guides(fill = guide_legend(ncol = 1))
inwood_1_t_p

inwood_2_t_p <- ggplot(fire_trees_i_2, aes(x = Treatment, y = Abundance, fill = Scientific.name)) +
  geom_vline(xintercept = c(1.5), color = "grey60", linewidth = 0.9, linetype = "dotted") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  ylim(0,325) +
  labs(title = "Inwood Hill Burn 2",
       x = "Treatment",
       y = "Abundance",
       fill = "Species") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title   = element_text(size = 24, hjust = 0.5),
        axis.title = element_blank(),
        axis.text.x  = element_text(size = 20),
        legend.title = element_blank()) +
  guides(fill = guide_legend(ncol = 1))
inwood_2_t_p


# Prospect
pp_t_p <- ggplot(fire_trees_p, aes(x = Treatment, y = Abundance, fill = Scientific.name)) +
  geom_vline(xintercept = c(1.5), color = "grey60", linewidth = 0.9, linetype = "dotted") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  ylim(0,325) +
  labs(title = "Prospect Park",
       x = "Treatment",
       y = "Abundance",
       fill = "Species")  +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title   = element_text(size = 24, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size = 20),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.title = element_blank()) +
  guides(fill = guide_legend(ncol = 1))
pp_t_p



figure4 <- ggpubr::ggarrange(pp_t_p, inwood_1_t_p, inwood_2_t_p, 
                             ncol = 3, 
                             nrow = 1, 
                             common.legend = T, 
                             legend = "right",
                             labels = c("A", "B", "C"),
                             font.label = list(size = 28, face = "bold"),
                             label.x = 0.01,
                             label.y = 1.0)
figure4



figure4 <- annotate_figure(figure4,
                           top = text_grob(
                             "Understory Woody Plant Community",
                             face = "bold",
                             size = 40))

figure4


























#### Pollinators ----

library(vegan)
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(viridis)
library(lme4)
library(lmerTest)
library(performance)
library(glmmTMB)

polls <- read.csv("../Data/Fire Inwood Prospect/fire_pollinators_2025.csv")
str(polls)

# Create a just burned and unburned column
polls <- polls %>%
  mutate(Burn = case_when(
    grepl("^Burned", Treatment) ~ "Burned",
    grepl("^Unburned", Treatment) ~ "Unburned",
    TRUE ~ NA_character_   # catches anything unexpected
  ))

# Total pollinator abundance by Burn treatment
# Total pollinator abundance by Burn treatment
polls %>%
  filter(!is.na(Abundance)) %>%
  group_by(Burn) %>%
  summarise(total_observed = sum(Abundance, na.rm = TRUE))


# Model it

m_polls <- glmer(Abundance ~ Burn + (1|Round),
                 family = poisson, data = polls)
summary(m_polls)
check_overdispersion(m_polls)

m_polls <- glmmTMB(Abundance ~ Burn + (1|Round),
                   family = nbinom2, data = polls)
summary(m_polls)



# Stacked bar plot

# Summarize data
polls_summary <- polls %>%
  filter(!is.na(Pollinator)) %>%
  group_by(Burn, Pollinator) %>%
  summarise(TotalAbundance = sum(Abundance), .groups = "drop")
polls_summary <- polls_summary %>%
  mutate(Pollinator = stringr::str_to_title(Pollinator))

# Plot stacked bar chart
ggplot(polls_summary, aes(x = Burn, y = TotalAbundance, fill = Pollinator)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Dark2") +   # nice muted scheme
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  )

ggplot(polls_summary, aes(x = Burn, y = TotalAbundance, fill = Pollinator)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "magma") +  # try "cividis", "magma", etc.
  theme_bw() +
  labs(y = "Pollinator Abundance",
       fill = "Taxa") +
  theme(
    axis.title.y = element_text(size = 28, face = "bold"),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 24),
    legend.title = element_text(size = 24, hjust = 0.5, face = "bold"),
    legend.text = element_text(size = 18)
  )


# Custom colors
pollinator_colors_title <- c(
  "Bombus Spp."      = "goldenrod",
  "Lepidoptera"      = "mediumorchid2",
  "Non-Syrphid Fly"  = "bisque",
  "Small Wild Bee"   = "dodgerblue",
  "Syrphid Fly"      = "seagreen",
  "Wasp"             = "coral"
)


unique(polls_summary$Pollinator)

ggplot(polls_summary, aes(x = Burn, y = TotalAbundance, fill = Pollinator)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = pollinator_colors_title) +
  labs(y = "Pollinator Abundance", fill = "Taxa") +
  theme_bw() +
  theme(
    axis.title.y = element_text(size = 28, face = "bold"),
    axis.title.x = element_blank(),
    axis.text.y  = element_text(size = 18),
    axis.text.x  = element_text(size = 24),
    legend.title = element_text(size = 24, hjust = 0.5, face = "bold"),
    legend.text  = element_text(size = 18)
  )






### Interaction Network ----
library(dplyr)
library(tidyr)
library(bipartite)

# 1. Clean data and sum frequencies
df_clean <- polls %>%
  filter(!is.na(Pollinator), 
         !is.na(Activity), 
         Activity != "Nest Searching") %>%
  mutate(Treatment_Group = case_when(
    grepl("^Burned", Treatment) ~ "Burned",
    grepl("^Unburned", Treatment) ~ "Unburned",
    TRUE ~ Treatment
  )) %>%
  group_by(Treatment_Group, Pollinator, Activity) %>%
  summarize(Total_Abundance = sum(Abundance, na.rm = TRUE), .groups = "drop")

# 2. Fully compatible plotting function
plot_treatment_network <- function(data, treatment_name) {
  
  web_matrix <- data %>%
    filter(Treatment_Group == treatment_name) %>%
    pivot_wider(
      id_cols = Activity, 
      names_from = Pollinator, 
      values_from = Total_Abundance, 
      values_fill = 0
    ) %>%
    column_to_rownames("Activity") %>%
    as.matrix()
  
  # Call plotweb with ZERO custom display arguments to prevent version errors
  bipartite::plotweb(web_matrix)
  
  # Add title
  title(main = paste("Pollinator Network -", treatment_name))
}

# 3. Render side-by-side plots with adjusted label size via base R graphics
par(mfrow = c(1, 2), cex = 0.7) # cex = 0.7 shrinks all text to prevent overlapping
plot_treatment_network(df_clean, "Burned")
plot_treatment_network(df_clean, "Unburned")
par(mfrow = c(1, 1), cex = 1.0) # Reset graphic settings







library(dplyr)
library(ggplot2)
library(ggalluvial)

# 1. Clean data
df_clean <- polls %>%
  filter(!is.na(Pollinator), 
         !is.na(Activity), 
         Activity != "Nest Searching") %>%
  mutate(Treatment_Group = case_when(
    grepl("^Burned", Treatment) ~ "Burned",
    grepl("^Unburned", Treatment) ~ "Unburned",
    TRUE ~ Treatment
  )) %>%
  group_by(Treatment_Group, Pollinator, Activity) %>%
  summarize(Total_Abundance = sum(Abundance, na.rm = TRUE), .groups = "drop")

# Get full list of unique plants and pollinators across the entire study
all_plants <- unique(df_clean$Activity)
all_polls  <- unique(df_clean$Pollinator)

# 2. Build aligned matrices with identical dimensions
make_aligned_matrix <- function(data, treatment_name) {
  data %>%
    filter(Treatment_Group == treatment_name) %>%
    complete(Activity = all_plants, Pollinator = all_polls, fill = list(Total_Abundance = 0)) %>%
    pivot_wider(
      id_cols = Activity, 
      names_from = Pollinator, 
      values_from = Total_Abundance, 
      values_fill = 0
    ) %>%
    column_to_rownames("Activity") %>%
    as.matrix()
}

burned_mat   <- make_aligned_matrix(df_clean, "Burned")
unburned_mat <- make_aligned_matrix(df_clean, "Unburned")

# 3. Plot both side-by-side (Zero extra arguments = Zero version errors)
par(mfrow = c(1, 2), cex = 0.7)

bipartite::plotweb(burned_mat)
title(main = "Pollinator Network - Burned")

bipartite::plotweb(unburned_mat)
title(main = "Pollinator Network - Unburned")

par(mfrow = c(1, 1), cex = 1.0)










# 1. Map positions for bipartite nodes
df_net <- df_clean %>%
  group_by(Pollinator) %>%
  mutate(Poll_Total = sum(Total_Abundance)) %>%
  group_by(Activity) %>%
  mutate(Plant_Total = sum(Total_Abundance)) %>%
  ungroup()

# Assign y-coordinates based on factor levels
plants <- unique(df_net$Activity)
polls_list  <- unique(df_net$Pollinator)

df_net <- df_net %>%
  mutate(
    x_plant = 1,
    y_plant = as.numeric(factor(Activity, levels = plants)),
    x_poll  = 2,
    y_poll  = as.numeric(factor(Pollinator, levels = polls_list))
  )

# Define pollinator color palette

pollinator_colors <- c(
  "Bombus spp."      = "goldenrod",
  "Lepidoptera"      = "mediumorchid2",
  "Non-syrphid fly"  = "bisque",
  "Small wild bee"   = "dodgerblue",
  "Syrphid fly"      = "seagreen",
  "Wasp"             = "coral"
)


# 2. Render plot with fixed linewidth scale
ggplot(df_net) +
  # Draw interaction links (linewidth corresponds directly to abundance)
  geom_segment(
    aes(x = x_plant, y = y_plant, xend = x_poll, yend = y_poll, 
        color = Pollinator, linewidth = Total_Abundance),
    alpha = 0.9
  ) +
  # Plant text labels
  geom_text(
    data = distinct(df_net, Activity, y_plant),
    aes(x = 0.85, y = y_plant, label = Activity), 
    hjust = 1, size = 3
  ) +
  # Pollinator text labels
  geom_text(
    data = distinct(df_net, Pollinator, y_poll),
    aes(x = 2.15, y = y_poll, label = Pollinator), 
    hjust = 0, size = 3
  ) +
  scale_color_manual(values = pollinator_colors) +
  scale_linewidth_continuous(range = c(0.5, 6), name = "Interaction Abundance") +
  scale_x_continuous(limits = c(0, 3)) +
  facet_wrap(~ Treatment_Group) +
  theme_void() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )

#






# 3. Compute coordinates for links
df_links <- df_clean %>%
  mutate(
    x_plant = as.numeric(factor(Activity, levels = all_plants)),
    y_plant = 1,
    x_poll  = as.numeric(factor(Pollinator, levels = all_polls)),
    y_poll  = 2
  )

# 4. Compute total abundance node boxes per treatment
# Pollinator nodes (Top row, y = 2)
nodes_poll <- df_clean %>%
  group_by(Treatment_Group, Pollinator) %>%
  summarize(Abundance = sum(Total_Abundance), .groups = "drop") %>%
  mutate(
    x = as.numeric(factor(Pollinator, levels = all_polls)),
    y = 2,
    # Scale box width based on abundance (max width = 0.4)
    xmin = x - (Abundance / max(Abundance)) * 0.2,
    xmax = x + (Abundance / max(Abundance)) * 0.2,
    ymin = 1.95,
    ymax = 2.05
  )

# Plant nodes (Bottom row, y = 1)
nodes_plant <- df_clean %>%
  group_by(Treatment_Group, Activity) %>%
  summarize(Abundance = sum(Total_Abundance), .groups = "drop") %>%
  mutate(
    x = as.numeric(factor(Activity, levels = all_plants)),
    y = 1,
    xmin = x - (Abundance / max(Abundance)) * 0.2,
    xmax = x + (Abundance / max(Abundance)) * 0.2,
    ymin = 0.95,
    ymax = 1.05
  )

# 5. Render Vertical Bipartite Network
# Render Vertical Bipartite Network with refined legend
ggplot() +
  # Draw interaction links
  geom_segment(
    data = df_links,
    aes(x = x_plant, y = y_plant, xend = x_poll, yend = y_poll, 
        color = Pollinator, linewidth = Total_Abundance),
    alpha = 0.55
  ) +
  # Draw Pollinator abundance boxes (Top)
  geom_rect(
    data = nodes_poll,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Pollinator),
    color = "black"
  ) +
  # Draw Plant abundance boxes (Bottom)
  geom_rect(
    data = nodes_plant,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "grey30", color = "black"
  ) +
  # Pollinator labels (Top)
  geom_text(
    data = distinct(df_links, Pollinator, x_poll),
    aes(x = x_poll, y = 2.15, label = Pollinator),
    angle = 45, hjust = 0, size = 3.5, fontface = "bold"
  ) +
  # Plant labels (Bottom)
  geom_text(
    data = distinct(df_links, Activity, x_plant),
    aes(x = x_plant, y = 0.85, label = Activity),
    angle = 45, hjust = 1, size = 3, fontface = "italic"
  ) +
  scale_color_manual(values = pollinator_colors, guide = "none") + 
  scale_fill_manual(values = pollinator_colors, guide = "none") + 
  scale_linewidth_continuous(range = c(0.8, 7), name = "Interaction Count") +
  facet_wrap(~ Treatment_Group, ncol = 1) +
  scale_y_continuous(limits = c(0.4, 2.4)) +
  scale_x_continuous(limits = c(0, max(length(all_plants), length(all_polls)) + 2)) +
  theme_void() +
  theme(
    strip.text = element_text(size = 13, face = "bold"),
    legend.position = "right", 
    legend.box.margin = margin(l = 10),
    plot.margin = margin(20, 20, 20, 20)
  )









# Calculate centering offset for pollinators over plants
n_plants <- length(all_plants)
n_polls  <- length(all_polls)
poll_offset <- (n_plants - n_polls) / 2

# 1. Update link coordinates with centered pollinator positions
df_links <- df_clean %>%
  mutate(
    x_plant = as.numeric(factor(Activity, levels = all_plants)),
    y_plant = 1,
    x_poll  = as.numeric(factor(Pollinator, levels = all_polls)) + poll_offset,
    y_poll  = 2
  )

# 2. Update Pollinator abundance nodes (Top row, y = 2) with centered offset
nodes_poll <- df_clean %>%
  group_by(Treatment_Group, Pollinator) %>%
  summarize(Abundance = sum(Total_Abundance), .groups = "drop") %>%
  mutate(
    x = as.numeric(factor(Pollinator, levels = all_polls)) + poll_offset,
    y = 2,
    xmin = x - (Abundance / max(Abundance)) * 0.25,
    xmax = x + (Abundance / max(Abundance)) * 0.25,
    ymin = 1.95,
    ymax = 2.05
  )

# Plant nodes remain aligned at x = 1:n_plants
nodes_plant <- df_clean %>%
  group_by(Treatment_Group, Activity) %>%
  summarize(Abundance = sum(Total_Abundance), .groups = "drop") %>%
  mutate(
    x = as.numeric(factor(Activity, levels = all_plants)),
    y = 1,
    xmin = x - (Abundance / max(Abundance)) * 0.25,
    xmax = x + (Abundance / max(Abundance)) * 0.25,
    ymin = 0.95,
    ymax = 1.05
  )

# Panel labels
panel_tags <- data.frame(
  Treatment_Group = levels(factor(df_links$Treatment_Group)),
  label = LETTERS[seq_along(levels(factor(df_links$Treatment_Group)))]
)
#



# 3. Render Plot
p_network <- ggplot() +
  # Draw interaction links
  geom_segment(
    data = df_links,
    aes(x = x_plant, y = y_plant, xend = x_poll, yend = y_poll, 
        color = Pollinator, linewidth = Total_Abundance),
    alpha = 0.55
  ) +
  # Pollinator abundance boxes (Top)
  geom_rect(
    data = nodes_poll,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
        fill = Pollinator, color = Pollinator),
  ) +
  # Plant abundance boxes (Bottom)
  geom_rect(
    data = nodes_plant,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "grey30", color = "black"
  ) +
  # Pollinator labels (Top)
  geom_text(
    data = distinct(df_links, Pollinator, x_poll),
    aes(x = x_poll, y = 2.15, label = Pollinator),
    angle = 45, hjust = 0, size = 3.5, fontface = "bold"
  ) +
  # Plant labels (Bottom)
  geom_text(
    data = distinct(df_links, Activity, x_plant),
    aes(x = x_plant, y = 0.85, label = Activity),
    angle = 45, hjust = 1, size = 3, fontface = "italic"
  ) +
  # Panel tags (A, B)
  geom_text(
    data = panel_tags,
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.3, vjust = 1.3,
    size = 9, fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = pollinator_colors, guide = "none") +
  scale_fill_manual(values = pollinator_colors, guide = "none") +
  scale_linewidth_continuous(range = c(0.8, 7), name = "Interaction Count") +
  facet_wrap(~ Treatment_Group, ncol = 1) +
  scale_y_continuous(limits = c(0.3, 2.5)) +
  # Tight x-limits remove the giant gap on the right
  scale_x_continuous(limits = c(0.5, n_plants + 0.5), expand = c(0, 0)) +
  # Prevents rotated text labels from getting clipped off-screen
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    strip.text = element_text(size = 24, face = "bold"),
    legend.position = "right",
    legend.box.margin = margin(l = 5),
    plot.margin = margin(t = 30, r = 30, b = 40, l = 40)
  )







# Bar plot
ggplot(polls_summary, aes(x = Burn, y = TotalAbundance, fill = Pollinator)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = pollinator_colors_title) +
  labs(y = "Pollinator Abundance", fill = "Taxa") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 28, face = "bold"),
    axis.title.x = element_blank(),
    axis.text.y  = element_text(size = 18),
    axis.text.x  = element_text(size = 24),
    legend.title = element_text(size = 24, hjust = 0.5, face = "bold"),
    legend.text  = element_text(size = 18)
  )




# Combine them
library(patchwork)

p_bar <- ggplot(polls_summary, aes(x = Burn, y = TotalAbundance, fill = Pollinator)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = pollinator_colors_title) +
  labs(y = "Pollinator Abundance", fill = "Taxa", tag = "C") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 28, face = "bold"),
    axis.title.x = element_blank(),
    axis.text.y  = element_text(size = 18),
    axis.text.x  = element_text(size = 24),
    legend.title = element_text(size = 24, hjust = 0.5, face = "bold"),
    legend.text  = element_text(size = 18),
    plot.tag = element_text(size = 26, face = "bold"),
    plot.tag.position = c(0.02, 0.98)
  )



p_combined <- p_network + p_bar + plot_layout(widths = c(2, 1))
p_combined


#







