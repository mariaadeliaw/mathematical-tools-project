library(tidyverse)  
library(janitor)    # cleaning column names
library(mice)       # handling missing data
library(corrplot)
library(FactoMineR) # For PCA
library(factoextra) # For PCA plots
library(vegan) # For CCA
library(randomForest) # variable importance only

lizards <- read_csv("lizard.csv")

# clean column names
lizards <- janitor::clean_names(lizards)

# summary of the data
summary(lizards)

# count missing values in each column
colSums(is.na(lizards))

# visualizing missing data
# library(VIM)
# missing_data_summary <- aggr(
#   lizards_clean, 
#   col = c("skyblue", "red"), 
#   numbers = TRUE, 
#   sortVars = TRUE, 
#   labels = abbreviate(names(lizards_clean), minlength = 10), 
#   cex.axis = 0.7, 
#   gap = 3, 
#   ylab = c("Missing data", "Pattern"), 
#   only.miss = TRUE, 
#   top = 5 
# )


# Select relevant columns for clustering analysis
lizards_clean <- lizards %>%
  select(
    family,
    genus,
    species,
    latitude,
    # general morphology
    mean_f_svl_adults_mm,
    average_female_adult_weight_g,
    f_svl_at_maturity_mm,
    offspring_svl_mm,
    mean_clutch_size,
    clutch_frequency,
    # reproduction
    rcm,
    mode_of_reproduction,
    # habitat
    foraging_mode,
    distribution,
    prefered_habitat_type
  )


# data imputation
lizards_imputed <- mice(lizards_clean, method = "pmm", m = 5, maxit = 50, seed = 123)
lizards_clean <- complete(lizards_imputed) %>% 
  drop_na(family)

lizards_clean$clutch_frequency <- as.factor(lizards_clean$clutch_frequency)

lizards_clean %>%
  select_if(is.numeric) %>% # Select only numeric columns
  cor() %>% # Calculate the empirical correlation matrix
  corrplot() 

lizards_scaled <- lizards_clean %>%
  # select(-rcm) %>% 
  select_if(is.numeric) %>% #Only numeric columns are selected
  mutate_all(.funs = scale) 


# pca ---------------------------------------------------------------------

result_pca <- PCA(lizards_scaled, 
                  scale.unit = TRUE, # Option to center and scale data (useless here)
                  ncp = 18, # Number of components to keep (here, all)
                  graph = FALSE)

fviz_eig(result_pca, choice = "variance")

fviz_pca_var(result_pca,
             axes = c(1, 2)) # Number of axes to represent 

fviz_pca_ind(result_pca,
             axes = c(1, 2),
             col.ind = lizards_clean$distribution)

fviz_pca_var(result_pca, col.var = "contrib")

fviz_pca_biplot(result_pca,
                axes = c(1,2))

pca_coords <- result_pca$ind$coord

# fviz_nbclust(pca_coords, kmeans, method = "silhouette", k.max = 8) +
#   labs(title = "Optimal Number of Clusters: Silhouette Method")
# 
# var<-get_pca_var(result_pca)
# a<-fviz_contrib(result_pca, "var", axes=1, xtickslab.rt=90) # default angle=45°
# plot(a,main = "Variables percentage contribution of first Principal Components")


# clustering on pca ---------------------------------------------------------

# kmeans cluster with 2 groups
km_result <- eclust(pca_coords, "kmeans", k = 3, hc_metric = "euclidean", graph = FALSE)

# # Visualize silhouette plot
# fviz_silhouette(km_result) +
#   labs(title = "Silhouette Plot for K-Means Clustering")

# Visualize the clusters on the PCA map
fviz_cluster(km_result, geom = "point", ellipse.type = "norm", data = pca_coords) +
  labs(title = "Clusters on PCA Map - PC Coord")
#add the clustering result to the scaled dataframe
lizards_clustered <- cbind(lizards_scaled, cluster = as.factor(km_result$cluster))


# clustering on raw -------------------------------------------------------

# lizards_scaled_raw <- lizards_clean %>%
#   # select(-rcm) %>% 
#   select_if(is.numeric) %>% #Only numeric columns are selected
#   mutate_all(.funs = scale) 
# 
# km_result_raw <- eclust(lizards_scaled_raw, "kmeans", k = 3, hc_metric = "euclidean", graph = FALSE)
# # Visualize the clusters on the PCA map
# fviz_cluster(km_result_raw, geom = "point", ellipse.type = "norm", data = pca_coords) +
#   labs(title = "Clusters on PCA Map - Raw Data")
# #add the clustering result to the scaled dataframe
# lizards_clustered_raw <- cbind(lizards_scaled, cluster = as.factor(km_result_raw$cluster))

# rf ----------------------------------------------------------------------

rf_model <- randomForest(cluster ~ ., data = lizards_clustered, importance = TRUE)
importance(rf_model)
varImpPlot(rf_model)

# rf_model_raw <- randomForest(cluster ~ ., data = lizards_clustered_raw, importance = TRUE)
# importance(rf_model_raw)
# varImpPlot(rf_model_raw)

# biplot clustering result
fviz_pca_biplot(result_pca,
                axes = c(1,2),
                col.ind = lizards_clustered$cluster)


# chi-square test ---------------------------------------------------------

# Chi-squared test for cluster and habitat type
chi_habitat <- chisq.test(table(lizards_clustered$cluster, lizards_clean$prefered_habitat_type))

# Chi-squared test for cluster and foraging mode
chi_cluster <- chisq.test(table(lizards_clustered$cluster, lizards_clean$foraging_mode))

# Chi-squared test for cluster and reproduction mode
chi_reproduction <- chisq.test(table(lizards_clustered$cluster, lizards_clean$mode_of_reproduction))

# Chi-squared test for cluster and distribution
chi_foraging <- chisq.test(table(lizards_clustered$cluster, lizards_clean$distribution))

chi_square_results <- tibble(
  Variable = c("Preferred Habitat Type", "Foraging Mode", "Mode of Reproduction", "Distribution"),
  X_squared = c(chi_habitat$statistic, chi_foraging$statistic, chi_reproduction$statistic, chi_distribution$statistic),
  df = c(chi_habitat$parameter, chi_foraging$parameter, chi_reproduction$parameter, chi_distribution$parameter),
  p_value = c(chi_habitat$p.value, chi_foraging$p.value, chi_reproduction$p.value, chi_distribution$p.value)
)

# ca cca ------------------------------------------------------------------
# 
# lizards_ca <- lizards_clean %>% 
#   group_by(prefered_habitat_type, distribution) %>%
#   summarise(count = n(), .groups = "drop") %>%
#   pivot_wider(names_from = distribution, values_from = count, values_fill = 0) %>% 
#   column_to_rownames(var = "prefered_habitat_type")
# 
# lizards_ca_long <- lizards_ca %>% 
#   rownames_to_column(var = "Zone") %>% 
#   pivot_longer(cols = -c("Zone"),
#                names_to = "distribution",
#                values_to = "NbIndividuals")
# library(vegan)
# result_ca_lizards <- CA(lizards_ca, graph = FALSE)
# result_ca_lizards$eig
# 
# # Generate the biplot with valid axes
# fviz_ca_biplot(result_ca_lizards, axes = c(1, 2), repel = TRUE)
# 
# lizards_cca <- lizards_clean %>%
#   group_by(family, distribution) %>%
#   summarise(mean_rcm = mean(mean_clutch_size), .groups = "drop") %>%
#   pivot_wider(names_from = distribution, values_from = mean_rcm, values_fill = 0) %>%
#   column_to_rownames(var = "family")
# 
# results_cca_lizards <- cca(lizards_ca ~ Tropical + Temperate,
#                    data = lizards_cca)
# 
# table_reduced_cca_l <- results_cca_lizards$CCA$u
# 
# corr_pc_lizards = cor(table_reduced_cca_l, lizards_cca[, c("Temperate", "Tropical")])
# corrplot(corr_pc_lizards)
# 
# plot(results_cca_lizards)
