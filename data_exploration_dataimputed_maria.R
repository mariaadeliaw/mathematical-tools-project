library(tidyverse)  
library(janitor)    # cleaning column names
library(mice)       # handling missing data
library(corrplot)
library(FactoMineR) # For PCA
library(factoextra) # For PCA plots
library(vegan) # For CCA

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
             col.ind = lizards_clean$clutch_frequency)

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

lizards_scaled_raw <- lizards_clean %>%
  # select(-rcm) %>% 
  select_if(is.numeric) %>% #Only numeric columns are selected
  mutate_all(.funs = scale) 

km_result_raw <- eclust(lizards_scaled_raw, "kmeans", k = 3, hc_metric = "euclidean", graph = FALSE)
# Visualize the clusters on the PCA map
fviz_cluster(km_result_raw, geom = "point", ellipse.type = "norm", data = pca_coords) +
  labs(title = "Clusters on PCA Map - Raw Data")
#add the clustering result to the scaled dataframe
lizards_clustered_raw <- cbind(lizards_scaled, cluster = as.factor(km_result_raw$cluster))

# rf ----------------------------------------------------------------------

# Example: Variable importance using Random Forest
library(randomForest)

rf_model <- randomForest(cluster ~ ., data = lizards_clustered, importance = TRUE)
importance(rf_model)
varImpPlot(rf_model)

rf_model_raw <- randomForest(cluster ~ ., data = lizards_clustered_raw, importance = TRUE)
importance(rf_model_raw)
varImpPlot(rf_model_raw)

# biplot on raw data clustering result
fviz_pca_biplot(result_pca,
                axes = c(1,2),
                col.ind = lizards_clean$prefered_habitat_type)


# chi-square test ---------------------------------------------------------

# Chi-squared test for cluster and habitat type
chisq.test(table(lizards_clustered$cluster, lizards_clean$prefered_habitat_type))

# Chi-squared test for cluster and foraging mode
chisq.test(table(lizards_clustered$cluster, lizards_clean$foraging_mode))

# Chi-squared test for cluster and reproduction mode
chisq.test(table(lizards_clustered$cluster, lizards_clean$mode_of_reproduction))

