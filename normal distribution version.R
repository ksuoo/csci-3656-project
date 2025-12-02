library(dplyr)
library(ggplot2)
library(ggpubr)
library(elasticnet)
library(scPCA)
library(microbenchmark)


# Set the seed for reproducibility
set.seed(42)

# Helper function to generate normal data with given mean and standard deviation
generate_normal_data <- function(n, mean, sd) {
  rnorm(n, mean, sd)
}

# Generate the data for each group of 100 rows
group1 <- matrix(NA, nrow = 100, ncol = 30)
group2 <- matrix(NA, nrow = 100, ncol = 30)
group3 <- matrix(NA, nrow = 100, ncol = 30)
group4 <- matrix(NA, nrow = 100, ncol = 30)

# First 100 rows: (0,10), (0,1), (-3,1)
for(i in 1:100) {
  group1[i, 1:10] <- generate_normal_data(10, 0, 10)
  group1[i, 11:20] <- generate_normal_data(10, 0, 1)
  group1[i, 21:30] <- generate_normal_data(10, -3, 1)
}

# Second 100 rows: (0,10), (0,1), (0,1)
for(i in 1:100) {
  group2[i, 1:10] <- generate_normal_data(10, 0, 10)
  group2[i, 11:20] <- generate_normal_data(10, 0, 1)
  group2[i, 21:30] <- generate_normal_data(10, 0, 1)
}

# Third 100 rows: (0,10), (3,1), (-3,1)
for(i in 1:100) {
  group3[i, 1:10] <- generate_normal_data(10, 0, 10)
  group3[i, 11:20] <- generate_normal_data(10, 3, 1)
  group3[i, 21:30] <- generate_normal_data(10, -3, 1)
}

# Fourth 100 rows: (0,10), (3,1), (0,1)
for(i in 1:100) {
  group4[i, 1:10] <- generate_normal_data(10, 0, 10)
  group4[i, 11:20] <- generate_normal_data(10, 3, 1)
  group4[i, 21:30] <- generate_normal_data(10, 0, 1)
}

# Combine all groups into one data frame
final_data <- rbind(group1, group2, group3, group4)

# Add the extra column with values 1, 2, 3, 4
extra_column <- rep(1:4, each = 100)

# Add the extra column to the dataframe
final_data <- cbind(final_data, extra_column)

# Convert to data frame
data_frame <- as.data.frame(final_data)

# Optional: Name the columns for clarity
colnames(data_frame) <- c(paste0("V", 1:30), "Group")

# View the first few rows of the data
head(data_frame)

##########################


pca_sim <- prcomp(data_frame[, 1:30])

# plot the 2D rep using first 2 components
df <- as_tibble(list("PC1" = pca_sim$x[, 1],
                     "PC2" = pca_sim$x[, 2],
                     "label" = as.character(toy_df[, 31])))
p_pca <- ggplot(df, aes(x = PC1, y = PC2, colour = label)) +
  ggtitle("PCA on sparse matrix (50 % zeros and 50 % rand(0, 1))") +
  geom_point(alpha = 0.5) +
  theme_minimal()
p_pca

#########


# perform sPCA on toy_df for a range of L1 penalty terms
penalties <- exp(seq(log(10), log(1000), length.out = 6))
df_ls <- lapply(penalties, function(penalty) {
  spca_sim_p <- elasticnet::spca(data_frame[, 1:30], K = 2, para = rep(penalty, 2),
                                 type = "predictor", sparse = "penalty")$loadings
  spca_sim_p <- as.matrix(data_frame[, 1:30]) %*% spca_sim_p
  spca_out <- list("SPC1" = spca_sim_p[, 1],
                   "SPC2" = spca_sim_p[, 2],
                   "penalty" = round(rep(penalty, nrow(data_frame))),
                   "label"  = as.character(data_frame[, 31])) %>%
    as_tibble()
  return(spca_out)
})
df <- dplyr::bind_rows(df_ls)

# plot the results of sPCA
p_spca <- ggplot(df, aes(x = SPC1, y = SPC2, colour = label)) +
  geom_point(alpha = 0.5) +
  ggtitle("SPCA on Simulated Data for Varying L1 Penalty Terms (50 % zeros and 50 % rand(0, 1))") +
  facet_wrap(~ penalty, nrow = 2) +
  theme_minimal()
p_spca

#########

cpca_sim <- scPCA(target = data_frame[, 1:30],
                  background = background_df,
                  penalties = 0,
                  n_centers = 4)

# create a dataframe to be plotted
cpca_df <- cpca_sim$x %>%
  as_tibble() %>%
  mutate(label = data_frame[, 31] %>% as.character)
colnames(cpca_df) <- c("cPC1", "cPC2", "label")

# plot the results
p_cpca <- ggplot(cpca_df, aes(x = cPC1, y = cPC2, colour = label)) +
  geom_point(alpha = 0.5) +
  ggtitle("cPCA of Simulated Data (50 % zeros and 50 % rand(0, 1))") +
  theme_minimal()
p_cpca

#########


# run scPCA for using 40 logarithmically seperated contrastive parameter values
# and possible 20 L1 penalty terms
scpca_sim <- scPCA(target = data_frame[, 1:30],
                   background = background_df,
                   n_centers = 4,
                   penalties = exp(seq(log(0.01), log(0.5), length.out = 10)),
                   alg = "var_proj")
## Registered S3 method overwritten by 'sparsepca':
##   method     from      
##   print.spca elasticnet
# create a dataframe to be plotted
scpca_df <- scpca_sim$x %>%
  as_tibble() %>%
  mutate(label = data_frame[, 31] %>% as.character)
colnames(scpca_df) <- c("scPC1", "scPC2", "label")

# plot the results
p_scpca <- ggplot(scpca_df, aes(x = scPC1, y = scPC2, colour = label)) +
  geom_point(alpha = 0.5) +
  ggtitle("scPCA of Simulated Data (50 % zeros and 50 % rand(0, 1))") +
  theme_minimal()
p_scpca

