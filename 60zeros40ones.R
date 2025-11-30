library(dplyr)
library(ggplot2)
library(ggpubr)
library(elasticnet)
library(scPCA)
library(microbenchmark)


set.seed(123)  # for reproducibility

# Define the number of rows and columns
n_rows <- 400
n_cols <- 30

# Total number of elements
n_elements <- n_rows * n_cols

# Create the data vector with 20% zeros and 80% random numbers between 0 and 1
data_vector <- c(rep(0, 0.6 * n_elements), runif(0.4 * n_elements))  # Random numbers between 0 and 1
data_vector <- sample(data_vector, length(data_vector))  # Shuffle the values

# Create the data matrix and convert it to a data frame
data_matrix <- matrix(data_vector, nrow = n_rows, ncol = n_cols)
data_frame <- as.data.frame(data_matrix)

# Add the new column with numbers 1, 2, 3, 4 (100 each)
new_column <- rep(1:4, each = 100)  # 400 elements in total, divided equally into 1, 2, 3, 4

# Add the new column to the data frame
data_frame$NewColumn <- new_column

# Print the first few rows of the updated data frame
head(data_frame)

##########################


pca_sim <- prcomp(data_frame[, 1:30])

# plot the 2D rep using first 2 components
df <- as_tibble(list("PC1" = pca_sim$x[, 1],
                     "PC2" = pca_sim$x[, 2],
                     "label" = as.character(toy_df[, 31])))
p_pca <- ggplot(df, aes(x = PC1, y = PC2, colour = label)) +
  ggtitle("PCA on sparse matrix (60 % zeros and 40 % rand(0, 1))") +
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
  ggtitle("SPCA on Simulated Data for Varying L1 Penalty Terms (60 % zeros and 40 % rand(0, 1))") +
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
  ggtitle("cPCA of Simulated Data (60 % zeros and 40 % rand(0, 1))") +
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
  ggtitle("scPCA of Simulated Data (60 % zeros and 40 % rand(0, 1))") +
  theme_minimal()
p_scpca

