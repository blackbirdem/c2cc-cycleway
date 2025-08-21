library(pcaMethods)
library(MASS)
library(GPArotation)

# set working directory
directory <- 'C:/Users/emmah/Documents/GitHub/c2cc-cycleway/data/survey/'
setwd(directory)

# read table that specifies which independent variables to consider
variables <- read.csv('selected_dependents.csv')
num <- read.csv('C2CC_form_numeric.csv')
# remove redundant index columns
num <- num[,3:length(names(num))]

# remove variables with too many NA values`
prep <- num[, !names(num) %in% 
              c('car_changes', 'walk_changes', 'cycle_frequ_history', 'use_cycleway',
                'use_cycleway_frequ', 'perception_stress', 'perception_safety',
                'perception_speed', 'perception_stress2', 'perception_safety2',
                'perception_speed2', 'cycle_frequ_history')]
# remove demographic variables
prep <- prep[, 6:length(names(prep))]
# remove variables without information
prep <- prep[, !grepl('other', names(prep), fixed=TRUE)]
prep <- prep[, !grepl('NA', names(prep), fixed=TRUE)]

X <- prep

# correlation matrix
corr <- cor(X, method='spearman', use = "pairwise.complete.obs")

# compute eigenvalues of the correlation matrix
eigenvalues <- eigen(corr)$values
print(eigenvalues)

# scree plot
eigenv <- 2.4
plot(eigenvalues, type = "b", main = "Scree Plot", xlab = "Component Number", ylab = "Eigenvalue")
abline(h = eigenv, col = "red", lty = 2)  # Line at eigenvalue

# apply latent root
lr_comp <- sum(eigenvalues > eigenv)
print(lr_comp)

# run pca nipals classification
numpc <- pca(X, method = 'nipals', scale='uv', nPcs = lr_comp) # uv = unit variance scaling
# print loadings
round(loadings(numpc),2)

# extract loadings
loadings_mat <- as.matrix(loadings(numpc))  # dimensions: variables × components
raw_loadings_df <- as.data.frame(loadings_mat)

# eigenvalue per PC
eigen_unrotated <- (numpc@sDev)^2
raw_loadings_df <- rbind(raw_loadings_df, eigen_unrotated)
print(round(eigen_unrotated, 3))
rownames(raw_loadings_df)[row.names(raw_loadings_df) == paste(length(rownames(raw_loadings_df))-1)] <- 'Eigenvalue'

# individual variance explained (proportion)
explained_var <- numpc@R2  
raw_loadings_df <- rbind(raw_loadings_df, explained_var)
print(round(explained_var, 2))
rownames(raw_loadings_df)[row.names(raw_loadings_df) == paste(length(rownames(raw_loadings_df)))] <- 'variance explained'

write.csv(raw_loadings_df,'PCA__unrotated_loadings.csv')

# apply rotation
rot_pc <- oblimin(loadings_mat)

# rotated loadings
loadings_df <- as.data.frame(unclass(rot_pc$loadings))

# get scores for rotated components
X_scaled <- scale(X)
rotated_scores <- X_scaled %*% rot_pc$loadings
scores_df <- as.data.frame(rotated_scores)

# eigenvalue per PC
eigen_rotated <- colSums(unclass(rot_pc$loadings)^2)
loadings_df <- rbind(loadings_df, eigen_rotated)
print(round(eigen_rotated, 3))
rownames(loadings_df)[row.names(loadings_df) == paste(length(rownames(loadings_df))-1)] <- 'Eigenvalue'

# total variance (number of variables if scaled)
total_variance <- ncol(X_scaled)
# percent variance explained
explained_variance <- eigen_rotated / total_variance * 100
loadings_df <- rbind(loadings_df, explained_variance)
print(round(explained_variance, 2))
rownames(loadings_df)[row.names(loadings_df) == paste(length(rownames(loadings_df)))] <- 'variance explained'

# check significance
print(with(loadings_df,abs(loadings_df) > 0.3))

write.csv(loadings_df, 'PCA__loadings.csv')
write.csv(scores_df, 'PCA__scores.csv')