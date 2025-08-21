library(pcaMethods)
library(GPArotation)

# set working directory
directory <- 'C:/Users/emmah/Documents/GitHub/c2cc-cycleway/data/survey/'
setwd(directory)

# read table that specifies which independent variables to consider
variables <- read.csv('regression_dependents.csv')
num <- read.csv('C2CC_form_numeric.csv')
# remove redundant index columns
num <- num[,3:length(names(num))]

# subset dataframe without dependent and unnecessary variables
snum <- num[, !names(num) %in%
              c('car_changes', 'walk_changes', 'cycle_frequ_history', 'use_cycleway',
                'use_cycleway_frequ', 'perception_stress', 'perception_safety',
                'perception_speed', 'perception_stress2', 'perception_safety2',
                'perception_speed2', 'cycle_frequ_history')]

# TODO: set iteration here and rerun code below
iteration <- 3
# load dataframe of previous iteration
if (iteration != 1) {
  snum <- read.csv(paste('numeric_reduction_', iteration-1, '.csv', sep=''))
  snum <- snum[,2:length(names(snum))]
}

if (iteration == 1) {
  # using results from original PCA
  PC1 <- c('maintenance', 'bike_ownership', 'x_safety_gear_2', 'safety_gear_dummy')
  PC2 <- c('purpose_dummy', 'x_purpose_shopping', 'x_purpose_hobby', 'x_purpose_social')
  
  PCs <- list(PC1, PC2)
  idx <- c('idx_gear', 'idx_2nd_purposes')
} else if (iteration == 2) {
  # TODO: adjust as previous PCA results show
  PC1 <- c('idx_gear', 'x_safety_gear_3', 'bike_to_work')
  PC1b <- c('x_leave_reason_roadside', 'x_leave_reason_busstop')
  
  PCs <- list(PC1, PC1b)
  idx <- c('idx_gear', 'idx_leave_reason_infrastructure')
} else if (iteration == 3) {
  # TODO: adjust as previous PCA results show
  PC4b <- c('x_leave_reason_cyclists', 'x_leave_reason_pedestrians')
  
  PCs <- list(PC4b)
  idx <- c('idx_leave_reason_people')
}

# aggregate selected variables into one index
for (i in 1:length(PCs)) {
  df <- data.frame(num[,1])
  for (v in PCs[i]) {
    # get all unique values of variable
    values <- unique(snum[, v])
    # scale the values of each variable to lie between 0 and 1
    df[, v] <- with(snum,snum[,v]/length(values))
    # delete the original values from dataset
    snum <- snum[, !names(snum) %in% v]
  }
  df[1] <- list(NULL)
  
  # get mean of all variables
  snum[, idx[i]] <- rowMeans(df)
}

write.csv(snum, paste('numeric_reduction_', iteration, '.csv', sep = ''))

## rerun PCA to check for more groupings
# remove demographic variables
X <- snum[, 6:length(names(snum))]
# remove nas
X <- X[, !grepl('other', names(X), fixed=TRUE)]
X <- X[, !grepl('NA', names(X), fixed=TRUE)]

# correlation matrix
corr <- cor(X, method='spearman', use = "pairwise.complete.obs")

# compute eigenvalues of the correlation matrix
eigenvalues <- eigen(corr)$values
print(eigenvalues)

# scree plot
eigenv <- 2.1
plot(eigenvalues, type = "b", main = "Scree Plot", xlab = "Component Number", ylab = "Eigenvalue")
abline(h = eigenv, col = "red", lty = 2)  # Line at eigenvalue

# apply latent root
lr_comp <- sum(eigenvalues > eigenv)
print(lr_comp)

numpc <- pca(X, method = 'nipals', scale='uv', nPcs = lr_comp) # uv = unit variance scaling
# print loadings
round(loadings(numpc),2)

# extract loadings
loadings_mat <- as.matrix(loadings(numpc))  # dimensions: variables × components

# get scores for rotated components
X_scaled <- scale(X)

# rotate
rot_pc <- oblimin(loadings_mat)

# rotated loadings
loadings_df <- as.data.frame(unclass(rot_pc$loadings))

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

## if significant and sensible groups are visible, go back up to do another iteration
# write variables that should be aggregated as result of current iteration into the PC vector