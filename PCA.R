install.packages("corrr")
library(corrr)
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("factoextra")
install.packages("FactoMineR")
library(FactoMineR)
library(factoextra)

df2<-read.csv("C:/Users/Tanmay/Downloads/apple_quality.csv")
# Read the CSV file


# Check the structure of the dataframe
str(df2)

# Check for missing values
colSums(is.na(df2))

# Convert the "Quality" column to numeric
df2$Quality <- as.numeric(factor(df2$Quality))

# Remove rows with missing values
df3 <- na.omit(df2)
# Remove the "apple_id" column
df3 <- subset(df3, select = -c(A_id))

df3
# Convert all columns to numeric if they are not already
df3 <- sapply(df3, as.numeric)

# Check for missing values in the cleaned dataframe
colSums(is.na(df3))

# Normalize the data
data_normalized <- scale(df3)

# View the cleaned and normalized dataframe
head(data_normalized)

corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix,lab=TRUE,lab_size = 3)

data.pca <- princomp(corr_matrix)
summary(data.pca)

data.pca$loadings[, 1:2]
fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)
