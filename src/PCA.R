library(readr)
library(tidyverse)
library(XML)
library(corrplot)
library(factoextra)
library(MASS)
library(mvtnorm)
library(MVN)
library(psych)
library(ggfortify)
library(ggpubr)

# Read dataset----
df <- xmlToDataFrame("input.xml")
str(df)
nrow(df)
ncol(df)

# DATA PREPROCESSING----
# We only select rows which progress is 100
df <- df %>%
  filter(progress == '100')

# We got 136 observations that finished the questionnaire


# READ DATA---- 

df = re
travel_df <- read_csv("Travel Study 2.7.23.csv")
head(travel_df)
str(travel_df)

# DATA PREPROCESING ----
# DROP UNUSED COLUMN----
# First, drop two first rows. Next, filter only data that has 100 in progress
travel_df <- travel_df %>% 
  slice(-c(1,2)) %>%
  filter(Progress == '100')

# Drop the first 11 columns since it contains the questionnaire status
travel_df_clean <- travel_df[-c(1:11)]

# Drop all column that contains _RANK in the end of the name
travel_df_clean <- travel_df_clean[!grepl("_RANK$", names(travel_df_clean))]

# Drop optional column named Q20 and column contains _TEXT in the end of name
travel_df_clean <- travel_df_clean[!grepl("_TEXT$", names(travel_df_clean))]
travel_df_clean <- subset(travel_df_clean, select = -c(Q20))

# CHECK MISSING VALUE----
# Count the missing values by column wise
print("Count of missing values by column wise")
sapply(travel_df_clean, function(x) sum(is.na(x)))

# Missing value imputation
# Since our data contains 46 missing value, let's impute with mode
# Function to see mode
calc_mode <- function(x){
  
  # List the distinct / unique values
  distinct_values <- unique(na.omit(x))
  
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}

calc_mode(travel_df_clean$Q8)
calc_mode(travel_df_clean$Q7_0_GROUP)
calc_mode(travel_df_clean$Q7_1_GROUP)
calc_mode(travel_df_clean$Q7_2_GROUP)


# Impute missing value----
travel_df_clean <- travel_df_clean %>% 
  mutate(across(everything(), ~replace_na(.x, calc_mode(.x))))


# CONVERT DATA TYPE----
# First, convert column Q6_15 - Q6_19 to numeric data type
travel_df_clean[6:10] <- lapply(travel_df_clean[6:10], as.numeric)

# Second convert to character exclude Q7_0_GROUP , Q7_1_GROUP, Q7_2_GROUP to numeric
exclude <- travel_df_clean[,6:13]

travel_df_clean[,-6:-13] <- lapply(travel_df_clean[,-6:-13], factor)
travel_df_clean[,-6:-13] <- sapply(travel_df_clean[,-6:-13], as.numeric)


# CONVERT Q7_0_GROUP , Q7_1_GROUP, Q7_2_GROUP to dummy----
travel_df_clean =  travel_df_clean%>% 
  separate_rows(Q7_0_GROUP, sep = ",") %>% 
  mutate(Q7_0_GROUP = as.factor(Q7_0_GROUP)) %>% mutate(seen =1) %>%
  pivot_wider(names_from = Q7_0_GROUP, values_from = seen, 
              names_prefix = 'prefer_', values_fn =  list(seen = length), values_fill = 0)

travel_df_clean =  travel_df_clean%>% 
  separate_rows(Q7_1_GROUP, sep = ",") %>% 
  mutate(Q7_1_GROUP = as.factor(Q7_1_GROUP)) %>% mutate(seen =1) %>%
  pivot_wider(names_from = Q7_1_GROUP, values_from = seen, 
              names_prefix = 'avoid_', values_fn =  list(seen = length), values_fill = 0)

travel_df_clean =  travel_df_clean%>% 
  separate_rows(Q7_2_GROUP, sep = ",") %>% 
  mutate(Q7_2_GROUP = as.factor(Q7_2_GROUP)) %>% mutate(seen =1) %>%
  pivot_wider(names_from = Q7_2_GROUP, values_from = seen, 
              names_prefix = 'not_flown_', values_fn =  list(seen = length), values_fill = 0)


# START ANALYSIS----
# Q1 : EDA-----
# Histogram, since our variable is quite large
# I'll cut it to 9 subplot for each plot
mvn(travel_df_clean[,1:9], mvnTest = "energy", univariatePlot = "histogram")
mvn(travel_df_clean[,10:18], mvnTest = "energy", univariatePlot = "histogram")
mvn(travel_df_clean[,19:27], mvnTest = "energy", univariatePlot = "histogram")
mvn(travel_df_clean[,28:36], mvnTest = "energy", univariatePlot = "histogram")
mvn(travel_df_clean[,37:45], mvnTest = "energy", univariatePlot = "histogram")
mvn(travel_df_clean[,46:54], mvnTest = "energy", univariatePlot = "histogram")
mvn(travel_df_clean[,55:61], mvnTest = "energy", univariatePlot = "histogram")


# SUMMARY STATISTICS----
mvn(travel_df_clean, mvnTest = "royston", univariatePlot = "qq")

# BOXPLOT----
mvn(travel_df_clean[,1:5], mvnTest = "royston", univariatePlot = "box")
mvn(travel_df_clean[,6:10], mvnTest = "royston", univariatePlot = "box")
mvn(travel_df_clean[,11:20], mvnTest = "royston", univariatePlot = "box")
mvn(travel_df_clean[,21:23], mvnTest = "royston", univariatePlot = "box")
# since the rest of variables are dummy, the boxplot would look like prefer_JetBlue

# CHI-SQUARE QUANTILE PLOT----
# We got error in integer variable = system is exactly singular: U[2,2] = 0
# Thus we'll do qq plot in numeric (var 6 -10)
mvn(travel_df_clean[,6:10], mvnTest = "royston", multivariatePlot = "scatter", 
    multivariateOutlierMethod="quan")

#Q2-----
# Correlation Matrix----
corrplot(cor(travel_df_clean), 
         method="ellipse",
         tl.pos="n",
         title="Matrix Correlations")

correlation = cor(travel_df_clean)

# PCA should be used mainly for variables which are strongly correlated. 
# If the relationship is weak between variables, PCA does not work well to reduce data. 
# Refer to the correlation matrix to determine. 
# In general, if most of the correlation coefficients are smaller than 0.3, PCA will not help.
# Since some variables are highly correlated and some aren't. PCA will work for some variable and not for the weak correlation.


# Q3----
# BUILD PCA FROM CORRELATION MATRIX
pca <- prcomp(correlation)
print(pca)

# PCA summary
summary(pca)

# GET EIGEN VALUE
eig_val<-get_eigenvalue(pca)
eig_val

fviz_eig(pca, col.var="blue")
fviz_screeplot(p, type = "lines", addlabels = TRUE,
               main = "Scree Plot PCA using All Variables")

var <- get_pca_var(pca)
corrplot(var$contrib, is.corr = FALSE)

p <- princomp(correlation)
print(p$loadings,digits = 8, cutoff = 0.01)

# Unfortunately there is no eigen value > 1 if we use all variable


# LET'S BUILD SECOND PCA BY DROPPING ALL DUMMY
correlation2 <- cor(travel_df_clean[,1:22])
pca2 <- princomp(correlation2)
summary(pca2)

eig_val2 <- get_eigenvalue(pca2)

# Still, there is no Eigen value that > 1. The cause might due to the factor variable we got.

var2 <- get_pca_var(pca2)
var2$coord

corrplot(var2$contrib, is.corr = FALSE)

fviz_screeplot(pca2, type = "lines", addlabels = TRUE,
               main = "Scree Plot PCA without Dummy")


# Since we can't obtain any pca if we use KM1 rule
# Let's try parallel analysis
fa.parallel(correlation2,fm='pa', fa='fa',main = 'Parallel Analysis Scree Plot', n.iter=500)

pca2$loadings

print(pca2$loadings,digits = 8, cutoff = 0.01)


# AUTOPLOT
autoplot(pca2)
autoplot(pca2, 
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)


pca_2_ <- prcomp(travel_df_clean[,1:22], center = TRUE, scale = TRUE)
fviz_pca_biplot(pca_2_,
                col.ind = travel_df_clean$Q15, # color by groups
                palette = c('#00AFBB', '#E7B800', '#FC4E07'),
                geom.ind = 'points', # show points only (nbut not “text”)
                addEllipses = TRUE,
                legend.title = 'Gender'# Concentration ellipses
                )
  