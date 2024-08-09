#dplyr package is used for select filter bascially manuplation
install.packages("dplyr")
library("dplyr")
#tidyverse includes collection of packages like dplyr ggplot2 tidyr
install.packages("tidyverse")
library("tidyverse")
library("ggplot2")
coviddata<-read.csv("~/Downloads/owid-covid-data.csv", header=TRUE)
head(coviddata)
upcoviddata<-coviddata %>% 
  select(continent,location,date,total_cases,new_cases,total_deaths,new_deaths,icu_patients,hosp_patients,total_tests,new_tests,tests_per_case,total_vaccinations,median_age,people_fully_vaccinated,total_boosters,stringency_index,gdp_per_capita,population_density)
glimpse(upcoviddata)
upcoviddata$stringency_index<-as.factor(upcoviddata$stringency_index)
class(upcoviddata$stringency_index)
upcoviddata$continent<-as.factor(upcoviddata$continent)
class(upcoviddata$continent)
upcoviddata$location<-as.factor(upcoviddata$location)
class(upcoviddata$location)
upcoviddata$date<-as.Date(upcoviddata$date)
class(upcoviddata$date)

#  numerical columns
numerical_columns <- c("total_cases", "new_cases", "total_deaths", "new_deaths",
                       "icu_patients", "hosp_patients", "total_tests", "new_tests",
                       "tests_per_case", "total_vaccinations", "median_age",
                       "people_fully_vaccinated", "population_density","gdp_per_capita",
                       "total_boosters")

# Convert date to month and quarter
upcoviddata$date_month <- as.Date(cut(upcoviddata$date, "month"))
upcoviddata$date_quarter <- as.Date(cut(upcoviddata$date, "quarter"))

#  time-based median imputation 
for (col in numerical_columns) {
  # Calculate median values for each month and quarter
  median_month <- tapply(upcoviddata[[col]], upcoviddata$date_month, median, na.rm = TRUE)
  median_quarter <- tapply(upcoviddata[[col]], upcoviddata$date_quarter, median, na.rm = TRUE)
  
  # Fill missing values using time-based median
  upcoviddata[[col]] <- ifelse(is.na(upcoviddata[[col]]), 
                               ifelse(upcoviddata$date_month %in% names(median_month), 
                                      median_month[as.character(upcoviddata$date_month)], 
                                      median_quarter[as.character(upcoviddata$date_quarter)]), 
                               upcoviddata[[col]])
}

# Remove temporary date columns
upcoviddata <- subset(upcoviddata, select = -c(date_month, date_quarter))

summary(upcoviddata$icu_patients)

# Remove rows with NA values
upcoviddata <- na.omit(upcoviddata)

#removing locations
upcoviddata <- upcoviddata %>%
  filter(location != "High income" & location != "Upper middle income" & location!="Low income" & location!="Lower middle income" & location!="World")

# Select numerical variables to scale
numerical_vars <- select_if(upcoviddata, is.numeric)

# Min-max scaling function
min_max_scale <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Apply min-max scaling to numerical variables
scaled_data <- as.data.frame(lapply(numerical_vars, min_max_scale))
#scientific to decimal
for (col in colnames(scaled_data)) {
  scaled_data[[col]] <- format(as.numeric(scaled_data[[col]]), scientific = FALSE)
}

# Combining sacled numerical with non numerical
upcoviddata_scaled <- cbind(upcoviddata[,-which(colnames(upcoviddata) %in% colnames(numerical_vars))], scaled_data)

head(upcoviddata_scaled)
unique(upcoviddata_scaled$continent)
unique(upcoviddata_scaled$location)


# Convert character columns to numeric
upcoviddata_scaled <- upcoviddata_scaled %>%
  mutate(across(where(is.character), as.numeric))

# Filter out rows with zero population density or missing values in total deaths or population density
upcoviddata_scaled <- upcoviddata_scaled %>%
  filter(population_density > 0 & !is.na(total_deaths) & !is.na(population_density))

# Recalculate death rate
upcoviddata_scaled <- upcoviddata_scaled %>%
  mutate(death_rate = total_deaths / population_density)

# Print summary again
summary(upcoviddata_scaled$death_rate)
sum(is.na(upcoviddata_scaled$death_rate))
summary(upcoviddata_scaled)

#pie chart
ggplot(upcoviddata_scaled, aes(x = "", y = total_boosters, fill = continent)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Proportion of Total Boosters by Continent") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right")

#Correlation Matrix Heat Map
install.packages("reshape2")
library(reshape2)
correlation_matrix <- cor(upcoviddata_scaled[, c("total_cases", "total_deaths", "icu_patients", "hosp_patients", "total_tests", "total_vaccinations", "median_age", "people_fully_vaccinated", "total_boosters", "gdp_per_capita", "population_density", "death_rate")], use = "complete.obs")
melted_corr_matrix <- melt(correlation_matrix)

ggplot(data = melted_corr_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme_minimal() +
  labs(title = "Correlation Matrix Heatmap") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#faceted scatter plot of total boosters vs median age by continent
ggplot(upcoviddata_scaled, aes(x = median_age, y = total_boosters)) +
  geom_point(alpha = 0.6, color = "blue") +
  labs(title = "Total Boosters vs. Median Age by Continent",
       x = "Median Age",
       y = "Total Boosters") +
  theme_minimal() +
  facet_wrap(~ continent)

#GDP and total boosters
ggplot(upcoviddata_scaled, aes(x = gdp_per_capita, y = total_boosters)) +
  geom_point(alpha = 0.6, color = "green") +
  labs(title = "GDP per Capita vs. Total Boosters",
       x = "GDP per Capita",
       y = "Total Boosters") +
  theme_minimal()

# Create a box plot to visualize the distribution of total cases across continents
ggplot(data = upcoviddata_scaled, aes(x = continent, y = total_cases)) +
  geom_boxplot() +
  labs(title = "Distribution of Total Cases Across Continents",
       x = "Continent",
       y = "Total Cases") +
  theme_minimal()
#These gaps may suggest that there are subsets or clusters of data points within North America with particularly high total case counts that are spaced apart from each other. This could reflect regional variations or disparities in the severity or spread of COVID-19 within the continent.
#Concentration of outlier dots at certain points: The clustering of outlier dots at specific values indicates that there are particular instances or situations within North America where the total case counts are exceptionally high. These instances could correspond to outbreaks, hotspots, or regions with high population density or limited healthcare resources.

# Create a box plot to visualize the distribution of death rates across continents
ggplot(data = upcoviddata_scaled, aes(x = continent, y = death_rate)) +
  geom_boxplot() +
  labs(title = "Distribution of Total death rate Across Continents",
       x = "Continent",
       y = "Death Rate") +
  theme_minimal()
#South America likely has more variability in death rates compared to other continents, which is why the box is visible. This suggests that there are differences in the severity of the COVID-19 pandemic across countries within South America.
#reason why box is shown for south america
#It's possible that for those continents, the death rates are more uniform or concentrated around a certain value, resulting in a narrower or nonexistent box in the box plot.

# Filter data for a specific continent (e.g., Asia)
continent_data <- upcoviddata_scaled %>%
  filter(continent == "Asia")

# Create a scatter plot
ggplot(data = continent_data, aes(x = total_cases, y = total_deaths)) +
  geom_point() +
  labs(title = "Total Deaths vs Total Cases in Asia",
       x = "Total Cases",
       y = "Total Deaths") +
  theme_minimal()


# Filter data for a specific continent (e.g., Asia)
continent_data <- upcoviddata_scaled %>%
  filter(continent == "Asia")

# Group data by date and calculate the sum of total cases and total deaths
continent_data <- continent_data %>%
  group_by(date) %>%
  summarise(total_cases = sum(total_cases),
            total_deaths = sum(total_deaths))

# Create a line plot
ggplot(data = continent_data, aes(x = date, y = total_cases, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_line(aes(y = total_deaths), color = "red", size = 1) +
  labs(title = "Total Cases and Total Deaths Over Time in Asia",
       x = "Date",
       y = "Count") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()



# Bubble chart
ggplot(upcoviddata_scaled, aes(x = total_deaths, y = icu_patients, size = population_density)) +
  geom_point(alpha = 0.6) +
  scale_size_continuous(range = c(1,5)) +  # Adjust bubble size range
  labs(title = "Bubble Chart of Total Cases vs Total Deaths",
       x = "Total Cases",
       y = "Total Deaths",
       size = "Population Density") +
  theme_minimal()


# Scatter plot with trend line
ggplot(upcoviddata_scaled, aes(x = total_deaths, y = icu_patients)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +  # Add linear trend line
  labs(title = "Scatter Plot of Total deaths vs icu patients with Trend Line",
       x = "Total Cases",
       y = "Total Deaths") +
  theme_minimal()

#Line plot of hosp patients over time
ggplot(upcoviddata_scaled %>% filter(location == "India"), aes(x = date, y = hosp_patients)) +
  geom_line(color = "green") +
  labs(title = "Hospital Patients Over Time in India",
       x = "Date",
       y = "Hospital Patients (Scaled)") +
  theme_minimal()

#Line plot of icu patients over time

ggplot(upcoviddata_scaled %>% filter(location == "India"), aes(x = date, y = icu_patients)) +
  geom_line(color = "red") +
  labs(title = "ICU Patients Over Time in the India",
       x = "Date",
       y = "ICU Patients (Scaled)") +
  theme_minimal()

#Bar plot of people fully vaccinated per continent

ggplot(upcoviddata_scaled, aes(x = continent, y = people_fully_vaccinated, fill = continent)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "People Fully Vaccinated by Continent",
       x = "Continent",
       y = "People Fully Vaccinated (Scaled)") +
  theme_minimal() +
  theme(legend.position = "none")

#Clustering

library(stats)
# Assuming you have already loaded the necessary libraries and preprocessed the data
install.packages("cluster")
install.packages("fpc")

library(cluster)  # For silhouette score
     # For cluster.stats function

# Choose a continent for clustering, let's say "Europe"
continent_of_interest <- "Europe"
continent_data <- upcoviddata_scaled %>%
  filter(continent == continent_of_interest)

# Select variables for clustering
clustering_data_cases <- continent_data %>%
  select(location, total_cases)

clustering_data_deaths <- continent_data %>%
  select(location, total_deaths)

# Normalize the data if necessary (not required for k-means)
# clustering_data_cases <- scale(clustering_data_cases)
# clustering_data_deaths <- scale(clustering_data_deaths)

# Choose the number of clusters (you can use methods like Elbow method or silhouette method to find optimal k)
k_cases <- 3
k_deaths <- 3

# Perform k-means clustering for total cases
set.seed(123) # for reproducibility
kmeans_result_cases <- kmeans(clustering_data_cases[, -1], centers = k_cases)

# Add cluster labels to the data for total cases
clustered_data_cases <- cbind(clustering_data_cases, cluster_cases = kmeans_result_cases$cluster)

# Perform k-means clustering for total deaths
set.seed(123) # for reproducibility
kmeans_result_deaths <- kmeans(clustering_data_deaths[, -1], centers = k_deaths)

# Add cluster labels to the data for total deaths
clustered_data_deaths <- cbind(clustering_data_deaths, cluster_deaths = kmeans_result_deaths$cluster)

# Visualize the clusters based on total cases
ggplot(clustered_data_cases, aes(x = location, y = 1, size = total_cases, color = factor(cluster_cases))) +
  geom_point(shape = 21, fill = "white", stroke = 2) +
  scale_size_continuous(range = c(1, 10)) +
  labs(title = paste("Clustering of Countries in", continent_of_interest, "Based on Total Cases"),
       x = "Country", y = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Visualize the clusters based on total deaths
ggplot(clustered_data_deaths, aes(x = location, y = 1, size = total_deaths, color = factor(cluster_deaths))) +
  geom_point(shape = 21, fill = "white", stroke = 2) +
  scale_size_continuous(range = c(1, 10)) +
  labs(title = paste("Clustering of Countries in", continent_of_interest, "Based on Total Deaths"),
       x = "Country", y = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Calculate silhouette score for total cases
silhouette_score_cases <- silhouette(kmeans_result_cases$cluster, dist(clustering_data_cases[, -1]))
avg_silhouette_score_cases <- mean(silhouette_score_cases[, "sil_width"])

# Calculate silhouette score for total deaths
silhouette_score_deaths <- silhouette(kmeans_result_deaths$cluster, dist(clustering_data_deaths[, -1]))
avg_silhouette_score_deaths <- mean(silhouette_score_deaths[, "sil_width"])


# Print evaluation metrics
cat("Silhouette Score for Total Cases:", avg_silhouette_score_cases, "\n")
cat("Silhouette Score for Total Deaths:", avg_silhouette_score_deaths, "\n")


install.packages("dendextend")
library(dendextend)
# Perform Hierarchical Clustering
hierarchical_result_cases <- hclust(dist(clustering_data_cases[, -1]), method = "ward.D2")
hierarchical_clusters_cases <- cutree(hierarchical_result_cases, k = k_cases)

# Add cluster labels to the data for hierarchical clustering based on total cases
clustered_data_hierarchical_cases <- cbind(clustering_data_cases, cluster_hierarchical_cases = hierarchical_clusters_cases)

# Visualize the clusters based on hierarchical clustering for total cases
ggplot(clustered_data_hierarchical_cases, aes(x = location, y = 1, size = total_cases, color = factor(cluster_hierarchical_cases))) +
  geom_point(shape = 21, fill = "white", stroke = 2) +
  scale_size_continuous(range = c(1, 10)) +
  labs(title = paste("Clustering of Countries in", continent_of_interest, "Based on Total Cases (Hierarchical)"),
       x = "Country", y = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Perform Hierarchical Clustering for total deaths
hierarchical_result_deaths <- hclust(dist(clustering_data_deaths[, -1]), method = "ward.D2")
hierarchical_clusters_deaths <- cutree(hierarchical_result_deaths, k = k_deaths)

# Add cluster labels to the data for hierarchical clustering based on total deaths
clustered_data_hierarchical_deaths <- cbind(clustering_data_deaths, cluster_hierarchical_deaths = hierarchical_clusters_deaths)

# Visualize the clusters based on hierarchical clustering for total deaths
ggplot(clustered_data_hierarchical_deaths, aes(x = location, y = 1, size = total_deaths, color = factor(cluster_hierarchical_deaths))) +
  geom_point(shape = 21, fill = "white", stroke = 2) +
  scale_size_continuous(range = c(1, 10)) +
  labs(title = paste("Clustering of Countries in", continent_of_interest, "Based on Total Deaths (Hierarchical)"),
       x = "Country", y = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# Group by location (country) and calculate mean death rate
country_death_rates <- upcoviddata_scaled %>%
  group_by(location) %>%
  summarize(mean_death_rate = mean(death_rate, na.rm = TRUE)) %>%
  arrange(desc(mean_death_rate))

# Country with the highest death rate
highest_death_rate <- country_death_rates[1, ]

# Country with the lowest death rate
lowest_death_rate <- country_death_rates[nrow(country_death_rates), ]

highest_death_rate
lowest_death_rate

# Exclude non-numeric columns
numeric_data <- upcoviddata_scaled[, sapply(upcoviddata_scaled, is.numeric)]

# Calculate correlation matrix
correlation_matrix <- cor(numeric_data)

# Print correlation matrix
print(correlation_matrix)
# Convert stringency_index column to numeric
upcoviddata_scaled$stringency_index <- as.numeric(as.character(upcoviddata_scaled$stringency_index))

# Check if the conversion was successful
str(upcoviddata_scaled$stringency_index)

# Now calculate the correlation
correlation_death_stringency <- cor(upcoviddata_scaled$death_rate, upcoviddata_scaled$stringency_index)

# Print the correlation coefficient
print(correlation_death_stringency)

# Group data by continent and calculate summary statistics
continent_summary <- upcoviddata_scaled %>%
  group_by(continent) %>%
  summarise(mean_death_rate = mean(death_rate, na.rm = TRUE),
            median_death_rate = median(death_rate, na.rm = TRUE),
            min_death_rate = min(death_rate, na.rm = TRUE),
            max_death_rate = max(death_rate, na.rm = TRUE))

# Print summary statistics
print(continent_summary)


# Convert stringency_index column to numeric
upcoviddata_scaled$stringency_index <- as.numeric(as.character(upcoviddata_scaled$stringency_index))

# Check if the conversion was successful
str(upcoviddata_scaled$stringency_index)

# Now calculate the correlation between death rate and stringency index
correlation_death_stringency <- cor(upcoviddata_scaled$death_rate, upcoviddata_scaled$stringency_index)

# Print the correlation coefficient
print(correlation_death_stringency)

# Analyze relationships between death rate, ICU patients, hospitalizations, and stringency index
correlation_matrix <- cor(upcoviddata_scaled[, c("death_rate", "icu_patients", "hosp_patients", "stringency_index")])

# Print correlation matrix
print(correlation_matrix)

# Visualize relationships using scatter plots or other appropriate visualizations
ggplot(upcoviddata_scaled, aes(x = stringency_index, y = death_rate)) +
  geom_point() +
  labs(title = "Scatter Plot of Stringency Index vs Death Rate",
       x = "Stringency Index",
       y = "Death Rate") +
  theme_minimal()

#Linear Model

# Split data into features (X) and target variable (y)
X <- upcoviddata_scaled %>% select(-location, -death_rate)
y <- upcoviddata_scaled$death_rate

# Split data into training and testing sets
set.seed(123) # for reproducibility
train_index <- sample(1:nrow(X), 0.8 * nrow(X))
X_train <- X[train_index, ]
y_train <- y[train_index]
X_test <- X[-train_index, ]
y_test <- y[-train_index]

# Train a linear regression model
model <- lm(y_train ~ ., data = X_train)

# Model evaluation
summary(model)
predictions <- predict(model, newdata = X_test)
mse <- mean((y_test - predictions)^2)
mse

# Interpretation
coefficients <- coef(model)
print(coefficients)


# Extract coefficients from the model
coefficients <- coef(model)

# Extract p-values from the model summary
p_values <- summary(model)$coefficients[, 4]

# Create a data frame to store coefficients and p-values
analysis_results <- data.frame(
  Feature = names(coefficients),
  Coefficient = coefficients,
  P_Value = p_values
)

# Print the analysis results
print(analysis_results)

# Print the analysis results
print(analysis_results)



# Filter out the intercept term
analysis_results <- analysis_results[-1,]

# Identify statistically significant relationships (p-value < 0.05)
significant_results <- analysis_results[analysis_results$P_Value < 0.05, ]

# Print significant relationships
print("Significant Relationships:")
print(significant_results)

# Identify non-significant relationships
non_significant_results <- analysis_results[analysis_results$P_Value >= 0.05, ]

# Print non-significant relationships
print("Non-Significant Relationships:")
print(non_significant_results)

# Predict death rates for each continent
continent_predictions <- predict(model, newdata = X, type = "response")

# Combine predicted death rates with continent information
predictions_df <- data.frame(continent = upcoviddata_scaled$continent, predicted_death_rate = continent_predictions)

# Aggregate predicted death rates by continent
predicted_death_rate_by_continent <- aggregate(predicted_death_rate ~ continent, data = predictions_df, FUN = mean)

# Identify continent with the highest and lowest predicted death rates
continent_highest_death_rate <- predicted_death_rate_by_continent[which.max(predicted_death_rate_by_continent$predicted_death_rate), ]
continent_lowest_death_rate <- predicted_death_rate_by_continent[which.min(predicted_death_rate_by_continent$predicted_death_rate), ]

# Print the continents with the highest and lowest predicted death rates
print("Continent with the highest predicted death rate:")
print(continent_highest_death_rate)
print("Continent with the lowest predicted death rate:")
print(continent_lowest_death_rate)

# Aggregate actual death rates by continent
actual_death_rate_by_continent <- aggregate(death_rate ~ continent, data = upcoviddata_scaled, FUN = mean)

# Identify continent with the highest and lowest actual death rates
continent_highest_death_rate <- actual_death_rate_by_continent[which.max(actual_death_rate_by_continent$death_rate), ]
continent_lowest_death_rate <- actual_death_rate_by_continent[which.min(actual_death_rate_by_continent$death_rate), ]

# Print the continents with the highest and lowest actual death rates
print("Continent with the highest actual death rate:")
print(continent_highest_death_rate)
print("Continent with the lowest actual death rate:")
print(continent_lowest_death_rate)

# Predict death rates for each country in the training set
country_predictions <- predict(model, newdata = X_train, type = "response")

# Combine predicted death rates with country information
predictions_df <- data.frame(location = upcoviddata_scaled$location[train_index], predicted_death_rate = country_predictions)

# Aggregate predicted death rates by country
predicted_death_rate_by_country <- aggregate(predicted_death_rate ~ location, data = predictions_df, FUN = mean)

# Identify country with the highest and lowest predicted death rates
country_highest_death_rate <- predicted_death_rate_by_country[which.max(predicted_death_rate_by_country$predicted_death_rate), ]
country_lowest_death_rate <- predicted_death_rate_by_country[which.min(predicted_death_rate_by_country$predicted_death_rate), ]

# Print the countries with the highest and lowest predicted death rates
print("Country with the highest predicted death rate based on the training set:")
print(country_highest_death_rate)
print("Country with the lowest predicted death rate based on the training set:")
print(country_lowest_death_rate)
# Aggregate actual death rates by country in the training set
actual_death_rate_by_country <- aggregate(death_rate ~ location, data = upcoviddata_scaled[train_index, ], FUN = mean)

# Identify country with the highest and lowest actual death rates
country_highest_death_rate_actual <- actual_death_rate_by_country[which.max(actual_death_rate_by_country$death_rate), ]
country_lowest_death_rate_actual <- actual_death_rate_by_country[which.min(actual_death_rate_by_country$death_rate), ]

# Print the countries with the highest and lowest actual death rates
print("Country with the highest actual death rate based on the training set:")
print(country_highest_death_rate_actual)
print("Country with the lowest actual death rate based on the training set:")
print(country_lowest_death_rate_actual)

# Calculate mean squared error (MSE)
mse <- mean((predictions - y_test)^2)

# Calculate mean absolute error (MAE)
mae <- mean(abs(predictions - y_test))

# Calculate R-squared
rsquared <- summary(model)$r.squared
# Calculate RMSE
rmse <- sqrt(mse)

# Print the RMSE value
print(paste("Root Mean Squared Error (RMSE):", rmse))

# Print the evaluation metrics
print(paste("Mean Squared Error (MSE):", mse))
print(paste("Mean Absolute Error (MAE):", mae))
print(paste("R-squared:", rsquared))

# Combine actual and predicted death rates for each country
country_death_rates <- data.frame(
  location = upcoviddata_scaled$location[train_index],
  actual_death_rate = y_train,
  predicted_death_rate = country_predictions
)

# Aggregate actual and predicted death rates by country
aggregated_country_death_rates <- aggregate(cbind(actual_death_rate, predicted_death_rate) ~ location, data = country_death_rates, FUN = mean)

# Sort by actual death rates and select top 10
top_10_highest_actual <- aggregated_country_death_rates[order(aggregated_country_death_rates$actual_death_rate, decreasing = TRUE), ][1:10, ]
top_10_lowest_actual <- aggregated_country_death_rates[order(aggregated_country_death_rates$actual_death_rate), ][1:10, ]

# Sort by predicted death rates and select top 10
top_10_highest_predicted <- aggregated_country_death_rates[order(aggregated_country_death_rates$predicted_death_rate, decreasing = TRUE), ][1:10, ]
top_10_lowest_predicted <- aggregated_country_death_rates[order(aggregated_country_death_rates$predicted_death_rate), ][1:10, ]

# Print corrected results
print("Top 10 countries with highest actual death rates and predicted values:")
print(top_10_highest_actual)
print("Top 10 countries with lowest actual death rates and predicted values:")
print(top_10_lowest_actual)
print("Top 10 countries with highest predicted death rates and actual values:")
print(top_10_highest_predicted)
print("Top 10 countries with lowest predicted death rates and actual values:")
print(top_10_lowest_predicted)


# Calculate predictions for the test set
predictions <- predict(model, newdata = X_test)

# Create a data frame with actual and predicted values
results <- data.frame(
  Actual = y_test,
  Predicted = predictions
)

# Plot actual vs predicted values
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Values",
       x = "Actual Values",
       y = "Predicted Values") +
  theme_minimal()

# Print RMSE value on the plot
rmse <- sqrt(mean((y_test - predictions)^2))


library(randomForest)

# Convert stringency_index column to numeric
upcoviddata_scaled$stringency_index <- as.numeric(as.character(upcoviddata_scaled$stringency_index))

# Split the data into training and testing sets
set.seed(123) # for reproducibility
train_indices <- sample(1:nrow(upcoviddata_scaled), 0.8 * nrow(upcoviddata_scaled))
train_data <- upcoviddata_scaled[train_indices, ]
test_data <- upcoviddata_scaled[-train_indices, ]

# Target encoding for location variable
location_means <- aggregate(death_rate ~ location, data = train_data, FUN = mean)
train_data <- merge(train_data, location_means, by = "location", suffixes = c("", "_mean"), all.x = TRUE)
test_data <- merge(test_data, location_means, by = "location", suffixes = c("", "_mean"), all.x = TRUE)
str(train_data)
str(test_data)
# Remove duplicated columns
train_data <- train_data[ , !duplicated(colnames(train_data))]
test_data <- test_data[ , !duplicated(colnames(test_data))]
str(train_data)
str(test_data)
# Define predictor variables and the target variable
predictors <- c("icu_patients", "hosp_patients", "stringency_index", "total_cases", "total_deaths", "people_fully_vaccinated", "death_rate_mean","median_age","tests_per_case")
target <- "death_rate"

# Train the Random Forest model
rf_model <- randomForest(
  formula = as.formula(paste(target, "~", paste(predictors, collapse = " + "))),
  data = train_data,
  ntree = 100, # Number of trees in the forest
  importance = TRUE # Compute variable importance
)

# Print the trained model
print(rf_model)

# Make predictions on the test set
predictions <- predict(rf_model, newdata = test_data)

# Evaluate model performance (e.g., using Mean Absolute Error)
mae <- mean(abs(predictions - test_data$death_rate))
print(paste("Mean Absolute Error:", mae))

# Calculate RMSE
mse <- mean((predictions - test_data$death_rate)^2)
rmse <- sqrt(mse)
print(paste("Root Mean Squared Error (RMSE):", rmse))

# Inspect predicted versus actual values
comparison <- data.frame(actual = test_data$death_rate, predicted = predictions)
print(head(comparison))

# Plot predicted versus actual values
plot(comparison$actual, comparison$predicted, 
     xlab = "Actual Death Rate", ylab = "Predicted Death Rate",
     main = "Actual vs. Predicted Death Rate")
abline(0, 1, col = "red")

var_importance <- importance(rf_model)
var_importance

# For Random Forest model, feature importance can be plotted
varImpPlot(rf_model)

residuals <- comparison$actual - comparison$predicted
plot(comparison$predicted, residuals, 
     xlab = "Predicted Death Rate", ylab = "Residuals",
     main = "Residuals vs. Predicted Death Rate")
abline(h = 0, col = "red")

var_importance_df <- as.data.frame(var_importance)
ggplot(var_importance_df, aes(x = rownames(var_importance_df), y = `%IncMSE`)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Variable Importance (%IncMSE)",
       x = "Variable",
       y = "%IncMSE") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Load necessary libraries
library(forecast)
library(tseries)

# Assuming upcoviddata_scaled is your dataframe
# Ensure the date column is of Date type and sort the dataframe by date
upcoviddata_scaled$date <- as.Date(upcoviddata_scaled$date)
upcoviddata_scaled <- upcoviddata_scaled[order(upcoviddata_scaled$date), ]

# Create a time series object for total cases
total_cases_ts <- ts(upcoviddata_scaled$total_cases, start = c(year(min(upcoviddata_scaled$date)), month(min(upcoviddata_scaled$date))), frequency = 365)

# Plot the time series
plot(total_cases_ts, main = "Total COVID-19 Cases Over Time", xlab = "Date", ylab = "Total Cases")

# Check for stationarity
adf_result <- adf.test(total_cases_ts)
print(adf_result)

# Since the series is stationary, fit the ARIMA model
fit <- auto.arima(total_cases_ts)
summary(fit)

# Forecast future values
forecast_period <- 30  # Number of days to forecast
forecasted_values <- forecast(fit, h = forecast_period)

# Plot the forecasted values
plot(forecasted_values, main = "Forecast of Total COVID-19 Cases")

# Evaluate the model
checkresiduals(fit)

# Load necessary libraries
library(forecast)
library(ggplot2)
library(dplyr)
install.packages("lubridate")
library(lubridate)


# Convert the date column to Date type
upcoviddata_scaled$date <- as.Date(upcoviddata_scaled$date)

# Aggregate the data by month, continent, and location
monthly_data <- upcoviddata_scaled %>%
  group_by(continent, location, month = floor_date(date, "month")) %>%
  summarise(total_cases = sum(total_cases))

# Initialize an empty list to store results for each continent
results_list <- list()

# Loop through each continent
for(cont in unique(monthly_data$continent)) {
  # Filter data for the current continent
  continent_data <- filter(monthly_data, continent == cont)
  
  # Split the data into training and testing sets based on the date
  n <- nrow(continent_data)
  train_size <- floor(n / 2)
  
  train_data <- continent_data[1:train_size, ]
  test_data <- continent_data[(train_size + 1):n, ]
  
  # Extract the total_cases column for training and testing
  train_cases <- ts(train_data$total_cases, frequency = 12)  # Monthly data, frequency = 12
  
  # Fit an ARIMA model (or any other suitable model)
  fit <- auto.arima(train_cases)
  
  # Forecast the next half of the data points
  forecast_length <- n - train_size
  forecasts <- forecast(fit, h = forecast_length)
  
  # Extract the predicted values
  predicted_cases <- as.numeric(forecasts$mean)
  
  # Calculate residuals
  residuals <- test_data$total_cases - predicted_cases
  
  # Create a data frame with actual, predicted values, and residuals for comparison
  results <- data.frame(
    continent = cont,
    date = test_data$month,
    actual = test_data$total_cases,
    predicted = predicted_cases,
    residuals = residuals
  )
  
  # Append the results to the list
  results_list[[cont]] <- results
}

# Combine all results into a single data frame
all_results <- bind_rows(results_list)

# Plot the residuals for each continent
p_residuals <- ggplot(all_results, aes(x = date, y = residuals)) +
  geom_line(color = "black") +
  labs(title = "Residuals of Predictions by Continent (Monthly)",
       x = "Date",
       y = "Residuals") +
  theme_minimal() +
  facet_wrap(~ continent, scales = "free_y")

# Print the residuals plot
print(p_residuals)
# Plot the actual vs. predicted values for each continent
p_actual_vs_predicted <- ggplot(all_results, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), size = 1) +
  geom_line(aes(y = predicted, color = "Predicted"), size = 1, linetype = "dashed") +
  labs(title = "Actual vs. Predicted Total Cases by Continent (Monthly)",
       x = "Date",
       y = "Total Cases") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  facet_wrap(~ continent, scales = "free_y") +
  theme(legend.title = element_blank())

# Print the actual vs. predicted plot
print(p_actual_vs_predicted)

# Function to compute Mean Absolute Error (MAE)
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# Function to compute Mean Squared Error (MSE)
MSE <- function(actual, predicted) {
  mean((actual - predicted)^2)
}

# Function to compute Root Mean Squared Error (RMSE)
RMSE <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Function to compute Mean Absolute Percentage Error (MAPE)
MAPE <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# Calculate accuracy metrics for each continent
accuracy_metrics <- all_results %>%
  group_by(continent) %>%
  summarise(
    MAE = MAE(actual, predicted),
    MSE = MSE(actual, predicted),
    RMSE = RMSE(actual, predicted),
    MAPE = MAPE(actual, predicted)
  )

# Print accuracy metrics for each continent
print(accuracy_metrics)