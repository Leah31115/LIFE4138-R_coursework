"
This R script will read in pumpkin data in the form of a csv file. 
Essential columns required in the csv file include: 
- id, weight_lbs, variety, country, est_weight

This script will:
- Identify the heaviest pumpkin grown
- Convert weights from pounds to kilograms
- Classify pumpkin weight
- Identify the country (out of USA, UK, and Australia) with the highest mean pumpkin weight (lbs) and variety with the lowest mean pumpkin weight (lbs) 

The output of this script will make:
- A scatterplot of estimated vs actual weights
- A csv file of only USA, Australia and UK pumpkins
- A boxplot of pumpkin weights from USA, UK, and Australia with and without a variety facet

The output files will be stored within a new folder where your csv file is located named Pumpkin_output
"

library(tidyverse)

# Read in pumpkin data
pumpkin_path = file.choose()
pumpkin_parent_dir = dirname(pumpkin_path)
pumpkin_data <- read.csv(pumpkin_path, header = T)

# Set working directory to the parent folder of the pumpkin.csv file
setwd(pumpkin_parent_dir)

# Make a new folder where our generated files from this script will be stored
# The new folder will be within the parent folder of the pumpkin.csv file. 
# If the file already exists, output the statement "This folder already exists" so previous data is not overwritten.
ifelse(!dir.exists("Pumpkin_output"), dir.create("Pumpkin_output"), "This folder already exists")


# Identify the heaviest pumpkin
# ID represents the year in which the pumpkin was grown
heaviest_pumpkin <- pumpkin_data %>%
  select(id, weight_lbs, variety, country, city, state_prov) %>%
  arrange(desc(weight_lbs)) %>%
  slice(which.max(weight_lbs))

heaviest_pumpkin


# Function to convert the weight in pounds (lb) to kilograms (kg)
lb_to_kg_converter <- function(lb_mass){
  kg_mass <- 0.453592 * lb_mass

  return(kg_mass)
}

pumpkin_data["weight_kg"] <- lapply(pumpkin_data["weight_lbs"], lb_to_kg_converter)


# Categorize the pumpkins into light (if less than 700 lbs), medium (if 700 lbs <= x < 1400 lbs), and heavy (if equal or greater than 1400) in a new column called weight_class
pumpkin_data$weight_class <- NA

pumpkin_data$weight_class <- ifelse(pumpkin_data$weight_lbs < 700, "Light",
                                ifelse(pumpkin_data$weight_lbs > 1400, "Heavy", "Medium"))


# Scatter graph to compare estimated weight and actual weight
est_vs_weight <- ggplot(pumpkin_data, aes(x=weight_lbs, y=est_weight, colour = weight_class)) +
  geom_point(alpha = 0.5) +
  xlab("Weight (lbs)") +
  ylab("Estimated weight (lbs)") +
  ggtitle("Pumpkin weight vs estimated weight") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
  
est_vs_weight

# Save the plot as a jpeg, to the Pumpkin_output folder
jpeg("Pumpkin_output/estimated_vs_actual_weight_scatter_plot.jpg", width = 13, height = 10, units = "cm", res = 600)
est_vs_weight
dev.off()


# Filter pumpkin_data from the USA, UK, and Australia
USA_UK_Au_pumpkins <- pumpkin_data %>%
  filter(country %in% c("USA", "UK", "Australia"))

# save the filtered USA/UK/Australian pumpkins as a csv file
write.csv(USA_UK_Au_pumpkins, file = "Pumpkin_output/USA_UK_Au_pumpkins.csv")


# Mean pumpkin weight (lbs) for the USA_UK_Au_pumpkins
mean_USA_UK_Au_weights <- USA_UK_Au_pumpkins %>%
  group_by(country) %>%
  summarize(mean_weight_lbs = mean(weight_lbs, na.rm = T))

mean_USA_UK_Au_weights

# The country with the highest mean weight (lbs) out of the three countries:
mean_USA_UK_Au_weights %>%
  slice(which.max(mean_weight_lbs))

# The mean weight for each variety of pumpkin for each of the three countries
mean_variety <- USA_UK_Au_pumpkins %>%
  group_by(variety, country) %>%
  summarize(mean_weights_lbs = mean(weight_lbs, na.rm = T))

mean_variety

# The variety and country with the lowest mean weight
min_mean_var <- mean_variety %>%
  arrange(-desc(mean_weights_lbs))

min_mean_var[1,]  


# Boxplot of pumpkin weight (lbs) distributions for the three countries.
country_weight_distributions <- ggplot(USA_UK_Au_pumpkins, aes(x = country, y = weight_lbs)) +
  geom_boxplot() +
  xlab("Country") +
  ylab("Weight (lbs)") + 
  ggtitle("Pumpkin weights from Australia, USA, and UK") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

country_weight_distributions

# Save the boxplot as a jpeg, to the Pumpkin_output folder
jpeg("Pumpkin_output/country_weight_distributions_boxplot.jpg", width = 13, height = 10, units = "cm", res = 600)
country_weight_distributions
dev.off()


# Facet boxplot of pumpkin weight (lbs) distributions for the three countries showing the data from each variety of pumpkin as a separate sub-plot. 
variety_country_weight_distributions <- ggplot(USA_UK_Au_pumpkins, aes(x = country, y = weight_lbs)) +
  geom_boxplot() +
  facet_wrap(.~variety) +
  xlab("Country") +
  ylab("Weight (lbs)") +
  ggtitle("Pumpkin weights from Australia, USA, and UK") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

variety_country_weight_distributions

# Save the facet plot as a jpeg, to the Pumpkin_output folder
jpeg("Pumpkin_output/variety_country_weight_distributions_boxplot.jpg", width = 13, height = 10, units = "cm", res = 600)
variety_country_weight_distributions
dev.off()
