# Load necessary libraries
library(tidyverse) # For data manipulation and visualization

# Get and print the current working directory
current_dir <- getwd()
print(current_dir)

# Set your working directory
setwd("C:/Users/nhelder/Documents/projects/coral_reefs")

library(readxl)
# fish <- read_excel("GCRMN Fish data sheet analysis.xlsx", skip=4)
fish <- read_excel("GCRMN Fish data sheet analysis.xlsx", col_names=FALSE)
??read_excel
# Step 1: Import entire spreadsheet, excluding the first row from being columns

data <- fish
# Step 2: Remove the first 4 rows and store them in a separate list
metadata <- data[1:5, ]
data <- data[-(1:5), ]

# Step 3: Assign column names to the data
col_names <- c("Species_Class", paste("Transect_", 1:5, sep = ""))
colnames(data) <- col_names

# Step 4: Separate values in the first column into "Species" and "Class"
data2 <- data %>%
  separate(Species_Class, into = c("Species", "Class"), sep = " ", remove = FALSE)

# View the modified data
head(data)

# Step 5: Reshape data to long format for visualization
data_long <- data2 %>%
  pivot_longer(cols = starts_with("Transect_"), names_to = "Transect", values_to = "Count")



df <- metadata

# Remove all NA values and filter out rows with only NA values in X2
df_cleaned <- df %>%
  select(...1, ...2) %>%
  filter(!is.na(...1) & !is.na(...2))

# Add a unique ID column
df_cleaned <- df_cleaned %>%
  mutate(ID = row_number())

# Rename columns to appropriate names
colnames(df_cleaned) <- c("Category", "Value", "ID")

# View the cleaned dataframe
print("Cleaned Dataframe:")
print(df_cleaned)



# Load the necessary package
library(tidyverse)

# Create the initial dataframe
df <- data.frame(
  X1 = c("Date", "Site", "Depth", "Data recorded by"),
  X2 = c("June 8 2024", "Test Site", "30 m", "Noelle"),
  X3 = c(NA, NA, NA, NA),
  X4 = c(NA, NA, NA, NA),
  X5 = c(NA, NA, NA, NA),
  X6 = c(NA, NA, NA, NA)
)

# View the original dataframe
print("Original Dataframe:")
print(df)

# Remove rows where all columns are NA
df_cleaned <- df %>%
  select(X1, X2) %>%
  filter(!is.na(X1) & !is.na(X2))

# Transpose the dataframe
df_transposed <- df_cleaned %>%
  pivot_wider(names_from = X1, values_from = X2)

# Add a unique ID column
df_final <- df_transposed %>%
  mutate(ID = row_number())

# Reorder columns to place ID first
df_final <- df_final %>%
  select(ID, everything())

# View the final dataframe
print("Final Dataframe:")
print(df_final)





# Function to clean and reshape a single metadata dataframe
clean_and_reshape <- function(df) {
  # Remove rows where all columns are NA
  df_cleaned <- df %>%
    dplyr::select(`...1`, `...2`) %>%
    filter(!is.na(`...1`) & !is.na(`...2`))
  
  # Transpose the dataframe
  df_transposed <- df_cleaned %>%
    pivot_wider(names_from = `...1`, values_from = `...2`)
  
  # Add a unique ID column
  df_final <- df_transposed %>%
    mutate(ID = row_number())
  
  # Reorder columns to place ID first
  df_final <- df_final %>%
    select(ID, everything())
  
  return(df_final)
}


# List of file paths
file_paths <- list.files(path = "Fish_Data/", pattern = "*.xlsx", full.names = TRUE)

# Initialize an empty list to store the results
results_list <- list()

# Loop through each file, read the data, and apply the function
for (file in file_paths) {
  df <- read_excel(file, col_names=FALSE)
  metadata <- df[1:4, ]
  
  data <- df[-(1:5), ]
  
  cleaned_df <- clean_and_reshape(metadata)
  results_list <- append(results_list, list(cleaned_df))
}

# Combine all the results into a single dataframe
metadata <- bind_rows(results_list)

# View the final combined dataframe
print("Final Cleaned Metadata:")
print(final_df)

# Batch procesing code below! 
#######################################
#####################################
# Function to clean and reshape metadata
clean_and_reshape_metadata <- function(metadata, file_id) {
  # Remove rows where all columns are NA
  metadata_cleaned <- metadata %>%
    filter(!is.na(...1) & !is.na(...2))
  
  # Transpose the dataframe
  metadata_transposed <- metadata_cleaned %>%
    pivot_wider(names_from = ...1, values_from = ...2)
  
  # Add a unique ID column for each file
  metadata_final <- metadata_transposed %>%
    mutate(ID = file_id)
  
  return(metadata_final)
}

# Function to clean and process data
clean_and_process_data <- function(data, file_id) {
  # Assign column names to the data
  col_names <- c("Species_Class", paste("Transect_", 1:5, sep = ""))
  colnames(data) <- col_names
  
  # Separate values in the first column into "Species" and "Class"
  data2 <- data %>%
    separate(Species_Class, into = c("Species", "Class"), sep = " ", remove = FALSE)
  
  # Convert columns to appropriate types
  data2 <- data2 %>%
    mutate(across(starts_with("Transect_"), as.numeric)) %>%
    mutate(Species = as.factor(Species),
           Class = as.factor(Class))
  
  # Step 5: Reshape data to long format for visualization
  data_long <- data2 %>%
    pivot_longer(cols = starts_with("Transect_"), names_to = "Transect", values_to = "Count") %>%
    mutate(ID = file_id)
  
  return(data_long)
}

# Function to process each file
process_file <- function(file, file_id) {
  # Read the data
  df <- read_excel(file, col_names = FALSE)
  
  # Separate metadata and data
  metadata <- df[1:4, ]
  data <- df[-(1:5), ]
  
  # Clean and reshape metadata
  cleaned_metadata <- clean_and_reshape_metadata(metadata, file_id)
  
  # Clean and process data
  processed_data <- clean_and_process_data(data, file_id)
  
  return(list(metadata = cleaned_metadata, data = processed_data))
}



# Initialize an empty list to store the results
file_paths <- list.files(path = "Fish_Data/", pattern = "*.xlsx", full.names = TRUE)


# Initialize empty lists to store the results
metadata_list <- list()
data_list <- list()

# Loop through each file, read the data, and apply the function
for (i in seq_along(file_paths)) {
  file <- file_paths[i]
  results <- process_file(file, file_id = i)
  
  metadata_list <- append(metadata_list, list(results$metadata))
  data_list <- append(data_list, list(results$data))
}

# Combine all the results into two separate dataframes
final_metadata_df <- bind_rows(metadata_list)
final_data_df <- bind_rows(data_list)

# View the final combined dataframes
print("Final Metadata Dataframe:")
print(final_metadata_df)

print("Final Data Dataframe:")
print(final_data_df)


# Exploratory Plots with ggplot2 (Grammar of Graphics!)

# One of the BEST uses of R is creating data visualizations! We will use a package
# called ggplot2. This package bases its visualizations on a series of elements joined 
# together by a + sign. The elements are layered, so whatever comes first goes underneath. 
# ggplot2 also likes input data as a dataframe. 

# Histogram
ggplot(final_data_df) + # This line tells us which data we will use
  aes(x=Count) + # aes = "Aesthetics". Declare what goes on the x-axis (must be a column from the dataframe)
  geom_histogram() # this is called a 'geometry' object. This tells how how to draw the data onto the page. 

ggplot(final_data_df) +
  aes(x=Species, y=Count,) +
  geom_bar(stat="identity")
