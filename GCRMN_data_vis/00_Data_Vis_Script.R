# Intro to Data Visualization in R for Coral Reef Ecology
# Author: Noelle Helder
# Date: May 28, 2024


# This script provides the bare bones code for data tidying + visualization
# from the tutorial, without all of the extra explanations.  

# 1. Bringing data into R
# 2. Explore + Clean Data
#   - Working with columns
#   - Tidy data problem #1: multiple variables in one column
#   - Tidy data problem #2: column headers are values, not variables
#   - Calculate a new variable
#   - Combining data: 
#       - Add survey metadata to tidy data using bind
#   - Understanding data types
#   - Export tidy data
# 3. Data visualization with ggplot2



# install.packages(c("tidyverse", readxl"))

# Import the necessary libraries
library(tidyverse)
library(readxl)

##########################################
# Bringing Data into R
##########################################

# update dir with the actual path on YOUR computer
# dir = 'YOUR_PATH_HERE'
dir <- 'C:/Users/nhelder/Documents/projects/Union_2024/GRCMN_data_vis'
setwd(dir)

# Import an example fish data sheet (replace anything inside quotes with 
# the actual name of the file in your folder).
fish_load <- read_excel("Fish_Data/GCRMN Fish data sheet analysis.xlsx", col_names=FALSE)


##########################################
# Explore + Clean Data 
##########################################
# Look at the first 6 rows 
head(fish_load)

# I like to make a copy of the data that I just loaded in to start making edits to. 
# That way, if I make a mistake, I don't have to re-load everything, I can just start here again. 
fish <- fish_load

# Store the first 4 rows in a separate object called 'metadata'
metadata <- fish[1:4, ]

# ... then remove those same 4 rows from the fish df using the minus sign. 
fish <- fish[-(1:5), ]

# Check that this worked! We should see our data starting on the first row without 
# the header information. 
# head(fish)
# head(metadata)

### Working with columns in R ###
##################################
# Assign meaningful column names to the data - remember, no spaces!
# A sneaky option to rename these without typing Transect_ every time.
col_names <- c("Family_Class", paste("Transect_", 1:5, sep = ""))

# Assign the list of column names to your fish data using the colnames() function
colnames(fish) <- col_names

# Check that this worked!
colnames(fish)

### Tidy data problem #1: multiple variables in one column ### 
###################################################
# Separate values in the first column into separate "Family" and "Class" columns following
# the principles of Tidy data. Because the information is separated by a space, we enter a space in the sep argument. 
fish <- fish %>%
  separate(Family_Class, into = c("Family", "Class"), sep = " ", remove = TRUE)


### Tidy data problem #2: column headers are values, not variables
#####################################################
# Use the pivot_longer function to reshape the data
tidy_fish <- fish %>%
  pivot_longer(cols = starts_with("Transect_"), names_to = "Transect", values_to = "Count")


### Calculate a new variable for our data###
############################################
### fish density (fish density = surveyed abundance/survey area)
# First, let's set our area values - update accordingly
transect_length = 25
transect_width = 2

# You can multiply things together using the * operator
transect_area = transect_length*transect_width

# Calculate a new column called "Density" and add it to tidy_data
tidy_fish$Density <- (tidy_fish$Count/transect_area)


### Combining data
##################################
# We can add new columns to our tidy data using bind_cols()
wide_metadata <- metadata %>%
  # keep only the the first two columns from the metadata object (drops the rest!)
  select(...1, ...2) %>%
  # convert from long to wide format so that values from column 1 become variables with values from column 2
  pivot_wider(names_from = ...1, values_from = ...2)

# Add the new metadata columns to tidy_fish
tidy_fish <- tidy_fish %>%
  # add new columns
  bind_cols(wide_metadata) 


# What if I had multiple tidy fish surveys that I wanted to combine? 
# Let's duplicate our tidy_fish data for an example
tidy_fish_duplicate <- tidy_fish

# use bind_rows to add rows from one dataframe to another
combined_test <- bind_rows(tidy_fish, tidy_fish_duplicate)

# Check (should add up tonumber of rows from both input dataframes together)
nrow(combined_test)


### Understanding data types in R ###
#####################################
# Use `mutate` to convert several columns at once to different types. 
# Convert columns to appropriate types for plotting - add the rest of the columns as needed! 
tidy_fish <- tidy_fish %>%
  mutate(Family = as.factor(Family),
         Class = as.factor(Class),
         Transect = as.factor(Transect),
         Count = as.numeric(Count))

# Replace NAs with 0s in the 'Count' column - repeat for other columms as needed
tidy_fish$Count[is.na(tidy_fish$Count)] <- 0

### Export tidy data as a .csv file to your working directory ###
###########################################################
write.csv(tidy_fish, "tidy_fish.csv", row.names=FALSE)


##################################
# Visualize tidy data with ggplot2
##################################

### Histogram - frequency of counts
# This first line tells what object (dataframe) to use as input and creates an empty plot
ggplot(tidy_fish) + 
  # declare what goes on the x-axis (must be a column) in your dataframe
  aes(x=Count) + 
  # this is called a 'geometry' object
  # This tells how how to draw the data onto the page  
  geom_histogram() 


### Barplot - Counts across transects
ggplot(tidy_fish) +
  # specify our x axis for different transects, and our y axis for our count data. 
  aes(x=Transect, y=Count) +
  # Specify that we want the actual values used for our bar plot with stat="identity"
  geom_bar(stat="identity")


### Barplot - Counts across transects, colored by Family
ggplot(tidy_fish) +
  aes(x=Transect, y=Count, fill=Family) +
      # specifying the 'fill' argument changes the color of the bar based on a category in your data 
  geom_bar(stat="identity")

### Barplot - separated for each transect using facet_grid(~group_variable)
ggplot(tidy_fish) +
  aes(x=Family, y=Count, fill=Family) +
  geom_bar(stat="identity") +
  # facet by Transect 
  facet_grid(~Transect)

### Barplot with more advanced options - saved as an object called fish_plot
fish_plot <- ggplot(tidy_fish) +
  aes(x=Family, y=Count, fill=Family) +
  geom_bar(stat="identity") +
  # facet by Transect
  facet_grid(~Transect) +
  # add and edit plot labels 
  labs(x="Family", # x-axis
       y="Number Observed", # y-axis
       title="Title for my Very Important Data",
       subtitle = "Subtitle for my very important fish count data") +
  # add a pre-set theme
  theme_bw() + 
  # modify specific parts of the theme
  theme(legend.position="bottom", # move the legend below the plot
        # get rid of the legend title
        legend.title=element_blank(),
        # get rid of the x-axis title
        axis.title.x=element_blank(),
        # get rid of the x-axis labels
        axis.text.x=element_blank(),
        # get rid of the x-axis tick marks
        axis.ticks.x=element_blank())

# When you save a plot to a variable, it doesn't show up automatically.
# To view your plot, run this: 
fish_plot  


# Then you can add stuff to it without re-typing the code each time
# For example, lets add a caption and center it below the plot. 
fish_plot <- fish_plot +
  labs(caption="Figure 1: Counts of each fish family across transects.") +
  theme(plot.caption = element_text(hjust = 0.4))

# A plot with a caption
fish_plot

# Export your pretty plot as an image
# Save as PNG file to your working directory folder. 
# ggsave("pretty_fish_counts.png", fish_plot, width = 6, height = 4, dpi = 300)

# Use `subset` to visualize only specific parts of your data
# Filter the data to include only rows where the Family is "Parrotfish"
parrotfish_data <- subset(tidy_fish, Family == "Parrotfish")

# Create a bar plot of the counts of Parrotfish
ggplot(parrotfish_data, 
       aes(x = Class, y = Count)) +
  geom_bar(stat = "identity") +
  labs(title = "Parrotfish Counts", x = "Species", y = "Count") +
  theme_minimal()
