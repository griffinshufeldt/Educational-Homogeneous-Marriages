#-----
#This coding sample is from the Taiwan writing sample. The main goal was to randomly sort couples
#from the family income survey, then compare pre and post gini values. This was in order to see the 
#correlation between educational homogenous marriage, and income inequality. This exploration was done 
#to assist professor Shane Su from National Taiwan University on his paper concerned with positive educational
#sorting in Taiwan. Graphs from this data were made in Stata.
#-----

#Necessary libraries
library(tidyverse)
library(haven)
library(DescTools)

#Setting seed for reproducibility
set.seed(1129)

#Takes a list of files from a directory, edit pattern if your files are CSV instead of STATA
file_list <- list.files("INSERT PATH HERE", pattern = "\\.dta$", full.names = TRUE)

#Defines the number of times observations will be randomized
num_draws <- 10
#The goal of this loop is to randomly shuffle couples from Taiwan's Family Income Survey Dataset, then calculatin the ginicoefficients for each year.
#The loop accommodates for the fact that this analysis was done over 40 years of survey data, without having to combine every year into one larger dataset.
random_gini_values_list <- vector("list", length(file_list))
gini_values <- vector("numeric", length(file_list))

for (file_idx in seq_along(file_list)) {
  file_name <- file_list[file_idx]
  file_data <- read_dta(file_name)
  
  random_gini_values_list[[file_idx]] <- vector("numeric", num_draws)
  
  for (i in 1:num_draws) {
    #Creating two datasets, one for wife and husband
    husband_data <- filter(file_data, gender == 1)
    wife_data <- filter(file_data, gender == 2)
    
    #Shuffling each of the wife and husband's data sets separately, this ensures that we don't accidentally end up with same-sex couples
    shuffled_husband_data <- husband_data[sample(nrow(husband_data)), ]
    shuffled_wife_data <- wife_data[sample(nrow(wife_data)), ]
    
    #Combining the shuffled data sets and looking at their income
    matched_pairs <- data.frame(id_man = shuffled_husband_data$income, id_woman = shuffled_wife_data$income)
    
    #Creating combined income variable
    matched_pairs_income <- matched_pairs %>%
      mutate(matched_income = id_man+id_woman)
    
    #Calculating Gini
    random_gini_values_list[[file_idx]][i] <- Gini(matched_pairs_income$matched_income, na.rm = TRUE)
  }
  
  #Creating combined income variable directly in the data frame
  file_data <- file_data %>%
    mutate(combined_income = itm191 + itm192)
  
  #Calculating Gini
  coefficient <- Gini(file_data$combined_income, na.rm = TRUE)
  gini_values[file_idx] <- coefficient
}

#Combine the Gini coefficients and random Gini values into a data frame
gini_df <- data.frame(gini_coefficient = gini_values)

#Add the random Gini values as separate columns
gini_df <- cbind(gini_df, do.call(rbind, random_gini_values_list))

#Calculate the average of random Gini values and add it as a new column
average_gini <- rowMeans(do.call(rbind, random_gini_values_list))
gini_df$average_gini <- average_gini

#Assigning names to columns in new dataset
colnames(gini_df)[1] <- "actual_gini"

random_gini_cols <- paste0("random_gini_", 1:10)
colnames(gini_df)[2:11] <- random_gini_cols

print(gini_df)