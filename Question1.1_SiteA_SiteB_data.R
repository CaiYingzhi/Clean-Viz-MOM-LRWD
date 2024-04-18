# Load the dplyr package for data manipulation
library(dplyr)
library(lubridate)
library(Rserve)
library(writexl)

# List all CSV files with names starting with "Site" in the current working directory
csv_files_A <- list.files(pattern = "^SiteA.*\\.csv$")
csv_files_B <- list.files(pattern = "^SiteB.*\\.csv$")

# Initialize an empty list to store the data frames
data_list_A <- list()
data_list_B <- list()

# Loop through each CSV file, read it, rename headers, and store it in the list
for (file in csv_files_A) {
  # Read the CSV file with headers
  data <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
  
  # Rename headers, as Headers were not fixed. There was one file that called it Depts
  names(data) <- c("When", "Profile", "Dept", "CardNum")
  
  # Changing NA entries in Dept to "not staff"
  data$Dept[is.na(data$Dept)] <- "not staff"
  
  # Convert data types as some csv file had columns with different data type
  data$Profile <- as.factor(data$Profile)
  data$Dept <- as.character(data$Dept)
  data$CardNum <- as.integer(data$CardNum) # Return NA for values where conversion cannot be performed. E.g. #VALUE! and blank spaces
  
  # Store the renamed and converted data frame in the list
  data_list_A[[file]] <- data
}

for (file in csv_files_B) {
  # Read the CSV file with headers
  data <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
  
  # Rename headers, as Headers were not fixed. There was one file that called it Depts
  names(data) <- c("When", "Profile", "Dept", "CardNum")
  
  # Changing NA entries in Dept to "not staff"
  data$Dept[is.na(data$Dept)] <- "not staff"
  
  # Convert data types as some csv file had columns with different data type
  data$Profile <- as.factor(data$Profile)
  data$Dept <- as.character(data$Dept)
  data$CardNum <- as.integer(data$CardNum) # Return NA for values where conversion cannot be performed. E.g. #VALUE! and blank spaces
  
  # Store the renamed and converted data frame in the list
  data_list_B[[file]] <- data
}

# Combine all data frames into a single data frame using bind_rows
combined_data_A <- bind_rows(data_list_A)
combined_data_B <- bind_rows(data_list_B)

# Add column for combined_data_A to have Site. Similar for B
combined_data_A$Site <- "Site A"
combined_data_B$Site <- "Site B"

# Combine the two datasets
combined_data <- rbind(combined_data_A, combined_data_B)

# Remove NA values from CardNum column
combined_data <- combined_data[!is.na(combined_data$CardNum), ]

# Check for error
# # Check if Profile is 0 and Dept is "not staff"
# invalid_profiles <- combined_data$Profile == 0 & combined_data$Dept == "not staff"
# 
# # Display rows where Profile is 0 but Dept is "not staff"
# invalid_rows <- combined_data[invalid_profiles, ]
# 
# # Check for blank entries in CardNum column
# blank_entries <- which(combined_data$Dept == "")

# there is a blank with NA entry for dept and a blank for a staff member
# remove the row for the NA entry as we do not know if it is 0,1 or 2. For staff member, would just update his profile
for (i in 1:nrow(combined_data)) {
  if (combined_data$Profile[i] == "") {
    if (startsWith(combined_data$Dept[i], "Dept")) {
      combined_data$Profile[i] <- 0
    } else {
      combined_data <- combined_data[-i, ]  # Delete row if Profile is blank and Dept does not start with "Dept"
    }
  }
}
# Realised that there is entries in Profile that use Temp Pass and Staff Pass
combined_data$Profile[combined_data$Profile == "Temp Pass"] <- 1
combined_data$Profile[combined_data$Profile == "Staff Pass"] <- 0

# Realised that there are entries under dept so that it is dept  1 instead of dept 1. Note the 2 space bars
combined_data$Dept <- gsub("^Dept +", "Dept ", combined_data$Dept)

# Check if all entries in the "When" column match the specified format - 20/4/2020 7:17
all_entries_match <- all(grepl("^\\d{1,2}/\\d{1,2}/\\d{4} \\d{1,2}:\\d{2}$", combined_data$When))

# # Print the result
# print(all_entries_match)

# Convert "when" column to datetime format
combined_data$When <- dmy_hm(combined_data$When)

# Extract date and time components
combined_data$Date <- as.Date(combined_data$When)
combined_data$Time <- format(combined_data$When, "%H:%M")

# Assume that one can enter both Site A and Site B on the same day
# According to the question: An individual can tap in and out several times within the same day.
# When the individual first clock in, that would be the earliest time slot and the only record you will base off the analysis.
# Therefore, we assume that if a person clocks in at A and proceeds to clock in at B on the same day, we only need to take the earliest slot.
# Similarly, if person clocks in at B and clock in at A on the same day, we take the earliest slot

# From the question, When the individual first clock in, that would be the earliest time slot and the only record you will base off the analysis
# Assume that this is for each day
# Now, we want rows so that for each day, a CardNum will appear at most once 
# So we remove duplicate clock-in entries for the same CardNum on the same day. This would also remove those duplicate entries where CardNum and When are the same

# Group by date and CardNum, then filter for earliest time slot
filtered_data <- combined_data %>%
  group_by(Date, CardNum) %>%
  filter(When == min(When)) # selects row where When is minimum within the group

# Turning length of CardNum to at least 8 characters

# Loop through each value in CardNum column
# Convert CardNum to characters and pad with '0's
filtered_data$CardNum <- sprintf("%08d", filtered_data$CardNum)

# For Question 1.2
# Export filtered_data to Excel
write_xlsx(filtered_data, "filtered_data_both.xlsx")
