library(stringr)
library(tidyr)
library(dplyr)
library(zoo)
library(Metrics) # nolint
library(caret)
library(MASS) # nolint
library(ggplot2)
library(reshape2)
library(mltools)
library(DescTools) # nolint
library(plotly)
Intel_CPUs = read.csv("dataset/Intel_CPUs.csv", na.strings = c("", "N/A")) # nolint # c is base in R
CPUs_data = Intel_CPUs[,c("Lithography", # nolint 
                      "nb_of_Cores","nb_of_Threads","Processor_Base_Frequency", # nolint
                      "Cache","Instruction_Set","TDP","Max_Memory_Size","Max_nb_of_Memory_Channels","Max_Memory_Bandwidth", "Bus_Speed")] # nolint
# Print out table of summary data
# print(summary(CPUs_data)) # nolint
# Check for N/A data in the table
print(apply(is.na(CPUs_data), 2, sum)) # nolint

print(unique(CPUs_data$Lithography))

# Fill forward NAs
CPUs_data <- na.omit(CPUs_data)

# Remove " nm" and convert to numeric
CPUs_data$Lithography <- as.double(gsub(" nm$", "", CPUs_data$Lithography))

# Create a table of the lithography values
lithography_table <- table(CPUs_data$Lithography)
print(lithography_table)

table(CPUs_data$Bus_Speed)
 
CPUs_data <- separate(CPUs_data, Bus_Speed, c("Bus_Speed_Value", "Bus_Speed_Unit", "Bus_Interface_type"), sep = " ")
CPUs_data$Bus_Speed_Value <- as.numeric(CPUs_data$Bus_Speed_Value)
CPUs_data$Bus_Speed_Unit_GTs <- c(str_extract(CPUs_data$Bus_Speed_Unit, "GT/s"))
CPUs_data <- na.omit(CPUs_data)
CPUs_data <- select(CPUs_data, -Bus_Speed_Unit_GTs)
CPUs_data <- select(CPUs_data, -Bus_Speed_Unit)


# Drop all the N/A in this row
CPUs_data <- CPUs_data[complete.cases(CPUs_data$Max_Memory_Size), ] # count Max_Mem_Size # nolint
max_mem_size_clean <- function(size) {
  if (grepl("G", size)) {
    return(as.double(gsub(" GB", "", size)))
  }
  return(as.double(gsub(" TB", "", size)) * 1024)
}
CPUs_data$Max_Memory_Size <- sapply(CPUs_data$Max_Memory_Size, max_mem_size_clean) # apply to a table with only numeric # nolint
table(CPUs_data$Max_Memory_Size)

max_memory_bandwidth_clean <- function(mem) {
    return (as.double(strsplit(mem, " ")[[1]][1])) # strsplit() is split mem into 2 part seperate with a blank space. # nolint
}
CPUs_data$Max_Memory_Bandwidth <- sapply(CPUs_data$Max_Memory_Bandwidth, max_memory_bandwidth_clean) # apply to a table with only numeric # nolint
CPUs_data <- CPUs_data[!is.na(CPUs_data$Max_Memory_Bandwidth), ] # Remove rows with NA #nolint
# Now you can check the result
# table(CPUs_data$Max_Memory_Bandwidth)

# Drop rows with any NA values in relevant columns
CPUs_data <- CPUs_data[complete.cases(CPUs_data[, c("nb_of_Threads", "Processor_Base_Frequency")]), ] #nolint
table(CPUs_data$nb_of_Threads)
# Function to convert Processor_Base_Frequency
base_frequency <- function(f) {
  if (grepl(" GHz", f)){
    return(as.double(gsub(" GHz", "", f)) * 1000) # Convert GHz to MHz
  }
  return(as.double(gsub(" MHz", "", f))) # Keep MHz as is
}

# Apply the base_frequency function
CPUs_data$Processor_Base_Frequency <- as.integer(sapply(CPUs_data$Processor_Base_Frequency, base_frequency)) # Convert to int #nolint

# Drop any remaining rows with NA in Processor_Base_Frequency after conversion
CPUs_data <- CPUs_data[!is.na(CPUs_data$Processor_Base_Frequency), ]

CPUs_data$TDP <- as.double(gsub(" W", "", CPUs_data$TDP))
mean_TDP <- mean(CPUs_data$TDP, na.rm = TRUE)
CPUs_data$TDP[is.na(CPUs_data$TDP)] <- mean_TDP

Cache_Clean_Size <- function(size){
    if (grepl(' K', size)){ # nolint
        return(as.double(gsub(" K", "", size)) / 1024) # nolint
    } # nolint
    return(as.double(gsub(" M", "", size)))
}
#spilt the data into 2 part include cache size and cache type
CPUs_data <- separate(CPUs_data, Cache, into = c("Cache_Size", "Cache_Type"), sep = "B") # nolint
CPUs_data <- CPUs_data[complete.cases(CPUs_data[, c("Cache_Size", "Cache_Type")]), ] #nolint
#solving string and transform to int
CPUs_data$Cache_Size <- sapply(CPUs_data$Cache_Size, Cache_Clean_Size)
CPUs_data$Cache_Size <- log(CPUs_data$Cache_Size) # stabilize variance
#seperate will remove some data so we need to add it back
CPUs_data$Cache_Type <- ifelse(CPUs_data$Cache_Type == "", "Normal", sub(" ", "", CPUs_data$Cache_Type)) # nolint
table(CPUs_data$Cache_Type)

CPUs_data <- CPUs_data[!is.na(CPUs_data$Instruction_Set), ]
table(CPUs_data$Instruction_Set)

print(apply(is.na(CPUs_data),2,sum)) # nolint: commas_linter.