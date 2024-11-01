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
CPUs_data <- na.omit(CPUs_data) # nolint

# Remove " nm" and convert to numeric
CPUs_data$Lithography <- as.double(gsub(" nm$", "", CPUs_data$Lithography)) # nolint

# Create a table of the lithography values
lithography_table <- table(CPUs_data$Lithography)
print(lithography_table)

table(CPUs_data$Bus_Speed)
 
CPUs_data <- separate(CPUs_data, Bus_Speed, c("Bus_Speed_Value", "Bus_Speed_Unit", "Bus_Interface_type"), sep = " ") # nolint
CPUs_data$Bus_Speed_Value <- as.numeric(CPUs_data$Bus_Speed_Value) # nolint
CPUs_data$Bus_Speed_Unit_GTs <- c(str_extract(CPUs_data$Bus_Speed_Unit, "GT/s")) # nolint
CPUs_data <- na.omit(CPUs_data) # nolint
CPUs_data <- select(CPUs_data, -Bus_Speed_Unit_GTs) # nolint
CPUs_data <- select(CPUs_data, -Bus_Speed_Unit) # nolint


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
table(CPUs_data$Max_Memory_Bandwidth)

# Drop rows with any NA values in relevant columns
CPUs_data <- CPUs_data[complete.cases(CPUs_data[, c("nb_of_Threads", "Processor_Base_Frequency")]), ] #nolint
table(CPUs_data$nb_of_Threads)
# Function to convert Processor_Base_Frequency
base_frequency <- function(f) {
  if (grepl(" GHz", f)) {
    return(as.double(gsub(" GHz", "", f)) * 1000) # Convert GHz to MHz
  }
  return(as.double(gsub(" MHz", "", f))) # Keep MHz as is
}

# Apply the base_frequency function
CPUs_data$Processor_Base_Frequency <- as.integer(sapply(CPUs_data$Processor_Base_Frequency, base_frequency)) # Convert to int #nolint

# Drop any remaining rows with NA in Processor_Base_Frequency after conversion
CPUs_data <- CPUs_data[!is.na(CPUs_data$Processor_Base_Frequency), ] # nolint

CPUs_data$TDP <- as.double(gsub(" W", "", CPUs_data$TDP)) # Remove " W" and convert to double # nolint
CPUs_data <- CPUs_data[!is.na(CPUs_data$TDP), ] # Drop rows with NA in TDP

Cache_Clean_Size <- function(size){ # nolint
    if (grepl(' K', size)){ # nolint
        return(as.double(gsub(" K", "", size)) / 1024) # nolint
    } # nolint
    return(as.double(gsub(" M", "", size)))
}
#spilt the data into 2 part include cache size and cache type
CPUs_data <- separate(CPUs_data, Cache, into = c("Cache_Size", "Cache_Type"), sep = "B") # nolint
CPUs_data <- CPUs_data[complete.cases(CPUs_data[, c("Cache_Size", "Cache_Type")]), ] #nolint
#solving string and transform to int
CPUs_data$Cache_Size <- sapply(CPUs_data$Cache_Size, Cache_Clean_Size) # nolint
table(CPUs_data$Cache_Size)
#seperate will remove some data so we need to add it back
CPUs_data$Cache_Type <- ifelse(CPUs_data$Cache_Type == "", "Normal", sub(" ", "", CPUs_data$Cache_Type)) # nolint
table(CPUs_data$Cache_Type)

CPUs_data <- CPUs_data[!is.na(CPUs_data$Instruction_Set), ] # nolint
table(CPUs_data$Instruction_Set)

print(apply(is.na(CPUs_data),2,sum)) # nolint: commas_linter.

str(CPUs_data)
# chia dữ liệu thành hai loại định tính và định lượng
numerical_cols <- c("Lithography", "nb_of_Cores", "nb_of_Threads",
                   "Processor_Base_Frequency","Cache_Size","TDP","Max_Memory_Size" #nolint
                   , "Max_nb_of_Memory_Channels",
                   "Max_Memory_Bandwidth", "Bus_Speed_Value")
categorical_cols <- c("Cache_Type", "Instruction_Set",
                      "Bus_Interface_type")

#-------------------------------------------------------------------------------------- #nolint
# tạo bảng thống kê các dữ liệu định lượng
summary_numeric_table <- data.frame(
  Staticstic=c("Count", "Mean", "STD", "Min", "First Quantile", "Median", "Third Quantile", "Max") #nolint
)

convert_to_numeric <- function(x) {
  as.numeric(gsub("[^0-9\\.]", "", x))  # Loại bỏ các ký tự không phải số hoặc dấu chấm #nolint
}


for (i in numerical_cols){
  #loại bỏ tên của vector
  if (is.character(CPUs_data[[i]])) {
    numeric_data <- convert_to_numeric(CPUs_data[[i]])  # Chuyển dữ liệu dạng ký tự về số #nolint
  } else {
    numeric_data <- unname(CPUs_data[[i]])  # Loại bỏ tên khỏi các giá trị
  }
  #tính toán từng cols
  count <- length(numeric_data)
  mean <- mean(numeric_data)
  std <- sd(numeric_data)
  min <- min(numeric_data)
  first_quantile <- quantile(numeric_data, probs = 0.25)
  median <- median(numeric_data)
  third_quantile <- quantile(numeric_data, probs = 0.75)
  max <- max(numeric_data)
  summary_numeric_table <- cbind(summary_numeric_table, new_col = c(count, mean, std, min, #nolint
                                                                    first_quantile, median, # nolint
                                                                    third_quantile, max)) #nolint
}
#chỉnh lại tên cột hợp lí
colnames(summary_numeric_table)[-1] <- numerical_cols
summary_numeric_table

cor_data <- cor(CPUs_data[numerical_cols])


#------------------------------------------------------------------------------------ #nolint
#tạo bảng thống kê biến định tính
summary_categorical_table <- data.frame(
  Staticstic = c("Count", "Unique", "Mode", "Freq")
)

Mode <- function(x) { #nolint
  uniqx <- unique(x)  # Tìm các giá trị duy nhất
  uniqx[which.max(tabulate(match(x, uniqx)))]  # Tìm giá trị xuất hiện nhiều nhất #nolint
}
#tạo bảng theo các biến định tính
for(i in categorical_cols){
  count <- length(CPUs_data[[i]])
  unique <- length(unique(CPUs_data[[i]]))
  mode <- as.character(Mode(CPUs_data[[i]]))
  freq <- sum(CPUs_data[[i]] == mode)
  summary_categorical_table <- cbind(summary_categorical_table, new_col = c(count, #nolint
                                                                         unique, #nolint
                                                                         mode,freq)) #nolint
}
#đổi lại tên cột cho hợp lí
colnames(summary_categorical_table)[-1] <- categorical_cols
#------------------------------------------------------------------------------------ #nolint
# vẽ biểu đồ phân phối đồ thị cho biến Bus_speed_value
ggplot(CPUs_data, aes(x = Bus_Speed_Value)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Bus Speed Value",
       x = "Bus Speed Value",
       y = "Frequency") +
  theme_minimal()
# vẽ biểu đồ phân phối đồ thị cho biến nb_of_cores
ggplot(CPUs_data, aes(x = nb_of_Cores)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Number of Cores",
       x = "Number of Cores",
       y = "Frequency") +
  theme_minimal()
# Vẽ biểu đồ phân phối đồ thị cho biến nb_of_Threads
ggplot(CPUs_data, aes(x = nb_of_Threads, y = after_stat(density))) +
  geom_histogram(binwidth = 2, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Probability Distribution of Number of Threads",
       x = "Number of Threads",
       y = "Density") +
  theme_minimal()

# vẽ đồ thị phân phối xác suất tích lũy cho biến Bus_speed_value
ggplot(CPUs_data, aes(x = Bus_Speed_Value)) +
  stat_ecdf(geom = "step", color = "blue") +
  labs(title = "Cumulative Distribution Plot of Bus Speed Value",
       x = "Bus Speed Value",
       y = "Cumulative Probability") +
  theme_minimal()
# vẽ đồ thị thể hiện sự phân phối của Bus_speed_value theo phân loại của biến Bus_interface_type
ggplot(CPUs_data, aes(x = Bus_Interface_type, y = Bus_Speed_Value)) +
  geom_boxplot(fill = "lightblue", outlier.colour = "red", outlier.shape = 16, outlier.size = 2, alpha = 0.7) +
  labs(title = "Boxplot of Bus Speed Value by Bus Interface Type",
       x = "Bus Interface Type",
       y = "Bus Speed Value (GT/s)") +
  theme_minimal()
# vẽ biểu đồ phân tán thể hiện phân phối của biến Bus speed value theo biến Processor base frequency
ggplot(CPUs_data, aes(x = Processor_Base_Frequency, y = Bus_Speed_Value)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "Scatter Plot of Bus Speed Value vs Processor Base Frequency",
       x = "Processor Base Frequency (MHz)",
       y = "Bus Speed Value (GT/s)") +
  theme_minimal()
# vẽ biểu đồ phân tán thể hiện phân phối của biến Bus speed value theo biến nb_of_cores
ggplot(CPUs_data, aes(x = nb_of_Cores, y = Bus_Speed_Value)) +
  geom_point(color = "green", alpha = 0.6) +
  labs(title = "Scatter Plot of Bus Speed Value vs Number of Cores",
       x = "Number of Cores",
       y = "Bus Speed Value (GT/s)") +
  theme_minimal()
# vẽ biểu đồ phân tán thể hiện phân phối của biến Bus speed value theo biến nb_of_threads
ggplot(CPUs_data, aes(x = nb_of_Threads, y = Bus_Speed_Value)) +
  geom_point(color = "purple", alpha = 0.6) +
  labs(title = "Scatter Plot of Bus Speed Value vs Number of Threads",
       x = "Number of Threads",
       y = "Bus Speed Value (GT/s)") +
  theme_minimal()
# vẽ biểu đồ phân tán thể hiện phân phối của biến Bus speed value theo biến Cache_value
ggplot(CPUs_data, aes(x = Cache_Size, y = Bus_Speed_Value)) +
  geom_point(color = "orange", alpha = 0.6) +
  labs(title = "Scatter Plot of Bus Speed Value vs Cache Size",
       x = "Cache Size (log scale)",
       y = "Bus Speed Value (GT/s)") +
  theme_minimal()


