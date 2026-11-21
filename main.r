install.packages(c("stringr","tidyr","dplyr","zoo","Metrics","caret","MASS",
                   "ggplot2","reshape2","mltools","DescTools","plotly","car"))
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
library(car)
Intel_CPUs = read.csv("dataset/Intel_CPUs.csv", na.strings = c("", "N/A")) # nolint # c is base in R
CPUs_data = Intel_CPUs[,c("Lithography", # nolint 
                      "nb_of_Cores","nb_of_Threads","Processor_Base_Frequency", # nolint
                      "Cache","Instruction_Set","TDP","Max_Memory_Size","Max_nb_of_Memory_Channels","Max_Memory_Bandwidth", "Bus_Speed", "Recommended_Customer_Price", "Product_Collection")] # nolint
# Print out table of summary data
print(summary(CPUs_data)) # nolint
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

product_collect <- c('Legacy', 'Celeron', 'Pentium', 'Quark', 'Atom', 'Itanium', 'Xeon','Core')
for (i in product_collect) {
    # nhóm dữ liệu thành các loại dòng chip hiện tại
  CPUs_data$Product_Collection <- ifelse(grepl(i, CPUs_data$Product_Collection), i, CPUs_data$Product_Collection)
}

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

recommend_price <- function(price_range) {
  if(grepl('-', price_range)) {
    range <- strsplit(price_range, "-")[[1]]
    return((as.double(range[1]) + as.double(range[2])) / 2)
  }
  return (price_range)
}
# sửa định dạng chuỗi
CPUs_data$Recommended_Customer_Price <- gsub("\\$", "", CPUs_data$Recommended_Customer_Price) 
CPUs_data$Recommended_Customer_Price <- gsub(",", "", CPUs_data$Recommended_Customer_Price)
# apply hàm để xử lý số liệu
CPUs_data$Recommended_Customer_Price <- sapply(CPUs_data$Recommended_Customer_Price, recommend_price) 
CPUs_data$Recommended_Customer_Price <- as.double(CPUs_data$Recommended_Customer_Price) 

table(CPUs_data$Recommended_Customer_Price)

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
                    "Max_Memory_Bandwidth", "Bus_Speed_Value","Recommended_Customer_Price")
categorical_cols <- c("Cache_Type", "Instruction_Set",
                      "Bus_Interface_type","Product_Collection")

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
summary_categorical_table
#------------------------------------------------------------------------------------ #nolint
#Vẽ đồ thị histogram cho Recommend_Customer_Price
ggplot(CPUs_data, aes(x = Recommended_Customer_Price)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black") +
  scale_x_continuous(breaks = seq(0, max(CPUs_data$Recommended_Customer_Price, na.rm = TRUE), by = 500)) +
  labs(title = "Histogram of Recommended Customer Price",
       x = "Recommended Customer Price", 
       y = "Frequency") +
  theme_minimal()
#Tạo tập biến định lượng
numeric_vars <- c("Lithography", "nb_of_Cores", "nb_of_Threads", 
                  "Processor_Base_Frequency", "Cache_Size", "TDP", 
                  "Max_Memory_Size", "Max_nb_of_Memory_Channels", 
                  "Max_Memory_Bandwidth", "Bus_Speed_Value")

# Vòng lặp vẽ scatterplot cho từng biến định lượng
for (var in numeric_vars) {
  print(
    ggplot(CPUs_data, aes_string(x = "Recommended_Customer_Price", y = var)) +
      geom_point(color = "blue", alpha = 0.7) +
      labs(title = paste("Scatterplot: Recommended Customer Price vs", var),
           x = "Recommended Customer Price (USD)", 
           y = var) +
      theme_minimal()
  )
}
#Vẽ boxplot cho Recommended_Customer_Price theo Product_Collection
ggplot(CPUs_data, aes(x = Recommended_Customer_Price, y = Product_Collection)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot of Recommended Customer Price by Product Collection",
       x = "Recommended Customer Price",
       y = "Product Collection") +
  theme_minimal()    

# vẽ biểu đồ phân tán thể hiện phân phối của giá thành đề xuất theo Cache Size và Cache Value
ggplot(CPUs_data, aes(x = Recommended_Customer_Price, y = Cache_Size, color = Cache_Type)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "Scatterplot of Cache Size and Recommended Customer Price",
       x = "Recommended Customer Price",
       y = "Cache Size (MB)",
       color = "Cache Type") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))


#--------------------- BÀI TOÁN 2 MẪU: SO SÁNH 2 TRUNG BÌNH ---------------------
# H0: mu_Xeon = mu_Core   ;   H1: mu_Xeon > mu_Core
xeon <- CPUs_data$Recommended_Customer_Price[CPUs_data$Product_Collection == "Xeon"]
core <- CPUs_data$Recommended_Customer_Price[CPUs_data$Product_Collection == "Core"]

n1 <- length(xeon); x1 <- mean(xeon); s1 <- sd(xeon)
n2 <- length(core); x2 <- mean(core); s2 <- sd(core)
data.frame(Nhom = c("Xeon", "Core"), n = c(n1, n2), Mean = c(x1, x2), SD = c(s1, s2))

# Kiểm tra điều kiện
shapiro.test(xeon)
shapiro.test(core)
par(mfrow = c(1, 2))
qqnorm(xeon, main = "QQ-plot giá - Xeon"); qqline(xeon, col = "red")
qqnorm(core, main = "QQ-plot giá - Core"); qqline(core, col = "red")
par(mfrow = c(1, 1))

# Dạng 4d: hai tổng thể không chuẩn, n1, n2 >= 30
z_qs <- (x1 - x2) / sqrt(s1^2 / n1 + s2^2 / n2)
z_qs
qnorm(0.95)
1 - pnorm(z_qs)


#--------------------- ANOVA 1 YẾU TỐ ---------------------
# Xét các CPU tiến trình 14 nm, tần số cơ sở có khác nhau theo loại giao tiếp bus không?
# H0: mu_DMI2 = mu_DMI3 = mu_OPI   ;   H1: tồn tại mu_i != mu_j
data_1way <- CPUs_data[CPUs_data$Lithography == 14 &
                         CPUs_data$Bus_Interface_type %in% c("DMI2", "DMI3", "OPI"), ]
data_1way$Bus_Interface_type <- factor(data_1way$Bus_Interface_type)

aggregate(Processor_Base_Frequency ~ Bus_Interface_type, data = data_1way,
          FUN = function(v) c(n = length(v), mean = mean(v), sd = sd(v)))

# Kiểm tra điều kiện: phân phối chuẩn
tapply(data_1way$Processor_Base_Frequency, data_1way$Bus_Interface_type,
       function(v) shapiro.test(v)$p.value)
ggplot(data_1way, aes(sample = Processor_Base_Frequency)) +
  stat_qq() + stat_qq_line(color = "red") +
  facet_wrap(~ Bus_Interface_type) +
  labs(title = "QQ-plot tần số cơ sở theo loại giao tiếp bus") +
  theme_minimal()

# Kiểm tra điều kiện: phương sai bằng nhau
leveneTest(Processor_Base_Frequency ~ Bus_Interface_type, data = data_1way)

# Bảng ANOVA
anova_1 <- aov(Processor_Base_Frequency ~ Bus_Interface_type, data = data_1way)
summary(anova_1)
qf(0.95, 2, 154)

# Hệ số R^2
SSB <- summary(anova_1)[[1]]$`Sum Sq`[1]
SSW <- summary(anova_1)[[1]]$`Sum Sq`[2]
SSB / (SSB + SSW)

# So sánh bội
PostHocTest(anova_1, method = "lsd")
TukeyHSD(anova_1)


#--------------------- ANOVA 2 YẾU TỐ CÓ LẶP ---------------------
# Yếu tố A: Product_Collection (Celeron, Core, Pentium) ; Yếu tố B: Bus_Interface_type (DMI, DMI3)
# Biến phụ thuộc: Processor_Base_Frequency
# H0A: không có ảnh hưởng của A ; H0B: không có ảnh hưởng của B
# H0AB: không có tương tác giữa A và B
rm(mean, min, max, median, unique)
data_2way <- CPUs_data[CPUs_data$Product_Collection %in% c("Celeron", "Core", "Pentium") &
                         CPUs_data$Bus_Interface_type %in% c("DMI", "DMI3"), ]
data_2way$Product_Collection <- factor(data_2way$Product_Collection)
data_2way$Bus_Interface_type <- factor(data_2way$Bus_Interface_type)

# Số quan sát mỗi ô
table(data_2way$Product_Collection, data_2way$Bus_Interface_type)

aggregate(Processor_Base_Frequency ~ Product_Collection + Bus_Interface_type, data = data_2way,
          FUN = function(v) c(n = length(v), mean = mean(v), sd = sd(v)))

# Kiểm tra điều kiện: phân phối chuẩn
tapply(data_2way$Processor_Base_Frequency,
       list(data_2way$Product_Collection, data_2way$Bus_Interface_type),
       function(v) shapiro.test(v)$p.value)

# Kiểm tra điều kiện: phương sai
leveneTest(Processor_Base_Frequency ~ Product_Collection * Bus_Interface_type, data = data_2way)

anova_2 <- aov(Processor_Base_Frequency ~ Product_Collection * Bus_Interface_type, data = data_2way)

# Kiểm tra phần dư
shapiro.test(residuals(anova_2))
qqnorm(residuals(anova_2)); qqline(residuals(anova_2), col = "red")

interaction.plot(x.factor = data_2way$Product_Collection,
                 trace.factor = data_2way$Bus_Interface_type,
                 response = data_2way$Processor_Base_Frequency,
                 fun = mean, type = "b", pch = c(16, 17), col = c("blue", "red"),
                 xlab = "Dòng sản phẩm", ylab = "Tần số cơ sở trung bình (MHz)",
                 trace.label = "Giao tiếp bus", main = "Biểu đồ tương tác")

# Bảng ANOVA (số quan sát các ô không bằng nhau nên dùng Anova type 2)
Anova(anova_2, type = 2)
qf(0.95, 2, 82)
qf(0.95, 1, 82)

# So sánh bội cho yếu tố A
TukeyHSD(anova_2, which = "Product_Collection")





#--------------------- HỒI QUY TUYẾN TÍNH ---------------
