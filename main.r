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
library(scales)
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
# --- FIX 1 (visualization_improvements_guide.md): Log-scaled Histogram with Density ---
print(
  ggplot(CPUs_data, aes(x = Recommended_Customer_Price)) +
    geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "#3182bd", color = "white", alpha = 0.8) +
    geom_density(color = "#e6550d", linewidth = 1) +
    scale_x_log10(labels = scales::dollar_format()) +
    labs(title = "Log-Transformed Distribution of Recommended Customer Price",
         x = "Recommended Customer Price (USD, Log Scale)",
         y = "Density") +
    theme_minimal()
)

# --- FIX 2 (visualization_improvements_guide.md): Jittered & Log-Scaled Feature Plots ---
numeric_vars <- c("Lithography", "nb_of_Cores", "nb_of_Threads", 
                  "Processor_Base_Frequency", "Cache_Size", "TDP", 
                  "Max_Memory_Size", "Max_nb_of_Memory_Channels", 
                  "Max_Memory_Bandwidth", "Bus_Speed_Value")

# Discrete features benefit from vertical jitter and factor levels
discrete_features <- c("Lithography", "nb_of_Cores", "nb_of_Threads", 
                       "Max_nb_of_Memory_Channels", "Bus_Speed_Value")

for (var in numeric_vars) {
  if (var %in% discrete_features) {
    p <- ggplot(CPUs_data, aes(x = Recommended_Customer_Price, y = factor(.data[[var]]))) +
      geom_jitter(color = "#2b8cbe", alpha = 0.45, width = 0, height = 0.22, size = 1.6) +
      scale_x_log10(labels = scales::dollar_format()) +
      labs(title = paste("Customer Price (Log Scale) vs.", var),
           x = "Recommended Customer Price (USD, Log Scale)", 
           y = var) +
      theme_minimal()
  } else {
    p <- ggplot(CPUs_data, aes(x = Recommended_Customer_Price, y = .data[[var]])) +
      geom_point(color = "#2b8cbe", alpha = 0.5, size = 1.6) +
      scale_x_log10(labels = scales::dollar_format()) +
      labs(title = paste("Customer Price (Log Scale) vs.", var),
           x = "Recommended Customer Price (USD, Log Scale)", 
           y = var) +
      theme_minimal()
  }
  print(p)
}

# --- FIX 3 (visualization_improvements_guide.md): Boxplot + Jitter Overlay on Log Scale ---
print(
  ggplot(CPUs_data, aes(x = Product_Collection, y = Recommended_Customer_Price, fill = Product_Collection)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.6) +
    geom_jitter(width = 0.2, alpha = 0.35, size = 1.5) +
    scale_y_log10(labels = scales::dollar_format()) +
    labs(title = "Price Distribution across Product Collections (Log Scale)",
         x = "Product Collection",
         y = "Recommended Customer Price (USD, Log Scale)") +
    theme_minimal() +
    theme(legend.position = "none")
)

# --- FIX 4 (visualization_improvements_guide.md): Faceted Multi-Category Plot ---
print(
  ggplot(CPUs_data, aes(x = Cache_Size, y = Recommended_Customer_Price, color = Cache_Type)) +
    geom_point(alpha = 0.6, size = 1.8) +
    scale_y_log10(labels = scales::dollar_format()) +
    facet_wrap(~ Cache_Type) +
    labs(title = "Price vs. Cache Size Faceted by Cache Type (Log Scale)",
         x = "Cache Size (MB)",
         y = "Recommended Customer Price (USD, Log Scale)",
         color = "Cache Type") +
    theme_bw() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 13, face = "bold"))
)

shapiro_legacy <- shapiro.test(CPUs_data$Recommended_Customer_Price[CPUs_data$Product_Collection == "Legacy"])
shapiro_celeron <- shapiro.test(CPUs_data$Recommended_Customer_Price[CPUs_data$Product_Collection == "Celeron"])
shapiro_pentium <- shapiro.test(CPUs_data$Recommended_Customer_Price[CPUs_data$Product_Collection == "Pentium"])
shapiro_xeon <- shapiro.test(CPUs_data$Recommended_Customer_Price[CPUs_data$Product_Collection == "Xeon"])
shapiro_core <- shapiro.test(CPUs_data$Recommended_Customer_Price[CPUs_data$Product_Collection == "Core"])

# In kết quả cho từng nhóm
shapiro_legacy
shapiro_celeron
shapiro_pentium
shapiro_xeon
shapiro_core

library(car)
# Chỉ giữ lại các nhóm Legacy, Celeron, Pentium, Xeon, Core
filtered_data <- CPUs_data[CPUs_data$Product_Collection %in% c("Legacy", "Celeron", "Pentium", "Xeon", "Core"), ]
#QQ-Plot cho nhóm Legacy
ggplot(filtered_data[filtered_data$Product_Collection == "Legacy", ], aes(sample = Recommended_Customer_Price)) +
  stat_qq() + stat_qq_line(color = "blue") +
  labs(title = "QQ Plot for Legacy Group", x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_minimal()

#QQ-Plot cho nhóm Celeron
ggplot(filtered_data[filtered_data$Product_Collection == "Celeron", ], aes(sample = Recommended_Customer_Price)) +
  stat_qq() + stat_qq_line(color = "blue") +
  labs(title = "QQ Plot for Celeron Group", x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_minimal()

#QQ-Plot cho nhóm Pentium
ggplot(filtered_data[filtered_data$Product_Collection == "Pentium", ], aes(sample = Recommended_Customer_Price)) +
  stat_qq() + stat_qq_line(color = "blue") +
  labs(title = "QQ Plot for Pentium Group", x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_minimal()

#QQ-Plot cho nhóm Xeon
ggplot(filtered_data[filtered_data$Product_Collection == "Xeon", ], aes(sample = Recommended_Customer_Price)) +
  stat_qq() + stat_qq_line(color = "blue") +
  labs(title = "QQ Plot for Xeon Group", x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_minimal()

#QQ-Plot cho nhóm Core
ggplot(filtered_data[filtered_data$Product_Collection == "Core", ], aes(sample = Recommended_Customer_Price)) +
  stat_qq() + stat_qq_line(color = "blue") +
  labs(title = "QQ Plot for Core Group", x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_minimal()

leveneTest(Recommended_Customer_Price ~ Product_Collection, data = filtered_data)
# Thực hiện ANOVA trên dữ liệu đã lọc

anova_result <- aov(Recommended_Customer_Price ~ Product_Collection, data = filtered_data)
summary(anova_result)

# Thực hiện kiểm định Tukey HSD và xem kết quả
TukeyHSD(anova_result)

#--------------------- KIỂM ĐỊNH MỘT MẪU (ONE-SAMPLE TEST) ---------------------
print("=== 1-SAMPLE TEST: Core Price vs Target MSRP ($300) ===")
# Kiểm tra xem mức giá trung bình của dòng CPU phổ thông (Core) có bằng mức giá chuẩn $300 hay không
core_price <- CPUs_data$Recommended_Customer_Price[CPUs_data$Product_Collection == "Core"]
print(paste("Sample size of Core:", length(core_price)))
print(paste("Mean price of Core:", round(mean(core_price), 2)))
print(paste("SD of Core:", round(sd(core_price), 2)))

# 1. Kiểm định giả định phân phối chuẩn (đã tính ở shapiro_core, in lại p-value)
shapiro_core_1s <- shapiro.test(core_price)
print(shapiro_core_1s)

# 2. Kiểm định One-Sample t-test: H0: mu = 300 vs H1: mu != 300
t_one_sample <- t.test(core_price, mu = 300)
print(t_one_sample)

# 3. Kiểm định phi tham số Wilcoxon Signed-Rank test (dự phòng khi phân phối lệch)
wilcox_one_sample <- wilcox.test(core_price, mu = 300)
print(wilcox_one_sample)

#--------------------- KIỂM ĐỊNH HAI MẪU (TWO-SAMPLE TEST) ---------------------
print("=== 2-SAMPLE TEST: Core vs Xeon Price ===")
# Lọc 2 nhóm đại diện lớn nhất: Core (phổ thông/tiêu dùng) và Xeon (máy chủ/doanh nghiệp)
two_sample_data <- CPUs_data[CPUs_data$Product_Collection %in% c("Core", "Xeon"), ]
two_sample_data$Product_Collection <- droplevels(as.factor(two_sample_data$Product_Collection))

# 1. Kiểm định phương sai đồng nhất (Levene's Test)
levene_2s <- leveneTest(Recommended_Customer_Price ~ Product_Collection, data = two_sample_data)
print(levene_2s)

# 2. Thực hiện Two-Sample t-test (chọn Welch t-test do phương sai khác nhau)
equal_var <- levene_2s$`Pr(>F)`[1] > 0.05
if (equal_var) {
  t_two_sample <- t.test(Recommended_Customer_Price ~ Product_Collection, data = two_sample_data, var.equal = TRUE)
} else {
  t_two_sample <- t.test(Recommended_Customer_Price ~ Product_Collection, data = two_sample_data, var.equal = FALSE)
}
print(t_two_sample)

# 3. Kiểm định phi tham số Mann-Whitney U (Wilcoxon Rank-Sum test)
mw_two_sample <- wilcox.test(Recommended_Customer_Price ~ Product_Collection, data = two_sample_data, exact = FALSE)
print(mw_two_sample)

# 4. Tính độ lớn ảnh hưởng (Cohen's d)
n1 <- sum(two_sample_data$Product_Collection == "Core")
n2 <- sum(two_sample_data$Product_Collection == "Xeon")
s1 <- sd(two_sample_data$Recommended_Customer_Price[two_sample_data$Product_Collection == "Core"])
s2 <- sd(two_sample_data$Recommended_Customer_Price[two_sample_data$Product_Collection == "Xeon"])
sp <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
m1 <- mean(two_sample_data$Recommended_Customer_Price[two_sample_data$Product_Collection == "Core"])
m2 <- mean(two_sample_data$Recommended_Customer_Price[two_sample_data$Product_Collection == "Xeon"])
cohens_d <- (m1 - m2) / sp
print(paste("Cohen's d:", round(cohens_d, 3)))

# Vẽ boxplot so sánh Core vs Xeon
print(
  ggplot(two_sample_data, aes(x = Product_Collection, y = Recommended_Customer_Price, fill = Product_Collection)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
    labs(title = "Boxplot of Recommended Customer Price: Core vs Xeon",
         x = "Product Collection", y = "Recommended Customer Price (USD)") +
    theme_minimal() +
    theme(legend.position = "none")
)

#--------------------- HỒI QUY TUYẾN TÍNH (REGRESSION) -------------------------
print("=== REGRESSION MODELLING ===")
reg_data <- CPUs_data[!is.na(CPUs_data$Recommended_Customer_Price), ]
# Lọc 5 dòng chip chính có đủ số lượng quan sát
reg_data <- reg_data[reg_data$Product_Collection %in% c("Legacy", "Celeron", "Pentium", "Xeon", "Core"), ]

# Dùng Log_Price để khắc phục độ lệch phải nặng của giá
reg_data$Log_Price <- log(reg_data$Recommended_Customer_Price)
reg_data$Product_Collection <- droplevels(as.factor(reg_data$Product_Collection))
reg_data$Bus_Interface_type <- droplevels(as.factor(reg_data$Bus_Interface_type))

# Gộp các mức factor hiếm (< 30 quan sát) thành 'Other' để tránh lỗi khi cross-validation
collapse_rare <- function(x, min_n = 30) {
  tab <- table(x)
  x <- as.character(x)
  x[x %in% names(tab)[tab < min_n]] <- "Other"
  return(as.factor(x))
}
reg_data$Cache_Type <- collapse_rare(reg_data$Cache_Type, min_n = 30)

# Chia tập dữ liệu 80% train - 20% test
set.seed(100)
train_index <- createDataPartition(reg_data$Log_Price, p = 0.8, list = FALSE)
train_data <- reg_data[train_index, ]
test_data  <- reg_data[-train_index, ]

# --- FIX 6 (visualization_improvements_guide.md): Spearman & Log-Scale Correlation Matrix ---
# Tính tương quan Spearman (cho mối quan hệ phi tuyến đơn điệu)
train_data_numeric <- train_data[sapply(train_data, is.numeric)]
corr_matrix_spearman <- cor(train_data_numeric, method = "spearman")
melted_corr_spearman <- melt(corr_matrix_spearman)

print(
  ggplot(data = melted_corr_spearman, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", value)), size = 2.7) +
    scale_fill_gradient2(low = "#2166ac", high = "#b2182b", mid = "#f7f7f7", 
                         midpoint = 0, limit = c(-1, 1), space = "Lab", 
                         name = "Spearman\nCorrelation") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1),
          axis.text.y = element_text(size = 8)) +
    coord_fixed() +
    labs(title = "Spearman Rank Correlation Heatmap (Train Data)", x = "", y = "")
)

# 1. HỒI QUY TUYẾN TÍNH ĐƠN (SIMPLE LINEAR REGRESSION)
print("--- 1. Simple Linear Regression (Log_Price ~ Cache_Size) ---")
simple_model <- lm(Log_Price ~ Cache_Size, data = train_data)
summary(simple_model)

# Dự đoán trên train và test cho hồi quy đơn
y_train_simple <- predict(simple_model, newdata = train_data)
y_test_simple  <- predict(simple_model, newdata = test_data)

mse_train_simple <- mse(y_train_simple, train_data$Log_Price)
mse_test_simple  <- mse(y_test_simple, test_data$Log_Price)
mae_train_simple_usd <- mean(abs(exp(y_train_simple) - train_data$Recommended_Customer_Price))
mae_test_simple_usd  <- mean(abs(exp(y_test_simple) - test_data$Recommended_Customer_Price))

# Vẽ đồ thị đường hồi quy tuyến tính đơn
print(
  ggplot(train_data, aes(x = Cache_Size, y = Log_Price)) +
    geom_point(color = "darkblue", alpha = 0.5) +
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    labs(title = "Simple Linear Regression: Log(Price) vs Cache Size",
         x = "Cache Size (MB)", y = "Log(Recommended Customer Price)") +
    theme_minimal()
)

# 2. HỒI QUY TUYẾN TÍNH BỘI (MULTIPLE LINEAR REGRESSION)
print("--- 2. Multiple Linear Regression with stepAIC ---")
predictors <- c("Lithography", "nb_of_Cores", "nb_of_Threads", 
                "Processor_Base_Frequency", "Cache_Size", "TDP", 
                "Max_Memory_Size", "Max_nb_of_Memory_Channels", 
                "Max_Memory_Bandwidth", "Bus_Speed_Value", 
                "Bus_Interface_type", "Product_Collection", "Cache_Type")

full_formula <- as.formula(paste("Log_Price ~", paste(predictors, collapse = " + ")))
full_model <- lm(full_formula, data = train_data)

# Tự động chọn biến tối ưu bằng stepAIC trên tập train
multi_model <- stepAIC(full_model, direction = "both", trace = FALSE)
summary(multi_model)

# Kiểm tra số lượng hệ số bị NA (ma trận suy biến)
na_coefs <- sum(is.na(coef(multi_model)))
print(paste("So he so bi NA (ma tran suy bien):", na_coefs))

# Dự đoán mô hình hồi quy bội
y_train_multi <- predict(multi_model, newdata = train_data)
y_test_multi  <- predict(multi_model, newdata = test_data)

mse_train_multi <- mse(y_train_multi, train_data$Log_Price)
mse_test_multi  <- mse(y_test_multi, test_data$Log_Price)
mae_train_multi_usd <- mean(abs(exp(y_train_multi) - train_data$Recommended_Customer_Price))
mae_test_multi_usd  <- mean(abs(exp(y_test_multi) - test_data$Recommended_Customer_Price))

# Baseline model: luôn đoán giá trị trung bình Log_Price của tập train
baseline_pred <- mean(train_data$Log_Price)
mse_test_baseline <- mean((baseline_pred - test_data$Log_Price)^2)

# 3. KIỂM CHỨNG CHÉO (5-FOLD CROSS VALIDATION)
cv_regression <- function(data, formula, K = 5, seed = 100) {
  set.seed(seed)
  folds <- createFolds(data$Log_Price, k = K, list = TRUE, returnTrain = FALSE)
  mse_log <- numeric(K)
  mae_usd <- numeric(K)
  
  for (i in seq_len(K)) {
    te <- data[folds[[i]], ]
    tr <- data[-folds[[i]], ]
    fit <- lm(formula, data = tr)
    pr <- predict(fit, newdata = te)
    mse_log[i] <- mean((pr - te$Log_Price)^2)
    mae_usd[i] <- mean(abs(exp(pr) - te$Recommended_Customer_Price))
  }
  return(data.frame(CV_MSE_log = mean(mse_log), CV_MAE_USD = mean(mae_usd)))
}

cv_results <- cv_regression(reg_data, formula(multi_model), K = 5, seed = 100)
print(cv_results)

# 4. SO SÁNH CÁC MÔ HÌNH
comparison_table <- data.frame(
  Model = c("Baseline (Mean)", "Simple Linear (Cache_Size)", "Multiple Linear (stepAIC)"),
  MSE_Test_Log = c(round(mse_test_baseline, 4), round(mse_test_simple, 4), round(mse_test_multi, 4)),
  MAE_Test_USD = c(round(mean(abs(exp(baseline_pred) - test_data$Recommended_Customer_Price)), 2),
                   round(mae_test_simple_usd, 2),
                   round(mae_test_multi_usd, 2)),
  R_squared = c(NA, round(summary(simple_model)$r.squared, 4), round(summary(multi_model)$r.squared, 4)),
  Adj_R_squared = c(NA, round(summary(simple_model)$adj.r.squared, 4), round(summary(multi_model)$adj.r.squared, 4))
)
print("=== BANG TONG HOP SO SANH MO HINH ===")
print(comparison_table)

# 5. CHẨN ĐOÁN PHẦN DƯ HỒI QUY BỘI (DIAGNOSTICS PLOTS)
par(mfrow = c(2, 2))
plot(multi_model)
par(mfrow = c(1, 1))

# Đồ thị so sánh MAE giữa Train và Test
metric_plot_data <- data.frame(
  Model = rep(c("Simple Linear", "Multiple Linear"), each = 2),
  Type = rep(c("Train", "Test"), times = 2),
  MAE_USD = c(mae_train_simple_usd, mae_test_simple_usd, mae_train_multi_usd, mae_test_multi_usd)
)

print(
  ggplot(metric_plot_data, aes(x = Model, y = MAE_USD, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.5) +
    labs(title = "MAE (USD) Comparison: Simple vs Multiple Linear Regression",
         y = "MAE (USD)", x = "") +
    theme_minimal()
)
