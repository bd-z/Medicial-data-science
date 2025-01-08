library(tidyverse)
library(ggplot2)
library(patchwork)

data_file <- "data_cardio.csv"
data_raw <- read.csv2(data_file)

# 1. Transform the variables of the data set to appropriate data types and assign
# factor labels for the categorical variables.

data <- data_raw %>%
  mutate(
    age = round(age/365, 2),
    weight = as.numeric(weight),
   # BMI = weight/((height/100)^2)
  )

columns_to_factor <- c("gender",
                       "cholesterol",
                       "gluc")

labels_list <- list(
  gender = c("Male", "Female"),
  cholesterol = c("normal", "above normal", "well above normal"),
  gluc =  c("normal", "above normal", "well above normal"))

df <- data %>%
  mutate(across(all_of(names(labels_list)), 
                ~ factor(., levels = seq_along(
                  labels_list[[cur_column()]]
                  ),
                  labels = labels_list[[cur_column()]]
                  )
                )
         )

columns_to_factor_bin <- c("smoke",
                       "alco",
                       "active",
                       "cardio" 
                       )

labels_list_bin <- list(
  smoke = c("no", "yes"),
  alco = c("no", "yes"),
  active  = c("no", "yes"),
  cardio = c("absent", "present")
)

df_bin <- df %>%
  mutate(across(all_of(names(labels_list_bin)), 
                ~ factor(., levels = seq_along(
                  labels_list_bin[[cur_column()]] 
                ) -1 ,
                labels = labels_list_bin[[cur_column()]]
                )
  )
  )

data <- df_bin

# 2. Check the continuous variables for outliers and remove implausible values.
continuous_variables <- c("age", "height", "weight", "ap_hi", "ap_lo") 
summary(data[continuous_variables])

# Visualize each variable
# Define the list of continuous variables
continuous_variables <- c("age", "height", "weight", "ap_hi", "ap_lo")

# Define the generate_box_hist_plots function
generate_box_hist_plots <- function(data, continuous_variables) {
  # Create empty lists to store plots
  boxplots <- list()
  histograms <- list()
  
  # Loop through each variable and create plots
  for (var in continuous_variables) {
    # Generate a boxplot
    boxplot <- ggplot(data, aes_string(y = var)) +
      geom_boxplot(fill = "lightblue", color = "darkblue", outlier.color = "red", outlier.shape = 16) +
      labs(title = paste("Boxplot of", var), y = var) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Generate a histogram
    histogram <- ggplot(data, aes_string(x = var)) +
      geom_histogram(binwidth = 10, fill = "lightgreen", color = "black") +
      labs(title = paste(var, "Distribution"), x = var, y = "Count") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Append the plots to the respective lists
    boxplots[[var]] <- boxplot
    histograms[[var]] <- histogram
  }
  
  # Combine all boxplots and histograms
  combined_boxplots <- wrap_plots(boxplots, ncol = 2)
  combined_histograms <- wrap_plots(histograms, ncol = 2)
  
  # Combine boxplots and histograms into a single layout
  combined_plot <- combined_boxplots / combined_histograms
  
  # Return the combined plot
  return(combined_plot)
}

combined_plot <- generate_box_hist_plots(data, continuous_variables)
# Display the generated plot
print(combined_plot)


# Function to remove outliers using the IQR method
remove_outliers <- function(x, factor = 1.5) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - factor * iqr
  upper_bound <- q3 + factor * iqr
  x[x < lower_bound | x > upper_bound] <- NA
  return(x)
}

# Set thresholds for plausible values (based on domain knowledge)
plausible_thresholds <- list(
  age = c(0, 120),
  height = c(50, 250),
  weight = c(40, 300),
  ap_hi = c(60, 250),
  ap_lo = c(40, 150)
)

# Loop through each variable to clean outliers and implausible values

for (var in continuous_variables) {
  # Replace outliers with NA
  data[[var]] <- remove_outliers(data[[var]], factor = 3)
  # Apply plausible thresholds
  min_val <- plausible_thresholds[[var]][1]
  max_val <- plausible_thresholds[[var]][2]
  data <- data[data[[var]] >= min_val & data[[var]] <= max_val, ]
}

# remove the NA rows
data <- na.omit(data)

combined_plot_clean <- generate_box_hist_plots(data, continuous_variables)
# Display the generated plot
print(combined_plot_clean)

# 3. Create a new variable BMI and provide a summary table for the variable BMI
# for both cardio groups.

summary_table <- data %>%
  group_by(cardio) %>%
  summarize(
    Count = n(),
    Mean_BMI = mean(BMI),
    Median_BMI = median(BMI),
    SD_BMI = sd(BMI),
    Min_BMI = min(BMI),
    Max_BMI = max(BMI)
  )
# 4. How does the systolic blood pressure and the BMI correlate to each other?
# Is there any difference between the two classes of cardiovascular disease?

# Function 1: Calculate Pearson correlation


calculate_correlation <- function(data, col_BMI, col_bp_type) {
  correlation <- cor(data[[col_BMI]], data[[col_bp_type]], method = "pearson", use = "complete.obs")
  return(correlation)
}


# Function 2: Create and display scatter plot
create_scatter_plot <- function(data, col_BMI, col_bp_type) {
  bp_type_name <- ifelse(col_bp_type == "ap_hi", "Systolic", 
                         ifelse(col_bp_type == "ap_lo", "Diastolic", NA))
  
  if (is.na(bp_type_name)) {
    stop("Invalid bp_type column name. Please use 'ap_hi' for Systolic or 'ap_lo' for Diastolic.")
  }
 
  scatter_plot <- ggplot(data, aes(x = .data[[col_BMI]], y = .data[[col_bp_type]])) +
    geom_point(alpha = 0.5, color = "blue") +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(
      title = paste("Correlation between BMI and", bp_type_name, "Blood Pressure"),
      x = "BMI",
      y = paste(bp_type_name, "Blood Pressure (mmHg)")
    ) +
    theme_minimal()
  
  print(scatter_plot)
}


# Function 3: Group by `cardio` and calculate correlation for each group
calculate_correlation_by_group <- function(data, bp_type, group = "cardio") {
  library(dplyr)
  correlation_by_cardio <- data %>%
    group_by(.data[[group]]) %>%
    summarize(
      Correlation = cor(BMI, .data[[bp_type]], method = "pearson", use = "complete.obs")
    )
  return(correlation_by_cardio)
}

# 
correlation <- calculate_correlation(data, "BMI", "ap_hi")
create_scatter_plot(data, "BMI", "ap_hi")
correlation_by_cardio_ap_hi <- calculate_correlation_by_group(data, bp_type = "ap_hi", group = "cardio")
print(correlation_by_cardio_ap_hi)



# Is there any difference between the two classes of cardiovascular disease?
compare_BMI_BLT_correlations_of_diease_nondiease <- function(correlation_by_cardio_BPT, data) {
  # Step 1: Extract correlations for the two groups
  r1 <- correlation_by_cardio_BPT$Correlation[correlation_by_cardio_BPT$cardio == 0]
  r2 <- correlation_by_cardio_BPT$Correlation[correlation_by_cardio_BPT$cardio == 1]
  
  if (length(r1) == 0 | length(r2) == 0) {
    stop("Both groups (cardio == 0 and cardio == 1) must have correlations.")
  }
  
  # Step 2: Calculate Fisher's Z transformation
  z1 <- atanh(r1)  # Fisher's Z for group 1
  z2 <- atanh(r2)  # Fisher's Z for group 2
  
  # Step 3: Calculate standard error and Z-score
  n1 <- sum(data$cardio == 0, na.rm = TRUE)  # Sample size for group 1
  n2 <- sum(data$cardio == 1, na.rm = TRUE)  # Sample size for group 2
  
  if (n1 <= 3 | n2 <= 3) {
    stop("Sample sizes in both groups must be greater than 3.")
  }
  
  se <- sqrt(1 / (n1 - 3) + 1 / (n2 - 3))
  z_score <- (z1 - z2) / se
  
  # Step 4: Calculate p-value
  p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  # Return results as a list
  return(list(Z_score = z_score, P_value = p_value))
}

results_ap_hi <- compare_BMI_BLT_correlations_of_diease_nondiease(correlation_by_cardio_ap_hi, data)
cat("Z-score:", results_ap_hi$Z_score, "\n")
cat("P-value:", results_ap_hi$P_value, "\n")



# 5.Answer the same question for the diastolic blood pressure.
correlation <- calculate_correlation(data, "BMI", "ap_lo")
create_scatter_plot(data, "BMI", "ap_lo")
correlation_by_cardio_ap_lo <- calculate_correlation_by_group(data, bp_type = "ap_lo", group = "cardio")
print(correlation_by_cardio_ap_lo)
results_ap_lo <- compare_BMI_BLT_correlations_of_diease_nondiease(correlation_by_cardio_ap_lo, data)
cat("Z-score:", results_ap_lo$Z_score, "\n")
cat("P-value:", results_ap_lo$P_value, "\n")

# 6. Categorize the variable BMI according to its quartiles and visualize the distribution of the BMI
# categories in both cardio types.
# Categorize BMI into quartiles
data <- data %>%
  mutate(BMI_category = cut(BMI,
                            breaks = quantile(BMI, probs = seq(0, 1, 0.25), na.rm = TRUE),
                            include.lowest = TRUE,
                            labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)")))

# Group data and count occurrences
bmi_cardio_distribution <- data %>%
  group_by(BMI_category, cardio) %>%
  summarise(count = n(), .groups = "drop")

# Plot the distribution
ggplot(bmi_cardio_distribution, aes(x = BMI_category, y = count, fill = as.factor(cardio))) +
  geom_bar(stat = "identity", position = "stack") + #position_dodge()) + 
  labs(title = "Distribution of BMI Categories in Cardio Types",
       x = "BMI Category",
       y = "Count",
       fill = "Cardio") +
  scale_fill_manual(values = c("0" = "orange", "1" = "red"),
                    labels = c("No (0)", "Yes (1)")) +
  theme_minimal()


bmi_cardio_heatmap <- data %>%
  group_by(BMI_category, cardio) %>%
  summarise(count = n(), .groups = "drop")


ggplot(bmi_cardio_heatmap, aes(x = BMI_category, y = as.factor(cardio), fill = count)) +
  geom_tile() +
  labs(title = "BMI Category vs Cardio (Heatmap)",
       x = "BMI Category",
       y = "Cardio",
       fill = "Count") +
  scale_fill_gradient(low = "#FFE4B5", high = "#FF4500") +  
  theme_minimal()

# 7. How is age distributed in the different categories of cardio? Display age in years.

# Plot the distribution of age in different cardio categories
ggplot(data, aes(x = as.factor(cardio), y = age, fill = as.factor(cardio))) +
  geom_boxplot() +
  labs(title = "Age Distribution by Cardio Categories",
       x = "Cardio",
       y = "Age (years)",
       fill = "Cardio") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),
                    labels = c("No (0)", "Yes (1)")) +
  theme_minimal()


ggplot(data, aes(x = age, fill = as.factor(cardio))) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7) +
  labs(title = "Age Distribution by Cardio Categories",
       x = "Age (years)",
       y = "Count",
       fill = "Cardio") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),
                    labels = c("No (0)", "Yes (1)")) +
  theme_minimal()

ggplot(data, aes(x = as.factor(cardio), y = age, fill = as.factor(cardio))) +
  geom_violin(trim = FALSE) +
  labs(title = "Age Distribution by Cardio Categories",
       x = "Cardio",
       y = "Age (years)",
       fill = "Cardio") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),
                    labels = c("No (0)", "Yes (1)")) +
  theme_minimal()


# Create a plot that shows the distribution of age for both types of gender and both types of cardio.
ggplot(data, aes(x = as.factor(gender), y = age, fill = as.factor(cardio))) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  facet_wrap(~ as.factor(cardio), labeller = labeller(.default = c("0" = "No Cardio", "1" = "Yes Cardio"))) +
  labs(title = "Age Distribution by Gender and Cardio",
       x = "Gender (1 = Female, 2 = Male)",
       y = "Age (Years)",
       fill = "Cardio") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),
                    labels = c("No Cardio", "Yes Cardio")) +
  theme_minimal()

ggplot(data, aes(x = as.factor(gender), y = age, fill = as.factor(cardio))) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ as.factor(cardio), labeller = labeller(.default = c("0" = "No Cardio", "1" = "Yes Cardio"))) +
  labs(title = "Age Distribution by Gender and Cardio",
       x = "Gender (1 = Female, 2 = Male)",
       y = "Age (Years)",
       fill = "Cardio") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),
                    labels = c("No Cardio", "Yes Cardio")) +
  theme_minimal()

# Create the plot
# ggplot(data, aes(x = interaction(gender, cardio), y = age, fill = as.factor(gender))) +
#   geom_violin(trim = FALSE, alpha = 0.6) +       # Violin plot with some transparency
#   geom_boxplot(width = 0.2, position = position_dodge(width = 0.9)) +  # Boxplot overlay
#   labs(title = "Age Distribution by Gender and Cardio",
#        x = "Gender and Cardio",
#        y = "Age (years)",
#        fill = "Gender") +
#   scale_x_discrete(labels = c("1_0" = "Male No Cardio",
#                               "1_1" = "Male Cardio",
#                               "2_0" = "Female No Cardio",
#                               "2_1" = "Female Cardio")) +
#   scale_fill_manual(values = c("1" = "blue", "2" = "pink")) +
#   theme_minimal()



#9 Extend this plot by taking the different types of glucose into account.

# Create the plot with glucose levels as facets
ggplot(data, aes(x = interaction(gender, cardio), y = age, fill = as.factor(gender))) +
  geom_violin(trim = FALSE, alpha = 0.6) +       # Violin plot with some transparency
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.9)) +  # Boxplot overlay
  facet_wrap(~ gluc, labeller = labeller(gluc = c("1" = "Normal", "2" = "Above Normal", "3" = "Well Above Normal"))) +
  labs(title = "Age Distribution by Gender, Cardio, and Glucose Levels",
       x = "Gender and Cardio",
       y = "Age (years)",
       fill = "Gender") +
  scale_x_discrete(labels = c("1_0" = "Male No Cardio",
                              "1_1" = "Male Cardio",
                              "2_0" = "Female No Cardio",
                              "2_1" = "Female Cardio")) +
  scale_fill_manual(values = c("1" = "orange", "2" = "red")) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12),  # Adjust facet label font size
        strip.background = element_rect(fill = "lightgray", color = "black"))  # Adjust facet label background


# Create a lookup table for labels
labels <- c("1_0" = "Male No Cardio",
            "1_1" = "Male Cardio",
            "2_0" = "Female No Cardio",
            "2_1" = "Female Cardio")

# Add a column for group labels
data$group_label <- labels[interaction(data$gender, data$cardio, sep = "_")]

# Create the plot with labels
ggplot(data, aes(x = interaction(gender, cardio), y = age, fill = as.factor(gender))) +
  geom_violin(trim = FALSE, alpha = 0.6) +       # Violin plot with some transparency
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.9)) +  # Boxplot overlay
  facet_wrap(~ gluc, labeller = labeller(gluc = c("1" = "Normal", "2" = "Above Normal", "3" = "Well Above Normal"))) +
  geom_text(data = unique(data.frame(x = interaction(data$gender, data$cardio), 
                                     y = max(data$age) + 5, 
                                     label = labels)),
            aes(x = x, y = y, label = label), inherit.aes = FALSE, size = 4, vjust = -0.5) +
  labs(title = "Age Distribution by Gender, Cardio, and Glucose Levels",
       x = "Gender and Cardio",
       y = "Age (years)",
       fill = "Gender") +
  scale_x_discrete(labels = labels) +
  scale_fill_manual(values = c("1" = "blue", "2" = "pink")) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12),  # Adjust facet label font size
        strip.background = element_rect(fill = "lightgray", color = "black"))  # Adjust facet label background

# Create a new variable for legend labels
data$group_label <- factor(interaction(data$gender, data$cardio),
                           levels = c("1.0", "1.1", "2.0", "2.1"),
                           labels = c("Male No Cardio", "Male Cardio", "Female No Cardio", "Female Cardio"))

# Create the plot with glucose levels as facets and legend for group labels
ggplot(data, aes(x = group_label, y = age, fill = group_label)) +
  geom_violin(trim = FALSE, alpha = 0.6) +       # Violin plot with some transparency
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.9)) +  # Boxplot overlay
  facet_wrap(~ gluc, labeller = labeller(gluc = c("1" = "Normal", "2" = "Above Normal", "3" = "Well Above Normal"))) +
  labs(title = "Age Distribution by Gender, Cardio, and Glucose Levels",
       x = "Group",
       y = "Age (years)",
       fill = "Group") +
  scale_fill_manual(values = c("Male No Cardio" = "blue", 
                               "Male Cardio" = "darkblue", 
                               "Female No Cardio" = "pink", 
                               "Female Cardio" = "red")) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12),  # Adjust facet label font size
        strip.background = element_rect(fill = "lightgray", color = "black"))  # Adjust facet label background
