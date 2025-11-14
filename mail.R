##############################################
# LOST MAIL DATA ANALYSIS - NITJ (R SCRIPT)
##############################################

# INSTALL REQUIRED PACKAGES (run only once)
# install.packages(c("readxl", "dplyr", "ggplot2", "lubridate"))

# LOAD LIBRARIES
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

##############################################
# LOAD THE PREPROCESSED DATASET
##############################################

lost_data <- read_excel("/Users/sohitkumar/Downloads/NITJ_lost_mails_processed.xlsx")

##############################################
# ANALYSIS & CHART 1: CATEGORY-WISE LOST ITEMS
##############################################

ggplot(lost_data, aes(x = Category)) +
  geom_bar(fill = "#0072B2") +
  labs(title = "Most Frequently Lost Items (Category-wise)",
       x = "Item Category", y = "Lost Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


##############################################
# ANALYSIS & CHART 2: MONTHLY LOST ITEM TREND
##############################################

lost_data %>%
  count(Month) %>%
  ggplot(aes(x = Month, y = n, group = 1)) +
  geom_line(size = 1.3, color = "#D55E00") +
  geom_point(size = 3, color = "#D55E00") +
  labs(title = "Lost Items Trend (Month-wise)",
       x = "Month", y = "Lost Count") +
  theme_minimal()


##############################################
# ANALYSIS & CHART 3: TOP LOCATIONS WHERE ITEMS ARE LOST
##############################################

lost_data %>%
  group_by(Location) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(Location, count), y = count)) +
  geom_col(fill = "#009E73") +
  labs(title = "Top 10 Locations Where Students Lost Items",
       x = "Location", y = "Lost Count") +
  coord_flip() +
  theme_minimal()


##############################################
# ANALYSIS & CHART 4: LOST ITEMS BY STUDENT YEAR GROUP
##############################################

lost_data %>%
  mutate(YearGroup = case_when(
    grepl("First", Owner, ignore.case = TRUE) ~ "1st Year",
    grepl("Second", Owner, ignore.case = TRUE) ~ "2nd Year",
    grepl("Third|Final", Owner, ignore.case = TRUE) ~ "3rd/4th Year",
    TRUE ~ "Others"
  )) %>%
  ggplot(aes(x = YearGroup)) +
  geom_bar(fill = "#CC79A7") +
  labs(title = "Lost Items by Student Academic Year",
       x = "Student Year", y = "Count") +
  theme_minimal()


##############################################
# ANALYSIS & CHART 5: HEATMAP (CATEGORY vs MONTH)
##############################################

lost_data %>%
  count(Month, Category) %>%
  ggplot(aes(x = Month, y = Category, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightyellow", high = "red") +
  labs(title = "Heatmap: Category vs Month",
       x = "Month", y = "Category", fill = "Count") +
  theme_minimal()

##############################################
# END OF FILE
##############################################
