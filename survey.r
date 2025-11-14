# -------------------------------
# 📦 Load Required Libraries
# -------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(wordcloud)
library(RColorBrewer)

# -------------------------------
# 📂 Import Dataset
# -------------------------------
df <- read_excel("/Users/sohitkumar/Downloads/Lost_and_Found_Survey_Responses.xlsx")

# -------------------------------
# 🧹 Data Preprocessing
# -------------------------------

# Remove Name column (not needed for analysis)
df <- df %>% select(-Name)

# Standardize text casing
df$`Year of Study` <- tolower(trimws(df$`Year of Study`))
df$`Department/Branch` <- tolower(trimws(df$`Department/Branch`))
df$`Have you ever lost something in campus?` <- tolower(df$`Have you ever lost something in campus?`)
df$`Were you able to recover your lost item(s)?` <- tolower(df$`Were you able to recover your lost item(s)?`)
df$`Would you support having a central 'Lost & Found' system in college` <- tolower(df$`Would you support having a central 'Lost & Found' system in college`)

# Replace NA in categorical columns with "Not Applicable"
df <- df %>% mutate(across(where(is.character), ~replace_na(., "Not Applicable")))

# -------------------------------
# 📈 1. Pie Chart – Lost Something or Not
# -------------------------------
loss_summary <- df %>%
  count(`Have you ever lost something in campus?`)

ggplot(loss_summary, aes(x="", y=n, fill=`Have you ever lost something in campus?`)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  labs(title="Have Students Ever Lost Something on Campus?") +
  theme_void() +
  scale_fill_brewer(palette="Set3")

# -------------------------------
# 📊 2. Bar Chart – Most Common Lost Items
# -------------------------------
lost_items <- df %>%
  filter(`If yes, what have you lost?` != "Not Applicable") %>%
  count(`If yes, what have you lost?`, sort=TRUE)

ggplot(lost_items, aes(x=reorder(`If yes, what have you lost?`, n), y=n, fill=n)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title="Most Common Lost Items", x="Item", y="Count") +
  theme_minimal()

# -------------------------------
# 📊 3. Department-wise Loss Count
# -------------------------------
dept_loss <- df %>%
  filter(`Have you ever lost something in campus?` == "yes") %>%
  count(`Department/Branch`, sort=TRUE)

ggplot(dept_loss, aes(x=reorder(`Department/Branch`, n), y=n, fill=n)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title="Loss Incidents by Department", x="Department", y="Count") +
  theme_minimal()

# -------------------------------
# 📊 4. Recovery Success Rate
# -------------------------------
recovery <- df %>%
  filter(`Were you able to recover your lost item(s)?` != "Not Applicable") %>%
  count(`Were you able to recover your lost item(s)?`)

ggplot(recovery, aes(x="", y=n, fill=`Were you able to recover your lost item(s)?`)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y") +
  labs(title="Recovery Success Rate") +
  theme_void() +
  scale_fill_brewer(palette="Pastel1")

# -------------------------------
# ☁️ 5. Word Cloud – Requested Features for Lost & Found System
# -------------------------------
features_text <- paste(df$`What features would you like in a 'Lost & Found' system?`, collapse=" ")
features_words <- unlist(strsplit(features_text, "[, ]+"))
features_freq <- table(tolower(features_words))
set.seed(123)
wordcloud(names(features_freq), freq=features_freq, min.freq=2,
          colors=brewer.pal(8, "Dark2"), random.order=FALSE,
          scale=c(4,0.5))

# -------------------------------
# 📊 6. Support for Central System
# -------------------------------
support_summary <- df %>%
  count(`Would you support having a central 'Lost & Found' system in college`)

ggplot(support_summary, aes(x="", y=n, fill=`Would you support having a central 'Lost & Found' system in college`)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y") +
  labs(title="Support for Central Lost & Found System") +
  theme_void() +
  scale_fill_brewer(palette="Set2")

# -------------------------------
# ✅ End of Analysis
# -------------------------------

