# ==========================================
# CUSTOMER LIFECYCLE & RFM ANALYTICS PROJECT
# Tools: Tidyverse, Treemapify, GGally
# ==========================================

# 1. INITIALIZATION
# Run these install lines once if you haven't already:
# install.packages("tidyverse")
# install.packages("treemapify")
# install.packages("GGally")

library(tidyverse)
library(lubridate)
library(treemapify)
library(GGally)

# 2. DATA SYNTHESIS
# Creating a high-density retail dataset (2500 transactions)
set.seed(42)
df <- data.frame(
  CustomerID = sample(1001:1200, 2500, replace = TRUE),
  OrderDate = sample(seq(as.Date('2023/01/01'), as.Date('2023/12/31'), by="day"), 2500, replace = TRUE),
  Revenue = round(runif(2500, 10, 1000), 2),
  Category = sample(c("Electronics", "Home", "Fashion", "Beauty"), 2500, replace = TRUE)
)

# 3. ADVANCED RFM LOGIC
analysis_date <- as.Date("2024-01-01")

rfm_data <- df %>%
  group_by(CustomerID) %>%
  summarise(
    Recency = as.numeric(analysis_date - max(OrderDate)),
    Frequency = n(),
    Monetary = sum(Revenue),
    AvgOrderValue = round(sum(Revenue) / n(), 2)
  ) %>%
  mutate(
    R_Score = 6 - ntile(Recency, 5),
    F_Score = ntile(Frequency, 5),
    M_Score = ntile(Monetary, 5)
  ) %>%
  mutate(Segment = case_when(
    R_Score >= 4 & F_Score >= 4 & M_Score >= 4 ~ "Platinum Champions",
    R_Score <= 2 & M_Score >= 4 ~ "High-Value At Risk",
    F_Score >= 4 & M_Score <= 2 ~ "Frequent Low-Spenders",
    R_Score >= 4 & F_Score == 1 ~ "New Leads",
    TRUE ~ "Standard Customer"
  ))

# 4. PLOT 1: STRATEGIC BUSINESS TREEMAP
# Purpose: Show revenue contribution by customer segment
plot_data <- rfm_data %>%
  group_by(Segment) %>%
  summarise(Cust_Count = n(), Total_Rev = sum(Monetary))

ggplot(plot_data, aes(area = Total_Rev, fill = Segment, label = paste(Segment, "\n", Cust_Count, "Users"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "Business Value Segmentation",
       subtitle = "Area size represents Total Revenue contribution") +
  theme_minimal()

# 5. PLOT 2: MULTIVARIATE CORRELATION MATRIX
# Purpose: Show statistical relationships between KPIs
ggpairs(rfm_data, columns = c("Recency", "Frequency", "Monetary", "AvgOrderValue"),
        title = "Statistical Correlation of Customer Metrics") + 
  theme_bw()

