# Station: CDF (Chaux-de-Fonds), precipitation dataset rre150d0
# Daily total precipitation, from 6 UTC to 6 UTC (next day)
# Temporal coverage: 01.01.1900 - 31.12.2023
# Homogenized data: 1959 - 1997
# Displayed data: 1959 - 2023

# Packages for data processing
install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

install.packages("zoo")
library(zoo)

# Preprocessing
read.table("CDF_rre150d0.txt",header=T,as.is=F)
CDF_P <- CDF_rre150d0

colnames(CDF_P)[2] <- "date"
colnames(CDF_P)[3] <- "precip"

CDF_P$date <- as.Date(as.character(CDF_P$date), format = "%Y%m%d")
CDF_P$year <- format(CDF_P$date, "%Y")
CDF_P$month <- format(CDF_P$date, "%m")
CDF_P$Year <- as.numeric(CDF_P$year)
CDF_P$month <- as.numeric(CDF_P$month) 
CDF_P$precip <- as.numeric(CDF_P$precip) 

# Filter years prior 1959
CDF_P_filtered <- CDF_P[CDF_P$Year >= 1959, ]

# Check for missing values
sum(is.na(CDF_P))

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
  
# R10mm
# Annual count of days with precipitation ≥ 10 mm
R10mm_days_CDF <- CDF_P_filtered[CDF_P_filtered$precip >= 10, ]
R10mm_summary_CDF <- aggregate(R10mm_days_CDF$precip,
  by = list(Year = R10mm_days_CDF$Year),FUN = length)
colnames(R10mm_summary_CDF)[2] <- "Count_R10mm_CDF"

# Linear trend analysis (based on all availible data)
model_R10mm_CDF <- lm(Count_R10mm_CDF ~ Year, data = R10mm_summary_CDF)
summary(model_R10mm_CDF)

# Data homogenization period (CDF) : 1959 - 1997
R10mm_summary_CDF$Dataset <- ifelse(R10mm_summary_CDF$Year <= 1997,
  "homogenized", "non-homogenized")
R10mm_homog_CDF <- R10mm_summary_CDF[R10mm_summary_CDF$Dataset == "homogenized", ]

# Graph with data homogenization period
ggplot(R10mm_summary_CDF, aes(x = Year, y = Count_R10mm_CDF)) +
  geom_line(aes(color = Dataset), size = 0.5) +
  geom_point(aes(color = Dataset)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", linewidth = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.5) +
  labs(title = "R10mm: Annual count of days with precipitation ≥ 10 mm (CDF)",
  x = "Year",y = "Number of days", caption = "p-value: non-significant") +
  scale_y_continuous(limits = c(10, 75), breaks = seq(10, 75, by = 10)) +  
  scale_x_continuous(limits = c(1865, 2025), breaks = seq(1875, 2025, by = 25)) +
  scale_color_manual(values = c("homogenized" = "darkblue","non-homogenized" = "skyblue")) +
  theme_minimal() +theme(legend.position = "none")

# Summary statistics (homogenized data only)
#Mean
mean_R10mm_homog_CDF <- mean(R10mm_homog_CDF$Count_R10mm_CDF, na.rm = TRUE)

#Usual range 
range_R10mm_homog_CDF <- quantile(R10mm_homog_CDF$Count_R10mm_CDF,
  probs = c(0.05, 0.95),na.rm = TRUE)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

# R95p
# Total annual precipitation from days exceeding the 95th percentile,
# Based on wet days (P ≥ 1 mm), during the 1961–1990 reference period  
  
# 95th percentile for wet days (1961–1990)
CDF_reference_period <- CDF_P[CDF_P$date >= as.Date("1961-01-01") &
  CDF_P$date <= as.Date("1990-12-31") & CDF_P$precip >= 1, ]
threshold_95_CDF <- quantile(CDF_reference_period$precip, 0.95, na.rm = TRUE)

# Days above the 95th percentile, and compute annual total
R95p_summary_CDF <- CDF_P_filtered[CDF_P_filtered$precip >= 1 &
  CDF_P_filtered$precip > threshold_95_CDF, ]
R95p_summary_CDF <- aggregate(R95p_summary_CDF$precip,
  by = list(Year = R95p_summary_CDF$Year),FUN = sum)
colnames(R95p_summary_CDF)[2] <- "Total_R95p_CDF"
R95p_summary_CDF$Year <- as.numeric(R95p_summary_CDF$Year)

# Linear trend analysis (based on all availible data)
model_R95p_CDF <- lm(Total_R95p_CDF ~ Year, data = R95p_summary_CDF)
summary(model_R95p_CDF)
  
# Data homogenization period (CDF) : 1959 - 1997
R95p_summary_CDF$Dataset <- ifelse(R95p_summary_CDF$Year <= 1997,
  "homogenized", "non-homogenized")
R95p_homog_CDF <- R95p_summary_CDF[R95p_summary_CDF$Dataset == "homogenized", ]

# Graph with data homogenization period
ggplot(R95p_summary_CDF, aes(x = Year, y = Total_R95p_CDF)) +
  geom_line(aes(color = Dataset), size = 0.5) +
  geom_point(aes(color = Dataset)) +
  geom_smooth(method = "lm", se = FALSE, color = "black",
linetype = "dashed", linewidth = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.5) +
  labs(title = "R95p: Annual total precipitation from days > 95th percentile (CDF)",
  x = "Year", y = "Annual precipitation (mm)", caption = "p-value: non-significant") +
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 100)) +
  scale_x_continuous(limits = c(1865, 2025), breaks = seq(1875, 2025, by = 25)) +
  scale_color_manual(values = c("homogenized" = "darkblue","non-homogenized" = "skyblue")) +
  theme_minimal() +
  theme(legend.position = "none") + 
  annotate("text", x = 1870, y = 590, label = "95th percentile = 26.3 mm", 
           hjust = 0, vjust = 1, size = 3, color = "black")

# Summary statistics (homogenized data only)
  # Mean
mean_R95p_homog_CDF <- mean(R95p_homog_CDF$Total_R95p_CDF, na.rm = TRUE)

# Range
range_R95p_homog_CDF <- quantile(R95p_homog_CDF$Total_R95p_CDF,
  probs = c(0.05, 0.95), na.rm = TRUE)

----------------------------------------------------------------------------
-------------------------------------------------------------------------------
  
# Complementary approach : R95p days
# Annual count of days with precipitation > 95th percentile
# Based on wet days (P ≥ 1 mm), during the 1961–1990 reference period  
  
# 95th percentile for wet days (1961–1990)
CDF_reference_period <- CDF_P[CDF_P$date >= as.Date("1961-01-01") &
  CDF_P$date <= as.Date("1990-12-31") & CDF_P$precip >= 1, ]
threshold_95_days_CDF <- quantile(CDF_reference_period$precip, 0.95, na.rm = TRUE)

# Number of days per year exceeding this threshold
R95p_days_CDF <- CDF_P_filtered[CDF_P_filtered$precip >= 1 &
  CDF_P_filtered$precip > threshold_95_days_CDF, ]
R95p_days_summary_CDF <- aggregate(R95p_days_CDF$precip,
  by = list(Year = R95p_days_CDF$Year),FUN = length)
colnames(R95p_days_summary_CDF)[2] <- "Days_R95p_CDF"
  
# Linear trend analysis (based on all availible data)
model_R95p_days_CDF <- lm(Days_R95p_CDF ~ Year, data = R95p_days_summary_CDF)
summary(model_R95p_days_CDF)
  
# Data homogenization period (CDF) : 1959 - 1997
R95p_days_summary_CDF$Dataset <- ifelse(R95p_days_summary_CDF$Year <= 1997,
  "homogenized", "non-homogenized")
R95p_days_homog_CDF <- R95p_days_summary_CDF[R95p_days_summary_CDF$Dataset == "homogenized", ]

# Graph with data homogenization period
ggplot(R95p_days_summary_CDF, aes(x = Year, y = Days_R95p_CDF)) +
  geom_line(aes(color = Dataset), size = 0.5) +
  geom_point(aes(color = Dataset)) +
  geom_smooth(method = "lm", se = FALSE, color = "black",
  linetype = "dashed", linewidth = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.5) +
  labs(title = "Annual number of days with daily precipitation > 95th percentile (CDF)",
  x = "Year",y = "Number of days",
  caption = "p-value: non-significant") +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 2)) +
  scale_x_continuous(limits = c(1860, 2025), breaks = seq(1875, 2025, by = 25)) +
  scale_color_manual(values = c("homogenized" = "darkblue","non-homogenized" = "skyblue")) +
  theme_minimal() + theme(legend.position = "none") +
  annotate("text", x = 1870, y = 19, label = "95th percentile = 26.3 mm", 
  hjust = 0, vjust = 1, size = 3, color = "black")

# Summary statistics (homogenized data only)
  # Mean 
mean_R95p_days_homog_CDF <- mean(R95p_days_homog_CDF$Days_R95p_CDF, na.rm = TRUE)
  
  # Range 
range_R95p_days_homog_CDF <- quantile(R95p_days_homog_CDF$Days_R95p_CDF,
  probs = c(0.05, 0.95), na.rm = TRUE)
  
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
  
# Rx1day
# Annual maximum of daily precipitation 

# Compute annual maximum of daily precipitation
Rx1day_summary_CDF <- aggregate(CDF_P_filtered$precip,
  by = list(Year = CDF_P_filtered$Year),FUN = max, na.rm = TRUE)  
colnames(Rx1day_summary_CDF)[2] <- "Yearly_Max_Rx1day_CDF"

# Linear trend analysis (based on all availible data)
model_Rx1day_CDF <- lm(Yearly_Max_Rx1day_CDF ~ Year, data = Rx1day_summary_CDF)
summary(model_Rx1day_CDF)
  
# Data homogenization period (CDF) : 1959 - 1997
Rx1day_summary_CDF$Dataset <- ifelse(Rx1day_summary_CDF$Year <= 1997,
  "homogenized", "non-homogenized")
Rx1day_homog_CDF <- Rx1day_summary_CDF[Rx1day_summary_CDF$Dataset == "homogenized", ]

# Graph with data homogenization period
ggplot(Rx1day_summary_CDF, aes(x = Year, y = Yearly_Max_Rx1day_CDF)) +
  geom_line(aes(color = Dataset), size = 0.5) +
  geom_point(aes(color = Dataset)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", 
  linetype = "dashed", linewidth = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.5) +
  labs(title = "Rx1day: Annual maximum of daily precipitation (CDF)",
  x = "Year", y = "Daily precipitation (mm)", caption = "p-value: non-significant") +
  scale_y_continuous(limits = c(15, 120), breaks = seq(20, 120, by = 10)) +
  scale_x_continuous(limits = c(1865, 2025), breaks = seq(1875, 2025, by = 25)) +
  scale_color_manual(values = c("homogenized" = "darkblue", "non-homogenized" = "skyblue")) +
  theme_minimal() + theme(legend.position = "none")
  
# Summary statistics (homogenized data only)
  # Mean 
mean_Rx1day_homog_CDF <- mean(Rx1day_homog_CDF$Yearly_Max_Rx1day_CDF, na.rm = TRUE)

# Range 
range_Rx1day_homog_CDF <- quantile(Rx1day_homog_CDF$Yearly_Max_Rx1day_CDF,
  probs = c(0.05, 0.95), na.rm = TRUE)
  
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

# CWD
# Maximum number of consecutive wet days
  
# Binary wet day indicator
CDF_P_filtered$Wet_Day <- ifelse(CDF_P_filtered$precip >= 1, 1, 0)

# Create the loop to compute the maximum wet spell length
CWD_summary_CDF <- data.frame(Year = unique(CDF_P_filtered$Year), Max_CWD_CDF = NA)
for (i in seq_along(CWD_summary_CDF$Year)) {
  year_data <- CDF_P_filtered[CDF_P_filtered$Year == CWD_summary_CDF$Year[i], ]
  rle_wet <- rle(year_data$Wet_Day)
  if (any(rle_wet$values == 1)) {CWD_summary_CDF$Max_CWD_CDF[i] <- max(rle_wet$lengths[rle_wet$values == 1], na.rm = TRUE)} 
  else {CWD_summary_CDF$Max_CWD_CDF[i] <- 0}}
CWD_summary_CDF$Year <- as.numeric(CWD_summary_CDF$Year)

# Linear trend analysis (based on all availible data)
model_CWD_CDF <- lm(Max_CWD_CDF ~ Year, data = CWD_summary_CDF)
summary(model_CWD_CDF)

# Data homogenization period (CDF) : 1959 - 1997
CWD_summary_CDF$Dataset <- ifelse(CWD_summary_CDF$Year <= 1997,
  "homogenized", "non-homogenized")
CWD_homog_CDF <- CWD_summary_CDF[CWD_summary_CDF$Dataset == "homogenized", ]

# Graph with data homogenization period
ggplot(CWD_summary_CDF, aes(x = Year, y = Max_CWD_CDF)) +
  geom_line(aes(color = Dataset), size = 0.5) +
  geom_point(aes(color = Dataset)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", 
  linetype = "dashed", linewidth = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.5) +
  labs(title = "CWD: Maximum number of consecutive wet days (CDF)",
  x = "Year", y = "Number of days", caption = "p-value: non-significant") +
  scale_y_continuous(limits = c(3, 23), breaks = seq(4, 23, by = 2)) +
  scale_x_continuous(limits = c(1865, 2025), breaks = seq(1875, 2025, by = 25)) +
  scale_color_manual(values = c("homogenized" = "darkblue","non-homogenized" = "skyblue")) +
  theme_minimal() + theme(legend.position = "none")

# Summary statistics (homogenized data only)
  # Mean 
mean_CWD_homog_CDF <- mean(CWD_homog_CDF$Max_CWD_CDF, na.rm = TRUE)

  # Range 
range_CWD_homog_CDF <- quantile(CWD_homog_CDF$Max_CWD_CDF,
  probs = c(0.05, 0.95), na.rm = TRUE)

--- 








