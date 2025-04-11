# Load necessary packages
library(tidyverse)  

# Check the current working directory to understand where R is looking for files
getwd()
# Set the working directory to the folder where the data files are stored.
setwd("C:/Users/LENOVO/Downloads/[Bdata] AP/Individual Report")


#Task 1: Data Inspection and Cleaning

## A. replace "?" by NA
engine <- read.csv("Engine.csv", na.strings = "?")
maintenance <- read.csv("Maintenance.csv", na.strings = "?")
automobile <- read.csv("Automobile.csv", na.strings = "?")

#Output checking
sum(engine == "?", na.rm = TRUE)
sum(automobile == "?", na.rm = TRUE) 
sum(maintenance == "?", na.rm = TRUE)

## B. Converted categorical variables to factors
automobile$BodyStyles <- as.factor(automobile$BodyStyles) 
engine$FuelTypes <- as.factor(engine$FuelTypes)
maintenance$ErrorCodes <- as.factor(maintenance$ErrorCodes)

#Output checking
str(automobile$BodyStyles)
str(engine$FuelTypes)
str(maintenance$ErrorCodes)

## C. Replace missing value in Horsepower column by mean of Horsepower
mean_horsepower <- mean(engine$Horsepower, na.rm = TRUE)
engine <- engine %>%
  mutate(Horsepower = if_else(is.na(Horsepower), mean_horsepower, Horsepower))

#Output Checking
sum(is.na(engine$Horsepower))  #Should be 0

##D. Visualize Horsepower distribution
ggplot(engine, aes(x = Horsepower)) +
  geom_histogram(binwidth = 20, fill = "skyblue", color = "darkblue") +
  # Creates a histogram with bin width of 20 and styled appearance
  labs(title = "Distribution of Horsepower",
       x = "Horsepower",
       y = "Frequency") # Adds title and labels


#Task 2: Horsepower Distribution by Cylinders and Engine Size

## A. Horsepower Distribution by Cylinder Count
### Step 1:Convert to factor to ensure correct grouping in plots
engine$NumCylinders <- as.factor(engine$NumCylinders)

### Step 2:Plot the histogram
ggplot(engine, aes(x = Horsepower)) +
  geom_histogram(binwidth = 20, fill = "skyblue", color = "darkblue") +
   # Creates a histogram with bin width of 10 and styled appearance
  facet_wrap(~ NumCylinders, scales = "free_y") +
   # Splits the histogram into subplots for each cylinder group using facet_wrap
  labs(title = "Distribution of Horsepower",
       x = "Horsepower",
       y = "Frequency") +  # Adds title and labels
  theme_minimal() 


## B. Horsepower Distribution by Engine Size Group
### Step 1:Checking data type to ensure later grouping data. 
str(engine)

### Step 2:Group EngineSize into 4 categorical size bands
engine <- engine %>%
  mutate(EngineSizeGroup = case_when(
    EngineSize >= 60 & EngineSize <= 100 ~ "60–100",
    EngineSize > 100 & EngineSize <= 200 ~ "101–200",
    EngineSize > 200 & EngineSize <= 300 ~ "201–300",
    EngineSize > 300 ~ "301+"
  ))

### Step 3:Convert EngineSizeGroup to factor for ordering in plots
engine$EngineSizeGroup <- factor(engine$EngineSizeGroup,
                                 levels = c("60–100", "101–200", "201–300", "301+"))

### Step 4:Plot histogram by EngineSizeGroup
ggplot(engine, aes(x = Horsepower)) +
  geom_histogram(binwidth = 20, fill = "skyblue", color = "darkblue") + 
  # Creates a histogram with bin width of 10 and styled appearance
  facet_wrap(~ EngineSizeGroup, scales = "free_y") + 
  # Splits the histogram into subplots for each cylinder group using facet_wrap
  labs(title = "Horsepower Distribution by Engine Size Group",
       x = "Horsepower",
       y = "Engine Size Groups") +  # Adds title and labels
  theme_minimal()


# Task 3: Analyze engine troubles and compare by fuel type

## A. Filter only trouble engines
# Filter to only trouble cases (engine or other component failure)
trouble_cases <- maintenance %>%
  filter(ErrorCodes != "0")  
#ErrorCode = 1 (engine fails) or -1 (component fails)

##Output checking
table(trouble_cases$ErrorCodes) 
# Confirm all ErrorCodes in filtered data are non-zero
# Should only show 1 and -1

## B.Top 5 Most Common Troubles (All Engines)
### Step 1: Merge trouble_cases with automobile and engine data to get FuelTypes and EngineModel information
trouble_cases <- trouble_cases %>%  
  inner_join(automobile, by = "PlateNumber") %>% 
  inner_join(engine, by = "EngineModel")


### Step 2: View top 5 most common engine troubles 
top_troubles <- trouble_cases %>% 
  group_by(Troubles) %>% 
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  head(5)

#Output checking
print(top_troubles) 

## C. Compare Troubles by Fuel Type
### Step 1: Get top 5 troubles for Gas engines
top_gas_troubles <- trouble_cases %>%
  filter(FuelTypes == "gas") %>% 
  group_by(Troubles) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  head(5)

# Output checking
print(top_gas_troubles) 

### Step 2:Get top 5 troubles for Diesel engines
top_diesel_troubles <- trouble_cases %>%
  filter(FuelTypes == "diesel") %>% 
  group_by(Troubles) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  head(5)

# Output checking
print(top_diesel_troubles) 

### Step 3: Combine into one table for comparison
top_troubles_combined <- full_join(  
  top_diesel_troubles %>% 
    rename(Diesel_Frequency = Frequency), 
  top_gas_troubles %>% 
    rename(Gas_Frequency = Frequency),
  by = "Troubles" 
)
print(top_troubles_combined)


# Task 4: Analyzing Maintenance Methods by Engine Error and Fuel Type
## A.Maintenance Method by Fuel Type Analysis
### Step 1: Filter only vehicles with confirmed or suspected troubles
trouble_vehicles <- maintenance %>%
  filter(ErrorCodes != 0)

### Step 2: Join trouble_vehicles with maintenance and engine data to include FuelTypes
trouble_vehicles <- trouble_vehicles %>%
  left_join(automobile, by = "PlateNumber")  %>%
  left_join(engine, by = "EngineModel")

### Step 3: Visualize Maintenance Methods by Fuel Type
ggplot(trouble_vehicles, aes(x = FuelTypes, fill = Methods)) +
  geom_bar(position = "dodge") +  
  labs(
    title = "Distribution of Maintenance Methods by Fuel Type",
    x = "Fuel Type",
    y = "Proportion of Maintenance Methods"
  ) + 
  theme_minimal() 

### B. Plot Maintenance Methods by ErrorCode
ggplot(trouble_vehicles, aes(x = factor(ErrorCodes), fill = Methods)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribution of Maintenance Methods by Error Code",
    x = "Error Code (1 = Engine Failure, -1 = Other Failure)",
    y = "Proportion of Maintenance Methods"
  ) +
  theme_minimal()


