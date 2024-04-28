# Viz-A-Thon
# Final Script by Shoumik Kumbham and Roshan Verma

# Required libraries
library(usmap)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

# Plot 1: US Demographics

# Data Aggregation
# Calculate the total sales for each state from the dataset
state_summary <- data %>%
  group_by(state) %>%
  summarise(total_sales = sum(sale))

# Categorization Setup
# Define sales range breaks and corresponding labels
breaks <- c(0, 100000, 300000, 500000, 1000000, Inf)
labels <- c("Under $100k", "$100k - $300k", "$300k - $500k", "$500k - $1M", "Over $1M")

# Assign categorical levels to total sales using defined breaks and labels
state_summary$sales_category <- cut(
  state_summary$total_sales,
  breaks = breaks,
  labels = labels,
  include.lowest = TRUE
)

# Visualization Setup
# Define colors for each sales category
colors <- c("Under $100k" = "#ece0d1", 
            "$100k - $300k" = "#dbc1ac", 
            "$300k - $500k" = "#967259", 
            "$500k - $1M" = "#634832", 
            "Over $1M" = "#38220f")

# Map Plotting
# Create a US map to display sales categories by state
p <- plot_usmap(
  data = state_summary, 
  values = "sales_category", 
  lines = "black",  # Outline each state with black lines
  labels = TRUE  # Display state labels
) + 
  scale_fill_manual(
    values = colors,
    name = "Sales Category",
    breaks = labels,  # Use defined labels in the legend
    labels = labels
  ) +
  labs(
    title = "Total Sales by State",
    subtitle = "Categorized by Sales Volume"
  ) +
  theme(legend.position = "right")  # Position the legend on the right side

# Enhance border thickness
p$layers[[2]]$aes_params$size <- 1.5

# Output the plot
print(p)


# Plot 2 : Top 10 Representatives

df <- data

# Calculate total sales and total sale profit grouped by employee
employee_performance <- aggregate(cbind(sale, sale.profit) ~ rep, data = df, FUN = sum)

# Sort data frame by total sales in ascending order
employee_performance <- employee_performance[order(employee_performance$sale), ]

# Select the top 10 employees based on total sales
top_10_employees <- tail(employee_performance, 10)

# Combine employee names with their state for better identification
top_10_employees$rep <- paste(top_10_employees$rep, "(", df$state[match(top_10_employees$rep, df$rep)], ")")

# Plot horizontal stacked bars for sales and sale profit for the top 10 employees
ggplot(top_10_employees, aes(x = sale, y = reorder(rep, sale))) +
  geom_bar(aes(fill = "Sales"), stat = "identity", width = 0.6) +
  geom_bar(aes(x = sale.profit, y = rep, fill = "Sale Profit"), stat = "identity", width = 0.8) +
  scale_fill_manual(values = c("Sales" = "skyblue", "Sale Profit" = "lightgreen")) +
  labs(x = "Amount (Millions)", y = "Employee (State)", title = "Top 10 Employees: Sales vs Sale Profit") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma)  # Format x-axis labels with commas for thousands


# Plot 3: Monthly Analysis

# Data Preparation
# Convert date strings to date objects and extract the year and month for aggregation
data$date <- as.Date(data$date, format="%Y-%m-%d")
data$month <- floor_date(data$date, "month")

# Data Aggregation
# Summarize total sales and profit, and calculate average unit cost and discount percentage per month
monthly_data <- data %>%
  group_by(month) %>%
  summarise(
    TotalSales = sum(sale),
    TotalProfit = sum(sale.profit),  # Aggregate total profit
    AvgUnitCost = mean(unit.cost),
    AvgDiscountPct = mean(discount.pct) * 100  # Convert discount ratio to percentage
  )

# Visualization
# Create a combined bar and line plot for monthly sales data and total profit
ggplot(data = monthly_data) +
  geom_bar(aes(x = month, y = TotalSales), stat="identity", fill="steelblue") +
  geom_line(aes(x = month, y = TotalProfit), group=1, colour="red", size=0.5) +
  geom_point(aes(x = month, y = TotalProfit), color="red", size=2) +
  scale_y_continuous(
    name = "Total Sales",
    sec.axis = sec_axis(~./max(monthly_data$TotalSales) * max(monthly_data$TotalProfit), name="Total Profit")
  ) +
  labs(x = "Month", y = "Total Sales", title = "Monthly Sales and Total Profit") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=0, hjust=1))  # Ensure x-axis labels are horizontal for clarity


# Plot 4: Tea-Coffee Pie Chart

# Data Preparation
# Summarize total revenue by type (tea and coffee)
summary_data <- data %>%
  group_by(type) %>%
  summarise(Revenue = sum(sale))

# Visualization
# Create a pie chart to show the revenue distribution between tea and coffee
ggplot(summary_data, aes(x = "", y = Revenue, fill = type)) +
  geom_bar(stat = "identity", width = 1) +  # Use bars with no spacing between them
  coord_polar(theta = "y") +  # Convert the bar chart to a pie chart by using polar coordinates
  scale_fill_manual(values = c("coffee" = "#6b486b", "tea" = "#ff8c00")) +  # Assign custom colors to each type
  labs(title = "Revenue Distribution: Tea vs Coffee",  # Add a chart title
       x = NULL, y = NULL, fill = "Type") +  # Remove axis labels and adjust legend label
  theme_minimal() +  # Use a minimal theme
  theme(axis.title = element_blank(),  # Remove axis titles
        axis.text = element_blank(),  # Hide axis text
        axis.ticks = element_blank(),  # Hide axis ticks
        panel.grid = element_blank(),  # Remove grid lines
        legend.title = element_blank())  # Remove legend title


# Plot 5: Drinks Type 

# Data Preparation
# Summarize total revenue and profit by drink
summary_data <- data %>%
  group_by(drink) %>%
  summarise(
    Revenue = sum(sale), 
    Profit = sum(sale.profit)
  )

# Custom Order for Drinks
# Define a custom order for drinks 
desired_order <- c("Kona", "French", "Espresso")
other_drinks <- setdiff(summary_data$drink, desired_order)
final_order <- factor(summary_data$drink, levels = c(desired_order, sort(other_drinks)))

# Visualization
# Create a side-by-side bar plot to show revenue and profit for each drink
ggplot(summary_data, aes(x = final_order, y = Revenue)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = "Revenue"), width = 0.7) +
  geom_bar(aes(y = Profit, fill = "Profit"), stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(
    labels = c("Revenue", "Profit"), 
    values = c("blue", "red")  # Assign custom colors for revenue and profit
  ) +
  labs(
    x = "Drink",
    y = "Amount ($)",
    title = "Revenue and Profit by Drink",
    fill = "Type"  # Legend title
  ) +
  scale_y_continuous(labels = label_comma()) +  # Format y-axis labels with commas
  theme_minimal()  # Use a minimal theme for clean aesthetics


