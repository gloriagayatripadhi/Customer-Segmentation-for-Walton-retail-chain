data <- read.csv('Complete_data.csv')

colnames(data)

data$Purchase.Date = as.Date(data$Purchase.Date, "%d-%m-%Y")
data$date_of_purchase <- format(as.Date(data$Purchase.Date), "%Y-%m-%d")
data$year_of_purchase = as.numeric(format(as.Date(data$Purchase.Date, "%d-%m-%Y"), "%Y"))
data$days_since       = as.numeric(difftime(time1 = "2015-01-01",
                                            time2 = data$date_of_purchase,
                                            units = "days"))
summary(data)

library(sqldf)

colnames(data)[which(names(data) == "ï..Customer.ID")] <- "Customer_ID"

customers_2014 = sqldf("SELECT Customer_ID,
                               MIN(days_since) AS 'recency',
                               MAX(days_since) AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(Revenue) AS 'amount'
                        FROM data GROUP BY Customer_ID")

summary(customers_2014)
hist(customers_2014$recency)
hist(customers_2014$frequency)
hist(customers_2014$amount)
hist(customers_2014$amount, breaks = 100)

# Complete segment solution using which, and exploiting previous test as input
customers_2014$segment = "NA"
customers_2014$segment[which(customers_2014$recency > 365*2)] = "inactive"
customers_2014$segment[which(customers_2014$recency <= 365*2 & customers_2014$recency > 365*1)] = "warm"
customers_2014$segment[which(customers_2014$recency <= 365*1)] = "active"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount < 20)] = "warm low value"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount >= 20)] = "warm high value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$first_purchase <= 365)] = "new active"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount < 20)] = "active low value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount >= 20)] = "active high value"
customers_2014$segment = factor(x = customers_2014$segment, levels = c("inactive",
                                                                       "warm high value", "warm low value",
                                                                       "active high value", "active low value"))


customers_2013 = sqldf("SELECT Customer_ID,
                               MIN(days_since) AS 'recency',
                               MAX(days_since) AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(Revenue) AS 'amount'
                        FROM data 
                        WHERE days_since>365
                        GROUP BY Customer_ID")


# Complete segment solution using which, and exploiting previous test as input
customers_2013$segment = "NA"
customers_2013$segment[which(customers_2013$recency > 365*2)] = "inactive"
customers_2013$segment[which(customers_2013$recency <= 365*2 & customers_2013$recency > 365*1)] = "warm"
customers_2013$segment[which(customers_2013$recency <= 365*1)] = "active"
customers_2013$segment[which(customers_2013$segment == "warm" & customers_2013$amount < 20)] = "warm low value"
customers_2013$segment[which(customers_2013$segment == "warm" & customers_2013$amount >= 20)] = "warm high value"
customers_2013$segment[which(customers_2013$segment == "active" & customers_2013$first_purchase <= 365)] = "new active"
customers_2013$segment[which(customers_2013$segment == "active" & customers_2013$amount < 20)] = "active low value"
customers_2013$segment[which(customers_2013$segment == "active" & customers_2013$amount >= 20)] = "active high value"
customers_2013$segment = factor(x = customers_2013$segment, levels = c("inactive",
                                                                       "warm high value", "warm low value",
                                                                       "active high value", "active low value"))


# Compute transition matrix
merged_data = merge(x = customers_2013, y = customers_2014, by = "Customer_ID", all.x = TRUE)
head(merged_data)
transition = table(merged_data$segment.x, merged_data$segment.y)
print(transition)


transition = transition / rowSums(transition)

transition[4:5,] <- 0
print(transition)


# Initialize a matrix with the number of customers in each segment today and after 10 periods
segments = matrix(nrow = 5, ncol = 7)
segments[, 1] = table(customers_2014$segment)
colnames(segments) = 2014:2020
row.names(segments) = levels(customers_2014$segment)
print(segments)

# Compute for each an every period
for (i in 2:7) {
  segments[, i] = segments[, i-1] %*% transition
}

# Plot inactive, active high value customers over time
barplot(segments[1, ])
barplot(segments[2, ])

# Display how segments will evolve over time
print(round(segments))


# Compute how much revenue is generated by segments
# Notice that people with no revenue in 2015 do NOT appear
revenue_2014 = sqldf("SELECT Customer_ID, SUM(Revenue) AS 'revenue_2014'
                      FROM data
                      WHERE year_of_purchase = 2014
                      GROUP BY 1")
summary(revenue_2014)


# Merge 2015 customers and 2015 revenue (correct)
actual = merge(customers_2014, revenue_2014, all.x = TRUE)
actual$revenue_2014[is.na(actual$revenue_2014)] = 0

# Show average revenue per customer and per segment
aggregate(x = actual$revenue_2014, by = list(customers_2014$segment), mean)

yearly_revenue <- c(0,0,0,33.56,28.56)


# Compute revenue per segment
revenue_per_segment = yearly_revenue * segments
print(revenue_per_segment)

# Compute yearly revenue
yearly_revenue = colSums(revenue_per_segment)
print(round(yearly_revenue))
barplot(yearly_revenue)

# Compute cumulated revenue
cumulated_revenue = cumsum(yearly_revenue)
print(round(cumulated_revenue))
barplot(cumulated_revenue)

# Create a discount factor
discount_rate = 0.10
discount = 1 / ((1 + discount_rate) ^ ((1:7) - 1))
print(discount)

# Compute discounted yearly revenue
disc_yearly_revenue = yearly_revenue * discount
print(round(disc_yearly_revenue))
barplot(disc_yearly_revenue)
lines(yearly_revenue)

# Compute discounted cumulative revenue
disc_cumulated_revenue = cumsum(disc_yearly_revenue)
print(round(disc_cumulated_revenue))
barplot(disc_cumulated_revenue)

# What is the database worth?
print(disc_cumulated_revenue[6] - yearly_revenue[1])


