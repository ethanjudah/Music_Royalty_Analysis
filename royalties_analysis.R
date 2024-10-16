install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
library(readxl)  # For reading Excel files
library(dplyr)   # For data manipulation
library(ggplot2) # For visualization

Royalty_Analysis <- read_excel("Royalty_Analysis.xlsx")
View(Royalty_Analysis)   

getwd()

#Define the file name
file_name <- "Royalty_Analysis.xlsx"

#List available sheet names
sheet_names <- excel_sheets(file_name)
print(sheet_names)

#Load specific sheets into data frames
royalties_df <- read_excel(file_name, sheet = "Royalties")
accounts_df <- read_excel(file_name, sheet = "Accounts")
works_df <- read_excel(file_name, sheet = "Works")
territory_df <- read_excel(file_name, sheet = "Territory")
source_df <- read_excel(file_name, sheet = "Source")


#View the first few rows of each data frame
head(royalties_df)
head(accounts_df)
head(works_df)
head(territory_df)
head(source_df)

#Count missing values in each column
colSums(is.na(royalties_df)) # territory number has 96 missing rows 

#change UK territory code from 826A -> 826 for consistency 
#Count occurrences of "826A" while ignoring NAs
sum(royalties_df$territorynumber == "826A", na.rm = TRUE)

#Count occurrences of "826" while ignoring NAs
sum(royalties_df$territorynumber == "826", na.rm = TRUE)
#Replace "826A" with "826", ignoring NA values
royalties_df$territorynumber[royalties_df$territorynumber == "826A"] <- "826"
sum(royalties_df$territorynumber == "826A", na.rm = TRUE) #no more 826A, all values converted to 826


#Convert the final statement date to Date format
royalties_df$finalstatementdate <- as.Date(royalties_df$finalstatementdate)

#Check for missing values
summary(royalties_df)

#Merge Royalties and Accounts based on accountcode
merged_df <- left_join(royalties_df, accounts_df, by = "accountcode")

#Check the merged data
head(merged_df)


#Merge Royalties and Works based on songcode
royalties_works_df <- left_join(royalties_df, works_df, by = "songcode")

#Check the merged data
head(royalties_works_df)

install.packages("lubridate")
library(lubridate)

#Convert YearMonth to a proper date format
royalties_df$YearMonth <- ymd(paste0(royalties_df$YearMonth, "-01"))


royalties_growth <- royalties_df %>%
  group_by(YearMonth) %>%
  summarise(total_royalties = sum(netpublishersharepayment, na.rm = TRUE))


ggplot(royalties_growth, aes(x = YearMonth, y = total_royalties)) +
  geom_line() +
  labs(title = "Growth of Royalties Over Time", x = "Year-Month", y = "Total Royalties") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(lubridate)
library(ggplot2)

#Ensure YearMonth is in Date format
royalties_growth$YearMonth <- ymd(paste0(royalties_growth$YearMonth, "-01"))

#Plot the data
ggplot(royalties_growth, aes(x = YearMonth, y = total_royalties)) +
  geom_line(color = "blue", linewidth = 1) +        # Use linewidth for line thickness
  geom_point(color = "red", size = 2) +             # Add points
  geom_smooth(method = "loess", se = FALSE, color = "darkgreen") +  # Add a smooth trend line
  labs(title = "Growth of Royalties Over Time - Publisher", 
       x = "Year-Month", 
       y = "Total Royalties (£)") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +   # Adjust x-axis date formatting
  theme_minimal() +                                                 # Cleaner theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),          # Rotate x-axis labels
        plot.title = element_text(hjust = 0.5),                     # Center the title
        panel.grid.minor = element_blank())                         # Remove minor gridlines


 
#Analysis: Top-Earning Clients
#Identify which clients are top earners, group the data by accountcode and sum the royalties.
#Group by accountcode and sum the net publisher share payment
top_clients <- merged_df %>%
  group_by(accountcode) %>%
  summarise(total_royalties = sum(netpublishersharepayment, na.rm = TRUE)) %>%
  arrange(desc(total_royalties)) %>%
  head(10)

#Display top 10 clients
print(top_clients)

#Visualize top 10 clients
ggplot(top_clients, aes(x = reorder(accountcode, -total_royalties), y = total_royalties)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +  # Add color and adjust bar width
  geom_text(aes(label = round(total_royalties, 0)), vjust = -0.3, color = "black") +  # Add labels on bars
  labs(title = "Top 5 Clients by Royalties", 
       x = "Client", 
       y = "Total Royalties (in currency)") +  # Specify units for the y-axis
  theme_minimal() +  # Use a cleaner theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),   # Rotate x-axis labels
        plot.title = element_text(hjust = 0.5, size = 16),   # Center the title and adjust size
        axis.text = element_text(size = 12),                 # Increase axis text size
        axis.title = element_text(size = 14))                # Increase axis title size


#Analysis: Royalties by Territory
#royalties by territory by merging royalties_df with territory_df using territorynumber
# Convert territorynumber in royalties_df to numeric
royalties_df$territorynumber <- as.numeric(royalties_df$territorynumber)

#Merge Royalties and Territory data
royalties_territory_df <- left_join(royalties_df, territory_df, by = "territorynumber")

#Group by territory and sum royalties
royalties_by_territory <- royalties_territory_df %>%
  group_by(territoryname) %>%
  summarise(total_royalties = sum(netpublishersharepayment, na.rm = TRUE)) %>%
  arrange(desc(total_royalties))

#Top 5 territories
print(royalties_by_territory %>% head(5))

#Plot the top territories
ggplot(royalties_by_territory %>% head(5), aes(x = reorder(territoryname, -total_royalties), y = total_royalties)) +
  geom_bar(stat = "identity", fill = "darkorange", width = 0.7) +    # Add color and adjust bar width
  geom_text(aes(label = round(total_royalties, 0)), vjust = -0.3, color = "black", size = 4) +  # Add labels to bars
  labs(title = "Top 5 Territories by Royalties", 
       x = "Territory", 
       y = "Total Royalties (in currency)") +   # Specify unit for y-axis (replace with actual unit)
  theme_minimal() +   # Apply a minimalist theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),   # Rotate and size x-axis labels
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center and bold the title
        axis.title = element_text(size = 14),        # Increase axis title size
        axis.text = element_text(size = 12),         # Increase axis text size
        panel.grid.major = element_blank(),          # Remove major gridlines
        panel.grid.minor = element_blank())          # Remove minor gridlines


#Analysis: Suspicious Accounts and Deactivated Works
#Losses due to suspicious accounts or deactivated works, filter the data based on flags.
#filter for TRUE
suspicious_accounts <- accounts_df %>% filter(suspicious == TRUE)
#handle blanks
suspicious_accounts <- accounts_df %>% filter(suspicious == TRUE | is.na(suspicious))

#Filter deactivated works using the correct column name
deactivated_works <- works_df %>% filter(isdeactivated == TRUE)

#Merge to find suspicious royalties
suspicious_royalties <- left_join(royalties_df, suspicious_accounts, by = "accountcode")
deactivated_royalties <- left_join(royalties_df, deactivated_works, by = "songcode")

#Calculate total losses
lost_royalties_suspicious <- sum(suspicious_royalties$netpublishersharepayment, na.rm = TRUE)
lost_royalties_deactivated <- sum(deactivated_royalties$netpublishersharepayment, na.rm = TRUE)

#Print results
cat("Total lost royalties due to suspicious accounts:", lost_royalties_suspicious, "\n")
cat("Total lost royalties due to deactivated works:", lost_royalties_deactivated, "\n")

#visualise loss to suspicious and deavctivated 
# Create a data frame for visualization
library(scales)  # For better axis formatting

# Create a data frame for visualisation
losses_df <- data.frame(
  category = c("Suspicious Accounts", "Deactivated Works"),
  total_loss = c(lost_royalties_suspicious, lost_royalties_deactivated)
)

#Plot the total losses due to suspicious accounts and deactivated works
ggplot(losses_df, aes(x = category, y = total_loss, fill = category)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +  # Bar chart with black borders
  geom_text(aes(label = scales::comma(total_loss)), vjust = -0.5, size = 5, fontface = "bold") +  # Values on top of bars
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +  # colors for the bars
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas instead of scientific notation (e)
  labs(title = "Total Lost Royalties",
       subtitle = "Losses from Suspicious Accounts and Deactivated Works",
       x = "Category",
       y = "Total Loss (in Currency Units)",
       caption = "Source: Royalty Data") +
  theme_minimal(base_size = 15) +  # Minimalist theme with larger font size
  theme(axis.text.x = element_text(face = "bold", size = 12),  # Bold x-axis labels
        axis.text.y = element_text(face = "bold", size = 12),  # Bold y-axis labels
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18),  # Bold and centered title
        plot.subtitle = element_text(hjust = 0.5, size = 14),  # Centered subtitle
        legend.position = "none")  # Remove legend as categories are on x-axis

#Top earning Tiers 
#Merge royalties_df and accounts_df based on accountcode
merged_df <- merge(royalties_df, accounts_df, by = "accountcode")

#Group by clienttier and sum the netpublishersharepayment
top_tiers <- merged_df %>%
  group_by(clienttier) %>%
  summarise(total_earnings = sum(netpublishersharepayment, na.rm = TRUE)) %>%
  arrange(desc(total_earnings))  # Sort by total earnings in descending order

#Display the top-earning client tiers
print(top_tiers)

#Plot the top client tiers by earnings
ggplot(top_tiers, aes(x = reorder(clienttier, -total_earnings), y = total_earnings, fill = clienttier)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +  # Add black border to bars for contrast
  scale_fill_brewer(palette = "Set2") +  # Use a more vibrant color palette
  labs(title = "Top Earning Client Tiers",
       x = "Client Tier",
       y = "Total Earnings (£)",
       caption = "Source: Royalty Data") +
  theme_minimal(base_size = 15) +  # Apply a clean, minimal theme with larger font size
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # Rotate and bold x-axis labels
        axis.text.y = element_text(face = "bold"),  # Bold y-axis labels
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18),  # Center and bold the title
        plot.subtitle = element_text(hjust = 0.5, size = 14),  # Center the subtitle
        legend.position = "none")  # Hide the legend, since clienttier labels are on the x-axis



#Top earning tiers over time - line graph 
#Filter the data for the specific client tier entries
filtered_df <- merged_df %>%
  filter(clienttier %in% c("A: Funded", "B: Admin", "C: Self Service", "N/A") | is.na(clienttier))

#Extract Year-Month from the final statement date
filtered_df <- filtered_df %>%
  mutate(YearMonth = floor_date(finalstatementdate, "month"))  # Round to month

#Group the data by client tier and Year-Month, then summarize total royalties
growth_df <- filtered_df %>%
  group_by(clienttier, YearMonth) %>%
  summarise(total_royalties = sum(netpublishersharepayment, na.rm = TRUE)) %>%
  ungroup()

#Plot the growth of royalties over time for specific client tiers
ggplot(growth_df, aes(x = YearMonth, y = total_royalties, color = clienttier, group = clienttier)) +
  geom_line(size = 1.2) +  # Plot a line graph for each tier
  geom_point(size = 3) +   # Add points to each line
  labs(title = "Growth of Royalties Over Time by Client Tier",
       subtitle = "Comparing Tiers A: Funded, B: Admin, C: Self Service, and N/A",
       x = "Year-Month",
       y = "Total Royalties (£)",
       color = "Client Tier") +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +  # Format x-axis with 3-month intervals
  theme_minimal(base_size = 15) +  # Clean theme with larger font size
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18),  # Bold, centered title
        plot.subtitle = element_text(hjust = 0.5, size = 14))  # Centered subtitle

#bar chart of growth by tier - including years
#Extract the Year from the final statement date
merged_df <- merged_df %>%
  mutate(Year = year(finalstatementdate))

#Group data by client tier and year, then summarize total royalties
grouped_df <- merged_df %>%
  filter(clienttier %in% c("A: Funded", "B: Admin", "C: Self Service", "N/A") | is.na(clienttier)) %>%
  group_by(clienttier, Year) %>%
  summarise(total_earnings = sum(netpublishersharepayment, na.rm = TRUE)) %>%
  ungroup()

#Plot the total earnings by client tier and year
ggplot(grouped_df, aes(x = reorder(clienttier, -total_earnings), y = total_earnings, fill = factor(Year))) +
  geom_bar(stat = "identity", color = "black", width = 0.7, position = "dodge") +  # Dodge bars by year
  scale_fill_brewer(palette = "Set2") +  # Use different color palette for different years
  labs(title = "Total Earnings by Client Tier and Year",
       x = "Client Tier",
       y = "Total Earnings (£)",
       fill = "Year",
       caption = "Source: Royalty Data") +
  theme_minimal(base_size = 15) +  # Apply a clean, minimal theme with larger font size
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # Rotate and bold x-axis labels
        axis.text.y = element_text(face = "bold"),  # Bold y-axis labels
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18),  # Center and bold the title
        plot.subtitle = element_text(hjust = 0.5, size = 14),  # Center the subtitle
        legend.position = "right")  # Place the legend on the right for year differentiation


#Top earning works 
#Group by songcode (work) and sum total royalties generated
top_works <- royalties_df %>%
  group_by(songcode) %>%
  summarise(total_royalties = sum(netpublishersharepayment, na.rm = TRUE),
            latest_statement = max(finalstatementdate, na.rm = TRUE)) %>%
  ungroup()

#Join with works_df to get the title of the work
top_works <- left_join(top_works, works_df, by = "songcode")

#Sort by total royalties and get the top 10 works
top_10_works <- top_works %>%
  arrange(desc(total_royalties)) %>%
  head(10)

#Highlight older works
# For long-term revenue, we can look at older works with recent final statements
long_term_revenue_works <- top_10_works %>%
  filter(latest_statement >= as.Date("2022-01-01")) 

# Print the results
print("Top 10 Works by Total Royalties:")
print(top_10_works)

print("Works with Long-Term Revenue Streams (Older Works with Recent Royalties):")
print(long_term_revenue_works)

#Create a new column indicating whether the work has long-term revenue (recent royalty payments)
top_10_works <- top_10_works %>%
  mutate(long_term_revenue = ifelse(latest_statement >= as.Date("2022-01-01"), "Recent Revenue", "Older Revenue"))

#Plot the top 10 works by total royalties with long-term revenue highlighted
ggplot(top_10_works, aes(x = reorder(worktitle, -total_royalties), y = total_royalties, fill = long_term_revenue)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +  # Bar chart with black borders
  scale_fill_manual(values = c("Recent Revenue" = "#00BFC4", "Older Revenue" = "#F8766D")) +  # Custom colors
  labs(title = "Top 10 Earning Works by Total Royalties",
       subtitle = "Highlighting Recent vs. Older Revenue",
       x = "Work Title",
       y = "Total Royalties (£)",
       fill = "Revenue Type",
       caption = "Source: Royalty Data") +
  theme_minimal(base_size = 15) +  # Minimalist theme with larger font size
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # Rotate and bold x-axis labels
        axis.text.y = element_text(face = "bold"),  # Bold y-axis labels
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18),  # Center and bold the title
        plot.subtitle = element_text(hjust = 0.5, size = 14),  # Center the subtitle
        legend.position = "right")  #legend on the right

#top earners currently which were released over 1 year old 
#Calculate one year ago from today's date
one_year_ago <- Sys.Date() - years(1)

#Filter songs that were released over a year ago but are still generating revenue
# Assume that 'works_df' has a release date or use the earliest royalty date as proxy for release date
songs_over_a_year_old <- royalties_df %>%
  group_by(songcode) %>%
  summarise(total_royalties = sum(netpublishersharepayment, na.rm = TRUE),
            first_statement = min(finalstatementdate, na.rm = TRUE),
            latest_statement = max(finalstatementdate, na.rm = TRUE)) %>%
  filter(first_statement < one_year_ago,  # Released over a year ago
         latest_statement >= Sys.Date() - months(6))  # Still earning in the last 6 months

#Join with works_df to get the work titles
songs_over_a_year_old <- left_join(songs_over_a_year_old, works_df, by = "songcode")

#Find the top 10 earning works released over a year ago but still earning royalties
top_10_old_but_active_works <- songs_over_a_year_old %>%
  arrange(desc(total_royalties)) %>%
  head(10)

# Print the results
print("Top 10 Earning Songs Released Over a Year Ago and Still Generating Revenue:")
print(top_10_old_but_active_works)

ggplot(top_10_old_but_active_works, aes(x = reorder(worktitle, -total_royalties), y = total_royalties, fill = worktitle)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +  # Bar chart with black borders
  scale_fill_brewer(palette = "Set3") +  #color palette
  labs(title = "Top 10 Earning Songs Released Over a Year Ago and Still Generating Revenue",
       x = "Work Title",
       y = "Total Royalties (£)",
       fill = "Work Title",
       caption = "Source: Royalty Data") +
  theme_minimal(base_size = 15) +  # Clean, minimalist theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # Rotate and bold x-axis labels
        axis.text.y = element_text(face = "bold"),  # Bold y-axis labels
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18),  # Bold, centered title
        legend.position = "none")

# Royalties by source 
# Use shipmentcode as the key for joining royalties_df and source_df
royalties_by_source <- left_join(royalties_df, source_df, by = "shipmentcode")

#Group by royaltysource and calculate total royalties for each source
royalties_by_source_summary <- royalties_by_source %>%
  group_by(royaltysource) %>%
  summarise(total_royalties = sum(netpublishersharepayment, na.rm = TRUE)) %>%
  ungroup()

#Sort by total royalties in descending order
royalties_by_source_summary <- royalties_by_source_summary %>%
  arrange(desc(total_royalties))

# Print the results
print("Royalties by Source:")
print(royalties_by_source_summary)

# Visualize the royalties by source
ggplot(royalties_by_source_summary, aes(x = reorder(royaltysource, -total_royalties), y = total_royalties, fill = royaltysource)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +  # Bar chart with black borders
  scale_fill_brewer(palette = "Set2") +  # Use a more vibrant color palette
  labs(title = "Royalties by Source",
       x = "Source",
       y = "Total Royalties (£)",
       fill = "Source",
       caption = "Source: Royalty Data") +
  theme_minimal(base_size = 15) +  # Apply a clean, minimal theme with larger font size
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # Rotate and bold x-axis labels
        axis.text.y = element_text(face = "bold"),  # Bold y-axis labels
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18),  # Center and bold the title
        legend.position = "none")



#Royalties by Geography 

#Merge the royalties data with the territory information using territorynumber
royalties_by_territory <- left_join(royalties_df, territory_df, by = "territorynumber")

#Group by the territory name and calculate total royalties for each geography
royalties_by_territory_summary <- royalties_by_territory %>%
  group_by(territoryname) %>%  # Assuming territoryname contains the name of the country or region
  summarise(total_royalties = sum(netpublishersharepayment, na.rm = TRUE)) %>%
  ungroup()

#Sort by total royalties in descending order
royalties_by_territory_summary <- royalties_by_territory_summary %>%
  arrange(desc(total_royalties))

# Print  results
print("Royalties by Geography:")
print(royalties_by_territory_summary)
# Visualize the royalties by geography
install.packages("RColorBrewer")
library(RColorBrewer)
# Identify the top 3 geographies by total royalties
top_3_territories <- royalties_by_territory_summary %>%
  top_n(3, wt = total_royalties)

# Visualize royalties by geography
ggplot(royalties_by_territory_summary, aes(x = reorder(territoryname, -total_royalties), y = total_royalties, fill = territoryname)) +
  
  # Bar chart with identity and added borders
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  
  #value labels only for the top 3 geographies
  geom_text(data = top_3_territories, aes(label = scales::comma(total_royalties)), 
            vjust = -0.5, size = 4, fontface = "bold") +
  
  #color
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Set3"))(nrow(royalties_by_territory_summary))) +
  
  #Commas for large numbers
  scale_y_continuous(labels = scales::comma) +
  
  # Add chart labels and title
  labs(title = "Royalties by Geography",
       subtitle = "Total Royalties Generated from Different Territories",
       x = "Territory",
       y = "Total Royalties (£)",
       fill = "Territory",
       caption = "Source: Royalty Data") +
  
  # larger font size and minimal theme
  theme_minimal(base_size = 16) +
  
  # Customize x-axis labels for readability and bold them
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        legend.position = "none",
        plot.background = element_rect(fill = "white", color = NA)) +  # White background
  
  # Adjust the limits - prevent text cutoff and increase the plot margins
  coord_cartesian(clip = "off") +
  
  # Add extra margin
  theme(plot.margin = unit(c(1, 1, 2, 1), "cm"))


#Percentage of Each Client Tier
#Count the number of clients in each client tier
tier_distribution <- accounts_df %>%
  group_by(clienttier) %>%
  summarise(count = n()) %>%
  ungroup()

#Calculate  percentage for each client tier
tier_distribution <- tier_distribution %>%
  mutate(percentage = (count / sum(count)) * 100)

# Print tier distribution with percentages
print("Client Tier Distribution (Percentage):")
print(tier_distribution)

# Pie chart to visualize percentage distribution of client tiers
ggplot(tier_distribution, aes(x = "", y = percentage, fill = clienttier)) +
  geom_bar(stat = "identity", width = 1) +  # Bar chart in polar coordinates for pie chart effect
  coord_polar(theta = "y") +  # Turn the bar chart into a pie chart
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 5, fontface = "bold") +
  scale_fill_brewer(palette = "Set2") +  # Vibrant color palette
  labs(title = "Percentage Distribution of Client Tiers",
       fill = "Client Tier",
       caption = "Source: Client Data") +
  theme_void() +  # Remove background and grid lines for a clean pie chart look
  theme(legend.position = "right",  # legend on the right for pie chart
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18))

#percentage of royalties from UK and US
#Filter royalties from UK and US
uk_us_royalties <- royalties_by_territory_summary %>%
  filter(territoryname %in% c("UNITED KINGDOM", "UNITED STATES")) %>%
  summarise(total_royalties_uk_us = sum(total_royalties))

#Calculate total royalties from all regions
total_royalties <- sum(royalties_by_territory_summary$total_royalties)

#Calculate the percentage of royalties from UK and US
percentage_uk_us <- (uk_us_royalties$total_royalties_uk_us / total_royalties) * 100

#result
cat("Percentage of royalties from the UK and US:", round(percentage_uk_us, 2), "%\n")

#% of contribution top client earns 
#Aggregate total royalties by client
royalties_by_client <- royalties_df %>%
  group_by(accountcode) %>%
  summarise(total_royalties = sum(netpublishersharepayment, na.rm = TRUE))

#Find the highest-earning client
top_client <- royalties_by_client %>%
  filter(total_royalties == max(total_royalties)) %>%
  pull(accountcode)

#Calculate the total royalties for the top-earning client
top_client_royalties <- royalties_by_client %>%
  filter(accountcode == top_client) %>%
  pull(total_royalties)

#Calculate the total royalties across all clients
total_royalties <- sum(royalties_by_client$total_royalties, na.rm = TRUE)

#Calculate the percentage contribution of the top client
percentage_top_client <- (top_client_royalties / total_royalties) * 100

#result
cat("Top earning client (", top_client, ") contributes: ", round(percentage_top_client, 2), "% of total royalties\n", sep = "")
