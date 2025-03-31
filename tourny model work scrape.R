library(httr)
library(rvest)
library(dplyr)
library(readr)

# Step 1: Read the CSV file
df <- read_csv("analyzeAllWithInj_2025.csv", show_col_types = FALSE)

# Step 2: Initialize a vector to store SOS values
sos_values <- vector("character", nrow(df))

# Step 3: Loop through each team name, construct the URL, and scrape the SOS
base_url <- "https://www.sports-reference.com/cbb/schools/"

# Debugging: Print the first team's URL and HTML content
team_name <- df$teams[1]
team_name_clean <- tolower(gsub("\\s+", "-", gsub("\\d+", "", team_name)))  # Clean the team name
team_url <- paste0(base_url, team_name_clean, "/men/2025.html")

# Print the constructed URL for debugging
print(paste("Constructed URL:", team_url))

# Fetch and print the HTML content
tryCatch({
  response <- GET(team_url)
  if (status_code(response) == 200) {
    page <- content(response, "text")
    print(page)  # Print the entire HTML content for debugging
  } else {
    print(paste("Failed to fetch URL:", team_url, "| Status Code:", status_code(response)))
  }
}, error = function(e) {
  print(paste("Error fetching URL:", team_url))
})

# Step 4: Scrape the SOS for all teams
for (i in 1:nrow(df)) {
  # Extract the team name and clean it for the URL
  team_name <- df$teams[i]
  team_name_clean <- tolower(gsub("\\s+", "-", gsub("\\d+", "", team_name)))  # Remove numbers and replace spaces with hyphens
  
  # Construct the URL
  team_url <- paste0(base_url, team_name_clean, "/men/2025.html")
  
  # Print the constructed URL for debugging
  print(paste("Processing:", team_name, "| URL:", team_url))
  
  # Scrape the SOS
  tryCatch({
    response <- GET(team_url)
    if (status_code(response) == 200) {
      page <- content(response, "text")
      sos <- read_html(page) %>%
        html_node("td[data-stat='sos']") %>%
        html_text() %>%
        trimws()
      
      # Store the SOS value
      sos_values[i] <- sos
    } else {
      sos_values[i] <- NA  # If the page fails to load, assign NA
      print(paste("Failed to fetch URL:", team_url, "| Status Code:", status_code(response)))
    }
  }, error = function(e) {
    sos_values[i] <- NA  # If scraping fails, assign NA
    print(paste("Error scraping SOS for:", team_name))
  })
}

# Step 5: Add the SOS column to the DataFrame
df <- df %>%
  mutate(SOS = sos_values)

# Step 6: Save the updated DataFrame to a new CSV file
write_csv(df, "analyzeAllWithInj_2025_with_SOS.csv")

# Print the updated DataFrame
print(df)