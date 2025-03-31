# Function to safely read CSV files
safe_read_csv <- function(file) {
  if (file.exists(file)) {
    return(read.csv(file))
  } else {
    warning(paste("File not found:", file))
    return(NULL)
  }
}

# Initialize vectors to store data
teams_2024 <- c()
seed_2024 <- c()
ws.40_2024 <- c()
aBPM_2024 <- c()
ts_2024 <- c()
tov_2024 <- c()
stlBlk_2024 <- c()
or_2024 <- c()
per_2024 <- c()
depth_2024 <- c()

teams_2025 <- c()
seed_2025 <- c()
ws.40_2025 <- c()
aBPM_2025 <- c()
ts_2025 <- c()
tov_2025 <- c()
stlBlk_2025 <- c()
or_2025 <- c()
per_2025 <- c()
depth_2025 <- c()

# Loop through all games
for (i in 1:nrow(games_all)) {
  year <- games_all$Year[i]
  betterTeam <- tolower(games_all$Team...Better.Seed[i])
  worseTeam <- tolower(games_all$Team...Worse.Seed[i])
  
  # Handle team name variations
  betterTeam <- gsub(" ", "-", betterTeam)
  worseTeam <- gsub(" ", "-", worseTeam)
  betterTeam <- gsub(".", "", betterTeam, fixed = TRUE)
  worseTeam <- gsub(".", "", worseTeam, fixed = TRUE)
  betterTeam <- gsub("'", "", betterTeam)
  worseTeam <- gsub("'", "", worseTeam)
  betterTeam <- gsub("&", "", betterTeam)
  worseTeam <- gsub("&", "", worseTeam)
  betterTeam <- gsub("(", "", betterTeam, fixed = TRUE)
  worseTeam <- gsub("(", "", worseTeam, fixed = TRUE)
  betterTeam <- gsub(")", "", betterTeam, fixed = TRUE)
  worseTeam <- gsub(")", "", worseTeam, fixed = TRUE)
  
  # Construct file paths
  csvLinkBT <- paste0(betterTeam, " ", year, ".csv")
  csvLinkWT <- paste0(worseTeam, " ", year, ".csv")
  
  # Read CSV files with error handling
  betterTeamData <- safe_read_csv(csvLinkBT)
  worseTeamData <- safe_read_csv(csvLinkWT)
  
  # Skip iteration if data is not loaded
  if (is.null(betterTeamData) || is.null(worseTeamData)) {
    message(paste("Skipping iteration due to missing data for game", i))
    next
  }
  
  # Standardize column names
  colnames(betterTeamData) <- gsub("TS%", "TS.", colnames(betterTeamData))
  colnames(worseTeamData) <- gsub("TS%", "TS.", colnames(worseTeamData))
  
  # Check for missing columns and assign NA if they are missing
  if (!"eFG." %in% colnames(betterTeamData)) {
    betterTeamData$eFG. <- NA
  }
  if (!"eFG." %in% colnames(worseTeamData)) {
    worseTeamData$eFG. <- NA
  }
  
  # Process data
  min_cutoff <- 240
  btMP <- betterTeamData$MP[betterTeamData$MP > min_cutoff]
  wtMP <- worseTeamData$MP[worseTeamData$MP > min_cutoff]
  btGP <- betterTeamData$G[betterTeamData$MP > min_cutoff]
  wtGP <- worseTeamData$G[worseTeamData$MP > min_cutoff]
  btMPpG <- btMP / btGP
  wtMPpG <- wtMP / wtGP
  
  # Calculate BPM (Box Plus-Minus)
  btdBPM <- sum(betterTeamData$BPM[betterTeamData$MP > min_cutoff] * btMPpG) / sum(btMPpG)
  wtdBPM <- sum(worseTeamData$BPM[worseTeamData$MP > min_cutoff] * wtMPpG) / sum(wtMPpG)
  
  # Calculate WS/40 (Win Shares per 40 Minutes)
  btdWS.40 <- sum(betterTeamData$WS.40[betterTeamData$MP > min_cutoff] * btMPpG) / sum(btMPpG) * 40
  wtdWS.40 <- sum(worseTeamData$WS.40[worseTeamData$MP > min_cutoff] * wtMPpG) / sum(wtMPpG) * 40
  
  # Calculate PER (Player Efficiency Rating)
  btdPER <- sum(betterTeamData$PER[betterTeamData$MP > min_cutoff] * btMPpG) / sum(btMPpG)
  wtdPER <- sum(worseTeamData$PER[worseTeamData$MP > min_cutoff] * wtMPpG) / sum(wtMPpG)
  
  # Calculate TS (True Shooting Percentage)
  if (!"TS." %in% colnames(betterTeamData)) {
    warning("TS. column missing in betterTeamData. Skipping TS calculation.")
    btTS <- NA
  } else {
    btTS <- sum(betterTeamData$TS.[betterTeamData$MP > min_cutoff] * betterTeamData$PProd[betterTeamData$MP > min_cutoff]) / sum(betterTeamData$PProd[betterTeamData$MP > min_cutoff])
  }
  
  if (!"TS." %in% colnames(worseTeamData)) {
    warning("TS. column missing in worseTeamData. Skipping TS calculation.")
    wtTS <- NA
  } else {
    wtTS <- sum(worseTeamData$TS.[worseTeamData$MP > min_cutoff] * worseTeamData$PProd[worseTeamData$MP > min_cutoff]) / sum(worseTeamData$PProd[worseTeamData$MP > min_cutoff])
  }
  
  # Calculate TOV (Turnover Percentage)
  if (!"TOV." %in% colnames(betterTeamData)) {
    warning("TOV. column missing in betterTeamData. Skipping TOV calculation.")
    btTOV <- NA
  } else {
    btTotTOV <- sum((betterTeamData$PProd[betterTeamData$MP > min_cutoff] / (2 * (betterTeamData$TS.[betterTeamData$MP > min_cutoff]))) * (betterTeamData$TOV.[betterTeamData$MP > min_cutoff] / (100 - betterTeamData$TOV.[betterTeamData$MP > min_cutoff])))
    btTOV <- 100 * btTotTOV / (sum(betterTeamData$PProd[betterTeamData$MP > min_cutoff]) / (2 * btTS) + btTotTOV)
  }
  
  if (!"TOV." %in% colnames(worseTeamData)) {
    warning("TOV. column missing in worseTeamData. Skipping TOV calculation.")
    wtTOV <- NA
  } else {
    wtTotTOV <- sum((worseTeamData$PProd[worseTeamData$MP > min_cutoff] / (2 * (worseTeamData$TS.[worseTeamData$MP > min_cutoff]))) * (worseTeamData$TOV.[worseTeamData$MP > min_cutoff] / (100 - worseTeamData$TOV.[worseTeamData$MP > min_cutoff])))
    wtTOV <- 100 * wtTotTOV / (sum(worseTeamData$PProd[worseTeamData$MP > min_cutoff]) / (2 * wtTS) + wtTotTOV)
  }
  
  # Calculate StlBlk (Steals + Blocks)
  if (!"STL." %in% colnames(betterTeamData) || !"BLK." %in% colnames(betterTeamData)) {
    warning("STL. or BLK. column missing in betterTeamData. Skipping StlBlk calculation.")
    btSB <- NA
  } else {
    btSB <- sum((betterTeamData$STL.[betterTeamData$MP > min_cutoff] * 3 + betterTeamData$BLK.[betterTeamData$MP > min_cutoff] * 2) * betterTeamData$MP[betterTeamData$MP > min_cutoff]) / sum(betterTeamData$MP[betterTeamData$MP > min_cutoff])
  }
  
  if (!"STL." %in% colnames(worseTeamData) || !"BLK." %in% colnames(worseTeamData)) {
    warning("STL. or BLK. column missing in worseTeamData. Skipping StlBlk calculation.")
    wtSB <- NA
  } else {
    wtSB <- sum((worseTeamData$STL.[worseTeamData$MP > min_cutoff] * 3 + worseTeamData$BLK.[worseTeamData$MP > min_cutoff] * 2) * worseTeamData$MP[worseTeamData$MP > min_cutoff]) / sum(worseTeamData$MP[worseTeamData$MP > min_cutoff])
  }
  
  # Calculate OR (Offensive Rebound Percentage)
  if (!"ORB." %in% colnames(betterTeamData)) {
    warning("ORB. column missing in betterTeamData. Skipping OR calculation.")
    btOR <- NA
  } else {
    btOR <- sum((betterTeamData$ORB.[betterTeamData$MP > min_cutoff]) * btMPpG) / sum(btMPpG)
  }
  
  if (!"ORB." %in% colnames(worseTeamData)) {
    warning("ORB. column missing in worseTeamData. Skipping OR calculation.")
    wtOR <- NA
  } else {
    wtOR <- sum((worseTeamData$ORB.[worseTeamData$MP > min_cutoff]) * wtMPpG) / sum(wtMPpG)
  }
  
  # Calculate depth (Number of players with MP > min_cutoff)
  btdepth <- sum((betterTeamData$MP > min_cutoff) * 1)
  wtdepth <- sum((worseTeamData$MP > min_cutoff) * 1)
  
  # Append data to vectors based on year
  if (year == 2024) {
    teams_2024 <- c(teams_2024, paste0(betterTeam, " ", year), paste0(worseTeam, " ", year))
    seed_2024 <- c(seed_2024, as.numeric(games_all$Better.Seed[i]), as.numeric(games_all$Worse.Seed[i]))
    ws.40_2024 <- c(ws.40_2024, btdWS.40, wtdWS.40)
    aBPM_2024 <- c(aBPM_2024, btdBPM, wtdBPM)
    ts_2024 <- c(ts_2024, btTS * 10, wtTS * 10)
    tov_2024 <- c(tov_2024, -.5 * btTOV, -.5 * wtTOV)
    stlBlk_2024 <- c(stlBlk_2024, btSB, wtSB)
    or_2024 <- c(or_2024, .1 * btOR, .1 * wtOR)
    per_2024 <- c(per_2024, .1 * btdPER, .1 * wtdPER)
    depth_2024 <- c(depth_2024, btdepth, wtdepth)
  } else if (year == 2025) {
    teams_2025 <- c(teams_2025, paste0(betterTeam, " ", year), paste0(worseTeam, " ", year))
    seed_2025 <- c(seed_2025, as.numeric(games_all$Better.Seed[i]), as.numeric(games_all$Worse.Seed[i]))
    ws.40_2025 <- c(ws.40_2025, btdWS.40, wtdWS.40)
    aBPM_2025 <- c(aBPM_2025, btdBPM, wtdBPM)
    ts_2025 <- c(ts_2025, btTS * 10, wtTS * 10)
    tov_2025 <- c(tov_2025, -.5 * btTOV, -.5 * wtTOV)
    stlBlk_2025 <- c(stlBlk_2025, btSB, wtSB)
    or_2025 <- c(or_2025, .1 * btOR, .1 * wtOR)
    per_2025 <- c(per_2025, .1 * btdPER, .1 * wtdPER)
    depth_2025 <- c(depth_2025, btdepth, wtdepth)
  }
}

# Create analyze.2024 data frame
analyze.2024 <- data.frame(
  teams = teams_2024,
  seed = seed_2024,
  ws.40 = ws.40_2024,
  aBPM = aBPM_2024,
  TS = ts_2024,
  TOV = tov_2024,
  StlBlk = stlBlk_2024,
  OR = or_2024,
  PER = per_2024,
  depth = depth_2024
)

# Create analyze.2025 data frame
analyze.2025 <- data.frame(
  teams = teams_2025,
  seed = seed_2025,
  ws.40 = ws.40_2025,
  aBPM = aBPM_2025,
  TS = ts_2025,
  TOV = tov_2025,
  StlBlk = stlBlk_2025,
  OR = or_2025,
  PER = per_2025,
  depth = depth_2025
)

# Print data frames for debugging
print("analyze.2024 created successfully:")
print(head(analyze.2024))

print("analyze.2025 created successfully:")
print(head(analyze.2025))

# Write analyze.2025 to CSV
output_file <- "C:/Users/b3nja/Onedrive/Desktop/NCAA Tournament/analyzeAllWithInj_2025.csv"
if (!dir.exists(dirname(output_file))) {
  dir.create(dirname(output_file), recursive = TRUE)
}
write.csv(analyze.2025, file = output_file)