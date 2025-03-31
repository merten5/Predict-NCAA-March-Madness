setwd("C:/Users/b3nja/Onedrive/Desktop/NCAA Tournament")
library('dplyr')
library('datasets')
library('graphics')
library('grDevices')
library('methods')
library('stats')
library('utils')
library('rpart')
library('ROCR')
library('polycor')
library('corrgram')
library('party')
library('class')
library('rvest')
library('ISLR')
library('caret')
library('randomForest')


games_2024 <- read.csv("NCAA Tournament Games 2025 and before.csv",stringsAsFactors = FALSE, strip.white = TRUE)
#for (i in 1:length(players_2024[[1]])) {
new_df_names <- c(names(games_2024), "All.BPM", "WS.40", "PER", "Top.Off.Top.Def.BPM", "Top.Off.Xtreme.Def.BPM")
games_all <- games_2024

finalFourTeams <- c("conneticut 2024", "alabama 2024", "north-carolina-state 2024", "purdue 2024", 
                    "conneticut 2023", "florida-atlantic 2023", "miami-fl 2023", "san-diego-state 2023", 
                    "duke 2022", "kansas 2022", "villanova 2022", "north-carolina 2022", 
                    "ucla 2021", "houston 2021", "gonzaga 2021", "baylor 2021", 
                    "auburn 2019", "michigan-state 2019", "texas-tech 2019", "virginia 2019", 
                    "loyola-il 2018", "michigan 2018", "villanova 2018", "kansas 2018", 
                    "south-carolina 2017", "oregon 2017", "gonzaga 2017", "north-carolina 2017", 
                    "north-carolina 2016", "oklahoma 2016", "syracuse 2016", "villanova 2016", 
                    "duke 2015", "kentucky 2015", "wisconsin 2015", "michigan-state 2015", 
                    "florida 2014", "kentucky 2014", "wisconsin 2014", "conneticut 2014")
finalFourTeam <- c()
f4 <- 0

teams <- c()
gamesLookedAt <- c(1:36)
allGames <- length(games_2024[1])
or <- c()
aBPM <- c()
ws.40 <- c()
per <- c()
totdBPM <- c()
toxdBPM <- c()
KP <- c()
seed <- c()
roundNum <- c()
dayNum <- c()
ts <- c()
tov <- c()
stlBlk <- c()
depth <- c()
seed <- c()

teams_d <- c()
score_d <- c()
favWin <- c()
score_r <- c()
roundPoints <- c()
levRoundPoints <- c()
aBPM_d <- c()
ws.40_d <- c()
per_d <- c()
totdBPM_d <- c()
toxdBPM_d <- c()
ts_d <- c()
tov_d <- c()
stlBlk_d <- c()
or_d <- c()
depth_d <- c()
seed_d <- c()

gamesPast <- c(37:599)
games2024 <- c(1:36)
#for (i in 1:length(games_2024[1])) {
#for (i in gamesLookedAt) {
for (i in gamesPast) {
  # html_base_beggining <- "https://www.sports-reference.com/cbb/schools/"
  # html_base_ending <- ".html"
  csv_base_ending <- ".csv"
  year <- games_2024$Year[i]
  betterTeam <- tolower(games_2024$Team...Better.Seed[i])
  worseTeam <- tolower(games_2024$Team...Worse.Seed[i])
  if (betterTeam=="unc wilmington"){betterTeam<-"north-carolina-wilmington"}
  if (worseTeam=="unc wilmington"){worseTeam<-"north-carolina-wilmington"}
  if (betterTeam=="usc"){betterTeam<-"southern-california"}
  if (worseTeam=="usc"){worseTeam<-"southern-california"}
  if (betterTeam=="smu"){betterTeam<-"southern-methodist"}
  if (worseTeam=="smu"){worseTeam<-"southern-methodist"}
  if (betterTeam=="vcu"){betterTeam<-"virginia-commonwealth"}
  if (worseTeam=="vcu"){worseTeam<-"virginia-commonwealth"}
  if (betterTeam=="uc davis"){betterTeam<-"california-davis"}
  if (worseTeam=="uc davis"){worseTeam<-"california-davis"}
  if (betterTeam=="uc-davis"){betterTeam<-"california-davis"}
  if (worseTeam=="uc-davis"){worseTeam<-"california-davis"}
  if (betterTeam=="miami (fl)"){betterTeam<-"miami-fl"}
  if (worseTeam=="miami (fl)"){worseTeam<-"miami-fl"}
  if (betterTeam=="unc asheville"){betterTeam<-"north-carolina-asheville"}
  if (worseTeam=="unc asheville"){worseTeam<-"north-carolina-asheville"}
  if (betterTeam=="little rock"){betterTeam<-"arkansas-little-rock"}
  if (worseTeam=="little rock"){worseTeam<-"arkansas-little-rock"}
  if (betterTeam=="lsu"){betterTeam<-"louisiana-state"}
  if (worseTeam=="lsu"){worseTeam<-"louisiana-state"}
  if (betterTeam=="uc irvine"){betterTeam<-"california-irvine"}
  if (worseTeam=="uc irvine"){worseTeam<-"california-irvine"}
  betterTeam<-gsub(" ", "-", betterTeam)
  worseTeam<-gsub(" ", "-", worseTeam)
  if (betterTeam=="albany"){betterTeam<-"albany-ny"}
  if (worseTeam=="albany"){worseTeam<-"albany-ny"}
  if (betterTeam=="st.-john's"){betterTeam<-"st-johns-ny"}
  if (worseTeam=="st.-john's"){worseTeam<-"st-johns-ny"}
  betterTeam<-gsub(".", "", betterTeam, fixed=TRUE)
  worseTeam<-gsub(".", "", worseTeam, fixed=TRUE)
  betterTeam<-gsub("'", "", betterTeam)
  worseTeam<-gsub("'", "", worseTeam)
  betterTeam<-gsub("&", "", betterTeam)
  worseTeam<-gsub("&", "", worseTeam)
  if (betterTeam=="uab"){betterTeam<-"alabama-birmingham"}
  if (worseTeam=="uab"){worseTeam<-"alabama-birmingham"}
  if (betterTeam=="byu"){betterTeam<-"brigham-young"}
  if (worseTeam=="byu"){worseTeam<-"brigham-young"}
  if (betterTeam=="cal-poly-slo"){betterTeam<-"cal-poly"}
  if (worseTeam=="cal-poly-slo"){worseTeam<-"cal-poly"}
  if (betterTeam=="unlv"){betterTeam<-"nevada-las-vegas"}
  if (worseTeam=="unlv"){worseTeam<-"nevada-las-vegas"}
  if (betterTeam=="liu-brooklyn"){betterTeam<-"long-island-university"}
  if (worseTeam=="liu-brooklyn"){worseTeam<-"long-island-university"}
  if (betterTeam== "southern-miss"){betterTeam<-"southern-mississippi"}
  if (worseTeam== "southern-miss"){worseTeam<-"southern-mississippi"}
  betterTeam<-gsub("(", "", betterTeam, fixed=TRUE)
  worseTeam<-gsub("(", "", worseTeam, fixed=TRUE)
  betterTeam<-gsub(")", "", betterTeam, fixed=TRUE)
  worseTeam<-gsub(")", "", worseTeam, fixed=TRUE)
  if (betterTeam=="detroit"){betterTeam<-"detroit-mercy"}
  if (worseTeam=="detroit"){worseTeam<-"detroit-mercy"}
  if (betterTeam=="utsa"){betterTeam<-"texas-san-antonio"}
  if (worseTeam=="utsa"){worseTeam<-"texas-san-antonio"}
  if (betterTeam=="uc-santa-barbara"){betterTeam<-"california-santa-barbara"}
  if (worseTeam=="uc-santa-barbara"){worseTeam<-"california-santa-barbara"}
  if (betterTeam=="utep"){betterTeam<-"texas-el-paso"}
  if (worseTeam=="utep"){worseTeam<-"texas-el-paso"}
  if (betterTeam=="portland-st"){betterTeam<-"portland-state"}
  if (worseTeam=="portland-st"){worseTeam<-"portland-state"}
  if (betterTeam=="umbc"){betterTeam<-"maryland-baltimore-county"}
  if (worseTeam=="umbc"){worseTeam<-"maryland-baltimore-county"}
  if (betterTeam=="ut-arlington"){betterTeam<-"texas-arlington"}
  if (worseTeam=="ut-arlington"){worseTeam<-"texas-arlington"}
  if (betterTeam=="texas-am-corpus-chris"){betterTeam<-"texas-am-corpus-christi"}
  if (worseTeam=="texas-am-corpus-chris"){worseTeam<-"texas-am-corpus-christi"}
  if (betterTeam=="central-connecticut"){betterTeam<-"central-connecticut-state"}
  if (worseTeam=="central-connecticut"){worseTeam<-"central-connecticut-state"}
  if (betterTeam=="penn"){betterTeam<-"pennsylvania"}
  if (worseTeam=="penn"){worseTeam<-"pennsylvania"}
  if (betterTeam=="ucf"){betterTeam<-"central-florida"}
  if (worseTeam=="ucf"){worseTeam<-"central-florida"}
  if (betterTeam=="troy-state"){betterTeam<-"troy"}
  if (worseTeam=="troy-state"){worseTeam<-"troy"}
  if (betterTeam=="uc-irvine"){betterTeam<-"california-irvine"}
  if (worseTeam=="uc-irvine"){worseTeam<-"california-irvine"}
  if (betterTeam=="texas-so"){betterTeam<-"texas-southern"}
  if (worseTeam=="texas-so"){worseTeam<-"texas-southern"}
  if (betterTeam=="charleston"){betterTeam<-"college-of-charleston"}
  if (worseTeam=="charleston"){worseTeam<-"college-of-charleston"}
  if (betterTeam=="unc-wilmington"){betterTeam<-"north-carolina-wilmington"}
  if (worseTeam=="unc-wilmington"){worseTeam<-"north-carolina-wilmington"} 
  if (betterTeam=="unc-asheville"){betterTeam<-"north-carolina-asheville"}
  if (worseTeam=="unc-asheville"){worseTeam<-"north-carolina-asheville"}
  if (betterTeam=="little-rock"){betterTeam<-"arkansas-little-rock"}
  if (worseTeam=="little-rock"){worseTeam<-"arkansas-little-rock"}
  
  # htmlLinkBT <- paste0(html_base_beggining, betterTeam, "/", year, html_base_ending)
  # betterTeamWebPage <- html(htmlLinkBT)
  # htmlLinkWT <- paste0(html_base_beggining, worseTeam, "/", year, html_base_ending)
  # worseTeamWebPage <- html(htmlLinkWT)
  csvLinkBT <- paste0(betterTeam, " ", year, csv_base_ending)
  betterTeamData <- read.csv(csvLinkBT)
  csvLinkWT <- paste0(worseTeam, " ", year, csv_base_ending)
  worseTeamData <- read.csv(csvLinkWT)
  
  min_cutoff<-240
  
  btMP<-betterTeamData$MP[betterTeamData$MP>min_cutoff]
  wtMP<-worseTeamData$MP[worseTeamData$MP>min_cutoff]
  
  btGP<-betterTeamData$G[betterTeamData$MP>min_cutoff]
  wtGP<-worseTeamData$G[worseTeamData$MP>min_cutoff]
  
  btMPpG<-btMP/btGP
  wtMPpG<-wtMP/wtGP
  
  btdBPM <- sum(betterTeamData$BPM[betterTeamData$MP>min_cutoff]*btMPpG)/sum(btMPpG)
  wtdBPM <- sum(worseTeamData$BPM[worseTeamData$MP>min_cutoff]*wtMPpG)/sum(wtMPpG)
  btdWS.40 <- sum(betterTeamData$WS.40[betterTeamData$MP>min_cutoff]*btMPpG)/sum(btMPpG)*40
  wtdWS.40 <- sum(worseTeamData$WS.40[worseTeamData$MP>min_cutoff]*wtMPpG)/sum(wtMPpG)*40
  btdPER <- sum(betterTeamData$PER[betterTeamData$MP>min_cutoff]*btMPpG)/sum(btMPpG)
  wtdPER <- sum(worseTeamData$PER[worseTeamData$MP>min_cutoff]*wtMPpG)/sum(wtMPpG)
  btdtotdBPM <- max(betterTeamData$OBPM[betterTeamData$MP>min_cutoff])+max(betterTeamData$DBPM[betterTeamData$MP>min_cutoff])
  wtdtotdBPM <- max(worseTeamData$OBPM[worseTeamData$MP>min_cutoff])+max(worseTeamData$DBPM[worseTeamData$MP>min_cutoff])
  btdtoXdBPM <- max(betterTeamData$OBPM[betterTeamData$MP>min_cutoff])+(max(betterTeamData$DBPM[betterTeamData$MP>min_cutoff])+min(betterTeamData$DBPM[betterTeamData$MP>min_cutoff]))/2
  wtdtoXdBPM <- max(worseTeamData$OBPM[worseTeamData$MP>min_cutoff])+(max(worseTeamData$DBPM[worseTeamData$MP>min_cutoff])+min(worseTeamData$DBPM[worseTeamData$MP>min_cutoff]))/2
  btTS <- sum(betterTeamData$TS.[betterTeamData$MP>min_cutoff]*betterTeamData$PProd[betterTeamData$MP>min_cutoff])/sum(betterTeamData$PProd[betterTeamData$MP>min_cutoff])
  wtTS <- sum(worseTeamData$TS.[worseTeamData$MP>min_cutoff]*worseTeamData$PProd[worseTeamData$MP>min_cutoff])/sum(worseTeamData$PProd[worseTeamData$MP>min_cutoff])
  btTotTOV <- sum((betterTeamData$PProd[betterTeamData$MP>min_cutoff]/(2*(betterTeamData$TS.[betterTeamData$MP>min_cutoff]))) * (betterTeamData$TOV.[betterTeamData$MP>min_cutoff] / (100 - betterTeamData$TOV.[betterTeamData$MP>min_cutoff])))
  btTOV <- 100 * btTotTOV / (sum(betterTeamData$PProd[betterTeamData$MP>min_cutoff]) / (2 * btTS) + btTotTOV)
  wtTotTOV <- sum((worseTeamData$PProd[worseTeamData$MP>min_cutoff]/(2*(worseTeamData$TS.[worseTeamData$MP>min_cutoff]))) * (worseTeamData$TOV.[worseTeamData$MP>min_cutoff] / (100 - worseTeamData$TOV.[worseTeamData$MP>min_cutoff])))
  wtTOV <- 100 * wtTotTOV / (sum(worseTeamData$PProd[worseTeamData$MP>min_cutoff]) / (2 * wtTS) + wtTotTOV)
  btSB <- sum((betterTeamData$STL.[betterTeamData$MP>min_cutoff]*3+betterTeamData$BLK.[betterTeamData$MP>min_cutoff]*2)*betterTeamData$MP[betterTeamData$MP>min_cutoff])/sum(betterTeamData$MP[betterTeamData$MP>min_cutoff])
  wtSB <- sum((worseTeamData$STL.[worseTeamData$MP>min_cutoff]*3+worseTeamData$BLK.[worseTeamData$MP>min_cutoff]*2)*worseTeamData$MP[worseTeamData$MP>min_cutoff])/sum(worseTeamData$MP[worseTeamData$MP>min_cutoff])
  btOR <- sum((betterTeamData$ORB.[betterTeamData$MP>min_cutoff])*btMPpG)/sum(btMPpG)
  wtOR <- sum((worseTeamData$ORB.[worseTeamData$MP>min_cutoff])*wtMPpG)/sum(wtMPpG)
  btdepth <- sum((betterTeamData$MP>min_cutoff)*1)
  wtdepth <- sum((worseTeamData$MP>min_cutoff)*1)
  
  # KP <- c(KP, games_2024$Better.Seed.KP.Expected.Margin[i], games_2024$Worse.Seed.KP.Expected.Marg.[i])
  if (games_2024$Round[i] == "1) Round of 64") {
    teams <- c(teams, paste0(betterTeam, " ", year), paste0(worseTeam, " ", year)) 
    #teams <- c(teams, betterTeam, worseTeam)
    aBPM <- c(aBPM, btdBPM, wtdBPM)
    ws.40 <- c(ws.40, btdWS.40, wtdWS.40)
    per <- c(per, .1*btdPER, .1*wtdPER)
    totdBPM <- c(totdBPM, btdtotdBPM, wtdtotdBPM)
    toxdBPM <- c(toxdBPM, btdtoXdBPM, wtdtoXdBPM)
    ts <- c(ts, btTS*10, wtTS*10)
    tov <- c(tov, -.5*btTOV, -.5*wtTOV)
    stlBlk <- c(stlBlk, btSB, wtSB)
    or <- c(or, .1*btOR, .1*wtOR)
    depth <- c(depth, btdepth, wtdepth)
    seed <- c(seed, as.numeric(games_2024$Better.Seed[i]), as.numeric(games_2024$Worse.Seed[i]))
    if (paste0(betterTeam, " ", year) %in% finalFourTeams) {
        finalFourTeam <- c(finalFourTeam, 1, 0)
        f4 <- 1
      }
    if (paste0(worseTeam, " ", year) %in% finalFourTeams) {
        finalFourTeam <- c(finalFourTeam, 0, 1)
        f4 <- 1}
    if (f4 == 0) {finalFourTeam <- c(finalFourTeam, 0, 0)}
    if (f4 == 1) {f4 <- 0}
  }
  
  teams_d <- c(teams_d, paste0(betterTeam, " ", worseTeam, " ", year))
  score_d <- c(score_d, as.numeric(games_all$Better.Seed...Score[i])-as.numeric(games_all$Worse.Seed...Score[i]))
  score_d_1 <- as.numeric(games_all$Better.Seed...Score[i])-as.numeric(games_all$Worse.Seed...Score[i])
  if (score_d_1 > 0) {favWin <- c(favWin, 1)} else {favWin <- c(favWin, 0)}
  score_r <- c(score_r, as.numeric(games_all$Better.Seed...Score[i])/as.numeric(games_all$Worse.Seed...Score[i]))
  if (games_2024$Round[i] == "0) Play-in") {
    roundPoints <- c(roundPoints, 1)
    levRoundPoints <- c(levRoundPoints, 1)
  }
  if (games_2024$Round[i] == "1) Round of 64") {
    roundPoints <- c(roundPoints, 1)
    levRoundPoints <- c(levRoundPoints, 1)
  }
  if (games_2024$Round[i] == "2) Second Round") {
    roundPoints <- c(roundPoints, 2)
    levRoundPoints <- c(levRoundPoints, 3/2)
  }
  if (games_2024$Round[i] == "3) Sweet 16") {
    roundPoints <- c(roundPoints, 4)
    levRoundPoints <- c(levRoundPoints, 7/4)
  }
  if (games_2024$Round[i] == "4) Elite 8") {
    roundPoints <- c(roundPoints, 8)
    levRoundPoints <- c(levRoundPoints, 15/8)
  }
  if (games_2024$Round[i] == "National Semi-Final") {
    roundPoints <- c(roundPoints, 16)
    levRoundPoints <- c(levRoundPoints, 31/16)
  }
  if (games_2024$Round[i] == "National Championship") {
    roundPoints <- c(roundPoints, 32)
    levRoundPoints <- c(levRoundPoints, 63/32)
  }
  #teams <- c(teams, betterTeam, worseTeam)
  aBPM_d <- c(aBPM_d, btdBPM-wtdBPM)
  ws.40_d <- c(ws.40_d, btdWS.40-wtdWS.40)
  per_d <- c(per_d, .1*(btdPER-wtdPER))
  totdBPM_d <- c(totdBPM_d, btdtotdBPM-wtdtotdBPM)
  toxdBPM_d <- c(toxdBPM_d, btdtoXdBPM-wtdtoXdBPM)
  ts_d <- c(ts_d, (btTS-wtTS*10))
  tov_d <- c(tov_d, -.5*(btTOV-wtTOV))
  stlBlk_d <- c(stlBlk_d, btSB-wtSB)
  or_d <- c(or_d, .1*(btOR-wtOR))
  depth_d <- c(depth_d, btdepth-wtdepth)
  seed_d <- c(seed_d, -1*as.numeric(games_2024$Better.Seed[i])+as.numeric(games_2024$Worse.Seed[i]))
}

# scoreKPandBPM <- KP*-1.63191+aBPM*6.52362
# powerScoreSig <- KP*-1.63191+aBPM*6.52362+ws.40*1.98870+per*-3.41488+tov*1.64012+ts*1.67948+or*2.24970
# powerScore <- KP*-1.63191+aBPM*6.52362+ws.40*1.98870+per*-3.41488+tov*1.64012+ts*1.67948+or*2.24970+totdBPM*-0.036833+
# toxdBPM*0.074073+stlBlk*0.054815+seed*0.024429+depth*0.004792
# powerRank <- rank(-powerScore)


analyze.past_d <- data.frame(teams_d)
analyze.past_d[["ScoreDiff"]]<-score_d
analyze.past_d[["RoundPoints"]]<-roundPoints
analyze.past_d[["LevRoundPts"]]<-levRoundPoints
analyze.past_d[["favWin"]]<- favWin
analyze.past_d[["score_dxLRPts"]]<-score_d*levRoundPoints
analyze.past_d[["score_dxRPts"]]<-score_d*roundPoints
analyze.past_d[["seed_d"]]<-seed_d
analyze.past_d[["ws.40_d"]]<-ws.40_d
analyze.past_d[["aBPM_d"]]<-aBPM_d
analyze.past_d[["TS_d"]]<- ts_d
analyze.past_d[["TOV_d"]]<- tov_d
analyze.past_d[["StlBlk_d"]]<- stlBlk_d
analyze.past_d[["OR_d"]]<- or_d
analyze.past_d[["PER_d"]]<-per_d
analyze.past_d[["depth_d"]]<-depth_d

write.csv(analyze.past_d, file = "analyzePast_DmodelWork.csv")

ap_lm <- lm(score_dxLRPts ~ seed_d + ws.40_d + aBPM_d + TS_d + TOV_d + StlBlk_d + OR_d + PER_d + depth_d, data = analyze.past_d)
ap_lm_red <- lm(score_dxLRPts ~ seed_d + aBPM_d, data = analyze.past_d)

ap2_lm <- lm(score_dxRPts ~ seed_d + ws.40_d + aBPM_d + TS_d + TOV_d + StlBlk_d + OR_d + PER_d + depth_d, data = analyze.past_d)
ap2_lm_red <- lm(score_dxRPts ~ seed_d + aBPM_d + TS_d + TOV_d + PER_d, data = analyze.past_d)

ap_glm <- glm(favWin ~ seed_d + ws.40_d + aBPM_d + TS_d + TOV_d + StlBlk_d + OR_d + PER_d + depth_d, data = analyze.past_d)
ap_glm_red <- glm(favWin ~ seed_d + aBPM_d + OR_d + PER_d + depth_d, data = analyze.past_d)
ap_glm_red2 <- glm(favWin ~ seed_d + aBPM_d + OR_d, data = analyze.past_d)

analyze.past <- data.frame(teams)
#analyze.past[["KP"]]<-KP
analyze.past[["finalFourTeam"]]<-finalFourTeam
analyze.past[["seed"]]<-seed
analyze.past[["ws.40"]]<-ws.40
#analyze.past[["ws.40.R"]]<-rank(-ws.40)
analyze.past[["aBPM"]]<-aBPM
#analyze.past[["BPM.R"]]<-rank(-aBPM)
# analyze.past[["totdBPM"]]<-totdBPM
# analyze.past[["toxdBPM"]]<-toxdBPM
analyze.past[["TS"]]<- ts
analyze.past[["TOV"]]<- tov
analyze.past[["StlBlk"]]<- stlBlk
analyze.past[["OR"]]<- or
analyze.past[["PER"]]<-per
analyze.past[["depth"]]<-depth
#analyze.past[["powerScore"]]<-powerScore
#analyze.past[["pS.R"]]<-powerRank

write.csv(analyze.past, file = "analyzeAllWithInj.csv")

f4_glm <- glm(finalFourTeam ~ seed + ws.40 + aBPM + TS + TOV + StlBlk + OR + PER + depth, data = analyze.past)

##############
teams <- c()
gamesLookedAt <- c(1:36)
allGames <- length(games_2024[1])
or <- c()
aBPM <- c()
ws.40 <- c()
per <- c()
totdBPM <- c()
toxdBPM <- c()
KP <- c()
seed <- c()
roundNum <- c()
dayNum <- c()
ts <- c()
tov <- c()
stlBlk <- c()
depth <- c()
seed <- c()

for (i in c(1:36)) {
  # html_base_beggining <- "https://www.sports-reference.com/cbb/schools/"
  # html_base_ending <- ".html"
  csv_base_ending <- ".csv"
  year <- games_2024$Year[i]
  betterTeam <- tolower(games_2024$Team...Better.Seed[i])
  worseTeam <- tolower(games_2024$Team...Worse.Seed[i])
  if (betterTeam=="unc wilmington"){betterTeam<-"north-carolina-wilmington"}
  if (worseTeam=="unc wilmington"){worseTeam<-"north-carolina-wilmington"}
  if (betterTeam=="usc"){betterTeam<-"southern-california"}
  if (worseTeam=="usc"){worseTeam<-"southern-california"}
  if (betterTeam=="smu"){betterTeam<-"southern-methodist"}
  if (worseTeam=="smu"){worseTeam<-"southern-methodist"}
  if (betterTeam=="vcu"){betterTeam<-"virginia-commonwealth"}
  if (worseTeam=="vcu"){worseTeam<-"virginia-commonwealth"}
  if (betterTeam=="uc davis"){betterTeam<-"california-davis"}
  if (worseTeam=="uc davis"){worseTeam<-"california-davis"}
  if (betterTeam=="uc-davis"){betterTeam<-"california-davis"}
  if (worseTeam=="uc-davis"){worseTeam<-"california-davis"}
  if (betterTeam=="miami (fl)"){betterTeam<-"miami-fl"}
  if (worseTeam=="miami (fl)"){worseTeam<-"miami-fl"}
  if (betterTeam=="unc asheville"){betterTeam<-"north-carolina-asheville"}
  if (worseTeam=="unc asheville"){worseTeam<-"north-carolina-asheville"}
  if (betterTeam=="little rock"){betterTeam<-"arkansas-little-rock"}
  if (worseTeam=="little rock"){worseTeam<-"arkansas-little-rock"}
  if (betterTeam=="lsu"){betterTeam<-"louisiana-state"}
  if (worseTeam=="lsu"){worseTeam<-"louisiana-state"}
  if (betterTeam=="uc irvine"){betterTeam<-"california-irvine"}
  if (worseTeam=="uc irvine"){worseTeam<-"california-irvine"}
  betterTeam<-gsub(" ", "-", betterTeam)
  worseTeam<-gsub(" ", "-", worseTeam)
  if (betterTeam=="albany"){betterTeam<-"albany-ny"}
  if (worseTeam=="albany"){worseTeam<-"albany-ny"}
  if (betterTeam=="st.-john's"){betterTeam<-"st-johns-ny"}
  if (worseTeam=="st.-john's"){worseTeam<-"st-johns-ny"}
  betterTeam<-gsub(".", "", betterTeam, fixed=TRUE)
  worseTeam<-gsub(".", "", worseTeam, fixed=TRUE)
  betterTeam<-gsub("'", "", betterTeam)
  worseTeam<-gsub("'", "", worseTeam)
  betterTeam<-gsub("&", "", betterTeam)
  worseTeam<-gsub("&", "", worseTeam)
  if (betterTeam=="uab"){betterTeam<-"alabama-birmingham"}
  if (worseTeam=="uab"){worseTeam<-"alabama-birmingham"}
  if (betterTeam=="byu"){betterTeam<-"brigham-young"}
  if (worseTeam=="byu"){worseTeam<-"brigham-young"}
  if (betterTeam=="cal-poly-slo"){betterTeam<-"cal-poly"}
  if (worseTeam=="cal-poly-slo"){worseTeam<-"cal-poly"}
  if (betterTeam=="unlv"){betterTeam<-"nevada-las-vegas"}
  if (worseTeam=="unlv"){worseTeam<-"nevada-las-vegas"}
  if (betterTeam=="liu-brooklyn"){betterTeam<-"long-island-university"}
  if (worseTeam=="liu-brooklyn"){worseTeam<-"long-island-university"}
  if (betterTeam== "southern-miss"){betterTeam<-"southern-mississippi"}
  if (worseTeam== "southern-miss"){worseTeam<-"southern-mississippi"}
  betterTeam<-gsub("(", "", betterTeam, fixed=TRUE)
  worseTeam<-gsub("(", "", worseTeam, fixed=TRUE)
  betterTeam<-gsub(")", "", betterTeam, fixed=TRUE)
  worseTeam<-gsub(")", "", worseTeam, fixed=TRUE)
  if (betterTeam=="detroit"){betterTeam<-"detroit-mercy"}
  if (worseTeam=="detroit"){worseTeam<-"detroit-mercy"}
  if (betterTeam=="utsa"){betterTeam<-"texas-san-antonio"}
  if (worseTeam=="utsa"){worseTeam<-"texas-san-antonio"}
  if (betterTeam=="uc-santa-barbara"){betterTeam<-"california-santa-barbara"}
  if (worseTeam=="uc-santa-barbara"){worseTeam<-"california-santa-barbara"}
  if (betterTeam=="utep"){betterTeam<-"texas-el-paso"}
  if (worseTeam=="utep"){worseTeam<-"texas-el-paso"}
  if (betterTeam=="portland-st"){betterTeam<-"portland-state"}
  if (worseTeam=="portland-st"){worseTeam<-"portland-state"}
  if (betterTeam=="umbc"){betterTeam<-"maryland-baltimore-county"}
  if (worseTeam=="umbc"){worseTeam<-"maryland-baltimore-county"}
  if (betterTeam=="ut-arlington"){betterTeam<-"texas-arlington"}
  if (worseTeam=="ut-arlington"){worseTeam<-"texas-arlington"}
  if (betterTeam=="texas-am-corpus-chris"){betterTeam<-"texas-am-corpus-christi"}
  if (worseTeam=="texas-am-corpus-chris"){worseTeam<-"texas-am-corpus-christi"}
  if (betterTeam=="central-connecticut"){betterTeam<-"central-connecticut-state"}
  if (worseTeam=="central-connecticut"){worseTeam<-"central-connecticut-state"}
  if (betterTeam=="penn"){betterTeam<-"pennsylvania"}
  if (worseTeam=="penn"){worseTeam<-"pennsylvania"}
  if (betterTeam=="ucf"){betterTeam<-"central-florida"}
  if (worseTeam=="ucf"){worseTeam<-"central-florida"}
  if (betterTeam=="troy-state"){betterTeam<-"troy"}
  if (worseTeam=="troy-state"){worseTeam<-"troy"}
  if (betterTeam=="uc-irvine"){betterTeam<-"california-irvine"}
  if (worseTeam=="uc-irvine"){worseTeam<-"california-irvine"}
  if (betterTeam=="texas-so"){betterTeam<-"texas-southern"}
  if (worseTeam=="texas-so"){worseTeam<-"texas-southern"}
  if (betterTeam=="charleston"){betterTeam<-"college-of-charleston"}
  if (worseTeam=="charleston"){worseTeam<-"college-of-charleston"}
  if (betterTeam=="unc-wilmington"){betterTeam<-"north-carolina-wilmington"}
  if (worseTeam=="unc-wilmington"){worseTeam<-"north-carolina-wilmington"} 
  if (betterTeam=="unc-asheville"){betterTeam<-"north-carolina-asheville"}
  if (worseTeam=="unc-asheville"){worseTeam<-"north-carolina-asheville"}
  if (betterTeam=="little-rock"){betterTeam<-"arkansas-little-rock"}
  if (worseTeam=="little-rock"){worseTeam<-"arkansas-little-rock"}
  
  # htmlLinkBT <- paste0(html_base_beggining, betterTeam, "/", year, html_base_ending)
  # betterTeamWebPage <- html(htmlLinkBT)
  # htmlLinkWT <- paste0(html_base_beggining, worseTeam, "/", year, html_base_ending)
  # worseTeamWebPage <- html(htmlLinkWT)
  csvLinkBT <- paste0(betterTeam, " ", year, csv_base_ending)
  betterTeamData <- read.csv(csvLinkBT)
  csvLinkWT <- paste0(worseTeam, " ", year, csv_base_ending)
  worseTeamData <- read.csv(csvLinkWT)
  
  min_cutoff<-240
  
  btMP<-betterTeamData$MP[betterTeamData$MP>min_cutoff]
  wtMP<-worseTeamData$MP[worseTeamData$MP>min_cutoff]
  
  btGP<-betterTeamData$G[betterTeamData$MP>min_cutoff]
  wtGP<-worseTeamData$G[worseTeamData$MP>min_cutoff]
  
  btMPpG<-btMP/btGP
  wtMPpG<-wtMP/wtGP
  
  btdBPM <- sum(betterTeamData$BPM[betterTeamData$MP>min_cutoff]*btMPpG)/sum(btMPpG)
  wtdBPM <- sum(worseTeamData$BPM[worseTeamData$MP>min_cutoff]*wtMPpG)/sum(wtMPpG)
  btdWS.40 <- sum(betterTeamData$WS.40[betterTeamData$MP>min_cutoff]*btMPpG)/sum(btMPpG)*40
  wtdWS.40 <- sum(worseTeamData$WS.40[worseTeamData$MP>min_cutoff]*wtMPpG)/sum(wtMPpG)*40
  btdPER <- sum(betterTeamData$PER[betterTeamData$MP>min_cutoff]*btMPpG)/sum(btMPpG)
  wtdPER <- sum(worseTeamData$PER[worseTeamData$MP>min_cutoff]*wtMPpG)/sum(wtMPpG)
  btdtotdBPM <- max(betterTeamData$OBPM[betterTeamData$MP>min_cutoff])+max(betterTeamData$DBPM[betterTeamData$MP>min_cutoff])
  wtdtotdBPM <- max(worseTeamData$OBPM[worseTeamData$MP>min_cutoff])+max(worseTeamData$DBPM[worseTeamData$MP>min_cutoff])
  btdtoXdBPM <- max(betterTeamData$OBPM[betterTeamData$MP>min_cutoff])+(max(betterTeamData$DBPM[betterTeamData$MP>min_cutoff])+min(betterTeamData$DBPM[betterTeamData$MP>min_cutoff]))/2
  wtdtoXdBPM <- max(worseTeamData$OBPM[worseTeamData$MP>min_cutoff])+(max(worseTeamData$DBPM[worseTeamData$MP>min_cutoff])+min(worseTeamData$DBPM[worseTeamData$MP>min_cutoff]))/2
  btTS <- sum(betterTeamData$TS.[betterTeamData$MP>min_cutoff]*betterTeamData$PProd[betterTeamData$MP>min_cutoff])/sum(betterTeamData$PProd[betterTeamData$MP>min_cutoff])
  wtTS <- sum(worseTeamData$TS.[worseTeamData$MP>min_cutoff]*worseTeamData$PProd[worseTeamData$MP>min_cutoff])/sum(worseTeamData$PProd[worseTeamData$MP>min_cutoff])
  btTotTOV <- sum((betterTeamData$PProd[betterTeamData$MP>min_cutoff]/(2*(betterTeamData$TS.[betterTeamData$MP>min_cutoff]))) * (betterTeamData$TOV.[betterTeamData$MP>min_cutoff] / (100 - betterTeamData$TOV.[betterTeamData$MP>min_cutoff])))
  btTOV <- 100 * btTotTOV / (sum(betterTeamData$PProd[betterTeamData$MP>min_cutoff]) / (2 * btTS) + btTotTOV)
  wtTotTOV <- sum((worseTeamData$PProd[worseTeamData$MP>min_cutoff]/(2*(worseTeamData$TS.[worseTeamData$MP>min_cutoff]))) * (worseTeamData$TOV.[worseTeamData$MP>min_cutoff] / (100 - worseTeamData$TOV.[worseTeamData$MP>min_cutoff])))
  wtTOV <- 100 * wtTotTOV / (sum(worseTeamData$PProd[worseTeamData$MP>min_cutoff]) / (2 * wtTS) + wtTotTOV)
  btSB <- sum((betterTeamData$STL.[betterTeamData$MP>min_cutoff]*3+betterTeamData$BLK.[betterTeamData$MP>min_cutoff]*2)*betterTeamData$MP[betterTeamData$MP>min_cutoff])/sum(betterTeamData$MP[betterTeamData$MP>min_cutoff])
  wtSB <- sum((worseTeamData$STL.[worseTeamData$MP>min_cutoff]*3+worseTeamData$BLK.[worseTeamData$MP>min_cutoff]*2)*worseTeamData$MP[worseTeamData$MP>min_cutoff])/sum(worseTeamData$MP[worseTeamData$MP>min_cutoff])
  btOR <- sum((betterTeamData$ORB.[betterTeamData$MP>min_cutoff])*btMPpG)/sum(btMPpG)
  wtOR <- sum((worseTeamData$ORB.[worseTeamData$MP>min_cutoff])*wtMPpG)/sum(wtMPpG)
  btdepth <- sum((betterTeamData$MP>min_cutoff)*1)
  wtdepth <- sum((worseTeamData$MP>min_cutoff)*1)
  
  # KP <- c(KP, games_2024$Better.Seed.KP.Expected.Margin[i], games_2024$Worse.Seed.KP.Expected.Marg.[i])
  if (games_2024$Round[i] == "1) Round of 64") {
    teams <- c(teams, paste0(betterTeam, " ", year), paste0(worseTeam, " ", year)) 
    #teams <- c(teams, betterTeam, worseTeam)
    aBPM <- c(aBPM, btdBPM, wtdBPM)
    ws.40 <- c(ws.40, btdWS.40, wtdWS.40)
    per <- c(per, .1*btdPER, .1*wtdPER)
    totdBPM <- c(totdBPM, btdtotdBPM, wtdtotdBPM)
    toxdBPM <- c(toxdBPM, btdtoXdBPM, wtdtoXdBPM)
    ts <- c(ts, btTS*10, wtTS*10)
    tov <- c(tov, -.5*btTOV, -.5*wtTOV)
    stlBlk <- c(stlBlk, btSB, wtSB)
    or <- c(or, .1*btOR, .1*wtOR)
    depth <- c(depth, btdepth, wtdepth)
    seed <- c(seed, as.numeric(games_2024$Better.Seed[i]), as.numeric(games_2024$Worse.Seed[i]))
  }
}

analyze.2024 <- data.frame(teams)
#analyze.2024[["KP"]]<-KP
#analyze.2024[["finalFourTeam"]]<-finalFourTeam
analyze.2024[["seed"]]<-seed
analyze.2024[["ws.40"]]<-ws.40
#analyze.2024[["ws.40.R"]]<-rank(-ws.40)
analyze.2024[["aBPM"]]<-aBPM
#analyze.2024[["BPM.R"]]<-rank(-aBPM)
# analyze.2024[["totdBPM"]]<-totdBPM
# analyze.2024[["toxdBPM"]]<-toxdBPM
analyze.2024[["TS"]]<- ts
analyze.2024[["TOV"]]<- tov
analyze.2024[["StlBlk"]]<- stlBlk
analyze.2024[["OR"]]<- or
analyze.2024[["PER"]]<-per
analyze.2024[["depth"]]<-depth
#analyze.2024[["powerScore"]]<-powerScore
#analyze.2024[["pS.R"]]<-powerRank

predict_2024 <- predict(f4_glm, newdata = analyze.2024, type="response")
