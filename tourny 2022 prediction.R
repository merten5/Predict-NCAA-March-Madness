setwd("C:/Users/b3nja/Desktop/NCAA Tournament")
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


games_2018 <- read.csv("NCAA Tournament Games plus 2021.csv",stringsAsFactors = FALSE, strip.white = TRUE)
#for (i in 1:length(players_2018[[1]])) {
new_df_names <- c(names(games_2018), "All.BPM", "WS.40", "PER", "Top.Off.Top.Def.BPM", "Top.Off.Xtreme.Def.BPM")
working_df <- games_2018

teams <- c()
gamesLookedAt <- c(1:33)
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

#for (i in 1:length(games_2018[[1]])) {
for (i in gamesLookedAt) {
  # html_base_beggining <- "https://www.sports-reference.com/cbb/schools/"
  # html_base_ending <- ".html"
  csv_base_ending <- ".csv"
  year <- games_2018$Year[i]
  betterTeam <- tolower(games_2018$Team...Better.Seed[i])
  worseTeam <- tolower(games_2018$Team...Worse.Seed[i])
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
  
  # htmlLinkBT <- paste0(html_base_beggining, betterTeam, "/", year, html_base_ending)
  # betterTeamWebPage <- html(htmlLinkBT)
  # htmlLinkWT <- paste0(html_base_beggining, worseTeam, "/", year, html_base_ending)
  # worseTeamWebPage <- html(htmlLinkWT)
  csvLinkBT <- paste0(betterTeam, " ", year, csv_base_ending)
  betterTeamData <- read.csv(csvLinkBT)
  csvLinkWT <- paste0(worseTeam, " ", year, csv_base_ending)
  worseTeamData <- read.csv(csvLinkWT)
  value<-250
  
  btMP<-betterTeamData$MP[betterTeamData$MP>value]
  wtMP<-worseTeamData$MP[worseTeamData$MP>value]
  
  btGP<-betterTeamData$G[betterTeamData$MP>value]
  wtGP<-worseTeamData$G[worseTeamData$MP>value]
  
  btMPpG<-btMP/btGP
  wtMPpG<-wtMP/wtGP
  
  btdBPM <- sum(betterTeamData$BPM[betterTeamData$MP>value]*btMPpG)/sum(btMPpG)
  wtdBPM <- sum(worseTeamData$BPM[worseTeamData$MP>value]*wtMPpG)/sum(wtMPpG)
  btdWS.40 <- sum(betterTeamData$WS.40[betterTeamData$MP>value]*btMPpG)/sum(btMPpG)*40
  wtdWS.40 <- sum(worseTeamData$WS.40[worseTeamData$MP>value]*wtMPpG)/sum(wtMPpG)*40
  btdPER <- sum(betterTeamData$PER[betterTeamData$MP>value]*btMPpG)/sum(btMPpG)
  wtdPER <- sum(worseTeamData$PER[worseTeamData$MP>value]*wtMPpG)/sum(wtMPpG)
  btdtotdBPM <- max(betterTeamData$OBPM[betterTeamData$MP>value])+max(betterTeamData$DBPM[betterTeamData$MP>value])
  wtdtotdBPM <- max(worseTeamData$OBPM[worseTeamData$MP>value])+max(worseTeamData$DBPM[worseTeamData$MP>value])
  btdtoXdBPM <- max(betterTeamData$OBPM[betterTeamData$MP>value])+(max(betterTeamData$DBPM[betterTeamData$MP>value])+min(betterTeamData$DBPM[betterTeamData$MP>value]))/2
  wtdtoXdBPM <- max(worseTeamData$OBPM[worseTeamData$MP>value])+(max(worseTeamData$DBPM[worseTeamData$MP>value])+min(worseTeamData$DBPM[worseTeamData$MP>value]))/2
  btTS <- sum(betterTeamData$TS.[betterTeamData$MP>value]*betterTeamData$PProd[betterTeamData$MP>value])/sum(betterTeamData$PProd[betterTeamData$MP>value])
  wtTS <- sum(worseTeamData$TS.[worseTeamData$MP>value]*worseTeamData$PProd[worseTeamData$MP>value])/sum(worseTeamData$PProd[worseTeamData$MP>value])
  btTotTOV <- sum((betterTeamData$PProd[betterTeamData$MP>value]/(2*(betterTeamData$TS.[betterTeamData$MP>value]))) * (betterTeamData$TOV.[betterTeamData$MP>value] / (100 - betterTeamData$TOV.[betterTeamData$MP>value])))
  btTOV <- 100 * btTotTOV / (sum(betterTeamData$PProd[betterTeamData$MP>value]) / (2 * btTS) + btTotTOV)
  wtTotTOV <- sum((worseTeamData$PProd[worseTeamData$MP>value]/(2*(worseTeamData$TS.[worseTeamData$MP>value]))) * (worseTeamData$TOV.[worseTeamData$MP>value] / (100 - worseTeamData$TOV.[worseTeamData$MP>value])))
  wtTOV <- 100 * wtTotTOV / (sum(worseTeamData$PProd[worseTeamData$MP>value]) / (2 * wtTS) + wtTotTOV)
  btSB <- sum((betterTeamData$STL.[betterTeamData$MP>value]*3+betterTeamData$BLK.[betterTeamData$MP>value]*2)*betterTeamData$MP[betterTeamData$MP>value])/sum(betterTeamData$MP[betterTeamData$MP>value])
  wtSB <- sum((worseTeamData$STL.[worseTeamData$MP>value]*3+worseTeamData$BLK.[worseTeamData$MP>value]*2)*worseTeamData$MP[worseTeamData$MP>value])/sum(worseTeamData$MP[worseTeamData$MP>value])
  btOR <- sum((betterTeamData$ORB.[betterTeamData$MP>value])*btMPpG)/sum(btMPpG)
  wtOR <- sum((worseTeamData$ORB.[worseTeamData$MP>value])*wtMPpG)/sum(wtMPpG)
  btdepth <- sum((betterTeamData$MP>value)*1)
  wtdepth <- sum((worseTeamData$MP>value)*1)
  
  
  
  # KP <- c(KP, games_2018$Better.Seed.KP.Expected.Margin[i], games_2018$Worse.Seed.KP.Expected.Marg.[i])
  teams <- c(teams, betterTeam, worseTeam)
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
  seed <- c(seed, games_2018$Better.Seed[i], games_2018$Worse.Seed[i])
}

# scoreKPandBPM <- KP*-1.63191+aBPM*6.52362
# powerScoreSig <- KP*-1.63191+aBPM*6.52362+ws.40*1.98870+per*-3.41488+tov*1.64012+ts*1.67948+or*2.24970
# powerScore <- KP*-1.63191+aBPM*6.52362+ws.40*1.98870+per*-3.41488+tov*1.64012+ts*1.67948+or*2.24970+totdBPM*-0.036833+
#  toxdBPM*0.074073+stlBlk*0.054815+seed*0.024429+depth*0.004792
# powerRank <- rank(-powerScore)

analyze.2018 <- data.frame(teams)
#analyze.2018[["KP"]]<-KP
analyze.2018[["seed"]]<-seed
analyze.2018[["ws.40"]]<-ws.40
#analyze.2018[["ws.40.R"]]<-rank(-ws.40)
analyze.2018[["aBPM"]]<-aBPM
#analyze.2018[["BPM.R"]]<-rank(-aBPM)
# analyze.2018[["totdBPM"]]<-totdBPM
# analyze.2018[["toxdBPM"]]<-toxdBPM
analyze.2018[["TS"]]<- ts
analyze.2018[["TOV"]]<- tov
analyze.2018[["StlBlk"]]<- stlBlk
analyze.2018[["OR"]]<- or
analyze.2018[["PER"]]<-per
analyze.2018[["depth"]]<-depth
#analyze.2018[["powerScore"]]<-powerScore
#analyze.2018[["pS.R"]]<-powerRank

write.csv(analyze.2018, file = "analyze2022withInj.csv")
