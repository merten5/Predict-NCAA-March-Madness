setwd("~/Desktop/Basketball Stats")
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
library('rvest')


games_all <- read.csv("NCAA Tournament Games.csv",stringsAsFactors = FALSE, strip.white = TRUE)
#for (i in 1:length(players_all[[1]])) {
new_df_names <- c(names(games_all), "Top.7.BPM", "Top.7.WS/48", "Top.7.PER", "Top.Off.C.Def")

for (i in 1:2){
     #length(games_all[[1]])) {
  html_base_beggining <- "https://www.sports-reference.com/cbb/schools/"
  html_base_ending <- ".html"
  year <- games_all$Year[i]
  betterTeam <- tolower(games_all$Team...Better.Seed[i])
  worseTeam <- tolower(games_all$Team...Worse.Seed[i])
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
  
  htmlLinkBT <- paste0(html_base_beggining, betterTeam, "/", year, html_base_ending)
  betterTeamWebPage <- html(htmlLinkBT)
  htmlLinkWT <- paste0(html_base_beggining, worseTeam, "/", year, html_base_ending)
  worseTeamWebPage <- html(htmlLinkWT)
  
  
  # bpm5top <- betterTeamWebPage %>%
  #   html_nodes("#advanced tr:nth-child(2) .right , #advanced tr:nth-child(3) .right , #advanced tr:nth-child(4) .right , #advanced tr:nth-child(5) .right , #advanced tr:nth-child(1) .right") 
}