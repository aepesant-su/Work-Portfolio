library(rvest)
library(tidyverse)
library(janitor)
teams <- c("arizona-cardinals","atlanta-falcons","baltimore-ravens","buffalo-bills",
           "carolina-panthers","chicago-bears","cincinnati-bengals","cleveland-browns",
           "dallas-cowboys","denver-broncos","detroit-lions","green-bay-packers",
           "houston-texans","indianapolis-colts","jacksonville-jaguars","kansas-city-chiefs",
           "miami-dolphins","minnesota-vikings","new-england-patriots","new-orleans-saints",
           "new-york-giants","new-york-jets","las-vegas-raiders","philadelphia-eagles",
           "pittsburgh-steelers","san-diego-chargers","seattle-seahawks","san-francisco-49ers",
           "los-angeles-rams","tampa-bay-buccaneers","tennessee-titans","washington-football-team")


gms_all <- list()

for (i in teams) {
  url<- paste0("https://www.retroseasons.com/teams/",i,"/history/general-managers/")
  gm_html <- read_html(url)
  gm_table<- gm_html %>% html_table(fill = TRUE)
  gm_table <- gm_table[[1]]
  gm_table <- gm_table[-1,]
  gm_table <- gm_table %>% row_to_names(row_number = 1) %>% select(Years, Name)
  gm_table <- head(gm_table,-1)
  gm_table$team <- i
  gms_all[[i]] <- gm_table 
}

gms_final <- do.call(rbind, gms_all)
rownames(gms_final) <- NULL
