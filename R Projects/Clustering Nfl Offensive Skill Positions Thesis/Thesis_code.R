library(nflscrapR)
library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(factoextra)
library(NbClust)

r_2009 <- read_csv('team_2009_rosters.csv')
r_2010 <- read_csv('team_2010_rosters.csv')
r_2011 <- read_csv('team_2011_rosters.csv')
r_2012 <- read_csv('team_2012_rosters.csv')
r_2013 <- read_csv('team_2013_rosters.csv')
r_2014 <- read_csv('team_2014_rosters.csv')
r_2015 <- read_csv('team_2015_rosters.csv')
r_2016 <- read_csv('team_2016_rosters.csv')
r_2017 <- read_csv('team_2017_rosters.csv')

roster <- rbind(r_2009,r_2010,r_2011,r_2012,r_2013,r_2014,r_2015,r_2016,r_2017)
roster1 <- distinct(roster, GSIS_ID, Pos, Team)
roster1$Team <- gsub('JAX', 'JAC', roster1$Team)

receiver_data1 <- read_csv("season_receiving_df.csv")
receiver_data1$GSIS_ID <- receiver_data1$Receiver_ID

qb_data1 <- read_csv("season_passing_df.csv")
qb_data1$GSIS_ID <- qb_data1$Passer_ID

hb_data1 <- read_csv("season_rushing_df.csv")
hb_data1$GSIS_ID <- hb_data1$Rusher_ID

receiver_data1 <- merge(receiver_data1, roster1, by = 'GSIS_ID')
qb_data1 <- merge(qb_data1, roster1, by = 'GSIS_ID')
hb_data1 <- merge(hb_data1, roster1, by = 'GSIS_ID')

###for recievers
receiver_data2 <- receiver_data1 %>% filter(Pos == "WR")
receiver_data2$Receiver_ID <- paste(receiver_data2$Receiver_ID, receiver_data2$Player_Name, receiver_data2$Season, sep = "-")
#receiver_data2$unique_player <- paste(receiver_data2$Receiver_ID, receiver_data2$Player_Name, receiver_data2$Season, sep = "-")

receiver_passing <- qb_data1 %>% filter(Pos == "WR")
receiver_passing$Receiver_ID <- paste(receiver_passing$Passer_ID, receiver_passing$Player_Name, receiver_passing$Season, sep = "-")
#receiver_passing$unique_player <- paste(receiver_passing$Receiver_ID, receiver_passing$Player_Name, receiver_passing$Season, sep = "-")


receiver_rushing <- hb_data1 %>% filter(Pos == "WR")
receiver_rushing$Receiver_ID <- paste(receiver_rushing$Rusher_ID, receiver_rushing$Player_Name, receiver_rushing$Season, sep = "-")
#receiver_rushing$unique_player <- paste(receiver_rushing$Receiver_ID, receiver_rushing$Player_Name, receiver_rushing$Season, sep = "-")

receiver_data2 <- left_join(receiver_data2, receiver_passing, by = "Receiver_ID")

receiver_data2 <- left_join(receiver_data2, receiver_rushing, by = "Receiver_ID")

receiver_data2[is.na(receiver_data2)] = 0

receiver_data2 <- receiver_data2 %>% rename(receiving_yards = Total_Yards.x,
                                            receiving_yards_per_drive = Yards_per_Drive.x,
                                            receiving_tds = TDs.x,
                                            passing_tds = TDs.y,
                                            passing_yards = Total_Yards.y,
                                            rushing_yards = Total_Yards,
                                            rushing_tds = TDs,
                                            rec_drives = Drives.x,
                                            clutch_epa_rec = Total_Clutch_EPA.x,
                                            EPA_rec = Total_EPA.x,
                                            WPA_rec = Total_WPA.x,
                                            qb_drives = Drives.y,
                                            hb_drives = Drives,
                                            team = Team.x)

receiver_data <- receiver_data2 %>% select(Receiver_ID, team,Targets, Receptions, rec_drives,
                                           Targets_per_Drive, Rec_per_Drive, receiving_yards,
                                           receiving_yards_per_drive, Total_Raw_YAC,
                                           Yards_per_Target, YAC_per_Target, Total_Caught_YAC, Total_Dropped_YAC, 
                                           Dropped_YAC_per_Target, YAC_per_Rec,
                                           YAC_per_Drive,
                                           Rec_Percentage, receiving_tds, TDs_per_Drive, TD_per_Targets, TD_per_Rec,
                                           clutch_epa_rec, Attempts, passing_yards, passing_tds,
                                           passing_tds, Carries, rushing_yards, rushing_tds,
                                           EPA_rec, WPA_rec)

str(receiver_data)

###for quarterbacks
qb_data2 <- qb_data1 %>% filter(Pos == "QB")
qb_data2$Passer_ID <- paste(qb_data2$Passer_ID, qb_data2$Player_Name, qb_data2$Season,sep = "-")
#qb_data2$unique_player <- paste(qb_data2$Passer_ID, qb_data2$Player_Name, qb_data2$Season,sep = "-")

qb_receiving <- receiver_data1 %>% filter(Pos == "QB")
qb_receiving$Passer_ID <- paste(qb_receiving$Receiver_ID, qb_receiving$Player_Name, qb_receiving$Season,sep = "-")
#qb_receiving$unique_player <- paste(qb_receiving$Passer_ID, qb_receiving$Player_Name, qb_receiving$Season,sep = "-")

qb_rushing <- hb_data1 %>% filter(Pos == "QB")
qb_rushing$Passer_ID <- paste(qb_rushing$Rusher_ID, qb_rushing$Player_Name, qb_rushing$Season,sep = "-")
#qb_rushing$unique_player <- paste(qb_rushing$Passer_ID, qb_rushing$Player_Name, qb_rushing$Season,sep = "-")

qb_data2 <- left_join(qb_data2, qb_receiving, by = "Passer_ID")

qb_data2 <- left_join(qb_data2, qb_rushing, by = "Passer_ID")

qb_data2[is.na(qb_data2)] = 0

sum(is.na(qb_data2$Team.x))

qb_data2 <- qb_data2 %>% rename(qb_drives = Drives.x,
                                passing_yards = Total_Yards.x,
                                total_raw_airyards_qb = Total_Raw_AirYards.x,
                                pass_yards_per_drive = Yards_per_Drive.x,
                                passing_tds = TDs.x,
                                EPA_qb = Total_EPA.x,
                                WPA_qb = Total_WPA.x,
                                total_clutch_epa_qb = Total_Clutch_EPA.x,
                                rec_drives = Drives.y,
                                rec_yards = Total_Yards.y,
                                rec_tds = TDs.y,
                                hb_drives = Drives,
                                rushing_yards = Total_Yards,
                                fumbles = Fumbles.y,
                                rushing_tds = TDs,
                                team = Team.x)

qb_data <- qb_data2 %>% select(Passer_ID, team, Attempts, Completions, qb_drives, Comp_Perc, passing_yards,
                               total_raw_airyards_qb, Total_Comp_AirYards, Yards_per_Att, Yards_per_Comp,
                               pass_yards_per_drive, Raw_AirYards_per_Att, Comp_AirYards_per_Att,
                               Comp_AirYards_per_Comp, Raw_AirYards_per_Drive, Comp_AirYards_per_Drive, TimesHit, TimesHit_per_Drive,
                               Interceptions, passing_tds, TD_per_Att, Int_per_Att, TD_per_Comp,
                               TD_per_Att, TD_per_Drive, Int_per_Drive, total_clutch_epa_qb, Targets,
                               Carries, rushing_yards, fumbles,
                               rushing_tds, EPA_qb, WPA_qb)


###for runningbacks
hb_data2 <- hb_data1 %>% filter(Pos == "RB")
hb_data2$Rusher_ID <- paste(hb_data2$Rusher_ID, hb_data2$Player_Name, hb_data2$Season, sep = "-")
#hb_data2$unique_player <- paste(hb_data2$Rusher_ID, hb_data2$Player_Name, hb_data2$Season,sep = "-")


hb_receiving <- receiver_data1 %>% filter(Pos == "RB")
hb_receiving$Rusher_ID <- paste(hb_receiving$Receiver_ID, hb_receiving$Player_Name, hb_receiving$Season,sep = "-")
#hb_receiving$unique_player <- paste(hb_receiving$Rusher_ID, hb_receiving$Player_Name, hb_receiving$Season,sep = "-")


hb_passing <- qb_data1 %>% filter(Pos == "RB")
hb_passing$Rusher_ID <- paste(hb_passing$Passer_ID, hb_passing$Player_Name, hb_passing$Season,sep = "-")
#hb_passing$unique_player <- paste(hb_passing$Rusher_ID, hb_passing$Player_Name, hb_passing$Season,sep = "-")

hb_data2 <- left_join(hb_data2, hb_receiving, by = "Rusher_ID")

hb_data2 <- left_join(hb_data2, hb_passing, by = "Rusher_ID")

hb_data2[is.na(hb_data2)] = 0

sum(is.na(hb_data2$Team.x))

hb_data2 <- hb_data2 %>% rename(hb_drives = Drives.x,
                                rushing_yards = Total_Yards.x,
                                rush_yards_per_drive = Yards_per_Drive.x,
                                fumbles = Fumbles.x,
                                rushing_tds = TDs.x,
                                td_to_fumble_hb = TD_to_Fumbles.x,
                                EPA_hb = Total_EPA.x,
                                WPA_hb = Total_WPA.x,
                                fumbles_per_drive_hb = Fumbles_per_Drive.x,
                                total_clutch_epa_hb = Total_Clutch_EPA.x,
                                rec_drives = Drives.y,
                                rec_yards = Total_Yards.y,
                                rec_yards_per_drive = Yards_per_Drive.y,
                                rec_fumbles = Fumbles.y,
                                rec_tds = TDs.y,
                                qb_drives = Drives,
                                pass_yards = Total_Yards,
                                pass_tds = TDs,
                                team = Team.x)

hb_data <- hb_data2 %>% select(Rusher_ID, team, Carries, hb_drives, Car_per_Drive, rushing_yards, Yards_per_Car, rush_yards_per_drive,
                               fumbles, rushing_tds,  TD_per_Car, Fumbles_per_Car, fumbles_per_drive_hb, TD_Drive,
                               total_clutch_epa_hb, Targets, Receptions, Targets_per_Drive, Rec_per_Drive, rec_yards,
                               rec_yards_per_drive, Total_Raw_YAC, Rec_Percentage,
                               rec_tds, qb_drives, Attempts, pass_yards,
                               pass_tds, EPA_hb, WPA_hb)


###receiver
receiver_data$Receiver_ID[duplicated(receiver_data$Receiver_ID)]
receiver_data <- receiver_data %>% distinct(Receiver_ID, .keep_all = TRUE)
r.df <- receiver_data[,3:(length(receiver_data)-2)]
row.names(r.df) <- receiver_data[,1]
scaled <- scale(r.df)
d <- dist(scaled)
set.seed(14)
hc <- hclust(d, method="ward.D")

###finding optimal clusters

# Elbow method
fviz_nbclust(scaled, hcut, method = "wss") +
  geom_vline(xintercept = 6, linetype = 2)+
  labs(subtitle = "Elbow method for WR Cluster")

# Silhouette method
fviz_nbclust(scaled, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method for WR Cluster")

# Gap Statistic method
set.seed(77)
fviz_nbclust(scaled, hcut, method = "gap_stat")+
  labs(subtitle = "Gap statistic method for WR Cluster")

###hier arch plot
plot(hc, hang=-1)

###making plot

library(RColorBrewer)
n <- 5
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
sample(col_vector, n)

labelColors <- c("#F4CAE4", "#FDBF6F", "#FFFF99", "#FFD92F", "#F781BF", "#BF5B17", "#FCCDE5")

clusMember <- cutree(hc, 5)       

colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}                


hcd <- as.dendrogram(hc)
clusDendro <- dendrapply(hcd, colLab)

#####Another way to show clusters
plot(clusDendro, main = "NFL Receiver Clusters", type = "triangle")

receiver_data$cluster <- as.factor(cutree(hc, 5))

###showing the ranks of the clusters 
boxplot(EPA_rec~cluster, data=receiver_data, main = "WidecReceiver Rank by Cluster(EPA)")
boxplot(WPA_rec~cluster, data=receiver_data, main = "Wide Receiver Rank by Cluster")

###looking at clusters

cluster_1_wr <- receiver_data %>% filter(cluster == 1)
cluster_2_wr <- receiver_data %>% filter(cluster == 2)
cluster_3_wr <- receiver_data %>% filter(cluster == 3) 
cluster_4_wr <- receiver_data %>% filter(cluster == 4) #best
cluster_5_wr <- receiver_data %>% filter(cluster == 5) 



summary(cluster_1_wr)
summary(cluster_2_wr)
summary(cluster_3_wr) 
summary(cluster_4_wr) #best
summary(cluster_5_wr) 


###qb
qb_data <- qb_data %>% distinct(Passer_ID, .keep_all = TRUE)
r.df <- qb_data[,3:(length(qb_data)-2)]
row.names(r.df) <- qb_data[,1]
scaled <- scale(r.df)
d <- dist(scaled)
set.seed(19)
hc <- hclust(d, method="ward.D")
str(qb_data)
###finding optimal clusters

# Elbow method
fviz_nbclust(scaled, hcut, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method for QB Cluster")

# Silhouette method
fviz_nbclust(scaled, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method for QB Cluster")

# Gap Statistic method
set.seed(79)
fviz_nbclust(scaled, hcut, method = "gap_stat")+
  labs(subtitle = "Gap statistic method for QB Cluster")

###hier arch plot
plot(hc, hang=-1)

###making plot
n <- 5
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
sample(col_vector, n)

labelColors <- c("#FF7F00", "#FFF2AE", "#E6AB02", "#CAB2D6")

clusMember <- cutree(hc, 5)      

colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}                


hcd <- as.dendrogram(hc)
clusDendro <- dendrapply(hcd, colLab)

#####Another wat ro show clusters
plot(clusDendro, main = "NFL QB Clusters", type = "triangle")

qb_data$cluster <- as.factor(cutree(hc, 5))

###showing the ranks of the clusters 
boxplot(EPA_qb~cluster, data=qb_data, main = "Quarterback Rank by Cluster(EPA)")
boxplot(WPA_qb~cluster, data=qb_data, main = "Quarterback Rank by Cluster")

###looking at clusters

cluster_1_qb <- qb_data %>% filter(cluster == 1)
cluster_2_qb <- qb_data %>% filter(cluster == 2)
cluster_3_qb <- qb_data %>% filter(cluster == 3)
cluster_4_qb <- qb_data %>% filter(cluster == 4)
cluster_5_qb <- qb_data %>% filter(cluster == 5) #best

summary(cluster_1_qb)
summary(cluster_2_qb)
summary(cluster_3_qb)
summary(cluster_4_qb)
summary(cluster_5_qb)


###hb
hb_data <- hb_data %>% distinct(Rusher_ID, .keep_all = TRUE)
r.df <- hb_data[,3:(length(hb_data) - 2)]
row.names(r.df) <- hb_data[,1]
scaled <- scale(r.df)
d <- dist(scaled)
set.seed(25)
hc <- hclust(d, method="ward.D")

###finding optimal clusters

# Elbow method
fviz_nbclust(scaled, hcut, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Elbow method for HB Cluster")

# Silhouette method
fviz_nbclust(scaled, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method for HB")

# Gap Statistic method
set.seed(79)
fviz_nbclust(scaled, hcut, method = "gap_stat")+
  labs(subtitle = "Gap statistic method for HB Cluster")

###hier arch plot
plot(hc, hang=-1)

###making plot
n <- 5
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
sample(col_vector, n)

labelColors <- c("#BC80BD", "#6A3D9A", "#BF5B17")

clusMember <- cutree(hc, 5)       

colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}                


hcd <- as.dendrogram(hc)
clusDendro <- dendrapply(hcd, colLab)

#####Another wat ro show clusters
plot(clusDendro, main = "NFL hb Clusters", type = "triangle")

hb_data$cluster <- as.factor(cutree(hc, 5))

###showing the ranks of the clusters 
boxplot(EPA_hb~cluster, data=hb_data, main = "Running Back Rank by Cluster(EPA)")
boxplot(WPA_hb~cluster, data=hb_data, main = "Running Back Rank by Cluster")

###looking at clusters

cluster_1_hb <- hb_data %>% filter(cluster == 1)
cluster_2_hb <- hb_data %>% filter(cluster == 2)
cluster_3_hb <- hb_data %>% filter(cluster == 3)
cluster_4_hb <- hb_data %>% filter(cluster == 4)
cluster_5_hb <- hb_data %>% filter(cluster == 5)

summary(cluster_1_hb)
summary(cluster_2_hb)
summary(cluster_3_hb)
summary(cluster_4_hb)
summary(cluster_5_hb)


##########################################################################################Skip
###HB within cluster 2

r.df <- cluster_2_hb[,2:(length(cluster_1_hb) - 3)]
row.names(r.df) <- cluster_2_hb[,1]
scaled <- scale(r.df)
d <- dist(scaled)
hc <- hclust(d, method="ward.D")

###finding optimal clusters

# Elbow method
fviz_nbclust(scaled, hcut, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method for HB Cluster")

# Silhouette method
fviz_nbclust(scaled, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method for HB")

###hier arch plot
plot(hc, hang=-1)

###making plot
n <- 3
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
sample(col_vector, n)

labelColors <- c("#BC80BD", "#6A3D9A", "#BF5B17")

clusMember <- cutree(hc, 3)       

colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}                


hcd <- as.dendrogram(hc)
clusDendro <- dendrapply(hcd, colLab)

#####Another wat ro show clusters
plot(clusDendro, main = "NFL hb Clusters", type = "triangle")

cluster_2_hb$cluster_within <- as.factor(cutree(hc, 3))

###showing the ranks of the clusters 
boxplot(EPA_hb~cluster_within, data=cluster_2_hb, main = "hb Rank within cluster 2 by Cluster(EPA)")
boxplot(WPA_hb~cluster_within, data=cluster_2_hb, main = "hb Rank within cluster 2 by Cluster(WPA)")

cluster_2_hb_1_within <- cluster_2_hb %>% filter(cluster_within == 1)
cluster_2_hb_2_within <- cluster_2_hb %>% filter(cluster_within == 2)
cluster_2_hb_3_within <- cluster_2_hb %>% filter(cluster_within == 3)

###HB within cluster 1

r.df <- cluster_1_hb[,2:(length(cluster_1_hb) - 3)]
row.names(r.df) <- cluster_1_hb[,1]
scaled <- scale(r.df)
d <- dist(scaled)
hc <- hclust(d, method="ward.D")

###finding optimal clusters

# Elbow method
fviz_nbclust(scaled, hcut, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method for HB Cluster")

# Silhouette method
fviz_nbclust(scaled, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method for HB")

###hier arch plot
plot(hc, hang=-1)

###making plot
n <- 3
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
sample(col_vector, n)

labelColors <- c("#BC80BD", "#6A3D9A", "#BF5B17")

clusMember <- cutree(hc, 3)       

colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}                


hcd <- as.dendrogram(hc)
clusDendro <- dendrapply(hcd, colLab)

#####Another wat ro show clusters
plot(clusDendro, main = "NFL hb Clusters", type = "triangle")

cluster_1_hb$cluster_within <- as.factor(cutree(hc, 3))

###showing the ranks of the clusters 
boxplot(EPA_hb~cluster_within, data=cluster_1_hb, main = "hb Rank within cluster 1 by Cluster(EPA)")
boxplot(WPA_hb~cluster_within, data=cluster_1_hb, main = "hb Rank within cluster 1 by Cluster(WPA)")

cluster_1_hb_1_within <- cluster_1_hb %>% filter(cluster_within == 1)
cluster_1_hb_2_within <- cluster_1_hb %>% filter(cluster_within == 2)
cluster_1_hb_3_within <- cluster_1_hb %>% filter(cluster_within == 3)

##########################################################################################END

###winning percentage
winning_pct <- read_csv("winning_pct_final.csv")
winning_pct$Team_Season_ID <- paste(winning_pct$Team, winning_pct$Season, sep = "-")
win_pct <- winning_pct %>% select(Team_Season_ID, `W-L%`)

library(stringr)

receiver_data3 <- receiver_data2 %>% select(Receiver_ID, team)
receiver_data3$Season <- str_sub(receiver_data3$Receiver_ID, - 4, - 1)
receiver_data4 <- receiver_data3[receiver_data3$Receiver_ID %in% names(which(table(receiver_data3$Receiver_ID) < 2)), ]
receiver_data4$Team_Season_ID <- paste(receiver_data4$team, receiver_data4$Season, sep = "-")

hb_data3 <- hb_data2 %>% select(Rusher_ID, team)
hb_data3$Season <- str_sub(hb_data3$Rusher_ID, - 4, - 1)
hb_data4 <- hb_data3[hb_data3$Rusher_ID %in% names(which(table(hb_data3$Rusher_ID) < 2)), ]
hb_data4$Team_Season_ID <- paste(hb_data4$team, hb_data4$Season, sep = "-")

qb_data3 <- qb_data2 %>% select(Passer_ID, team)
qb_data3$Season <- str_sub(qb_data3$Passer_ID, - 4, - 1)
qb_data4 <- qb_data3[qb_data3$Passer_ID %in% names(which(table(qb_data3$Passer_ID) < 2)), ]
qb_data4$Team_Season_ID <- paste(qb_data4$team, qb_data4$Season, sep = "-")

cluster_wr <- receiver_data %>% select(Receiver_ID, cluster)
cluster_hb <- hb_data %>% select(Rusher_ID, cluster)
cluster_qb <- qb_data %>% select(Passer_ID, cluster)

cluster_wr <- left_join(receiver_data4, cluster_wr, by = "Receiver_ID")
cluster_hb <- left_join(hb_data4, cluster_hb, by = "Rusher_ID")
cluster_qb <- left_join(qb_data4, cluster_qb, by = "Passer_ID")

win_wr_cluster <- left_join(cluster_wr, win_pct, by = "Team_Season_ID")
win_hb_cluster <- left_join(cluster_hb, win_pct, by = "Team_Season_ID")
win_qb_cluster <- left_join(cluster_qb, win_pct, by = "Team_Season_ID")

ggplot(win_wr_cluster, aes(x = cluster, y = `W-L%`)) + 
  geom_bar(stat = "summary", fun = "mean") +
  ggtitle("Receiver Clusters vs. Winning Percentage")

ggplot(win_hb_cluster, aes(x = cluster, y = `W-L%`)) + 
  geom_bar(stat = "summary", fun = "mean") +
  ggtitle("Running Back Clusters vs. Winning Percentage")

ggplot(win_qb_cluster, aes(x = cluster, y = `W-L%`)) + 
  geom_bar(stat = "summary", fun = "mean") +
  ggtitle("Quarterback Clusters vs. Winning Percentage")

library(corrplot)

receive <- receiver_data[,3:31]
receive <- cor(receive)

corrplot(receive, method = "color")

pass <- qb_data[,3:34]
pass <- cor(pass)

corrplot(pass, method = "color")

run <- hb_data[,3:30]
run <- cor(run)

corrplot(run, method = "color")

###comparing clusters

###receivers
compare_wr <- scale(receiver_data[,3:31])
row.names(compare_wr) <- receiver_data[,1]
compare_wr <- as.data.frame(compare_wr)
compare_wr$cluster <- receiver_data$cluster

###runningbacks
compare_hb <- scale(hb_data[,3:30])
compare_hb <- as.data.frame(compare_hb)
compare_hb$cluster <- hb_data$cluster

###quarterbacks
compare_qb <- scale(qb_data[,3:34])
compare_qb <- as.data.frame(compare_qb)
compare_qb$cluster <- qb_data$cluster

####WR Clusters

###cluster 1
mc23_3_plot_1 <- compare_wr %>% filter(cluster == 1) %>% select(c(receiving_yards, receiving_tds,
                                                                  Targets,
                                                                  Attempts, Carries,Total_Raw_YAC,
                                                                  clutch_epa_rec, rushing_yards, rushing_tds)) %>%
  gather() %>% ggplot(aes(x = reorder(key, -value, median), y = value))  + ylim(-2.5,2.5) +
  geom_boxplot(fill = c("firebrick1", "firebrick1", "firebrick1", "firebrick1", "firebrick1",
                        "firebrick1","firebrick1","firebrick1","firebrick1")) +
  ylim(-2,2) + 
  labs(x = "Statistics",y = "Standardized Value",
       title = "Wide Receiver Cluster 1",
       subtitle = "National Football League 2009-2017") +
  theme(legend.title = element_text(size = 18),
        plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  geom_hline(yintercept=0, linetype="dashed")

mc23_3_plot_1
###cluster 2
mc23_3_plot_2 <- compare_wr %>% filter(cluster == 2) %>% select(c(receiving_yards, receiving_tds,
                                                                  Targets,
                                                                  Attempts, Carries, Total_Raw_YAC,
                                                                  clutch_epa_rec, rushing_yards, rushing_tds)) %>%
  gather() %>% ggplot(aes(x = reorder(key, -value, median), y = value))  + ylim(-2.5,2.5) +
  geom_boxplot(fill = c("firebrick1", "firebrick1", "firebrick1", "firebrick1", "firebrick1",
                        "firebrick1","firebrick1","firebrick1","firebrick1")) +
  ylim(-2,2) + 
  labs(x = "Statistics",y = "Standardized Value",
       title = "Wide Receiver Cluster 2",
       subtitle = "National Football League 2009-2017") +
  theme(legend.title = element_text(size = 18),
        plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  geom_hline(yintercept=0, linetype="dashed")

mc23_3_plot_2
###cluster 3
mc23_3_plot_3 <- compare_wr %>% filter(cluster == 3) %>% select(c(receiving_yards, receiving_tds,
                                                                  Targets,
                                                                  Attempts, Carries, Total_Raw_YAC,
                                                                  clutch_epa_rec, rushing_yards, rushing_tds)) %>%
  gather() %>% ggplot(aes(x = reorder(key, -value, median), y = value))  + ylim(-2.5,2.5) +
  geom_boxplot(fill = c("springgreen", "springgreen", "springgreen", "firebrick1", "firebrick1",
                        "firebrick1","firebrick1","firebrick1","firebrick1")) +
  ylim(-2,2) + 
  labs(x = "Statistics",y = "Standardized Value",
       title = "Wide Receiver Cluster 3",
       subtitle = "National Football League 2009-2017") +
  theme(legend.title = element_text(size = 18),
        plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  geom_hline(yintercept=0, linetype="dashed")

mc23_3_plot_3
###cluster 4
mc23_3_plot_4 <- compare_wr %>% filter(cluster == 4) %>% select(c(receiving_yards, receiving_tds,
                                                                  Targets,
                                                                  Attempts, Carries, Total_Raw_YAC,
                                                                  clutch_epa_rec, rushing_yards, rushing_tds)) %>%
  gather() %>% ggplot(aes(x = reorder(key, -value, median), y = value))  + ylim(-2.5,2.5) +
  geom_boxplot(fill = c("springgreen", "springgreen", "springgreen", "springgreen", "springgreen",
                        "firebrick1","firebrick1","firebrick1","firebrick1")) +
  ylim(-2,2) + 
  labs(x = "Statistics",y = "Standardized Value",
       title = "Wide Receiver Cluster 4",
       subtitle = "National Football League 2009-2017") +
  theme(legend.title = element_text(size = 18),
        plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  geom_hline(yintercept=0, linetype="dashed")

mc23_3_plot_4

###cluster 5
mc23_3_plot_5 <- compare_wr %>% filter(cluster == 5) %>% select(c(receiving_yards, receiving_tds,
                                                                  Targets,
                                                                  Attempts, Carries, Total_Raw_YAC,
                                                                  clutch_epa_rec, rushing_yards, rushing_tds)) %>%
  gather() %>% ggplot(aes(x = reorder(key, -value, median), y = value))  + ylim(-2.5,2.5) +
  geom_boxplot(fill = c("firebrick1", "firebrick1", "firebrick1", "firebrick1", "firebrick1",
                        "firebrick1","firebrick1","firebrick1","firebrick1")) +
  ylim(-2,2) + 
  labs(x = "Statistics",y = "Standardized Value",
       title = "Wide Receiver Cluster 5",
       subtitle = "National Football League 2009-2017") +
  theme(legend.title = element_text(size = 18),
        plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  geom_hline(yintercept=0, linetype="dashed")

mc23_3_plot_5

###cluster 6
mc23_3_plot_6 <- compare_wr %>% filter(cluster == 6) %>% select(c(receiving_yards, receiving_tds,
                                                                  Targets,
                                                                  Receptions, Total_Raw_YAC,
                                                                  clutch_epa_rec, rushing_yards, rushing_tds)) %>%
  gather() %>% ggplot(aes(x = reorder(key, -value, median), y = value))  + ylim(-2.5,2.5) +
  geom_boxplot(fill = c("springgreen", "springgreen", "springgreen", "springgreen", "springgreen",
                        "springgreen","firebrick1","firebrick1")) +
  ylim(-2,2) + 
  labs(x = "Statistics",y = "Standardized Value",
       title = "Wide Receiver Cluster 5",
       subtitle = "National Football League 2009-2017") +
  theme(legend.title = element_text(size = 18),
        plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16))

mc23_3_plot_6

###cluster 7

mc23_3_plot_7 <- compare_wr %>% filter(cluster == 7) %>% select(c(receiving_yards, receiving_tds,
                                                                Targets,
                                                                Receptions, Total_Raw_YAC,
                                                                clutch_epa_rec, rushing_yards, rushing_tds)) %>%
  gather() %>% ggplot(aes(x = reorder(key, -value, median), y = value))  + ylim(-2.5,2.5) +
  geom_boxplot(fill = c("firebrick1", "firebrick1", "firebrick1", "firebrick1", "firebrick1",
                        "firebrick1","firebrick1","firebrick1")) +
  ylim(-2,2) + 
  labs(x = "Statistics",y = "Standardized Value",
       title = "Wide Receiver Cluster 7",
       subtitle = "National Football League 2009-2017") +
  theme(legend.title = element_text(size = 18),
        plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16))

mc23_3_plot_7

###Quarterbacks

###cluster 1

mc23_qb_plot_1 <- compare_qb %>% filter(cluster == 1) %>% select(c(passing_tds, passing_yards,
                                                                  rushing_yards,
                                                                  rushing_tds, Attempts, Targets,
                                                                  Carries, total_clutch_epa_qb, Interceptions)) %>%
  gather() %>% ggplot(aes(x = reorder(key, -value, median), y = value))  + ylim(-2.5,2.5) +
  geom_boxplot(fill = c("firebrick1", "firebrick1", "firebrick1", "firebrick1", "firebrick1",
                        "firebrick1","firebrick1","firebrick1","firebrick1")) +
  ylim(-2,2) + 
  labs(x = "Statistics",y = "Standardized Value",
       title = "Quarterback Cluster 1",
       subtitle = "National Football League 2009-2017") +
  theme(legend.title = element_text(size = 18),
        plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  geom_hline(yintercept=0, linetype="dashed") 

mc23_qb_plot_1

###cluster 2
mc23_qb_plot_2 <- compare_qb %>% filter(cluster == 2) %>% select(c(passing_tds, passing_yards,
                                                                   rushing_yards,
                                                                   rushing_tds, Attempts, Targets,
                                                                   Carries, total_clutch_epa_qb, Interceptions)) %>%
  gather() %>% ggplot(aes(x = reorder(key, -value, median), y = value))  + ylim(-2.5,2.5) +
  geom_boxplot(fill = c("firebrick1", "firebrick1", "firebrick1", "firebrick1", "firebrick1",
                        "firebrick1","firebrick1","firebrick1","firebrick1")) +
  ylim(-2,2) + 
  labs(x = "Statistics",y = "Standardized Value",
       title = "Quarterback Cluster 2",
       subtitle = "National Football League 2009-2017") +
  theme(legend.title = element_text(size = 18),
        plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  geom_hline(yintercept=0, linetype="dashed")

mc23_qb_plot_2

###cluster 3
mc23_qb_plot_3 <- compare_qb %>% filter(cluster == 3) %>% select(c(passing_tds, passing_yards,
                                                                   rushing_yards,
                                                                   rushing_tds, Attempts, Targets,
                                                                   Carries, total_clutch_epa_qb, Interceptions)) %>%
  gather() %>% ggplot(aes(x = reorder(key, -value, median), y = value))  + ylim(-2.5,2.5) +
  geom_boxplot(fill = c("firebrick1", "firebrick1", "firebrick1", "firebrick1", "firebrick1",
                        "firebrick1","firebrick1","firebrick1","firebrick1")) +
  ylim(-2,2) + 
  labs(x = "Statistics",y = "Standardized Value",
       title = "Quarterback Cluster 3",
       subtitle = "National Football League 2009-2017") +
  theme(legend.title = element_text(size = 18),
        plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  geom_hline(yintercept=0, linetype="dashed")

mc23_qb_plot_3

###cluster 4
mc23_qb_plot_4 <- compare_qb %>% filter(cluster == 4) %>% select(c(passing_tds, passing_yards,
                                                                   rushing_yards,
                                                                   rushing_tds, Attempts, Targets,
                                                                   Carries, total_clutch_epa_qb, Interceptions)) %>%
  gather() %>% ggplot(aes(x = reorder(key, -value, median), y = value))  + ylim(-2.5,2.5) +
  geom_boxplot(fill = c("springgreen", "springgreen", "firebrick1", "firebrick1", "firebrick1",
                        "firebrick1","firebrick1","firebrick1","firebrick1")) +
  ylim(-2,2) + 
  labs(x = "Statistics",y = "Standardized Value",
       title = "Quarterback Cluster 4",
       subtitle = "National Football League 2009-2017") +
  theme(legend.title = element_text(size = 18),
        plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  geom_hline(yintercept=0, linetype="dashed")

mc23_qb_plot_4

###cluster 5
mc23_qb_plot_5 <- compare_qb %>% filter(cluster == 5) %>% select(c(passing_tds, passing_yards,
                                                                   rushing_yards,
                                                                   rushing_tds, Attempts, Targets,
                                                                   Carries, total_clutch_epa_qb, Interceptions)) %>%
  gather() %>% ggplot(aes(x = reorder(key, -value, median), y = value))  + ylim(-2.5,2.5) +
  geom_boxplot(fill = c("springgreen", "springgreen", "springgreen", "springgreen", "springgreen",
                        "springgreen","springgreen","springgreen","firebrick1")) +
  ylim(-2,2) + 
  labs(x = "Statistics",y = "Standardized Value",
       title = "Quarterback Cluster 5",
       subtitle = "National Football League 2009-2017") +
  theme(legend.title = element_text(size = 18),
        plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  geom_hline(yintercept=0, linetype="dashed")

mc23_qb_plot_5

###running backs

###cluster 1
mc23_hb_plot_1 <- compare_hb %>% filter(cluster == 1) %>% select(c(Carries, rushing_yards,
                                                                   rushing_tds,
                                                                   total_clutch_epa_hb, Targets,
                                                                   rec_yards, rec_tds, TD_per_Car, Yards_per_Car, Attempts)) %>%
  gather() %>% ggplot(aes(x = reorder(key, -value, median), y = value))  + ylim(-2.5,2.5) +
  geom_boxplot(fill = c("firebrick1", "firebrick1", "firebrick1", "firebrick1", "firebrick1",
                        "firebrick1","firebrick1","firebrick1","firebrick1", "firebrick1")) +
  ylim(-2,2) + 
  labs(x = "Statistics",y = "Standardized Value",
       title = "Running Back Cluster 1",
       subtitle = "National Football League 2009-2017") +
  theme(legend.title = element_text(size = 18),
        plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  geom_hline(yintercept=0, linetype="dashed")

mc23_hb_plot_1

###cluster 2
mc23_hb_plot_2 <- compare_hb %>% filter(cluster == 2) %>% select(c(Carries, rushing_yards,
                                                                   rushing_tds,
                                                                   total_clutch_epa_hb, Targets,
                                                                   rec_yards, rec_tds, TD_per_Car, Yards_per_Car, Attempts)) %>%
  gather() %>% ggplot(aes(x = reorder(key, -value, median), y = value))  + ylim(-2.5,2.5) +
  geom_boxplot(fill = c("springgreen", "springgreen", "springgreen", "springgreen", "springgreen",
                        "springgreen","springgreen","springgreen","springgreen", "firebrick1")) +
  ylim(-2,2) + 
  labs(x = "Statistics",y = "Standardized Value",
       title = "Running Back Cluster 2",
       subtitle = "National Football League 2009-2017") +
  theme(legend.title = element_text(size = 18),
        plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  geom_hline(yintercept=0, linetype="dashed")

mc23_hb_plot_2

###cluster 3
mc23_hb_plot_3 <- compare_hb %>% filter(cluster == 3) %>% select(c(Carries, rushing_yards,
                                                                   rushing_tds,
                                                                   total_clutch_epa_hb, Targets,
                                                                   rec_yards, rec_tds, TD_per_Car, Yards_per_Car, Attempts)) %>%
  gather() %>% ggplot(aes(x = reorder(key, -value, median), y = value))  + ylim(-2.5,2.5) +
  geom_boxplot(fill = c("springgreen", "firebrick1", "firebrick1", "firebrick1", "firebrick1",
                        "firebrick1","firebrick1","firebrick1","firebrick1", "firebrick1")) +
  ylim(-2,2) + 
  labs(x = "Statistics",y = "Standardized Value",
       title = "Running Back Cluster 3",
       subtitle = "National Football League 2009-2017") +
  theme(legend.title = element_text(size = 18),
        plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  geom_hline(yintercept=0, linetype="dashed")

mc23_hb_plot_3

###cluster 4
mc23_hb_plot_4 <- compare_hb %>% filter(cluster == 4) %>% select(c(Carries, rushing_yards,
                                                                   rushing_tds,
                                                                   total_clutch_epa_hb, Targets,
                                                                   rec_yards, rec_tds, TD_per_Car, Yards_per_Car, Attempts)) %>%
  gather() %>% ggplot(aes(x = reorder(key, -value, median), y = value))  + ylim(-2.5,2.5) +
  geom_boxplot(fill = c("springgreen", "firebrick1", "firebrick1", "firebrick1", "firebrick1",
                        "firebrick1","firebrick1","firebrick1","firebrick1", "firebrick1")) +
  ylim(-2,2) + 
  labs(x = "Statistics",y = "Standardized Value",
       title = "Running Back Cluster 4",
       subtitle = "National Football League 2009-2017") +
  theme(legend.title = element_text(size = 18),
        plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  geom_hline(yintercept=0, linetype="dashed")

mc23_hb_plot_4

###cluster 5
mc23_hb_plot_5 <- compare_hb %>% filter(cluster == 5) %>% select(c(Carries, rushing_yards,
                                                                   rushing_tds,
                                                                   total_clutch_epa_hb, Targets,
                                                                   rec_yards, rec_tds, TD_per_Car, Yards_per_Car, Attempts)) %>%
  gather() %>% ggplot(aes(x = reorder(key, -value, median), y = value))  + ylim(-2.5,2.5) +
  geom_boxplot(fill = c("springgreen", "springgreen", "springgreen", "springgreen", "springgreen",
                        "springgreen","springgreen","springgreen", "firebrick1")) +
  ylim(-2,2) + 
  labs(x = "Statistics",y = "Standardized Value",
       title = "Running Back Cluster 5",
       subtitle = "National Football League 2009-2017") +
  theme(legend.title = element_text(size = 18),
        plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  geom_hline(yintercept=0, linetype="dashed")

mc23_hb_plot_5

###count of players in each cluster

count_wr <- receiver_data %>% count(cluster)
count_qb <- qb_data %>% count(cluster)
count_hb <- hb_data %>% count(cluster)

wr_count <- ggplot(count_wr, aes(x = cluster, y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.2) + ylab("count") + ggtitle("Number of Players per Cluster in Wide Receiver Dataset")


wr_count + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

qb_count <- ggplot(count_qb, aes(x = cluster, y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.2) + ylab("count") + ggtitle("Number of Players per Cluster in Quarterback Dataset")


qb_count + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

hb_count <- ggplot(count_hb, aes(x = cluster, y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.2) + ylab("count") + ggtitle("Number of Players per Cluster in Running Back Dataset")


hb_count + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

###hopkins test
library(clustertend)

#Null hypothesis: the data set D is uniformly distributed (i.e., no meaningful clusters)
#Alternative hypothesis: the data set D is not uniformly distributed (i.e., contains meaningful clusters)

#We can conduct the Hopkins Statistic test iteratively, 
#using 0.5 as the threshold to reject the alternative hypothesis.
#That is, if H < 0.5, then it is unlikely that data set has statistically significant clusters.

res <- get_clust_tendency(scaled, n = nrow(scaled)-1,
                          graph = FALSE)
res$hopkins_stat

hopkins(scaled, n = nrow(scaled)-1, byrow = F, header = F)

#Hopkins Stat on Receiver Data
0.970285
#Hopkins Stat on QB Data
0.9004466
#Hopkins Stat on HB Data
0.9409178

