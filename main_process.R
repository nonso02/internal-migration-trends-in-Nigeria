library(ggplot2)
library("ggrepel")
library(ggthemes)
library(foreign)
library(dplyr)
#library(rgdal)
library(foreach)
library(igraph)
library(wesanderson)

#Set working directory
setwd("C:/Users/nonso.obikili/OneDrive - United Nations/Data/Migration MICS")

#Load Relevant MICS files for 2021
#Downloaded SPSS Datasets in Folder -
#"Nigeria MICS6 SPSS Datasets"

#Households
hh <- read.spss("Nigeria MICS6 SPSS Datasets/hh.sav")
hh <- as.data.frame(hh)

#Household members
hl <- read.spss("Nigeria MICS6 SPSS Datasets/hl.sav")
hl <- as.data.frame(hl)

#women 15 to 49
wm <- read.spss("Nigeria MICS6 SPSS Datasets/wm.sav")
wm <- as.data.frame(wm)

#men 15 to 49
mn <- read.spss("Nigeria MICS6 SPSS Datasets/mn.sav")
mn <- as.data.frame(mn)


#Load other relevant state data
statedata <- read.csv("StateData.csv")

#load dataset to match state names in state data to mics
ccamatch <- read.csv("ccamatch.csv")

#Set parameters for images resolutions and dimensions
#image parameters
res <- 200
w <- 10
h <- 7


#export first 100 observations for IOM
#hhsmall <- head(hh, 100)
#write.csv(hhsmall, "hh_small.csv")

#hlsmall <- head(hl, 100)
#write.csv(hlsmall, "hl_small.csv")

#wmsmall <- head(wm, 100)
#write.csv(wmsmall, "wm_small.csv")

#mnsmall <- head(mn, 100)
#write.csv(mnsmall, "mn_small.csv")

#Filter datasets for only relevant data
#hhlong <- hh[c("HH1", "HH2", "HH6", "HH7", "HH14", "HH15", "HH16")]
#write.csv(hhlong, "hhlong.csv")

#hllong <- hl[c("HH1", "HH2", "HL1", "HL3", "HL4", "HL5Y", "HL6", "HL15", "HL19")] #nolint
#write.csv(hllong, "hllong.csv")

#Link men and women to household data
#wmlong <- wm[c("HH1", "HH2", "WB15", "WB16", "WB17")]
#write.csv(wmlong, "wmlong.csv")

#mnlong <- mn[c("HH1", "HH2", "MWB15", "MWB16", "MWB17")]
#write.csv(mnlong, "mnlong.csv")

#Important manipulations
#Load state maps
#shp <- readOGR(dsn = file.path("./adm1_STATES", "NGA_adm1.shp"), stringsAsFactors = F) #nolint


#plot map
#map <- ggplot() +
#        geom_polygon(
#          data = shp,
#          aes(x = long, y = lat, group = group),
#          colour = "black",
#          fill = NA
#          ) +
#        theme_void()
#map

###########################################
#useful functions

cp <- function(inp, tot){
  t <- (inp / tot) * 100
  t <- round(t, digits = 2)
  return(t)
}

#####################################################
#produce relevant data / graphs.

#1. Migration Status broken down by years
m1 <- NROW(mn[which(mn$MWB15 == "ALWAYS / SINCE BIRTH"), ])
mn$MWB15b <- as.character(mn$MWB15)
mn$MWB15b <- as.numeric(mn$MWB15b)
m2 <- NROW(mn[which(mn$MWB15b >= 0 & mn$MWB15b < 1), ])
m3 <- NROW(mn[which(mn$MWB15b >= 1 & mn$MWB15b <= 4), ])
m4 <- NROW(mn[which(mn$MWB15b >= 5 & mn$MWB15b <= 9), ])
m5 <- NROW(mn[which(mn$MWB15b >= 10), ])
summ <- NROW(mn)



w1 <- NROW(wm[which(wm$WB15 == "ALWAYS / SINCE BIRTH"), ])
wm$WB15b <- as.character(wm$WB15)
wm$WB15b <- as.numeric(wm$WB15b)
w2 <- NROW(wm[which(wm$WB15b >= 0 & wm$WB15b < 1), ])
w3 <- NROW(wm[which(wm$WB15b >= 1 & wm$WB15b <= 4), ])
w4 <- NROW(wm[which(wm$WB15b >= 5 & wm$WB15b <= 9), ])
w5 <- NROW(wm[which(wm$WB15b >= 10), ])
sumw <- NROW(wm)

df <- data.frame(
        Sex = rep(c("Women", "Men"), each = 5),
        since = rep(c(
          "Never",
          "Less than a year",
          "1 to 4 years",
          "5 to 9 years",
          "10 years and more"
          ), times = 2),
        order = rep(c(1, 2, 3, 4, 5), times = 2),
        numb = c(
          cp(w1, sumw),
          cp(w2, sumw),
          cp(w3, sumw),
          cp(w4, sumw),
          cp(w5, sumw),
          cp(m1, summ),
          cp(m2, summ),
          cp(m3, summ),
          cp(m4, summ),
          cp(m5, summ)
          )
        )
View(df)
p <- ggplot(df, aes(fill = Sex, y = numb, x = reorder(since, order))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(
    aes(label = numb),
    vjust = -1,
    position = position_dodge(width = 0.9)) +
  labs(x = "Years since last migration", y = "Percent of respondents") +
  scale_fill_manual(
    values = wes_palette(n = 2, name = "Zissou1")) +
  theme_tufte()
plot(p)
#save plot
jpeg(
  "graphs/perc_migrated.jpeg",
  units = "in",
  width = w,
  height = h,
  res = res
  )
plot(p)
dev.off()

#delete temporary variables.
rm(w1, w2, w3, w4, w5, m1, m2, m3, m4, m5, summ, sumw)


#2 Overall percent of migrants who moved within their states

#total women
tw <- wm[which(wm$WB15 != "ALWAYS / SINCE BIRTH"), ]
tw$WB17 <- as.character(tw$WB17)
tw$WB17[tw$WB17 == "AKWA-IBOM"] <- "AKWA IBOM" #fix

pws <- (NROW(tw[which(tw$WB17 == tw$HH7), ]) / NROW(tw)) * 100
print(paste("Percent women moved within same state:", (round(pws, digits = 2))))

#total men
tm <- mn[which(mn$MWB15 != "ALWAYS / SINCE BIRTH"), ]
tm$MWB17 <- as.character(tm$MWB17)
tm$MWB17[tm$MWB17 == "AKWA-IBOM"] <- "AKWA IBOM" #fix

pms <- (NROW(tm[which(tm$MWB17 == tm$HH7), ]) / NROW(tm)) * 100
print(paste("Percent women moved within same state:", (round(pms, digits = 2))))


##################
# Replicate Figure 2 - migrated from town or city etc

#men
m1 <- NROW(mn[which(mn$MWB15 == "ALWAYS / SINCE BIRTH"), ])
m2 <- NROW(mn[which(mn$MWB16 == "RURAL AREA"), ])
m3 <- NROW(mn[which(mn$MWB16 == "TOWN"), ])
m4 <- NROW(mn[which(mn$MWB16 == "CITY"), ])
summ <- NROW(mn)


#women
w1 <- NROW(wm[which(wm$WB15 == "ALWAYS / SINCE BIRTH"), ])
w2 <- NROW(wm[which(wm$WB16 == "RURAL AREA"), ])
w3 <- NROW(wm[which(wm$WB16 == "TOWN"), ])
w4 <- NROW(wm[which(wm$WB16 == "CITY"), ])
sumw <- NROW(wm)

df <- data.frame(
        Sex = rep(c("Women", "Men"), each = 4),
        since = rep(c(
          "Never",
          "Rural area",
          "Town",
          "City"
          ), times = 2),
        order = rep(c(1, 2, 3, 4), times = 2),
        numb = c(
          cp(w1, sumw),
          cp(w2, sumw),
          cp(w3, sumw),
          cp(w4, sumw),
          cp(m1, summ),
          cp(m2, summ),
          cp(m3, summ),
          cp(m4, summ)
          )
        )
View(df)

p <- ggplot(df, aes(fill = Sex, y = numb, x = reorder(since, order))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(
    aes(label = numb),
    vjust = -1,
    position = position_dodge(width = 0.9)) +
  labs(x = "Migrated from", y = "Percent of respondents") +
  scale_fill_manual(
    values = wes_palette(n = 2, name = "Zissou1")) +
  theme_tufte()
plot(p)
#save plot
jpeg("graphs/migrated_from.jpeg", units = "in", width = w, height = h, res = res)
plot(p)
dev.off()
rm(w1, w2, w3, w4, m1, m2, m3, m4, summ, sumw)


##############################################
#Distribution of age migrated

#remove never migrated
#women
tw <- wm[which(wm$WB15 != "ALWAYS / SINCE BIRTH"), ]
tw$WB17 <- as.character(tw$WB17)
tw$WB17[tw$WB17 == "AKWA-IBOM"] <- "AKWA IBOM" #fix
tw <- tw[which(tw$WB17 != tw$HH7), ]
tw$WB15 <- as.character(tw$WB15)
tw$WB15 <- as.numeric(tw$WB15)
tw$WB4 <- as.character(tw$WB4)
tw$WB4 <- as.numeric(tw$WB4)
tw$aom <- tw$WB4 - tw$WB15
tw$Sex <- "WOMEN"
hist(tw$aom)

#get percent of migrants by age of migration in five year batches
pw <- NROW(tw)
w1 <- NROW(tw[which(tw$aom < 10), ]) #15 to 20
w2 <- NROW(tw[which(tw$aom >= 11 & tw$aom <= 17), ]) #21 to 25
w3 <- NROW(tw[which(tw$aom >= 18 & tw$aom <= 24), ]) #26 to 30
w4 <- NROW(tw[which(tw$aom >= 25 & tw$aom <= 31), ]) #31 to 35
w5 <- NROW(tw[which(tw$aom >= 32 & tw$aom <= 38), ]) #36 to 40
w6 <- NROW(tw[which(tw$aom >= 39 & tw$aom <= 45), ]) #41 to 45
w7 <- NROW(tw[which(tw$aom >= 46), ]) #46 plus

#men
tm <- mn[which(mn$MWB15 != "ALWAYS / SINCE BIRTH"), ]
tm$MWB17 <- as.character(tm$MWB17)
tm$MWB17[tm$MWB17 == "AKWA-IBOM"] <- "AKWA IBOM" #fix
tm <- tm[which(tm$MWB17 != tm$HH7), ]
tm$MWB15 <- as.character(tm$MWB15)
tm$MWB15 <- as.numeric(tm$MWB15)
#tm$MWB4 <- as.character(tm$MWB4)
#tm$MWB4 <- as.numeric(tm$MWB4)
tm$aom <- tm$MWB4 - tm$MWB15
tm$Sex <- "MEN"
hist(tm$aom)

#get percent of migrants by age of migration in five year batches
pm <- NROW(tm)
m1 <- NROW(tm[which(tm$aom < 10), ]) #15 to 20
m2 <- NROW(tm[which(tm$aom >= 11 & tm$aom <= 17), ]) #21 to 25
m3 <- NROW(tm[which(tm$aom >= 18 & tm$aom <= 24), ]) #26 to 30
m4 <- NROW(tm[which(tm$aom >= 25 & tm$aom <= 31), ]) #31 to 35
m5 <- NROW(tm[which(tm$aom >= 32 & tm$aom <= 38), ]) #36 to 40
m6 <- NROW(tm[which(tm$aom >= 39 & tm$aom <= 45), ]) #41 to 45
m7 <- NROW(tm[which(tm$aom >= 46), ]) #46 plus

#combine
cm <- rbind(
  tw[c("aom", "Sex")],
  tm[c("aom", "Sex")]
)

#plot by Sex
p <- ggplot(cm, aes(aom, fill = Sex)) +
    geom_histogram(position = "dodge", binwidth = 2) +
    labs(x = "Age of last migration", y = "Density") +
    scale_fill_manual(
    values = wes_palette(n = 2, name = "Zissou1")) +
    theme_tufte() +
    theme(legend.position = "bottom")
plot(p)

jpeg("graphs/age_of_migration.jpeg", units = "in", width = w, height = h, res = res) #nolint
plot(p)
dev.off()

#Percent distributions
df <- data.frame(
        Sex = rep(c("Women", "Men"), each = 7),
        since = rep(c(
          "Less than 10",
          "11 to 17",
          "18 to 24",
          "25 to 31",
          "32 to 38",
          "39 to 45",
          "46 plus"
          ), times = 2),
        order = rep(c(1, 2, 3, 4, 5, 6, 7), times = 2),
        numb = c(
          cp(w1, pw),
          cp(w2, pw),
          cp(w3, pw),
          cp(w4, pw),
          cp(w5, pw),
          cp(w6, pw),
          cp(w7, pw),
          cp(m1, pm),
          cp(m2, pm),
          cp(m3, pm),
          cp(m4, pm),
          cp(m5, pm),
          cp(m6, pm),
          cp(m7, pm)
          )
        )
View(df)

p <- ggplot(df, aes(fill = Sex, y = numb, x = reorder(since, order))) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Age of Migration", y = "Percent of respondents") +
  geom_text(
    aes(label = numb),
    vjust = -1,
    position = position_dodge(width = 0.9)) +
  scale_fill_manual(
    values = wes_palette(n = 2, name = "Zissou1")) +
  theme_tufte() +
  theme(legend.position = "bottom")
plot(p)

#save plot
jpeg("graphs/age_of_migration_perc.jpeg", units = "in", width = w, height = h, res = res)
plot(p)
dev.off()



rm(tw, tm, cm)

####################
#Age distribution of migrants vs non-migrants

#remove never migrated
#women
tw <- wm[which(wm$WB15 != "ALWAYS / SINCE BIRTH"), ]
tw$Status <- "MIGRANTS"
tw$Age <- tw$WB4
pt <- NROW(tw)
p1 <- NROW(tw[which(tw$WB4 < 18), ]) #percent less than 17
p1 <- cp(p1, pt)

p2 <- NROW(tw[which(tw$WB4 >= 18 & tw$WB4 <= 21), ]) #percent between 21 and 30
p2 <- cp(p2, pt)

p3 <- NROW(tw[which(tw$WB4 > 21 & tw$WB4 <= 30), ]) #percent between 31 and 40
p3 <- cp(p3, pt)

p4 <- NROW(tw[which(tw$WB4 > 30 & tw$WB4 <= 40), ]) #percent between 31 and 40
p4 <- cp(p4, pt)

p5 <- NROW(tw[which(tw$WB4 > 40), ]) #percent 41 and above
p5 <- cp(p5, pt)



tw2 <- wm[which(wm$WB15 == "ALWAYS / SINCE BIRTH"), ]
tw2$Status <- "NEVER MIGRANTED"
tw2$Age <- tw2$WB4
ptb <- NROW(tw2)
p1b <- NROW(tw2[which(tw2$WB4 < 18), ]) #percent less than 18
p1b <- cp(p1b, ptb)

p2b <- NROW(tw2[which(tw2$WB4 >= 18 & tw2$WB4 <= 21), ]) #percent between 21 and 30
p2b <- cp(p2b, ptb)

p3b <- NROW(tw2[which(tw2$WB4 > 21 & tw2$WB4 <= 30), ]) #percent between 31 and 40
p3b <- cp(p3b, ptb)

p4b <- NROW(tw2[which(tw2$WB4 > 30 & tw2$WB4 <= 40), ]) #percent between 31 and 40
p4b <- cp(p4b, ptb)

p5b <- NROW(tw2[which(tw2$WB4 > 40), ]) #percent 41 and above
p5b <- cp(p5b, ptb)

#create dataframe

df <- data.frame(
        Status = rep(c("Migrated", "Never Migrated"), each = 5),
        since = rep(c(
          "Less than 18",
          "18 to 21",
          "21 to 30",
          "31 to 40",
          "Older then 40"
          ), times = 2),
        order = rep(c(1, 2, 3, 4, 5), times = 2),
        numb = c(
          p1,
          p2,
          p3,
          p4,
          p5,
          p1b,
          p2b,
          p3b,
          p4b,
          p5b
          )
        )
View(df)

p <- ggplot(df, aes(fill = Status, y = numb, x = reorder(since, order))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(
    aes(label = numb),
    vjust = -1,
    position = position_dodge(width = 0.9)) +
  labs(x = "Age", y = "Percent of respondents") +
  scale_fill_manual(
    values = wes_palette(n = 2, name = "Zissou1")) +
  theme_tufte() +
  theme(legend.position = "bottom")
plot(p)

#save plot
jpeg("graphs/age_wom_migvnon.jpeg", units = "in", width = w, height = h, res = res)
plot(p)
dev.off()

#men
tm <- mn[which(mn$MWB15 != "ALWAYS / SINCE BIRTH"), ]
tm$Status <- "MIGRANTS"
tm$Age <- tm$MWB4

pt <- NROW(tm)
p1 <- NROW(tm[which(tm$MWB4 < 18), ]) #percent less than 21
p1 <- cp(p1, pt)

p2 <- NROW(tm[which(tm$MWB4 >= 18 & tm$MWB4 <= 21), ]) #percent between 21 and 30
p2 <- cp(p2, pt)

p3 <- NROW(tm[which(tm$MWB4 > 21 & tm$MWB4 <= 30), ]) #percent between 31 and 40
p3 <- cp(p3, pt)

p4 <- NROW(tm[which(tm$MWB4 > 30 & tm$MWB4 <= 40), ]) #percent between 31 and 40
p4 <- cp(p4, pt)

p5 <- NROW(tm[which(tm$MWB4 >= 41), ]) #percent 41 and above
p5 <- cp(p5, pt)



tm2 <- mn[which(mn$MWB15 == "ALWAYS / SINCE BIRTH"), ]
tm2$Status <- "NEVER MIGRANTED"
tm2$Age <- tm2$MWB4
ptb <- NROW(tm2)
p1b <- NROW(tm2[which(tm2$MWB4 < 18), ]) #percent less than 21
p1b <- cp(p1b, ptb)

p2b <- NROW(tm2[which(tm2$MWB4 >= 18 & tm2$MWB4 <= 21), ]) #percent between 21 and 30
p2b <- cp(p2b, ptb)

p3b <- NROW(tm2[which(tm2$MWB4 > 21 & tm2$MWB4 <= 30), ]) #percent between 31 and 40
p3b <- cp(p3b, ptb)

p4b <- NROW(tm2[which(tm2$MWB4 > 31 & tm2$MWB4 <= 40), ]) #percent between 31 and 40
p4b <- cp(p4b, ptb)

p5b <- NROW(tm2[which(tm2$MWB4 >= 41), ]) #percent 41 and above
p5b <- cp(p5b, ptb)

#create dataframe

df <- data.frame(
        Status = rep(c("Migrated", "Never Migrated"), each = 5),
        since = rep(c(
          "Less than 18",
          "18 to 21",
          "21 to 30",
          "31 to 40",
          "Older then 40"
          ), times = 2),
        order = rep(c(1, 2, 3, 4, 5), times = 2),
        numb = c(
          p1,
          p2,
          p3,
          p4,
          p5,
          p1b,
          p2b,
          p3b,
          p4b,
          p5b
          )
        )
View(df)

p <- ggplot(df, aes(fill = Status, y = numb, x = reorder(since, order))) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Age", y = "Percent of respondents") +
  geom_text(
    aes(label = numb),
    vjust = -1,
    position = position_dodge(width = 0.9)) +
  scale_fill_manual(
    values = wes_palette(n = 2, name = "Zissou1")) +
  theme_tufte() +
  theme(legend.position = "bottom")
plot(p)

#save plot
jpeg("graphs/age_men_migvnon.jpeg", units = "in", width = w, height = h, res = res)
plot(p)
dev.off()
rm(p1, p2, p3, p4, p1b, p2b, p3b, p4b, pt, ptb, df, tm, tm2, tw, tw2)

##########################################
#Education of Migrants versus non-migrants

#Women
#Migrants vs non Migrants
wm$ms <- ifelse(wm$WB15 == "ALWAYS / SINCE BIRTH", "NEVER MIGRATED", "MIGRANTs")
agg1 <- aggregate(wm$ms, by = list(ms = wm$ms), FUN = "length")
agg2 <- aggregate(wm$ms, by = list(ms = wm$ms, WB6A = wm$WB6A), FUN = "length")
m <- merge(wm[c("ms", "WB6A")], agg1, by = "ms")
m2 <- merge(m, agg2, by = c("ms", "WB6A"))
m2$perc <- (m2$x.y / m2$x.x) * 100
m3 <- aggregate(m2$perc, by = list(Status = m2$ms, WB6A = m2$WB6A), FUN = "mean") #nolint
View(m3)

p <- ggplot(m3, aes(fill = Status, x = WB6A, y = x)) +
      geom_bar(position = "dodge", stat = "identity") +
      geom_text(
    aes(label = round(x, digits = 1)),
    hjust = 0.1,
    position = position_dodge(width = 0.9)) +
      coord_flip() +
      labs(x = "Education", y = "Percent") +
      scale_fill_manual(
    values = wes_palette(n = 2, name = "Zissou1")) +
      theme_tufte() +
      theme(legend.position = "bottom")
plot(p)
#save plot
jpeg("graphs/education_women.jpeg", units = "in", width = w, height = h, res = res)
plot(p)
dev.off()

#Men
mn$ms <- ifelse(mn$MWB15 == "ALWAYS / SINCE BIRTH", "NEVER MIGRATED", "MIGRANTS")
agg1 <- aggregate(mn$ms, by = list(ms = mn$ms), FUN = "length")
agg2 <- aggregate(mn$ms, by = list(ms = mn$ms, MWB6A = mn$MWB6A), FUN = "length")
m <- merge(mn[c("ms", "MWB6A")], agg1, by = "ms")
m2 <- merge(m, agg2, by = c("ms", "MWB6A"))
m2$perc <- (m2$x.y / m2$x.x) * 100
m3 <- aggregate(m2$perc, by = list(Status = m2$ms, MWB6A = m2$MWB6A), FUN = "mean") #nolint
#View(m3)

p <- ggplot(m3, aes(fill = Status, x = MWB6A, y = x)) +
      geom_bar(position = "dodge", stat = "identity") +
      geom_text(
    aes(label = round(x, digits = 1)),
    hjust = 0.1,
    position = position_dodge(width = 0.9)) +
      coord_flip() +
      labs(x = "Education", y = "Percent") +
      scale_fill_manual(
    values = wes_palette(n = 2, name = "Zissou1")) +
      theme_tufte() +
      theme(legend.position = "bottom")
plot(p)
#save plot
jpeg("graphs/education_men.jpeg", units = "in", width = w, height = h, res = res)
plot(p)
dev.off()

####################################################
#function to calculate percent by X


######################################################
# 1. URBAN RURAL MIGRATION TRENDS
#  a. Proportion of men and women who never migrated.
#MEN
#total percent never migrated
temp1 <- mn
temp1$MWB17 <- as.character(temp1$MWB17)
temp1$MWB17[temp1$MWB17 == "AKWA-IBOM"] <- "AKWA IBOM" #fix
temp1$marked <- ifelse(
  temp1$MWB15 == "ALWAYS / SINCE BIRTH",
  1,
  0
  )

#migrated to another state
temp1$marked2 <- ifelse(
  temp1$marked == 0 &
  as.character(temp1$MWB17) != as.character(temp1$HH7),
  1,
  0
)
temp1$marked2[which(is.na(temp1$marked2))] <- 0 #remove NAs

#rural urban within state
temp1$marked3 <- ifelse(
  temp1$MWB15 != "ALWAYS / SINCE BIRTH" &
  #as.character(temp1$MWB17) == as.character(temp1$HH7) &
  as.character(temp1$HH6) == "URBAN" &
  as.character(temp1$MWB16) == "RURAL AREA",
  1,
  0
)
temp1$marked3[which(is.na(temp1$marked3))] <- 0 #remove NAs

#living in rural area
temp1$marked4 <- ifelse(
  as.character(temp1$HH6) == "RURAL",
  1,
  0
)
temp1$marked4[which(is.na(temp1$marked4))] <- 0 #remove NAs


print("Total percent of men never migrated: ")
(sum(na.omit(temp1$marked)) / NROW(temp1)) * 100

#non migrants
#aggregate by state
temp2 <- aggregate(
  marked ~ HH7,
  data = temp1,
  FUN = mean
  )
#plot
#plot it
p <- ggplot(temp2, aes(x = marked*100, y = HH7)) +
  geom_bar(stat = "Identity") +
  theme_tufte()
plot(p)
ggsave("graphs/perc_never_migrated_men.jpeg", p)

#percent migrated out of state or country
#aggregate by state
temp2 <- aggregate(
  marked2 ~ HH7,
  data = temp1,
  FUN = mean
  )
#plot
#plot it
p <- ggplot(temp2, aes(x = marked2 * 100, y = HH7)) +
  geom_bar(stat = "Identity") +
  theme_tufte()
plot(p)
ggsave("graphs/perc_migrated_oos_men.jpeg", p)


#compare to poverty rate
temp3 <- merge(temp2, ccamatch, by = "HH7")
temp3$ID_1 <- temp3$ï..ID_1
temp3 <- merge(temp3, statedata, by = "ID_1")

p <- ggplot(temp3, aes(x = marked2 * 100, y = poverty.rate)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  geom_label_repel(aes(label = state)) +
  labs(
    x = "Percent of migrants from outside state",
    y = "Poverty rate"
  ) +
  theme_tufte()
plot(p)
ggsave("graphs/oos_v_poverty_men.jpeg", p)

#compare to unemployment rate
p <- ggplot(temp3, aes(x = marked2 * 100, y = unemployment)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  geom_label_repel(aes(label = state)) +
  labs(
    x = "Percent of migrants from outside state",
    y = "Unemployment rate"
  ) +
  theme_tufte()
plot(p)
ggsave("graphs/oos_v_unemp_men.jpeg", p)

#percent living in rural areas in state
temp5 <- aggregate(
  marked4 ~ HH7,
  data = temp1[which(!is.na(temp1$marked4)), ],
  FUN = mean
)

#Percent migrated within state from rural to urban
temp2 <- aggregate(
  marked3 ~ HH7,
  data = temp1[which(
    temp1$MWB15 != "ALWAYS / SINCE BIRTH" &
    as.character(temp1$MWB17) == as.character(temp1$HH7)
  ), ],
  FUN = mean
  )
#plot
#plot it
p <- ggplot(temp2, aes(x = marked3 * 100, y = HH7)) +
  geom_bar(stat = "Identity") +
  theme_tufte()
plot(p)
ggsave("graphs/perc_ruralurban_ws_men.jpeg", p)

#compare to rural urban migrant to rural population
temp3 <- merge(temp2, ccamatch, by = "HH7")
temp3$ID_1 <- temp3$ï..ID_1
temp3 <- merge(temp3, statedata, by = "ID_1")

p <- ggplot(temp3, aes(x = marked3 * 100, y = farm.employment.male)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  geom_label_repel(aes(label = state)) +
  labs(
    x = "Percent migrating from rural to urban areas",
    y = "Percent employed in agriculture"
  ) +
  theme_tufte()
plot(p)
ggsave("graphs/rym_v_farmemploy_men.jpeg", p)


#compare to change in unemployment rate

#for each state, where are men who migrated coming from
#unique state
temp3 <- aggregate(
  marked2 ~ HH7 + MWB17,
  data = temp1[which(temp1$marked2 == 1), ],
  FUN = length
  )
View(temp3)
write.csv(temp3, "state_patterns.csv")


################
#NETWORK MAP MEN
y <- temp1[which(
  temp1$marked2 == 1 &
  temp1$MWB17 != "OUTSIDE OF COUNTRY" &
  temp1$MWB17 != "NO RESPONSE"
  ), ]
y <- y[c("HH7", "MWB17")]
View(y)

net <- graph.data.frame(y, directed = T)
V(net) #Unique names
V(net)$label <- V(net)$name

#Weights
V(net)$degree <- degree(net)

hist(V(net)$degree,
  col = 'yellow',
  main = "Histogram of Node Degrees",
  ylab = "Number of NOdes",
  xlab = "Degree of vertices"
)

#plot node maps
set.seed(222)

plot(
  net,
  vertex.color = rainbow(52),
  vertex.size = V(net)$degree * 0.08,
  edge.arrow.size = 0.1,
  #vertex.label.cex = 0.8,
  layout = layout.fruchterman.reingold
)
#ggsave("graphs/network_men.jpeg", last_plot())

#Distrubtion of year of migration
temp1$migyears <- as.numeric(temp1$MWB15)
temp6 <- temp1[which(
  #temp1$marked2 == 1 &
  temp1$migyears <= 30
  ), ]

p <- ggplot() +
      geom_density(
        data = temp6,
        aes(x = migyears),
        color = "BLUE"
      ) +
      geom_density(
        data = temp6[which(temp6$MWB17 == "ADAMAWA"), ],
        aes(x = migyears),
        color = "RED"
      ) +
      geom_density(
        data = temp6[which(temp6$MWB17 == "YOBE"), ],
        aes(x = migyears),
        color = "GREEN"
      ) +
      geom_density(
        data = temp6[which(temp6$MWB17 == "BORNO"), ],
        aes(x = migyears),
        color = "YELLOW"
      )
plot(p)

######################################################################
#################
#WOMEN
#total percent never migrated
temp1 <- wm
temp1$WB17 <- as.character(temp1$WB17)
temp1$WB17[temp1$WB17 == "AKWA-IBOM"] <- "AKWA IBOM" #fix
temp1$marked <- ifelse(
  temp1$WB15 == "ALWAYS / SINCE BIRTH",
  1,
  0
  )

#migrated to another state
temp1$marked2 <- ifelse(
  temp1$marked == 0 &
  as.character(temp1$WB17) != as.character(temp1$HH7),
  1,
  0
)
temp1$marked2[which(is.na(temp1$marked2))] <- 0 #remove NAs

#rural urban within state
temp1$marked3 <- ifelse(
  temp1$WB15 != "ALWAYS / SINCE BIRTH" &
#  as.character(temp1$WB17) == as.character(temp1$HH7) &
  as.character(temp1$HH6) == "URBAN" &
  as.character(temp1$WB16) == "RURAL AREA",
  1,
  0
)
temp1$marked3[which(is.na(temp1$marked3))] <- 0 #remove NAs


print("Total percent of men never migrated: ")
(sum(na.omit(temp1$marked)) / NROW(temp1)) * 100

#non migrants
#aggregate by state
temp2 <- aggregate(
  marked ~ HH7,
  data = temp1,
  FUN = mean
  )
#plot
#plot it
p <- ggplot(temp2, aes(x = marked * 100, y = HH7)) +
  geom_bar(stat = "Identity") +
  theme_tufte()
plot(p)
ggsave("graphs/perc_never_migrated_women.jpeg", p)

#percent migrated out of state or country
#aggregate by state
temp2 <- aggregate(
  marked2 ~ HH7,
  data = temp1,
  FUN = mean
  )
#plot
#plot it
p <- ggplot(temp2, aes(x = marked2 * 100, y = HH7)) +
  geom_bar(stat = "Identity") +
  theme_tufte()
plot(p)
ggsave("graphs/perc_migrated_oos_women.jpeg", p)


#compare to poverty rate
temp3 <- merge(temp2, ccamatch, by = "HH7")
temp3$ID_1 <- temp3$ï..ID_1
temp3 <- merge(temp3, statedata, by = "ID_1")

p <- ggplot(temp3, aes(x = marked2 * 100, y = poverty.rate)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  geom_label_repel(aes(label = state)) +
  labs(
    x = "Percent of migrants from outside state",
    y = "Poverty rate"
  ) +
  theme_tufte()
plot(p)
ggsave("graphs/oos_v_poverty_women.jpeg", p)

#compare to unemployment rate
p <- ggplot(temp3, aes(x = marked2 * 100, y = unemployment)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  geom_label_repel(aes(label = state)) +
  labs(
    x = "Percent of migrants from outside state",
    y = "Unemployment rate"
  ) +
  theme_tufte()
plot(p)
ggsave("graphs/oos_v_unemp_women.jpeg", p)

#Percent migrated within state from rural to urban
temp2 <- aggregate(
  marked3 ~ HH7,
  data = temp1[which(
    temp1$WB15 != "ALWAYS / SINCE BIRTH" &
    as.character(temp1$WB17) == as.character(temp1$HH7)
  ), ],
  FUN = mean
  )
#plot
#plot it
p <- ggplot(temp2, aes(x = marked3 * 100, y = HH7)) +
  geom_bar(stat = "Identity") +
  theme_tufte()
plot(p)
ggsave("graphs/perc_ruralurban_ws_women.jpeg", p)

#compare to farm employment
temp3 <- merge(temp2, ccamatch, by = "HH7")
temp3$ID_1 <- temp3$ï..ID_1
temp3 <- merge(temp3, statedata, by = "ID_1")
temp3 <- temp3[which(temp3$HH7 != "BORNO"), ]

p <- ggplot(temp3, aes(x = marked3 * 100, y = farm.employment.female)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  geom_label_repel(aes(label = state)) +
  labs(
    x = "Percent migrating from rural to urban areas",
    y = "Percent employed in Agriculture"
  ) +
  theme_tufte()
plot(p)
ggsave("graphs/rym_v_farmemploy_women.jpeg", p)


#compare to change in unemployment rate

#for each state, where are men who migrated coming from
#unique state
temp3 <- aggregate(
  marked2 ~ HH7 + WB17,
  data = temp1[which(temp1$marked2 == 1), ],
  FUN = length
  )
View(temp3)
write.csv(temp3, "state_patterns_women.csv")

################
#NETWORK MAP
y <- temp1[which(
  temp1$marked2 == 1 &
  temp1$WB17 != "OUTSIDE OF COUNTRY" &
  temp1$WB17 != "NO RESPONSE"
  ), ]
y <- y[c("HH7", "WB17")]
View(y)

net <- graph.data.frame(y, directed = T)
V(net) #Unique names
V(net)$label <- V(net)$name

#Weights
V(net)$degree <- degree(net)

hist(V(net)$degree,
  col = 'yellow',
  main = "Histogram of Node Degrees",
  ylab = "Number of NOdes",
  xlab = "Degree of vertices"
)

#plot node maps
set.seed(222)

plot(
  net,
  vertex.color = rainbow(52),
  vertex.size = V(net)$degree * 0.04,
  edge.arrow.size = 0.1,
  #vertex.label.cex = 0.8,
  layout = layout.fruchterman.reingold
)
ggsave("graphs/network_women.jpeg", last_plot())


#Year of migration distribution

temp1$migyears <- as.character(temp1$WB15)
temp1$migyears <- as.numeric(temp1$migyears)
temp6 <- temp1[which(
  temp1$marked2 == 1 &
  temp1$migyears <= 30
  ), ]

p <- ggplot() +
      geom_density(
        data = temp6,
        aes(x = migyears),
        color = "BLUE"
      ) +
      geom_density(
        data = temp6[which(temp6$WB17 == "ADAMAWA"), ],
        aes(x = migyears),
        color = "RED"
      ) +
      geom_density(
        data = temp6[which(temp6$WB17 == "YOBE"), ],
        aes(x = migyears),
        color = "GREEN"
      ) +
      geom_density(
        data = temp6[which(temp6$WB17 == "BORNO"), ],
        aes(x = migyears),
        color = "YELLOW"
      ) +
      theme_economist_white()
plot(p)

#calculate distribution by year of migration
d1 <- density(temp6$migyears)
d2b <- density(temp6[which(temp6$WB17 == "BORNO"), ]$migyears)
d2y <- density(temp6[which(temp6$WB17 == "YOBE"), ]$migyears)
d2a <- density(temp6[which(temp6$WB17 == "ADAMAWA"), ]$migyears)



d3b <- d2b$y - d1$y
d3y <- d2y$y - d1$y
d3a <- d2a$y - d1$y

plot(d3b, type = "l", col = "BLUE")
lines(d3y, col = "RED")
lines(d3a, col = "GREEN")

#################################################################
###############################################################
#For households still married.
#Share of households with at least one parent living abroad
#mark as 1 if household is still married and living abroad
#create unique hh identifier
hl$marked <- ifelse(hl$HL15 == "ABROAD" | hl$HL19 == "ABROAD",
  1,
  0
  )
hl$marked <- ifelse(is.na(hl$marked), 0, 1) #remove NA's to avoid zeros

#aggregate by household sum marked.
test1 <- aggregate(marked ~ HH1 + HH2, data = hl, FUN = sum, na.rm = TRUE)

#merge states
test1 <- merge(
  test1,
  hh[c("HH1", "HH2", "HH7")],
  by = c("HH1", "HH2")
  )

#by state count total households
all <- aggregate(marked ~ HH7, data = test1, FUN = length)
#by state count households with marked.
some <- aggregate(
  marked ~ HH7,
  data = test1[which(test1$marked > 0), ],
  FUN = length
  )

#merge to create households with a parent abroad.
abroad <- merge(some, all, by = "HH7")
abroad$abroad <- (abroad$marked.x / abroad$marked.y) * 100

#plot it
p <- ggplot(abroad, aes(x = abroad, y = HH7)) +
  geom_bar(stat = "Identity") +
  theme_tufte()
plot(p)
ggsave("perc_abroad.jpeg", p)


####################################################################
#Share of households with at least one parent not living in the state
hl$marked <- ifelse(hl$HL15 == "IN ANOTHER HOUSEHOLD IN ANOTHER STATE" | hl$HL19 == "IN ANOTHER HOUSEHOLD IN ANOTHER STATE", #nolint
  1,
  0
  )
hl$marked <- ifelse(is.na(hl$marked), 0, 1) #remove NA's to avoid zeros

#aggregate by household sum marked.
test1 <- aggregate(marked ~ HH1 + HH2, data = hl, FUN = sum, na.rm = TRUE)

#merge states
test1 <- merge(
  test1,
  hh[c("HH1", "HH2", "HH7")],
  by = c("HH1", "HH2")
  )

#by state count total households
all <- aggregate(marked ~ HH7, data = test1, FUN = length)
#by state count households with marked.
some <- aggregate(
  marked ~ HH7,
  data = test1[which(test1$marked > 0), ],
  FUN = length
  )

#merge to create households with a parent abroad.
ano_state <- merge(some, all, by = "HH7")
ano_state$ano_state <- (ano_state$marked.x / ano_state$marked.y) * 100

#plot it
p <- ggplot(ano_state, aes(x = ano_state, y = HH7)) +
  geom_bar(stat = "Identity") +
  theme_tufte()
plot(p)
ggsave("perc_another_state.jpeg", p)

#check correlation
corr <- merge(
  abroad[c("HH7", "abroad")],
  ano_state[c("HH7", "ano_state")],
  by = "HH7"
)

p <- ggplot(corr, aes(x = ano_state, y = abroad, label = HH7)) +
  geom_point() +
  geom_label_repel() +
  labs(
    title = "Migrant Household - Internal vs External",
    x = "Percent with one member in another state",
    y = "Percent with one member abroad"
  ) +
  theme_tufte()
plot(p)
ggsave("migrants_int_v_abr.jpeg", p)


#########################################################################
#Examine Irregular migration from BAY States
#| wm$WB17 == "ADAMAWA" | wm$WB17 == "YOBE"
#women - all migration
wm$bay <- ifelse(
              wm$WB17 == "BORNO",
              "BAY",
              "NO"
              )

migsw <- wm[which(wm$WB15 != "ALWAYS / SINCE BIRTH"), ]
migsw$WB17 <- as.character(migsw$WB17)
migsw$WB17[migsw$WB17 == "AKWA-IBOM"] <- "AKWA IBOM" #fix
misgw <- migsw[which(migsw$WB17 != migsw$HH7), ]
ma <- aggregate(
            migsw$bay,
            by = list(Year = migsw$WB15),
            FUN = "length"
            )
migsw <- migsw[which(migsw$bay == "BAY"), ]
mb <- aggregate(
        migsw$bay,
        by = list(Year = migsw$WB15),
        FUN = "length"
)
mm <- merge(ma, mb, by = "Year")
mm$BAY <- (mm$x.y / mm$x.x) * 100
mm$y <- as.numeric(as.character(mm$Year))
mm$Year2 <- 2021 - mm$y

p <- ggplot() +
    geom_bar(
      data = mm[which(mm$Year2 < 2010), ],
      aes(y = BAY, x = Year2),
      stat = "Identity"
      ) +
    geom_bar(
      data = mm[which(mm$Year2 > 2009), ],
      aes(y = BAY, x = Year2),
      stat = "Identity",
      fill = "red"
      ) +
    labs(x = "Year", y = "Percent emigrated out-of-state from Borno") +
    theme_economist_white()
plot(p)
jpeg("graphs/emigration_borno_women.jpeg", units = "in", width = w, height = h, res = res)
plot(p)
dev.off()

#men
mn$bay <- ifelse(
              mn$MWB17 == "BORNO" ,
              "BAY",
              "NO"
              )

migsm <- mn[which(mn$MWB15 != "ALWAYS / SINCE BIRTH"), ]
migsm$MWB17 <- as.character(migsm$MWB17)
migsm$MWB17[migsm$MWB17 == "AKWA-IBOM"] <- "AKWA IBOM" #fix
misgm <- migsm[which(migsm$MWB17 != migsm$HH7), ]
ma <- aggregate(
            migsm$bay,
            by = list(Year = migsm$MWB15),
            FUN = "length"
            )
migsm <- migsm[which(migsm$bay == "BAY"), ]
mb <- aggregate(
        migsm$bay,
        by = list(Year = migsm$MWB15),
        FUN = "length"
)
mm <- merge(ma, mb, by = "Year")
mm$BAY <- (mm$x.y / mm$x.x) * 100
mm$y <- as.numeric(as.character(mm$Year))
mm$Year2 <- 2021 - mm$y

p <- ggplot() +
    geom_bar(
      data = mm[which(mm$Year2 < 2010), ],
      aes(y = BAY, x = Year2),
      stat = "Identity"
      ) +
    geom_bar(
      data = mm[which(mm$Year2 > 2009), ],
      aes(y = BAY, x = Year2),
      stat = "Identity",
      fill = "red"
      ) +
    labs(x = "Year", y = "Percent emigrated out-of-state from Borno") +
    theme_economist_white()
plot(p)
#save plot
jpeg("graphs/emigration_borno_men.jpeg", units = "in", width = w, height = h, res = res)
plot(p)
dev.off()


#children and labour
migsw <- wm[which(wm$WB15 != "ALWAYS / SINCE BIRTH"), ]
migsw$WB17 <- as.character(migsw$WB17)
migsw$WB17[migsw$WB17 == "AKWA-IBOM"] <- "AKWA IBOM" #fix

aws <- aggregate(migsw$WB4, by = list(migsw$HH7), FUN = "length")
migswc <- migsw[which(migsw$WB4 < 19), ]
