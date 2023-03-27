# Leveraging Machine Learning for Bibliometric Analysis of Emerging Fields #
#
# ANALYSIS CODE #


# Libraries ----

library(quanteda)
library(openalexR)
library(bibliometrix)
library(gplots)
library(igraph)
library(dplyr)


# Helper function ----

# get rounded relative frequencies in %
get_shares <- function(x, y){
  round((x/y)*100, 2)
}
  

# Get data ----

# read RDS file
setwd("~/R/issi_conference")
dataset_basic <- read.csv("./ESM/dataset.csv", header = TRUE)
level_1_basic <- read.csv("./ESM/dataset_level_1.csv", header = TRUE)

# code for retrieving the open alex dataset from the basic dataset:
# retrieve metadata from openalex (this takes some time)
# for local puproses here are the retrieved RDS files
# dataset <- readRDS(file = "./dataset/ml_dataset.RDS")
# level_1 <- readRDS(file = "./dataset/search_dataset.RDS")


# dataset is the ML Dataset
dataset <- NULL
temp <- NULL

for (i in 1:length(dataset_basic$DOI)) {
 temp <- oa_fetch(
   doi = dataset_basic$DOI[i],
   entity = "works",
   verbose = TRUE
 )
 dataset <- rbind(dataset, temp)
}

rm(temp)

# level_1 is the Search Dataset
level_1 <- NULL
temp <- NULL

for (i in 1:length(level_1_basic$DOI)) {
  temp <- oa_fetch(
    doi = level_1_basic$DOI[i],
    entity = "works",
    verbose = TRUE
  )
  level_1 <- rbind(level_1, temp)
}
rm(temp)

# convert to bibliometrix format
M <- oa2bibliometrix(dataset)
M_level_1 <- oa2bibliometrix(level_1)


# Descriptives ----

## Publication volume ----

# papers per year
table(dataset$publication_year)
table(level_1$publication_year)

# open access
table(is.na(dataset$is_oa))
table(dataset$is_oa)
get_shares(table(dataset$is_oa), sum(table(dataset$is_oa)))

table(is.na(level_1$is_oa))
table(level_1$is_oa)
get_shares(table(level_1$is_oa), sum(table(level_1$is_oa)))

# omit current year for temporal trends
# Dataset
dataset_no2022 <- dataset[dataset$publication_year < 2022,]
years_min <- min(dataset_no2022$publication_year)
years_max <- max(dataset_no2022$publication_year)

years_factor <- factor(dataset_no2022$publication_year, levels = min(years_min):max(years_max))
years_table <- table(years_factor)
oa_table <- table(dataset_no2022$is_oa, years_factor)


# plot Dataset
# dev.off()
plot(as.numeric(years_table), type = "l", ylab = "Number of Publications", xlab = "Year", xaxt = "n")
axis(1, at = 1:length(years_table), labels = years_min:years_max)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")
grid(NULL, NULL, lty = 3, col = "grey")
lines(as.numeric(years_table), type = "l", lwd = 3, col = "black")
lines(as.numeric(oa_table[2,]), type = "l", lwd = 3, col = "cornflowerblue")
legend("topleft", c("all", "open access"), lwd = 3, cex = 1.2, col = c("black", "cornflowerblue"))

# Level_1
level_1_no2022 <- level_1[level_1$publication_year < 2022,]
years_min <- min(level_1_no2022$publication_year)
years_max <- max(level_1_no2022$publication_year)

years_factor <- factor(level_1_no2022$publication_year, levels = min(years_min):max(years_max))
years_table <- table(years_factor)
oa_table <- table(level_1_no2022$is_oa, years_factor)


# plot Level_1
plot(as.numeric(years_table), type = "l", ylab = "Number of Publications", xlab = "Year", xaxt = "n")
axis(1, at = 1:length(years_table), labels = years_min:years_max)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")
grid(NULL, NULL, lty = 3, col = "grey")
lines(as.numeric(years_table), type = "l", lwd = 3, col = "black")
lines(as.numeric(oa_table[2,]), type = "l", lwd = 3, col = "cornflowerblue")
legend("topleft", c("all", "open access"), lwd = 3, cex = 1.2, col = c("black", "cornflowerblue"))


# bibliometrix results: all publication years
results <- biblioAnalysis(M)
summary_all <- summary(results, k = 25, pause = FALSE)
plot(results, k = 10, pause = FALSE)

results_level_1 <- biblioAnalysis(M_level_1)
summary_all_level_1 <- summary(results_level_1, k = 25, pause = FALSE)
plot(results_level_1, k = 10, pause = FALSE)

# omit current publication year (for temporal trends)
results_no2022 <- biblioAnalysis(M[M$PY < 2022,])
summary_no2022 <- summary(results_no2022, k = 25, pause = FALSE)
plot(results_no2022, k = 10, pause = FALSE)

results_no2022_level_1 <- biblioAnalysis(M[M_level_1$PY < 2022,])
summary_no2022_level_1 <- summary(results_no2022_level_1, k = 25, pause = FALSE)
plot(results_no2022_level_1, k = 10, pause = FALSE)

# annual growth rate
summary_no2022$MainInformation[5]
summary_no2022_level_1$MainInformation[5]


## Top Journals ----
journals <- sort(table(dataset$so))
journals <- get_shares(journals, nrow(dataset))
par(mar=c(1.8,7.9,1,1)*3)

# plot 800x750
par(mar=c(1.8, 7.3, 1, 0.5)*3)
barplot2(tail(journals, 25), horiz = TRUE, las = 1, xlim = c(0,10), xlab = "Relative Frequency (%)",
         plot.grid = TRUE, col = "#F8766D")

journals

journals_level_1 <- sort(table(level_1$so))
journals_level_1 <- get_shares(journals_level_1, nrow(level_1))


# plot 800x750
par(mar=c(1.8, 6.5, 1, 0.5)*3)
barplot2(tail(journals_level_1, 25), horiz = TRUE, las = 1, xlim = c(0,10), xlab = "Relative Frequency (%)",
         plot.grid = TRUE, col = "#00BFC4")

journals_level_1
View(as.data.frame(journals_level_1))

# merge list of journals
j_l1 <- as.data.frame(journals_level_1)
j_d <- as.data.frame(journals)

names(j_l1) <- "Journal"
names(j_l1)[2] <- "Level_1"
names(j_d)[1] <- "Journal"
names(j_d)[2] <- "Dataset"

journals_all <- merge(j_d, j_l1, by = "Journal", all = TRUE)
journals_all[is.na(journals_all)] <- 0

# # firs column and rowname
# journals_all <- data.frame(journals_all, row.names = 1)


# decreasing order and top ten only
View(j_l1[order(-j_l1$Level_1),])
View(tail(j_l1, n = 10))

j_l1_top <- tail(j_l1, n = 10)

j_l1_top <- j_l1[order(-j_l1$Level_1),]
j_d_top <- tail(j_d, n = 10)



journals_all_top <- tail(journals_all, n = 10)
journals_all_top <- data.frame(journals_all_top, row.names = 1)

names(journals_all_top)

# plot - reset parameters for plotting device
# dev.off()

par(cex = 1.8, mar=c(1.5, 6, 1, 1)*3)
barplot(t(journals_all_top), beside = TRUE, horiz = TRUE, las = 1,
        col = c("#F67280", "#355c7d"),
        legend.text = c("Final Dataset", "Training Dataset"),
        args.legend = list(x = "bottomright"),
        main = "TOP 10 Journals", adj = 0, xlab = "Relative Frequency (%)")



# # calculate chi square and Fischer ---------------------------------------------
# chisq.test(journals_all)$expected
# chisq.test(journals_all)
# 
# help("fisher.test")
# 
# fisher.test(journals_all, B=1e7, simulate.p.value = TRUE)
# 
# fisher.test(journals_all)
# fisher.test(journals_all)$p.value
# 
# saveRDS(journals_all, file = "./dataset/journals_all.RDS")



## Regional Differences ----

# top countries
get_shares(results$Countries, nrow(dataset))

countries <- results$CountryCollaboration
countries$SCP <- get_shares(countries$SCP, nrow(dataset))
countries$MCP <- get_shares(countries$MCP, nrow(dataset))

countries_level_1 <- results$CountryCollaboration
countries_level_1$SCP <- get_shares(countries_level_1$SCP, nrow(level_1))
countries_level_1$MCP <- get_shares(countries_level_1$MCP, nrow(level_1))


# # merge list of countries
# c_l1 <- as.data.frame(countries_level_1)
# c_d <- as.data.frame(countries)
# 
# names(c_l1) <- "Country"
# names(c_l1)[2] <- "Level_1 SCP"
# names(c_l1)[3] <- "Level_1 MCP"
# names(c_d)[1] <- "Country"
# names(c_d)[2] <- "Dataset SCP"
# names(c_d)[3] <- "Dataset MCP"
# 
# countries_all <- merge(c_d, c_l1, by = "Country", all = TRUE)
# 
# 
# # firs column and rowname
# countries_all <- data.frame(countries_all, row.names = 1)



# # calculate chi square and Fischer ---------------------------------------------
# chisq.test(journals_all)$expected
# chisq.test(journals_all)
# 
# help("fisher.test")
# 
# fisher.test(journals_all, B=1e7, simulate.p.value = TRUE)
# 
# fisher.test(journals_all)
# fisher.test(journals_all)$p.value
# 
# saveRDS(journals_all, file = "./dataset/journals_all.RDS")



# # reorder and top ten only
# countries <- countries[10:1,]
# rownames(countries) <- countries$Country
# 
# 
# 
# # plot
# par(mar=c(1.5,3,1,1)*3)
# barplot2(as.matrix(t(countries[,-1])), beside = FALSE, horiz = TRUE, las = 1,
#         plot.grid = TRUE,
#         xlim = c(0,25),
#         col = c("cornflowerblue", "orange"),
#         xlab = "Relative Frequency (%)")
# legend(13, 3, c("Single Country Publication", "Multiple Country Publication"),
#        cex = 1.2, fill = c("cornflowerblue", "orange"))
# 
# 
# # highest share of SCP / MCP
# countries$SCP_share <- countries$SCP / (countries$SCP + countries$MCP)
# countries$MCP_share <- countries$MCP / (countries$SCP + countries$MCP)


# top authors production over time
# TC = total Citations
authorProdOverTime(M[M$PY < 2022,], k = 10, graph = TRUE)
authorProdOverTime(M_level_1[M_level_1$PY < 2022,], k = 10, graph = TRUE)


## Authors Differences ----

# top countries
get_shares(results$Authors, nrow(dataset))

head(get_shares(results$Authors, nrow(dataset)), n = 10)
authors <- get_shares(results$Authors, nrow(dataset))

head(get_shares(results_level_1$Authors, nrow(level_1)), n = 10)
authors_level_1 <- get_shares(results_level_1$Authors, nrow(level_1))





# merge list of authors
a_l1 <- as.data.frame(authors_level_1)
a_d <- as.data.frame(authors)

names(a_l1) <- "Author"
names(a_l1)[2] <- "Level_1"
names(a_d)[1] <- "Author"
names(a_d)[2] <- "Dataset"


authors_all <- merge(a_d, a_l1, by = "Author", all = TRUE)
authors_all[is.na(authors_all)] <- 0


# decreasing order and top ten only
View(head(a_l1, n = 10))

a_l1_top <- head(a_l1, n = 10)
a_d_top <- head(a_d, n = 10)

names(journals_all)

View(head(authors_all, n = 10))

authors_all_top <- head(authors_all, n = 10)
authors_all_top[is.na(authors_all_top)] <- 0
authors_all_top <- data.frame(authors_all_top, row.names = 1)

# plot - reset parameters for plotting device
dev.off()

par(cex = 1.8, mar=c(1.5, 3.5, 1, 0.5)*3)
barplot(t(authors_all_top[10:1,]), beside = TRUE, xlim = c(0,5.5), horiz = TRUE, las = 1,
        col = c("#F67280", "#355c7d"),
        legend.text = c("Final Dataset", "Training Dataset"),
        args.legend = list(x = "bottomright"),
        main = "TOP 10 Authors", adj = 0.28, xlab = "Relative Frequency (%)")


# merge list of countries
c_l1 <- as.data.frame(countries_level_1)
c_d <- as.data.frame(countries)

names(c_l1) <- "Country"
names(c_l1)[2] <- "Level_1 SCP"
names(c_l1)[3] <- "Level_1 MCP"
names(c_d)[1] <- "Country"
names(c_d)[2] <- "Dataset SCP"
names(c_d)[3] <- "Dataset MCP"

countries_all <- merge(c_d, c_l1, by = "Country", all = TRUE)


# firs column and rowname
countries_all <- data.frame(countries_all, row.names = 1)


# Impact Levels ----

# Code for metadata retrieval and assignment of impact levels:
impact_levels <- read.csv("./ESM/impact_levels.csv", header = TRUE)
impact_levels_level_1 <- read.csv("./ESM/impact_levels_level_1.csv", header = TRUE)

#impact_levels <- readRDS("./ESM/impact_levels.RDS")
#impact_levels_level_1 <- readRDS("./ESM/impact_levels_level_1.RDS")

## Academic Citations ----
# all citations
summary(impact_levels$cited_by_count)
sd(impact_levels$cited_by_count)

summary(impact_levels_level_1$cited_by_count)
sd(impact_levels_level_1$cited_by_count)

## Altmetrics ----

# Tweets
table(!is.na(impact_levels$twitter)) / nrow(dataset)
impact_levels$twitter <- ifelse(is.na(impact_levels$twitter), 0, impact_levels$twitter)
summary(impact_levels$twitter)
sd(impact_levels$twitter, na.rm = TRUE)

table(!is.na(impact_levels_level_1$twitter)) / nrow(level_1)
impact_levels_level_1$twitter <- ifelse(is.na(impact_levels_level_1$twitter), 0, impact_levels_level_1$twitter)
summary(impact_levels_level_1$twitter)
sd(impact_levels_level_1$twitter, na.rm = TRUE)