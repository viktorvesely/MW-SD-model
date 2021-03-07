data_path <- "C:/Users/weseli/output" # Change to the location of your output folder

# List of the data files
behfiles <- list.files(path = data_path, pattern=".csv", full.names = TRUE)

# Combine all data files into a single data frame (behdat)
behdat <- data.frame()
for (i in 1:length(behfiles)) {
  behdat <- rbind(behdat, read.csv(behfiles[i], sep = ",", strip.white = TRUE))
}


## Analysis

# What is the mean response accuracy, and what is the standard error of the mean? Aggregate within and then across participants.
library(dplyr)
library(tidyr)
library(tidyverse)
library(sjmisc)
library(lme4)
library(FSA)

behdat$accuracy <- rep(0, length(behdat$response))

for(i in 1:length(behdat$response)) {
  if ((behdat$response[i] == "f" & behdat$stimulus[i] == "O") 
      | (behdat$response[i] == "NIL" & behdat$stimulus[i] == "Q")) {
    behdat$accuracy[i] <- 1
  } 
  else {
    behdat$accuracy[i] <- 0
  }
}

means <- aggregate(behdat$accuracy ~ behdat$participant, FUN = "mean")
ses <- aggregate(behdat$accuracy ~ behdat$participant, FUN = "se")

meanAcc <- mean(means$behdat$accuracy) * 100
seAcc <- se(means$behdat$accuracy) * 100
meanAcc

# Plot a histogram of response time for participant 1
participant1 <- filter(behdat, participant == 1 & rt != "NIL")
hist(as.numeric(participant1$rt), breaks = 40, 
     main = "Histogram of Response Time for Participant 1", 
     xlab = "Response time (s)")

df <- as_tibble(behdat) %>% filter(stimulus=="O")
df <- df %>% mutate(rt=as.numeric(as.character(df$rt)))
df <- na.omit(df)
mean(df$rt)
sd(df$rt)
plot(density(df$rt))