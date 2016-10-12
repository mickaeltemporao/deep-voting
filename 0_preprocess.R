#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        Deep voting preprocess
# Filename:     0_preprocess.R
# Description:  Preprocess file for deep learning algorithm to predict votes
# Version:      0.0.0.000
# Created:      2016-10-08 16:00:51
# Modified:     2016-10-11 10:04:10
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
rm(list=ls())
requirements <- c('mice')
install      <- requirements[!(requirements %in% installed.packages()[,"Package"])]
if(length(install)) install.packages(install)
lapply(requirements, library, character.only = TRUE)

# PARAMS
age_group <- FALSE

# Read & merge trian and test data, remove NA from train
tr    <- read.csv('data/train.csv', stringsAsFactors=F)
tr    <- tr[complete.cases(tr),]
tr_id <- tr$USER_ID
te    <- read.csv('data/test.csv', stringsAsFactors=F)
d     <- merge(tr, te, all=T)

# Handling Outliers in YOB
m <- median(subset(d$YOB, d$YOB >= 1920 & d$YOB <=2000), na.rm=TRUE) #1983
d$YOB[d$YOB <= 1935 | d$YOB >= 2000] <- NA
#plot(d$YOB)

# Imputing SES NA's
set.seed(1)
imputed <- d[2:6]
imputed[2:5] <- data.frame((lapply(imputed[2:5], as.factor)))
imputed <- complete(mice(imputed))
d$YOB   <- imputed$YOB
#plot(d$YOB)

if (age_group == T) {
# Creating AgeGroup
AgeGroup <- vector(mode="character", length=nrow(d))
for(i in 1:nrow(d)){
  age <- 2013 - d$YOB[i]
  if(is.na(age)){
    AgeGroup[i] <- ''
  }
  else if(age < 18){
    AgeGroup[i] <- "Under18"
  }
  else if(age >= 18 & age < 25){
    AgeGroup[i] <- "18-24"
  }
  else if(age >= 25 & age < 35){
    AgeGroup[i] <- "25-34"
  }
  else if(age >= 35 & age < 45){
    AgeGroup[i] <- "35-44"
  }
  else if(age >= 45 & age < 55){
    AgeGroup[i] <- "45-54"
  }
  else if(age >= 55 & age < 65){
    AgeGroup[i] <- "55-64"
  }
  else if(age >= 65){
    AgeGroup[i] <- "Over65"
  }
}
rm(i, age)
# Let's remove YOB and replace it with AgeGroup.
YOB <- 2013 - d$YOB
YOB2 <- log(d$YOB)
d$YOB <- NULL
USER_ID <- d$USER_ID
d <- cbind(USER_ID, AgeGroup, d[,2:ncol(d)])
rm(USER_ID)
}

# Replacing "DK" | "Yes" | "No"
d[d==''] <- "DK"

# Converting vars to ordered factors
if (age_group == T) {
d$AgeGroup <- factor(AgeGroup, c("Under18","18-24","25-34","35-44","45-54",
                               "55-64","Over65"), ordered = TRUE)
}
d$Income <- factor(d$Income, levels=c("DK", "under $25,000",
                                            "$25,001 - $50,000",
                                            "$50,000 - $74,999",
                                            "$75,000 - $100,000",
                                            "$100,001 - $150,000",
                                            "over $150,000"),
                      ordered = TRUE)
d$EducationLevel <- factor(d$EducationLevel,
                              levels=c("DK", "Current K-12","High School Diploma",
                                       "Associate's Degree",
                                       "Current Undergraduate",
                                       "Bachelor's Degree",
                                       "Master's Degree","Doctoral Degree"),
                              ordered = TRUE)


# Create Vector of Column Max and Min Values
# maxs <- apply(d, 2, max)
# mins <- apply(d, 2, min)

# Scale data
d$YOB <- d$YOB/max(d$YOB)

# Creating train and test sets
target      <- d$Party[d$USER_ID %in% tr_id]
d$Party     <- NULL
d           <- model.matrix(~.-1,data=d)
d           <- as.data.frame(d)
names(d)    <- make.names(names(d))
tr          <- subset(d, d$USER_ID %in% tr_id)
tr$democrat <- ifelse(target=='Democrat', 1, 0)
te          <- subset(d, !(d$USER_ID %in% tr_id))


# Cleaning environment
rm(list=setdiff(ls(), c('te', 'tr')))
