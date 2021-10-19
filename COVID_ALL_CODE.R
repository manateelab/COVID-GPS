df <- read.csv("GPS_Megafile_Interpolated_Symptoms.csv")
D1 <- read.csv("GPS_Megafile_Interpolated_Symptoms.csv", header = TRUE, sep = ",")
library(brms)
library(dplyr)
library(afex)
library(lmerTest)

df$ntype <- as.factor(df$ntype)
df$roaming_entropy.log <- log(df$roaming_entropy+1)
df$unique_locations.log <- log(df$unique_locations+3)
df$distance.log <- log(df$distance+1)
df$novel_minutes.log <- log(df$novel_minutes+10)

#The acute and chronic impact of COVID-19 on experiential diversity and emotion#
summary(lmer(roaming_entropy.log ~ ntype + dow + (1 | cnumeric), data = df))
summary(lmer(novel_locations.log ~ ntype + dow + (1 | cnumeric), data = df))
summary(lmer(PA_avg ~ ntype + dow + (1 | cnumeric), data = df))
summary(lmer(NA_avg ~ ntype + dow + (1 | cnumeric), data = df))
summary(lmer(roaming_entropy.log ~ ntype + dow + (1 | cnumeric), data = df[which(df$ntype!=3000),]))
summary(lmer(novel_locations.log ~ ntype + dow + (1 | cnumeric), data = df[which(df$ntype!=3000),]))
summary(lmer(PA_avg ~ ntype + dow + (1 | cnumeric), data = df[which(df$ntype!=3000),]))

#DOW#
D1$dow <-factor(D1$dow, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
D2 <- D1[D1$type != 4200,]
D2$type <-factor(D2$type)
D2$PA_avg.mo2 <- cut(D2$PA_avg, breaks=c(seq(0,100,10)), include.lowest = TRUE, right = TRUE, ordered_result = TRUE)
D2$type2<-D2$type
D2$type2[D2$type2 == 4000]<-3000
D2$unique_locations.log <- log(D2$unique_locations+3)
D2$novel_locations.log <- log(D2$novel_locations+10)
D2$roaming_entropy.log <- log(D2$roaming_entropy+1)
D2$distance.log <- log(D2$distance+1)

D2.pre <- D2[D2$type2 == 3000,]
D2.acute <- D2[D2$type2 == 4100,]
D2.noChronic <- D2[D2$type2 != 5000,]

D2.cyclcity.RE.pre <- lmer(roaming_entropy.log ~ dow + ( 1 | cnumeric), data = D2.pre)
D2.cyclcity.NL.pre <- lmer(novel_locations.log ~ dow + ( 1 | cnumeric), data = D2.pre)
D2.cyclcity.PA.pre <- lmer(PA_avg ~ dow + ( 1 | cnumeric), data = D2.pre)
D2.cyclcity.NA.pre <- lmer(NA_avg ~ dow + ( 1 | cnumeric), data = D2.pre)

D2.cyclicity.inter.RE <- lmer(roaming_entropy.log ~ dow*type2 + ( 1 | cnumeric), data = D2.noChronic)
D2.cyclicity.inter.NL <- lmer(novel_locations.log ~ dow*type2 + ( 1 | cnumeric), data = D2.noChronic)
D2.cyclicity.inter.PA <- lmer(PA_avg ~ dow*type2 + ( 1 | cnumeric), data = D2.noChronic)
D2.cyclicity.inter.NA <- lmer(NA_avg ~ dow*type2 + ( 1 | cnumeric), data = D2.noChronic)

## other models...

D2.cyclcity.RE.acute <- lmer(roaming_entropy.log ~ dow + ( 1 | cnumeric), data = D2.acute)
D2.cyclcity.NL.acute <- lmer(novel_locations.log ~ dow + ( 1 | cnumeric), data = D2.acute)
D2.cyclcity.PA.acute <- lmer(PA_avg ~ dow + ( 1 | cnumeric), data = D2.acute)
D2.cyclcity.NA.acute <- lmer(NA_avg ~ dow + ( 1 | cnumeric), data = D2.acute)

D2.cyclicity.inter.RE <- lmer(roaming_entropy.log ~ dow*type2 + ( 1 | cnumeric), data = D2)
D2.cyclicity.inter.NL <- lmer(novel_locations.log ~ dow*type2 + ( 1 | cnumeric), data = D2)
D2.cyclicity.inter.PA <- lmer(PA_avg ~ dow*type2 + ( 1 | cnumeric), data = D2)
D2.cyclicity.inter.NA <- lmer(NA_avg ~ dow*type2 + ( 1 | cnumeric), data = D2)



####

#The COVID-19 pandemic affected how we explored our environment#
summary(lmer(unique_locations.log ~ ntype + dow + (1 | cnumeric), data = df))
summary(lmer(roaming_entropy.log ~ ntype + dow + unique_locations.log + (1 | cnumeric), data = df))
summary(lmer(novel_locations.log ~ novel_minutes.log * ntype + dow * ntype + (1 | subject), data = df))
summary(lmer(novel_locations.log ~ novel_minutes.log * ntype + dow * ntype + (1 | subject), data = df[which(df$ntype!=3000),]))
summary(lmer(time_spent_away_from_home ~ ntype + dow + (1 | cnumeric), data = df))
summary(lmer(distance.log ~ roaming_entropy.log * ntype + dow * ntype + (1 | cnumeric), data = df))
summary(lmer(unique_locations.log ~ roaming_entropy.log * ntype + dow * ntype + (1 | cnumeric), data = df))
summary(lmer(unique_locations.log ~ roaming_entropy.log * ntype + dow * ntype + (1 | cnumeric), data = df[which(df$ntype!=3000),]))



#Bayesian Analyses
library(ggplot2)
totDF <- data.frame(novLoc = c(2.303,seq(from = 2.4, to = 6.0, by = .1)),
                    nov_mean1 = NA, nov_mean2 = NA, nov_mean3 = NA,
                    nov_sd1 = NA,  nov_sd2 = NA,  nov_sd3 = NA,
                    cohens_d12 = NA, quantLow_12 = NA, quantHigh_12 = NA,
                    cohens_d13 = NA, quantLow_13 = NA, quantHigh_13 = NA,
                    cohens_d23 = NA, quantLow_23 = NA, quantHigh_23 = NA,
                    percent0_12 = NA, percent0_13 = NA, percent0_23 = NA)

satDF <- data.frame(novLoc = c(2.303,seq(from = 2.4, to = 6.0, by = .1)),
                    nov_mean1 = NA, nov_mean2 = NA, nov_mean3 = NA,
                    nov_sd1 = NA,  nov_sd2 = NA,  nov_sd3 = NA,
                    cohens_d12 = NA, quantLow_12 = NA, quantHigh_12 = NA,
                    cohens_d13 = NA, quantLow_13 = NA, quantHigh_13 = NA,
                    cohens_d23 = NA, quantLow_23 = NA, quantHigh_23 = NA,
                    percent0_12 = NA, percent0_13 = NA, percent0_23 = NA)

# A<-dir("C:/Users/rickr/OneDrive/Documents/Manatee Lab/Downloads/PosteriorPredict_NovLoc//")
# setwd("C:/Users/rickr/OneDrive/Documents/Manatee Lab/Downloads/PosteriorPredict_NovLoc///")


totDF <- data.frame(novLoc = seq(from = 0.0, to = 2.0, by = .1),
                    nov_mean1 = NA, nov_mean2 = NA, nov_mean3 = NA, 
                    nov_sd1 = NA,  nov_sd2 = NA,  nov_sd3 = NA,
                    cohens_d12 = NA, quantLow_12 = NA, quantHigh_12 = NA,
                    cohens_d13 = NA, quantLow_13 = NA, quantHigh_13 = NA,
                    cohens_d23 = NA, quantLow_23 = NA, quantHigh_23 = NA,
                    percent0_12 = NA, percent0_13 = NA, percent0_23 = NA)

satDF <- data.frame(novLoc = seq(from = 0.0, to = 2.0, by = .1),
                    nov_mean1 = NA, nov_mean2 = NA, nov_mean3 = NA, 
                    nov_sd1 = NA,  nov_sd2 = NA,  nov_sd3 = NA,
                    cohens_d12 = NA, quantLow_12 = NA, quantHigh_12 = NA,
                    cohens_d13 = NA, quantLow_13 = NA, quantHigh_13 = NA,
                    cohens_d23 = NA, quantLow_23 = NA, quantHigh_23 = NA,
                    percent0_12 = NA, percent0_13 = NA, percent0_23 = NA)

# A <- dir("C:/Users/rickr/OneDrive/Documents/Manatee Lab/Downloads/PosteriorPredict_RE///")
# setwd("C:/Users/rickr/OneDrive/Documents/Manatee Lab/Downloads/PosteriorPredict_RE///")

for (i in 1:length(A))
{
  print(A[i])
  D1 <- read.csv(A[i])
  D1$dow <- factor(D1$dow, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday") )
  # D1$nov_mean1 <- D1$RE_mean1
  # D1$nov_mean2 <- D1$RE_mean2
  # D1$nov_mean3 <- D1$RE_mean3
  # D1$nov_sd1 <- D1$RE_sd1
  # D1$nov_sd2 <- D1$RE_sd2
  # D1$nov_sd3 <- D1$RE_sd3
  
  
  
  totDF$nov_mean1[i] <- mean(D1$nov_mean1)
  totDF$nov_mean2[i] <- mean(D1$nov_mean2)
  totDF$nov_mean3[i] <- mean(D1$nov_mean3)
  
  totDF$nov_sd1[i] <- mean(D1$nov_sd1)
  totDF$nov_sd2[i] <- mean(D1$nov_sd2)
  totDF$nov_sd3[i] <- mean(D1$nov_sd3)
  
  totDF$cohens_d12[i] <- mean(D1$cohens_d12)
  totDF$cohens_d13[i] <- mean(D1$cohens_d13)
  totDF$cohens_d23[i] <- mean(D1$cohens_d23)
  
  totDF$quantLow_12[i] = quantile(D1$cohens_d12, probs = .025)
  totDF$quantHigh_12[i] = quantile(D1$cohens_d12, probs = .975)
  totDF$quantLow_13[i] =  quantile(D1$cohens_d13, probs = .025)
  totDF$quantHigh_13[i] = quantile(D1$cohens_d13, probs = .975)
  totDF$quantLow_23[i] = quantile(D1$cohens_d23, probs = .025)
  totDF$quantHigh_23[i] = quantile(D1$cohens_d23, probs = .975)
  
  if(totDF$cohens_d12[i]>0){
    totDF$percent0_12[i] <- length(which(D1$cohens_d12>0))/nrow(D1)
  } else{
    totDF$percent0_12[i] <- length(which(D1$cohens_d12<0))/nrow(D1)
    
  }
  if(totDF$cohens_d13[i]>0){
    totDF$percent0_13[i] <- length(which(D1$cohens_d13>0))/nrow(D1)
  } else{
    totDF$percent0_13[i] <- length(which(D1$cohens_d13<0))/nrow(D1)
    
  }
  if(totDF$cohens_d23[i]>0){
    totDF$percent0_23[i] <- length(which(D1$cohens_d23>0))/nrow(D1)
  } else{
    totDF$percent0_23[i] <- length(which(D1$cohens_d23<0))/nrow(D1)
    
  }
  
  ggplot(data = D1, aes(x = cohens_d12, fill = dow)) + 
    geom_histogram(position = "identity", alpha = 0.7, binwidth = .01)
  
  D2<-D1[D1$dow == "Friday",]
  
  satDF$nov_mean1[i] <- mean(D2$nov_mean1)
  satDF$nov_mean2[i] <- mean(D2$nov_mean2)
  satDF$nov_mean3[i] <- mean(D2$nov_mean3)
  
  satDF$nov_sd1[i] <- mean(D2$nov_sd1)
  satDF$nov_sd2[i] <- mean(D2$nov_sd2)
  satDF$nov_sd3[i] <- mean(D2$nov_sd3)
  
  satDF$cohens_d12[i] <- mean(D2$cohens_d12)
  satDF$cohens_d13[i] <- mean(D2$cohens_d13)
  satDF$cohens_d23[i] <- mean(D2$cohens_d23)
  
  satDF$quantLow_12[i] = quantile(D2$cohens_d12, probs = .025)
  satDF$quantHigh_12[i] = quantile(D2$cohens_d12, probs = .975)
  satDF$quantLow_13[i] =  quantile(D2$cohens_d13, probs = .025)
  satDF$quantHigh_13[i] = quantile(D2$cohens_d13, probs = .975)
  satDF$quantLow_23[i] = quantile(D2$cohens_d23, probs = .025)
  satDF$quantHigh_23[i] = quantile(D2$cohens_d23, probs = .975)
  
  if(satDF$cohens_d12[i]>0){
    satDF$percent0_12[i] <- length(which(D2$cohens_d12>0))/nrow(D2)
  } else{
    satDF$percent0_12[i] <- length(which(D2$cohens_d12<0))/nrow(D2)
    
  }
  if(satDF$cohens_d13[i]>0){
    satDF$percent0_13[i] <- length(which(D2$cohens_d13>0))/nrow(D2)
  } else{
    satDF$percent0_13[i] <- length(which(D2$cohens_d13<0))/nrow(D2)
    
  }
  if(satDF$cohens_d23[i]>0){
    satDF$percent0_23[i] <- length(which(D2$cohens_d23>0))/nrow(D2)
  } else{
    satDF$percent0_23[i] <- length(which(D2$cohens_d23<0))/nrow(D2)
    
  }
  
  
}

hist(D1$cohens_d13, breaks = 1000)  
#color by day of week
ggplot(D1, aes(cohens_d12, fill=dow)) + geom_histogram(bins=100,position = "identity") + scale_fill_brewer(palette = "RdYlBu")



#Z-Score
A<-dir("C:/Users/rickr/OneDrive/Documents/Manatee Lab/Downloads/PosteriorPredict_NovLoc_z2/")
zorder <- order(as.numeric(c(substr(A[1:25],8,11),substr(A[26:51],8,10))))

totDF <- data.frame(novLoc = seq(from = -2.5, to = 2.5, by = .1),
                    nov_mean1 = NA, nov_mean2 = NA, nov_mean3 = NA,
                    nov_sd1 = NA,  nov_sd2 = NA,  nov_sd3 = NA,
                    cohens_d12 = NA, quantLow_12 = NA, quantHigh_12 = NA,
                    cohens_d13 = NA, quantLow_13 = NA, quantHigh_13 = NA,
                    cohens_d23 = NA, quantLow_23 = NA, quantHigh_23 = NA,
                    percent0_12 = NA, percent0_13 = NA, percent0_23 = NA)

satDF <- data.frame(novLoc = seq(from = -2.5, to = 2.5, by = .1),
                    nov_mean1 = NA, nov_mean2 = NA, nov_mean3 = NA,
                    nov_sd1 = NA,  nov_sd2 = NA,  nov_sd3 = NA,
                    cohens_d12 = NA, quantLow_12 = NA, quantHigh_12 = NA,
                    cohens_d13 = NA, quantLow_13 = NA, quantHigh_13 = NA,
                    cohens_d23 = NA, quantLow_23 = NA, quantHigh_23 = NA,
                    percent0_12 = NA, percent0_13 = NA, percent0_23 = NA)

setwd("C:/Users/rickr/OneDrive/Documents/Manatee Lab/Downloads/PosteriorPredict_NovLoc_z2/")

for (i in zorder)
{
  print(A[i])
  D1 <- read.csv(A[i])
  D1$dow <- factor(D1$dow, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday") )
  # D1$nov_mean1 <- D1$RE_mean1
  # D1$nov_mean2 <- D1$RE_mean2
  # D1$nov_mean3 <- D1$RE_mean3
  # D1$nov_sd1 <- D1$RE_sd1
  # D1$nov_sd2 <- D1$RE_sd2
  # D1$nov_sd3 <- D1$RE_sd3
  
  
  
  totDF$nov_mean1[i] <- mean(D1$nov_mean1)
  totDF$nov_mean2[i] <- mean(D1$nov_mean2)
  totDF$nov_mean3[i] <- mean(D1$nov_mean3)
  
  totDF$nov_sd1[i] <- mean(D1$nov_sd1)
  totDF$nov_sd2[i] <- mean(D1$nov_sd2)
  totDF$nov_sd3[i] <- mean(D1$nov_sd3)
  
  totDF$cohens_d12[i] <- mean(D1$cohens_d12)
  totDF$cohens_d13[i] <- mean(D1$cohens_d13)
  totDF$cohens_d23[i] <- mean(D1$cohens_d23)
  
  totDF$quantLow_12[i] = quantile(D1$cohens_d12, probs = .025)
  totDF$quantHigh_12[i] = quantile(D1$cohens_d12, probs = .975)
  totDF$quantLow_13[i] =  quantile(D1$cohens_d13, probs = .025)
  totDF$quantHigh_13[i] = quantile(D1$cohens_d13, probs = .975)
  totDF$quantLow_23[i] = quantile(D1$cohens_d23, probs = .025)
  totDF$quantHigh_23[i] = quantile(D1$cohens_d23, probs = .975)
  
  if(totDF$cohens_d12[i]>0){
    totDF$percent0_12[i] <- length(which(D1$cohens_d12>0))/nrow(D1)
  } else{
    totDF$percent0_12[i] <- length(which(D1$cohens_d12<0))/nrow(D1)
    
  }
  if(totDF$cohens_d13[i]>0){
    totDF$percent0_13[i] <- length(which(D1$cohens_d13>0))/nrow(D1)
  } else{
    totDF$percent0_13[i] <- length(which(D1$cohens_d13<0))/nrow(D1)
    
  }
  if(totDF$cohens_d23[i]>0){
    totDF$percent0_23[i] <- length(which(D1$cohens_d23>0))/nrow(D1)
  } else{
    totDF$percent0_23[i] <- length(which(D1$cohens_d23<0))/nrow(D1)
    
  }
  
  ggplot(data = D1, aes(x = cohens_d12, fill = dow)) + 
    geom_histogram(position = "identity", alpha = 0.7, binwidth = .01)
  
  D2<-D1[D1$dow == "Friday",]
  
  satDF$nov_mean1[i] <- mean(D2$nov_mean1)
  satDF$nov_mean2[i] <- mean(D2$nov_mean2)
  satDF$nov_mean3[i] <- mean(D2$nov_mean3)
  
  satDF$nov_sd1[i] <- mean(D2$nov_sd1)
  satDF$nov_sd2[i] <- mean(D2$nov_sd2)
  satDF$nov_sd3[i] <- mean(D2$nov_sd3)
  
  satDF$cohens_d12[i] <- mean(D2$cohens_d12)
  satDF$cohens_d13[i] <- mean(D2$cohens_d13)
  satDF$cohens_d23[i] <- mean(D2$cohens_d23)
  
  satDF$quantLow_12[i] = quantile(D2$cohens_d12, probs = .025)
  satDF$quantHigh_12[i] = quantile(D2$cohens_d12, probs = .975)
  satDF$quantLow_13[i] =  quantile(D2$cohens_d13, probs = .025)
  satDF$quantHigh_13[i] = quantile(D2$cohens_d13, probs = .975)
  satDF$quantLow_23[i] = quantile(D2$cohens_d23, probs = .025)
  satDF$quantHigh_23[i] = quantile(D2$cohens_d23, probs = .975)
  
  if(satDF$cohens_d12[i]>0){
    satDF$percent0_12[i] <- length(which(D2$cohens_d12>0))/nrow(D2)
  } else{
    satDF$percent0_12[i] <- length(which(D2$cohens_d12<0))/nrow(D2)
    
  }
  if(satDF$cohens_d13[i]>0){
    satDF$percent0_13[i] <- length(which(D2$cohens_d13>0))/nrow(D2)
  } else{
    satDF$percent0_13[i] <- length(which(D2$cohens_d13<0))/nrow(D2)
    
  }
  if(satDF$cohens_d23[i]>0){
    satDF$percent0_23[i] <- length(which(D2$cohens_d23>0))/nrow(D2)
  } else{
    satDF$percent0_23[i] <- length(which(D2$cohens_d23<0))/nrow(D2)
    
  }
  
  
}






#Momentary Anxiety
A<-dir("C:/Users/rickr/OneDrive/Documents/Manatee Lab/Downloads/PosteriorPredict_NovLoc_PA_EMA-Anxious (2)/")

totDF <- data.frame(novLoc = seq(from = 2.0, to = 6.5, by = .1),
                    nov_mean1 = NA, nov_mean2 = NA, nov_mean3 = NA,
                    nov_sd1 = NA,  nov_sd2 = NA,  nov_sd3 = NA,
                    cohens_d12 = NA, quantLow_12 = NA, quantHigh_12 = NA,
                    cohens_d13 = NA, quantLow_13 = NA, quantHigh_13 = NA,
                    cohens_d23 = NA, quantLow_23 = NA, quantHigh_23 = NA,
                    percent0_12 = NA, percent0_13 = NA, percent0_23 = NA)

satDF <- data.frame(novLoc = seq(from = 2.0, to = 6.5, by = .1),
                    nov_mean1 = NA, nov_mean2 = NA, nov_mean3 = NA,
                    nov_sd1 = NA,  nov_sd2 = NA,  nov_sd3 = NA,
                    cohens_d12 = NA, quantLow_12 = NA, quantHigh_12 = NA,
                    cohens_d13 = NA, quantLow_13 = NA, quantHigh_13 = NA,
                    cohens_d23 = NA, quantLow_23 = NA, quantHigh_23 = NA,
                    percent0_12 = NA, percent0_13 = NA, percent0_23 = NA)

setwd("C:/Users/rickr/OneDrive/Documents/Manatee Lab/Downloads/PosteriorPredict_NovLoc_PA_EMA-Anxious (2)/")

for(i in 1:length(A))
{
  D1 <- read.csv(A[i])
  D1$dow <- factor(D1$dow, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  D1$cohens_d12 <- NA
  D1$cohens_d13 <- NA
  D1$cohens_d23 <- NA
  
  if(i > 31){
    setwd("C:/Users/rickr/OneDrive/Documents/Manatee Lab/Downloads/PosteriorPredict_NovLoc_PA_EMA-Anxious_Friday/")
    B <- dir("C:/Users/rickr/OneDrive/Documents/Manatee Lab/Downloads/PosteriorPredict_NovLoc_PA_EMA-Anxious_Friday/")
    D_F <- read.csv(B[i-31])
    D1 <- rbind(D1, D_F)
    D1$cohens_d12 <- NA
    D1$cohens_d13 <- NA
    D1$cohens_d23 <- NA
    for(j in 1:6000){
      D1$cohens_d12[j] <- (D1$nov_mean1[j]-D1$nov_mean1[j+12000])/sqrt( (D1$nov_sd1[j]^2 + D1$nov_sd1[j+12000]^2)/2 )
      D1$cohens_d13[j] <- (D1$nov_mean2[j]-D1$nov_mean2[j+12000])/sqrt( (D1$nov_sd2[j]^2 + D1$nov_sd2[j+12000]^2)/2 )
      D1$cohens_d23[j] <- (D1$nov_mean3[j]-D1$nov_mean3[j+12000])/sqrt( (D1$nov_sd3[j]^2 + D1$nov_sd3[j+12000]^2)/2 )
    }
    for(j in 18001:19000){
      D1$cohens_d12[j] <- (D1$nov_mean1[j]-D1$nov_mean1[j+2000])/sqrt( (D1$nov_sd1[j]^2 + D1$nov_sd1[j+2000]^2)/2 )
      D1$cohens_d13[j] <- (D1$nov_mean2[j]-D1$nov_mean2[j+2000])/sqrt( (D1$nov_sd2[j]^2 + D1$nov_sd2[j+2000]^2)/2 )
      D1$cohens_d23[j] <- (D1$nov_mean3[j]-D1$nov_mean3[j+2000])/sqrt( (D1$nov_sd3[j]^2 + D1$nov_sd3[j+2000]^2)/2 )
    }
    
    setwd("C:/Users/rickr/OneDrive/Documents/Manatee Lab/Downloads/PosteriorPredict_NovLoc_PA_EMA-Anxious (2)/")
    
    
  } else{
    
    for(j in 1:7000){
      D1$cohens_d12[j] <- (D1$nov_mean1[j]-D1$nov_mean1[j+14000])/sqrt( (D1$nov_sd1[j]^2 + D1$nov_sd1[j+14000]^2)/2 )
      D1$cohens_d13[j] <- (D1$nov_mean2[j]-D1$nov_mean2[j+14000])/sqrt( (D1$nov_sd2[j]^2 + D1$nov_sd2[j+14000]^2)/2 )
      D1$cohens_d23[j] <- (D1$nov_mean3[j]-D1$nov_mean3[j+14000])/sqrt( (D1$nov_sd3[j]^2 + D1$nov_sd3[j+14000]^2)/2 )
    }
    
  }
  
  totDF$nov_mean1[i] <- mean(D1$nov_mean1[which(D1$Anxious==20)])-mean(D1$nov_mean1[which(D1$Anxious==80)])
  totDF$nov_mean2[i] <- mean(D1$nov_mean2[which(D1$Anxious==20)])-mean(D1$nov_mean2[which(D1$Anxious==80)])
  totDF$nov_mean3[i] <- mean(D1$nov_mean3[which(D1$Anxious==20)])-mean(D1$nov_mean3[which(D1$Anxious==80)])
  
  totDF$nov_sd1[i] <- sqrt( ((mean(D1$nov_sd1[which(D1$Anxious==20)])^2 + mean(D1$nov_sd1[which(D1$Anxious==80)])^2))/2 )
  totDF$nov_sd2[i] <- sqrt( ((mean(D1$nov_sd2[which(D1$Anxious==20)])^2 + mean(D1$nov_sd2[which(D1$Anxious==80)])^2))/2 )
  totDF$nov_sd3[i] <- sqrt( ((mean(D1$nov_sd3[which(D1$Anxious==20)])^2 + mean(D1$nov_sd3[which(D1$Anxious==80)])^2))/2 )
  
  totDF$cohens_d12[i] <- mean(D1$cohens_d12, na.rm = T)
  totDF$cohens_d13[i] <- mean(D1$cohens_d13, na.rm = T)
  totDF$cohens_d23[i] <- mean(D1$cohens_d23, na.rm = T)
  
  totDF$quantLow_12[i] = quantile(D1$cohens_d12, probs = .025, na.rm = T)
  totDF$quantHigh_12[i] = quantile(D1$cohens_d12, probs = .975, na.rm = T)
  totDF$quantLow_13[i] =  quantile(D1$cohens_d13, probs = .025, na.rm = T)
  totDF$quantHigh_13[i] = quantile(D1$cohens_d13, probs = .97, na.rm = T)
  totDF$quantLow_23[i] = quantile(D1$cohens_d23, probs = .025, na.rm = T)
  totDF$quantHigh_23[i] = quantile(D1$cohens_d23, probs = .975, na.rm = T)
  
  if(totDF$cohens_d12[i]>0){
    totDF$percent0_12[i] <- length(which(D1$cohens_d12>0))/nrow(D1)
  } else{
    totDF$percent0_12[i] <- length(which(D1$cohens_d12<0))/nrow(D1)
    
  }
  if(totDF$cohens_d13[i]>0){
    totDF$percent0_13[i] <- length(which(D1$cohens_d13>0))/nrow(D1)
  } else{
    totDF$percent0_13[i] <- length(which(D1$cohens_d13<0))/nrow(D1)
    
  }
  if(totDF$cohens_d23[i]>0){
    totDF$percent0_23[i] <- length(which(D1$cohens_d23>0))/nrow(D1)
  } else{
    totDF$percent0_23[i] <- length(which(D1$cohens_d23<0))/nrow(D1)
    
  }
  
  ggplot(data = D1, aes(x = cohens_d12, fill = dow)) + 
    geom_histogram(position = "identity", alpha = 0.7, binwidth = .01)
  
  D2<-D1[D1$dow == "Friday",]
  
  satDF$nov_mean1[i] <- mean(D2$nov_mean1)
  satDF$nov_mean2[i] <- mean(D2$nov_mean2)
  satDF$nov_mean3[i] <- mean(D2$nov_mean3)
  
  satDF$nov_sd1[i] <- mean(D2$nov_sd1)
  satDF$nov_sd2[i] <- mean(D2$nov_sd2)
  satDF$nov_sd3[i] <- mean(D2$nov_sd3)
  
  satDF$cohens_d12[i] <- mean(D2$cohens_d12,na.rm = T)
  satDF$cohens_d13[i] <- mean(D2$cohens_d13,na.rm = T)
  satDF$cohens_d23[i] <- mean(D2$cohens_d23,na.rm = T)
  
  satDF$quantLow_12[i] = quantile(D2$cohens_d12, probs = .025, na.rm = T)
  satDF$quantHigh_12[i] = quantile(D2$cohens_d12, probs = .975, na.rm = T)
  satDF$quantLow_13[i] =  quantile(D2$cohens_d13, probs = .025, na.rm = T)
  satDF$quantHigh_13[i] = quantile(D2$cohens_d13, probs = .975, na.rm = T)
  satDF$quantLow_23[i] = quantile(D2$cohens_d23, probs = .025, na.rm = T)
  satDF$quantHigh_23[i] = quantile(D2$cohens_d23, probs = .975, na.rm = T)
  
  if(satDF$cohens_d12[i]>0){
    satDF$percent0_12[i] <- length(which(D2$cohens_d12>0))/nrow(D2)
  } else{
    satDF$percent0_12[i] <- length(which(D2$cohens_d12<0))/nrow(D2)
    
  }
  if(satDF$cohens_d13[i]>0){
    satDF$percent0_13[i] <- length(which(D2$cohens_d13>0))/nrow(D2)
  } else{
    satDF$percent0_13[i] <- length(which(D2$cohens_d13<0))/nrow(D2)
    
  }
  if(satDF$cohens_d23[i]>0){
    satDF$percent0_23[i] <- length(which(D2$cohens_d23>0))/nrow(D2)
  } else{
    satDF$percent0_23[i] <- length(which(D2$cohens_d23<0))/nrow(D2)
    
  }
  
  
}




#LOCF Anxiety
setwd("C:/Users/rickr/OneDrive/Documents/Manatee Lab/Downloads")
df <- totDf[1,]
D1 <- read.csv("ordinalPA_singlepoint_locfAnx_PosteriorPredict_NovLoc_5.3.csv")
D1$dow <- factor(D1$dow, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
D1$cohens_d12 <- NA
D1$cohens_d13 <- NA
D1$cohens_d23 <- NA

for(j in 1:1000){
  D1$cohens_d12[j] <- (D1$nov_mean1[j]-D1$nov_mean1[j+2000])/sqrt( (D1$nov_sd1[j]^2 + D1$nov_sd1[j+2000]^2)/2 )
  D1$cohens_d13[j] <- (D1$nov_mean2[j]-D1$nov_mean2[j+2000])/sqrt( (D1$nov_sd2[j]^2 + D1$nov_sd2[j+2000]^2)/2 )
  D1$cohens_d23[j] <- (D1$nov_mean3[j]-D1$nov_mean3[j+2000])/sqrt( (D1$nov_sd3[j]^2 + D1$nov_sd3[j+2000]^2)/2 )
}

df$nov_mean1[1] <- mean(D1$nov_mean1[which(D1$anx==0)])-mean(D1$nov_mean1[which(D1$anx==10)])
df$nov_mean2[1] <- mean(D1$nov_mean2[which(D1$anx==0)])-mean(D1$nov_mean2[which(D1$anx==10)])
df$nov_mean3[1] <- mean(D1$nov_mean3[which(D1$anx==0)])-mean(D1$nov_mean3[which(D1$anx==10)])

df$nov_sd1[1] <- sqrt( ((mean(D1$nov_sd1[which(D1$anx==0)])^2 + mean(D1$nov_sd1[which(D1$anx==10)])^2))/2 )
df$nov_sd2[1] <- sqrt( ((mean(D1$nov_sd2[which(D1$anx==0)])^2 + mean(D1$nov_sd2[which(D1$anx==10)])^2))/2 )
df$nov_sd3[1] <- sqrt( ((mean(D1$nov_sd3[which(D1$anx==0)])^2 + mean(D1$nov_sd3[which(D1$anx==10)])^2))/2 )

df$cohens_d12[1] <- mean(D1$cohens_d12, na.rm = T)
df$cohens_d13[1] <- mean(D1$cohens_d13, na.rm = T)
df$cohens_d23[1] <- mean(D1$cohens_d23, na.rm = T)

df$quantLow_12[1] = quantile(D1$cohens_d12, probs = .1, na.rm = T)
df$quantHigh_12[1] = quantile(D1$cohens_d12, probs = .9, na.rm = T)
df$quantLow_13[1] =  quantile(D1$cohens_d13, probs = .1, na.rm = T)
df$quantHigh_13[1] = quantile(D1$cohens_d13, probs = .9, na.rm = T)
df$quantLow_23[1] = quantile(D1$cohens_d23, probs = .1, na.rm = T)
df$quantHigh_23[1] = quantile(D1$cohens_d23, probs = .9, na.rm = T)


# 
# 
# df$nov_mean1[1] <- mean(D1$nov_mean1[which(D1$Anxious==20)])-mean(D1$nov_mean1[which(D1$Anxious==80)])
# df$nov_mean2[1] <- mean(D1$nov_mean2[which(D1$Anxious==20)])-mean(D1$nov_mean2[which(D1$Anxious==80)])
# df$nov_mean3[1] <- mean(D1$nov_mean3[which(D1$Anxious==20)])-mean(D1$nov_mean3[which(D1$Anxious==80)])
# 
# df$nov_sd1[1] <- sqrt( ((mean(D1$nov_sd1[which(D1$Anxious==20)])^2 + mean(D1$nov_sd1[which(D1$Anxious==80)])^2))/2 )
# df$nov_sd2[1] <- sqrt( ((mean(D1$nov_sd2[which(D1$Anxious==20)])^2 + mean(D1$nov_sd2[which(D1$Anxious==80)])^2))/2 )
# df$nov_sd3[1] <- sqrt( ((mean(D1$nov_sd3[which(D1$Anxious==20)])^2 + mean(D1$nov_sd3[which(D1$Anxious==80)])^2))/2 )
# 
# df$cohens_d12[1] <- mean(D1$cohens_d12, na.rm = T)
# df$cohens_d13[1] <- mean(D1$cohens_d13, na.rm = T)
# df$cohens_d23[1] <- mean(D1$cohens_d23, na.rm = T)
# 
# df$quantLow_12[1] = quantile(D1$cohens_d12, probs = .025, na.rm = T)
# df$quantHigh_12[1] = quantile(D1$cohens_d12, probs = .975, na.rm = T)
# df$quantLow_13[1] =  quantile(D1$cohens_d13, probs = .025, na.rm = T)
# df$quantHigh_13[1] = quantile(D1$cohens_d13, probs = .97, na.rm = T)
# df$quantLow_23[1] = quantile(D1$cohens_d23, probs = .025, na.rm = T)
# df$quantHigh_23[1] = quantile(D1$cohens_d23, probs = .975, na.rm = T)
















#Distance

A<-dir("C:/Users/rickr/OneDrive/Documents/Manatee Lab/Downloads/PosteriorPredict_NovLoc_CtrlDist/")

totDF <- data.frame(novLoc = c(2.303,3.0,4.0,5.0,5.3,6.0),
                    nov_mean1 = NA, nov_mean2 = NA, nov_mean3 = NA,
                    nov_sd1 = NA,  nov_sd2 = NA,  nov_sd3 = NA,
                    cohens_d12 = NA, quantLow_12 = NA, quantHigh_12 = NA,
                    cohens_d13 = NA, quantLow_13 = NA, quantHigh_13 = NA,
                    cohens_d23 = NA, quantLow_23 = NA, quantHigh_23 = NA,
                    percent0_12 = NA, percent0_13 = NA, percent0_23 = NA)

satDF <- data.frame(novLoc = c(2.303,3.0,4.0,5.0,5.3,6.0),
                    nov_mean1 = NA, nov_mean2 = NA, nov_mean3 = NA,
                    nov_sd1 = NA,  nov_sd2 = NA,  nov_sd3 = NA,
                    cohens_d12 = NA, quantLow_12 = NA, quantHigh_12 = NA,
                    cohens_d13 = NA, quantLow_13 = NA, quantHigh_13 = NA,
                    cohens_d23 = NA, quantLow_23 = NA, quantHigh_23 = NA,
                    percent0_12 = NA, percent0_13 = NA, percent0_23 = NA)

setwd("C:/Users/rickr/OneDrive/Documents/Manatee Lab/Downloads/PosteriorPredict_NovLoc_CtrlDist/")

for (i in 1:length(A))
{
  print(A[i])
  D1 <- read.csv(A[i])
  D1$dow <- factor(D1$dow, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday") )
  # D1$nov_mean1 <- D1$RE_mean1
  # D1$nov_mean2 <- D1$RE_mean2
  # D1$nov_mean3 <- D1$RE_mean3
  # D1$nov_sd1 <- D1$RE_sd1
  # D1$nov_sd2 <- D1$RE_sd2
  # D1$nov_sd3 <- D1$RE_sd3
  
  
  
  totDF$nov_mean1[i] <- mean(D1$nov_mean1)
  totDF$nov_mean2[i] <- mean(D1$nov_mean2)
  totDF$nov_mean3[i] <- mean(D1$nov_mean3)
  
  totDF$nov_sd1[i] <- mean(D1$nov_sd1)
  totDF$nov_sd2[i] <- mean(D1$nov_sd2)
  totDF$nov_sd3[i] <- mean(D1$nov_sd3)
  
  totDF$cohens_d12[i] <- mean(D1$cohens_d12)
  totDF$cohens_d13[i] <- mean(D1$cohens_d13)
  totDF$cohens_d23[i] <- mean(D1$cohens_d23)
  
  totDF$quantLow_12[i] = quantile(D1$cohens_d12, probs = .025)
  totDF$quantHigh_12[i] = quantile(D1$cohens_d12, probs = .975)
  totDF$quantLow_13[i] =  quantile(D1$cohens_d13, probs = .025)
  totDF$quantHigh_13[i] = quantile(D1$cohens_d13, probs = .975)
  totDF$quantLow_23[i] = quantile(D1$cohens_d23, probs = .025)
  totDF$quantHigh_23[i] = quantile(D1$cohens_d23, probs = .975)
  
  if(totDF$cohens_d12[i]>0){
    totDF$percent0_12[i] <- length(which(D1$cohens_d12>0))/nrow(D1)
  } else{
    totDF$percent0_12[i] <- length(which(D1$cohens_d12<0))/nrow(D1)
    
  }
  if(totDF$cohens_d13[i]>0){
    totDF$percent0_13[i] <- length(which(D1$cohens_d13>0))/nrow(D1)
  } else{
    totDF$percent0_13[i] <- length(which(D1$cohens_d13<0))/nrow(D1)
    
  }
  if(totDF$cohens_d23[i]>0){
    totDF$percent0_23[i] <- length(which(D1$cohens_d23>0))/nrow(D1)
  } else{
    totDF$percent0_23[i] <- length(which(D1$cohens_d23<0))/nrow(D1)
    
  }
  
  ggplot(data = D1, aes(x = cohens_d12, fill = dow)) + 
    geom_histogram(position = "identity", alpha = 0.7, binwidth = .01)
  
  D2<-D1[D1$dow == "Friday",]
  
  satDF$nov_mean1[i] <- mean(D2$nov_mean1)
  satDF$nov_mean2[i] <- mean(D2$nov_mean2)
  satDF$nov_mean3[i] <- mean(D2$nov_mean3)
  
  satDF$nov_sd1[i] <- mean(D2$nov_sd1)
  satDF$nov_sd2[i] <- mean(D2$nov_sd2)
  satDF$nov_sd3[i] <- mean(D2$nov_sd3)
  
  satDF$cohens_d12[i] <- mean(D2$cohens_d12)
  satDF$cohens_d13[i] <- mean(D2$cohens_d13)
  satDF$cohens_d23[i] <- mean(D2$cohens_d23)
  
  satDF$quantLow_12[i] = quantile(D2$cohens_d12, probs = .025)
  satDF$quantHigh_12[i] = quantile(D2$cohens_d12, probs = .975)
  satDF$quantLow_13[i] =  quantile(D2$cohens_d13, probs = .025)
  satDF$quantHigh_13[i] = quantile(D2$cohens_d13, probs = .975)
  satDF$quantLow_23[i] = quantile(D2$cohens_d23, probs = .025)
  satDF$quantHigh_23[i] = quantile(D2$cohens_d23, probs = .975)
  
  if(satDF$cohens_d12[i]>0){
    satDF$percent0_12[i] <- length(which(D2$cohens_d12>0))/nrow(D2)
  } else{
    satDF$percent0_12[i] <- length(which(D2$cohens_d12<0))/nrow(D2)
    
  }
  if(satDF$cohens_d13[i]>0){
    satDF$percent0_13[i] <- length(which(D2$cohens_d13>0))/nrow(D2)
  } else{
    satDF$percent0_13[i] <- length(which(D2$cohens_d13<0))/nrow(D2)
    
  }
  if(satDF$cohens_d23[i]>0){
    satDF$percent0_23[i] <- length(which(D2$cohens_d23>0))/nrow(D2)
  } else{
    satDF$percent0_23[i] <- length(which(D2$cohens_d23<0))/nrow(D2)
    
  }
  
  
}


totDF[,2:19] <- round(totDF[,2:19],3)
totDF$cohens_12 <- paste(totDF$cohens_d12, " (", totDF$quantLow_12, ", ", totDF$quantHigh_12, ")")
totDF$cohens_13 <- paste(totDF$cohens_d13, " (", totDF$quantLow_13, ", ", totDF$quantHigh_13, ")")
totDF$cohens_23 <- paste(totDF$cohens_d23, " (", totDF$quantLow_23, ", ", totDF$quantHigh_23, ")")
write.csv(totDF,"Distance_totDF.csv",row.names=FALSE)

satDF[,2:19] <- round(satDF[,2:19],3)
satDF$cohens_12 <- paste(satDF$cohens_d12, " (", satDF$quantLow_12, ", ", satDF$quantHigh_12, ")")
satDF$cohens_13 <- paste(satDF$cohens_d13, " (", satDF$quantLow_13, ", ", satDF$quantHigh_13, ")")
satDF$cohens_23 <- paste(satDF$cohens_d23, " (", satDF$quantLow_23, ", ", satDF$quantHigh_23, ")")
write.csv(satDF,"Distance_FriDF.csv",row.names=FALSE)
