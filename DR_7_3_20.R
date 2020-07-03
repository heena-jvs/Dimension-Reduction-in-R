data <- read.csv("C:/Users/heena/OneDrive/Desktop/Cornell/JOB/6_26_2020/NYSEG.csv", header=T, na.strings=c("","NA"))
which( colnames(project)=="Weekend.mileage" )
which( colnames(project)=="Moving" )
which( colnames(project)=="Media" )
which( colnames(project)=="Motivation_7_TEXT" )
which( colnames(project)=="Delay_4_TEXT" )
which( colnames(project)=="SeeIncentive_3")
which( colnames(project)=="InterestEnerProds_1")
which( colnames(project)=="Agreement_4")
which( colnames(project)=="CompareNghb")
which( colnames(project)=="DiscountDisplay_4_TEXT")
which( colnames(project)=="SeeIncentive_1")

cols <- c(37:46,81:83,91:97, 99:118)
dfnew <- data[,cols]
summary(dfnew)
dfnew[is.na(dfnew)] <- 0

which( colnames(dfnew)=="Agreement_1")
which( colnames(dfnew)=="Agreement_4")
which( colnames(dfnew)=="Barriers_1")

SpecialNames1 <- c("Agreement_1", "Agreement_2","Agreement_3", "Agreement_4")
for (name in SpecialNames1) {
  dfnew[,name] <- as.numeric(factor(dfnew[,name],levels=c("Disagree strongly", "Disagree somewhat", "Agree somewhat", "Agree strongly")))
}

SpecialNames2 <- c("Barriers_1","Barriers_2","Barriers_3","Barriers_4","Barriers_5","Barriers_6","Barriers_7","Barriers_8","Barriers_9")
for (name in SpecialNames2) {
  dfnew[,name] <- as.numeric(factor(dfnew[,name],levels=c("Always applies", "Sometimes applies", "Rarely applies", "Very often apples", "Never applies")))
}


SpecialNames3 <- c("ReasonsEE_1","ReasonsEE_2","ReasonsEE_3","ReasonsEE_4","ReasonsEE_5","ReasonsEE_6","ReasonsEE_7")
for (name in SpecialNames3) {
  dfnew[,name] <- as.numeric(factor(dfnew[,name],levels=c("Always", "Sometimes", "Rarely", "Never")))
}


SpecialNames4 <- c(14:19)
for (i in SpecialNames4) {
  dfnew[,i] <- as.numeric(factor(dfnew[,i],levels=c("Already have", "Very likely", "Somewhat likely", "Not likely to sign up")))
}

dfnew$ImportHomeEE <- gsub("10 - Extremely important", "10", dfnew$ImportHomeEE)


#col1 = c(37:40);

dfnew$ImportHomeEE <- gsub("10 - Extremely important", "10", dfnew$ImportHomeEE)

dfnew_clean <- dfnew[-c(1), ]   
dfnew_clean <- sapply(dfnew_clean, as.numeric)
dfnew_clean[is.na(dfnew_clean)] <- 0
summary(dfnew_clean)
#dfnew_clean <- scale(dfnew_clean, center = TRUE, scale = TRUE)

psych::corr.test(dfnew_clean)

nFactors::nScree(data.frame(dfnew_clean))
eigen(cor(data.frame(dfnew_clean)))

factanal(na.omit (dfnew_clean), 10, rotation="varimax")
factanal(na.omit (dfnew_clean), 12, rotation="varimax")

factanal(na.omit (dfnew_clean), 10, rotation="oblimin")
output <- factanal(na.omit (dfnew_clean), 12, rotation="oblimin")
print(output$loadings, cutoff = 0.737)
