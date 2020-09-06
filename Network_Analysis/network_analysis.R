net_an<- read.csv("C:/Users/heena/OneDrive/Desktop/TAMU/NetworkAnalysis/network_sample.csv", header=T, na.strings=c("","NA")) #reading data
net_an_df <- data.frame(net_an)#putting in data frame
length (unique(net_an_df$tpi1))

no_of_tpi <- length (unique(net_an_df$tpi1))
no_of_tpi 
temp_min = pmin(net_an_df$tpi1, net_an_df$tpi2) #swapping a,b if a>b
#We need unique pairs of (tpi1,tpi2) as we find interaction between a given pair

net_an_df$tpi1 = pmax(net_an_df$tpi1, net_an_df$tpi2)
net_an_df$tpi2 = temp_min

# Where ans is 1, is only tie we want to consider
net_an_df$binary_ties <- ifelse(net_an_df$ans ==1, 1, 0) 
net_an_df_new <- net_an_df[ -c(4) ] #we just need to see "yes" replies for our measures
net_an_df_new
net_an_df_new2 <- net_an_df_new[
  order( net_an_df_new[,1], net_an_df_new[,2] ), #ordering by tpi's to see duplicates easily
]
#removing duplicates
#Here we see for some pairs (3,1) and (3,5), one of them answers yes and other is not certain for same type of tie
#We decide to keep yes entry
net_an_deduped.data <- unique( net_an_df_new2[ , 1:4 ] ) 

net_an_deduped.data[is.na(net_an_deduped.data)] <- 0

#grouping by tpi's and counting total ties for both types of ties
data_result <- aggregate( as.matrix(net_an_deduped.data$binary_ties), as.list(net_an_deduped.data[,1:2]), FUN = sum) 
names(data_result)[names(data_result) == "V1"] <- "M1.3"
data_result

data_result$M1.2 <- ifelse(data_result$M1.3>0, 1,0)
#Density calculation:
Num <- sum(data_result$M1.3)
Deno <- ((no_of_tpi)*(no_of_tpi-1)/2)
Density <- Num/Deno * 100

R <- data_result[order(data_result[,1]), ]
write.csv(data_result,"C:/Users/heena/OneDrive/Desktop/TAMU/NetworkAnalysis//R.csv", row.names = FALSE)


dataty <- data.frame(append(data_result$tpi1, data_result$tpi2))
dataty$M1.2 <- data.frame(append(data_result$M1.2, data_result$M1.2))
colnames(dataty)
names(dataty)[1] <- "TPI"
names(dataty)[2] <- "M1.2"

dataresult2 <- aggregate(dataty$M1.2, by=list(Category=dataty$TPI), FUN=sum)
names(dataresult2)[2] <- "connect" 
dataresult2$central <- max(dataresult2$connect)-dataresult2$connect
num1 <- sum(dataresult2$central)
den1 <- (no_of_tpi-1)*(no_of_tpi-2)
Degree_of_central <- sum(dataresult2$central)/((no_of_tpi-1)*(no_of_tpi-2))









#newtable <- data.frame(union((data_result$tpi1),(data_result$tpi2)))
#names(newtable)[1] <- "TPI1"
#colnames(newtable)

                          
               




    
 


