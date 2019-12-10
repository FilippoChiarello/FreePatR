setwd("/Users/filippochiarello/Desktop/Technimeter")

my.dict <- read.delim("technimeter.txt")


MyDict <- c()

for (i in 1:nrow(my.dict)){
	MyDict <- c(MyDict, as.character(my.dict[i,1]))
	
	
}

#MyDict <- MyDict[200:400]

Tech_number <- matrix(nrow=length(MyDict),ncol=1)

colnames(Tech_number) <- c("N.of_Patents")

rownames(Tech_number) <- MyDict

for(i in 1379:nrow(Tech_number)){

	Tech_number[i,1] <- countpaper(MyDict[i])
	
}



write.csv(Tech_number,"Tech_number.csv",)


Tech_number <- Tech_number [Tech_number<10000000]