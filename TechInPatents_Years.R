setwd("/Users/filippochiarello/Desktop/Technimeter")

my.dict <- read.delim("technimeter.txt")


MyDict <- c()

for (i in 1:nrow(my.dict)){
	MyDict <- c(MyDict, as.character(my.dict[i,1]))
	
	
}

#MyDict <- MyDict[200:400]

Tech_Year <- matrix(nrow=length(MyDict),ncol=10)

colnames(Tech_Year) <- c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")

rownames(Tech_Year) <- MyDict

for(i in 1:length(MyDict)){
	for(y in 2005:2014){
	Tech_Year[i,(y-2004)] <- countpaper(MyDict[i],y)
	}
}

Tech_Year_backup <- Tech_Year

#sottraggo il minimo e divido per il massimo

for(i in 1:nrow(Tech_Year)){
	
	Tech_Year[i,] <- Tech_Year[i,]-min(Tech_Year[i,])
	if(max(Tech_Year[i,])!=0){
	Tech_Year[i,] <- Tech_Year[i,]/max(Tech_Year[i,])}
	
}

Tech_Year <- as.data.frame(Tech_Year)

df <- Tech_Year[1:100,]

library(reshape2)
library(ggplot2)

df$metadata <- row.names(df)

df <- melt(df, "metadata")
df$variable <- as.numeric(df$variable)
df$metadata <- as.factor(df$metadata)

ggplot(df, aes(variable, value)) + geom_point() + facet_grid(~ metadata)

ggplot(data = df, aes(x=variable, y=value)) +
  geom_point() + 
  coord_cartesian(ylim = c(0, 1)) + 
  geom_smooth(aes(colour = metadata, fill = metadata))+
  facet_wrap( ~ metadata) +
  theme(legend.position="none")

  

