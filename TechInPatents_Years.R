source("FreePatR.R")
library(tidyverse)
library(reshape2)


query_table <- read_delim("ket_list.csv", delim= ";") %>% 
  group_by(KET_name) %>% 
  mutate(query= str_c(english_name, Synonym, sep = "; ")) %>% 
  mutate(query= ifelse(is.na(query), english_name, query)) %>% 
  select(-english_name, -Synonym)


query_table <- query_table %>% 
  mutate(query= str_to_lower(query))  %>% 
  mutate(query= str_replace_all(query, "; ", "' OR '")) %>% 
  mutate(query= str_replace(query, "$", "'")) %>% 
  mutate(query= str_replace(query, "^", "'")) %>% 
  mutate(query= str_replace(query, " OR ''", ""))
  
Tech_Year <- matrix(nrow=nrow(query_table),ncol=10)

colnames(Tech_Year) <- c("2008","2009","2010","2011","2012","2013","2014", "2015", "2016", "2017")

rownames(Tech_Year) <- query_table[["KET_name"]]

for(i in 1:nrow(query_table)){
	for(y in 2008:2017){
	Tech_Year[i,(y-2007)] <- countpaper(query_table[[i, "query"]],y)
	}
}

Tech_Year_backup <- Tech_Year

#sottraggo il minimo e divido per il massimo

for(i in 1:nrow(Tech_Year)){
	
	Tech_Year[i,] <- Tech_Year[i,]-min(Tech_Year[i,])
	if(max(Tech_Year[i,])!=0){
	Tech_Year[i,] <- Tech_Year[i,]/max(Tech_Year[i,])}
	
}

xlsx::write.xlsx(Tech_Year_backup, "Tech_Year.xlsx")

df <- as.data.frame(Tech_Year)

df$metadata <- row.names(df)

df <- reshape2::melt(df, "metadata")
df$variable <- as.integer(df$variable) + 2007
df$metadata <- as.factor(df$metadata)

g <- ggplot(data = df, aes(x=variable, y=value)) +
  geom_point() + 
  coord_cartesian(ylim = c(0, 1)) + 
  geom_smooth(aes(colour = metadata, fill = metadata))+
  facet_wrap( ~ metadata) +
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") +
  ylab("") +
  scale_x_continuous(breaks=seq(2008, 2017, 2))
  

ggsave(plot = g, filename = "union_cam_trends.pdf", width = 30, height = 20, units = "cm")
 

