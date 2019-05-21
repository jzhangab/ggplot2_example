library(ggplot2)
library(dplyr)

# Global declarations
dataset <- dataset[order(-dataset$CountMax),]
lendat <- length(dataset$Company)
peers<- c('M', 'B', 'E', 'Ba', 'T', 'J', 'A', 'R', 'BD' ,'D', 'S')
current_year <- format(Sys.Date(), "%G")
current_year_1 <- as.character(as.numeric(current_year)-1)
current_year_2 <- as.character(as.numeric(current_year)-2)
ylist <- c(current_year, current_year_1, current_year_2)

# Calculate max heights
total <- dataset %>%
  group_by(Company, Year) %>%
  summarise(sum_Recalls = sum(Recalls))
max_recalls <- as.numeric(max(total$sum_Recalls, na.rm = TRUE))
max_y <- max_recalls * 1.25

#offset for total labels
offset <- max_y/20

i <- sapply(dataset, is.factor)
dataset[i] <- lapply(dataset[i], as.character)

# Add rows if company does not exist
dataset$match = paste(dataset$Company, dataset$Year, sep="_")
for (p in peers){
  for (y in ylist){
    m = paste(p, y, sep="_")
    if ( m %in% dataset$match ){
      print(m)
    } else{
      tempdf<-data.frame(p,"1_Unrated", 0, 0, y, m)
      names(tempdf)<-c("Company","Class", "CountMax", "Recalls", "Year", "match")
      dataset <- rbind(dataset, tempdf)
    }
  }
}

# Create list of companies based on order of maximum current year recalls
l <- list()
for(i in 1:nrow(dataset)) 
{
  comp <- dataset[i, "Company"]
  print(comp)
  if (comp %in% l){
    print('Repeat')
  } else{
    l <- c(l, comp)
  }
}

dataset$ordering = factor(dataset$Company, levels=l)

mdata <- merge(dataset,total,by=c("Company","Year"))
ccc <- c('#c1c1c1', '#73e371', '#F4CA0F', '#78ccec')
lll <- c("Unrated", "Minor", "Major", "Severe")

ggplot(dataset, aes(x = Year, y = Recalls) ) + 
  geom_col(position = position_stack(reverse = TRUE), aes(fill=Class)) + 
  geom_text(aes(x=Year, y=Recalls, label=Recalls), size=5, position = position_stack(vjust=.5))+
  geom_text(aes(x = Year, y = sum_Recalls+offset, label = sum_Recalls), size = 8, data = mdata) + 
  facet_grid(.~ordering) +
  scale_color_manual(labels = c("Unrated", "Minor", "Major", "Severe"), values = c("blue", "red", "purple", "green")) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.title.x=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 12, margin = margin(0, 0, 10, 10)),
        strip.background = element_rect(colour="black", fill="white"),
        panel.spacing=unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.1) )+
  scale_fill_manual(labels = lll, values= ccc) + 
  guides(fill=guide_legend(
    keywidth=.3,
    keyheight=.4,
    default.unit="inch")
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(-1*offset/2, max_y)) 

