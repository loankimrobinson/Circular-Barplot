



library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)



######################################################
# 
Death_Injure <- read.csv("/Users/loanrobinson/GitHub/GitHub/Circular BarPlot/Death and Injuries by Gender and Age 2016.csv",header = TRUE,stringsAsFactors = FALSE)
str(Death_Injure)

#####################################
# CIRCULAR BARPLOT
# Create dataset

# Combind Death_Injure_and Gender together (so we have total 4 group)
# Death-Male, Death-Female, Injure-Male, and Injure-Female
Death_Injure$group_cb <- paste0(Death_Injure$Death_Injure,"-",Death_Injure$group,sep="")

# Order Fire_Death_Percent from small to large by group
Death_Injure <-  Death_Injure %>% arrange(group_cb, Fire_Death_Percent)


#Create fake 3 rows NA, so we have a gap between group
Death_Injure <- do.call("rbind",lapply(split(Death_Injure,Death_Injure$group_cb), function(i) {
  new <- rbind(i,i[NA,][1:3,])
  new[,6] <- i[1:3,6]
  return(new)
}))

# Create id for each row
Death_Injure$id=seq(1, nrow(Death_Injure))


# Get the name and the y position of each label
label_data <- Death_Injure
number_of_bar <- nrow(label_data) # 84 rows
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust  <-ifelse( angle < -90, 1.2, -0.2)
label_data$angle  <-ifelse(angle < -90, angle+180, angle)


# This one needs to be matched with row NA you created
empty_bar = 3

#table(Death_Injure$group_cb)

# prepare a data frame for base lines
# Each group we have 21 bars 
# so base_data will start from 1, 22, 43, 64
# base_data will end from 18, 39, 60, 681

base_data <- Death_Injure %>% 
                         group_by(group_cb) %>% 
                         summarise(start=min(id), end= max(id) - empty_bar) %>% 
                         rowwise() %>% 
                          mutate(title=mean(c(start, end)))

base_data 


# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

range(Death_Injure$Fire_Death_Percent, na.rm = TRUE)


breaks <- c(rep("",8),"Death-Female",rep("",9))
length(c(breaks,"","","",breaks,"","","",breaks,"","","",breaks,"","",""))

# Make the plot
p <-  ggplot(Death_Injure, aes(x=as.factor(id), y=Fire_Death_Percent, fill=group_cb)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=Fire_Death_Percent, fill=group_cb), stat="identity", alpha=0.999) +
  scale_fill_manual(name = "",values = c("#8c510a","#35978f","#01665e","#dfc27d"),labels=c("Death Female", "Death Male","Injuries Female","Injuries Male"))+
  coord_polar() +
  
  geom_segment(data=grid_data, aes(x = end, y = 12, xend = start, yend = 12), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 8, xend = start, yend = 8), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 4, xend = start, yend = 4), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  annotate("text", x = rep(max(Death_Injure$id)-1,4), y = c(1, 4, 8, 12), label = c("1", "4", "8", "12") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +

  ylim(-12,12) +
  theme_minimal() +
  labs(title = "Death/ Injuries Rate in a Fire by Age and Gender in 2016",
       subtitle = "Sources: National Center for Health Statistics and U.S. Census Bureau",
       caption = "Note: Data have been adjusted to account for unknown or unspecified ages.")+
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(color="steelblue", size=16, face="bold.italic",hjust = 0.5),
    plot.subtitle = element_text(color="steelblue",hjust = 0.5),
    plot.caption = element_text(color="#993333",hjust = 0.5, face="italic"),
    plot.margin = unit(rep(0.5,4), "cm") 
  ) +
  geom_text(data=label_data, aes(x=id, y=Fire_Death_Percent, label=Age, hjust=hjust), 
            color="steelblue", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE )

p
ggsave("C:/Users/LOAN/GitHub/Circular BarPlot/Circular_BarPlot.png", width = 20, height = 20, units = "cm", dpi = 700)
