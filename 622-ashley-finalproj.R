#cricket analysis
#importing the datasets
df1 <- read_csv('/Users/ashley/Desktop/EM-622/final project/cricket_ball.csv')
df2 <- read_csv('/Users/ashley/Desktop/EM-622/final project/cricket_matches.csv')
df3 <- read_csv('/Users/ashley/Desktop/EM-622/final project/batsman_info.csv')
df4 <- read_csv('/Users/ashley/Desktop/EM-622/final project/teamwise_home_and_away.csv')

#viewing the top 10 lines of each dataset
head(df1, 10)
head(df2, 10)
head(df3, 10)
head(df4, 10)

#printing the column names of each dataset
colnames(df1)
colnames(df2)
colnames(df3)
colnames(df4)

#viewing the datasets
View(df1)
View(df2)
View(df3)
View(df4)

#########################           1   geom_bar plot    #########################   

#transform date column into separate date,month,year 
df2$day <- format(as.Date(df2$date), "%d")
df2$month <- format(as.Date(df2$date), "%m")
df2$year <- format(as.Date(df2$date), "%Y")

# Q1- To find out most matches w.r.t season
df2 %>% 
  group_by(year) %>%
  summarize(number_of_matches = n())

#plotting the bar plot
#Creating a palette for more colors
#Define the number of colors you want
nb.cols <- 13
mycolors <- colorRampPalette(brewer.pal(6, "Set1"))(nb.cols)

#plotting using bar plot
 df2 %>% 
  group_by(year) %>%
  summarize(number_of_matches=n(),.groups='drop') %>%
  ggplot(aes(x=year, y= number_of_matches, fill=year)) + geom_bar(stat = "identity") +
  geom_col(colour = "black") +
  scale_fill_manual(values = mycolors) +
  theme_minimal()+
  labs(x="Season",y="Number of Matches", title ="Season wise number of matches")+ # giving title names and axis name
  theme(plot.title=element_text(face = "bold",hjust = 0.5,size = 20),  #adjusting size of each title axis and its elements
axis.title = element_text(size = 15), axis.text = element_text(size = 10),
legend.title = element_text(size = 15), legend.text = element_text(size = 10))+
  #
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #making the background cleaner
               panel.background = element_blank(), axis.line = element_line(colour = "black")) 


#########################         2  Stacked/gropued geom_bar plot    ######################### 
 
# Q2- analysis of toss decision  
results <- df2 %>%
  drop_na(result) %>%
  group_by(result, toss_decision) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

 #plotting using bar plot
ggplot(data = results, aes(x = result, y = count, fill = toss_decision)) +
  geom_bar(stat = "identity") +
  labs(title = "Wins", x = "Result", y = "Matches", fill = "Toss Decision") +  # giving title names and axis name
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), panel.background = element_rect(fill = "burlywood1"),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),    #adjusting size of each title axis and its elements
        axis.title = element_text(size = 15), axis.text = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15),
        strip.text = element_text(size = 20)) +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 6) +
  scale_fill_manual(values = c("cornflowerblue", "cadetblue"))


#########################         3   geom_line plot      #########################  

# Q3 - to find out Frequency of matches year wise 
a <- df2 %>% 
    group_by(year) %>%
   summarize(number_of_matches = n())
View(a)
#plotting using line plot 
ggplot(a) + aes(x=year,y=number_of_matches, group =1 ) +  
  geom_line(color = "red") + labs(title = "Matches played" , x= "Year" , y= "Frequency") + #adding title and axis names
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), panel.background = element_rect(fill = "white"),  #adjusting title axis and element position and size
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.line = element_line(colour = "black") ,
        axis.title = element_text(size = 15), axis.text = element_text(size = 10))


#########################         4    scatter plot graph   #########################  

#Q4- analysis of my favorite batsmen 
#finding out unique values in batsman column in df1
unique(df1$batsman)
#renaming two column names in df 1 to avoid error
d <- df1 %>% 
  rename("playerdismissed" = "player_dismissed")
e <- d %>% 
  rename("dismissalkind" = "dismissal_kind")
#viewing new dataset with renamed columns
View(d)
colnames(e)

#plotting using scatter plot
# filtering out favorite batsman from the batsman column for plotting 
e %>% filter(batsman=="V Kohli"| batsman=="MS Dhoni" |batsman=="RV Uthappa"|batsman=="KL Rahul") %>% group_by(id) %>%
  mutate(cum_run= cumsum(batsman_runs),cum_ball=1:length(id)) %>%
  filter(playerdismissed=="V Kohli"|playerdismissed=="MS Dhoni" |playerdismissed=="RV Uthappa"|playerdismissed=="KL Rahul") %>% 
  ggplot(aes(cum_ball,cum_run,col=batsman)) +geom_point() +
  labs(title = "Runs scored vs balls faced in all the matches" , x = "Balls", y = "Runs" ) + # giving title and axis names
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.line = element_line(colour = "black") ,  #adjusting the theme and title and axis names and size
        axis.title = element_text(size = 15), axis.text = element_text(size = 10),
        legend.title = element_text(size = 15), legend.text = element_text(size = 8))


####################### 5 pie chart #########################

# Q5- finding out teams with most number of wins 
# finding out na values in winnner column 
is.na(df2$winner)
#dropping unwanted column
df2 <- df2[ -c(15) ]
#viewing dataset
View(df2)
#dropping na values in df2
df5 <- df2[complete.cases(df2), ]
#finding unique values
unique(df5$winner)
View(df5)
#creating data for plotting
pie_data <- df5 %>% 
  group_by(winner) %>%
  summarize(wins = n() , .groups = 'drop')
View(pie_data)
#rename column winner 
pie_data1 <- pie_data %>% 
  rename("Teams" = "winner")

#adding a few columns like percent to ad to the pir chart
table_percent <- pie_data1 %>%
  mutate(Teams = factor(Teams, 
                       levels = Teams[length(Teams):1]),
         cumulative = cumsum(wins),
         midpoint = cumulative - wins / 2,
         labels = paste0(round((wins/ sum(wins)) * 100, 1), "%"))
View(table_percent)

##plotting using pie chart
ggplot(table_percent, aes(x="", y=wins, fill=Teams)) +
  geom_bar(stat="identity", width=1,color = "white",alpha = 1.5) +
  coord_polar("y", start=0)+
  theme_void()+
  
  #adding title and axis names and adjusting position and size
theme(plot.title = element_text(hjust=0.5),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()) +
  labs(fill = "Teams",
       x = NULL,
       y = NULL,
       title = "Number of wins per teams ")+
  geom_text(aes(x = 1.2, y = midpoint , label = labels), color="black",
            fontface = "bold")


#########################   6 Heat map    #########################   

# Q6- analysing player performance 
# renaming columns for better plot aesthetics 
datafile <- df4 %>%
  rename("Home wins" = "home_wins",
         "Away wins" = "away_wins",
         "Home matches" = "home_matches",
         "Away matches" = "away_matches",
         "Home win %" = "home_win_percentage",
         "Away win %" = "away_win_percentage")
View(datafile)
# creating a dataframe for heat map
treemap_df1 <- as.data.frame(datafile)
View(treemap_df1)
row.names(treemap_df1) <- treemap_df1$team
#converting to matrix
team_heatmap <- treemap_df1[1:14,2:7]
team_heatmap_matrix <- data.matrix(team_heatmap)
#plotting heat map using the matrix created 
View(team_heatmap_matrix)
team_heatmap_plot <- heatmap(team_heatmap_matrix, Rowv = NA,Colv = NA,margins = c(9,7),
                                col = brewer.pal(9,"Reds"), scale="column",
                                main = "Team performance information")

#########################  7 Heat map 2  #########################   

# Q7- analysing top 50 batsman information
#creating dataframe 
treemap_df <- as.data.frame(df3)
View(treemap_df)
row.names(treemap_df) <- treemap_df$batsman
# creating a matrix
batsman_heatmap <- treemap_df[1:50,2:6]
batsman_heatmap_matrix <- data.matrix(batsman_heatmap)
#plotting heat map using matrix
batsman_heatmap_plot <- heatmap(batsman_heatmap_matrix, Rowv = NA,Colv = NA,margins = c(9,3),
                                col = brewer.pal(9,"Blues"), scale="column",
                                main = "Top 50 batsman information")

#########################   Extra - geom_col plot      #########################   

#finding teams with most number of wins
df2 %>% 
  group_by(winner) %>%
  summarize(wins = n() , .groups = 'drop')

#plotting the graph
df2 %>% 
  group_by(winner) %>%
  summarize(wins = n(), .groups= 'drop') %>%
  ggplot(aes(x=wins, y=winner, fill=winner)) + geom_col(position="dodge") + 
  labs(x="Number of Wins", y="Team", title = "Number  of Matches by Team") 

###################################################################   
#################  interactive chart 1 - Line chart  ##############
###################################################################

#loading googleVis library
library(googleVis) 
# dropping few unwanted columns
line_data <- df4[ -c(3:7) ]
View(line_data)
#plotting line graph using gvisline chart
line <- gvisLineChart(line_data, options=list(legend='right', width=900, 
                                       height=900)) 
plot(line)

####################  interactive chart 2 - bar chart  ############

# dropping unwantded columns
bar_data <- df4[-c(2,4:7)]
#plotting bar plot using gvis bar chart
bar <- gvisBarChart(bar_data, 
                   options=list(
                     title="Interactive Bar chart",
                     legend="bottom",
                     width = 1200,
                     pointSize=10))
plot(bar)

######################### gvis merge #########################

# using gvismerge to plot both the chart in one page
merge <- gvisMerge(line, bar, horizontal = FALSE, tableOptions = "border=\"0\"")
plot(merge)


