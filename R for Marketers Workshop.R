#####Installing Libraries######
#uncomment this line to install the packages, you only need to install them once
#install.packages(c('tidyverse', 'tidyquant', 'quantmod', 'lubridate', 'scales', 'plotly'))

#####Loading Libraries#######
library(tidyverse)
library(tidyquant)
library(quantmod)
library(lubridate)
library(scales)
library(plotly)

#Turn off scientific notation, helps for producing nicer looking plots
options(scipen=999)


#####basic data and variables########
#Assignment
x <- 42
x

y <- 5
y

a_list <- c(1, 7, 9, 15, 21)


#Basic operations
x / y
x * y
x ** y

a_list * y

x == y
x >= y
x != y

#Datafames
datasets::iris

#make a dataframe
df <- datasets::iris

#access parts of the dataframe
df[1,1]
df[,1]
df[1,]
df[1:5,]

head(df)
head(df,)
tail(df)

df$Species
unique(df$Species)
length(unique(df$Species))

summary(df)
sum(df$Sepal.Length)
mean(df$Sepal.Width)
max(df$Sepal.Length)

df$Sepal.Area <- df$Sepal.Length * df$Sepal.Width

#######R Basics Practice#############

#Instructions:
#  1. Load the video game sales data into R
#  2. Look at the head and tail of the dataframe
#  3. What is the mean sales?
#  4. What are the different platforms in this dataset?
#  5. How many unique publishers are there?







#Solutions
#1 alternately, you can use the 'Import Dataset' feature in RStudio
vgsales <- read_csv("vgsales.csv")

#2
head(vgsales)
tail(vgsales)

#3
mean(vgsales$Sales)

#4
unique(vgsales$Platform)

#5
length(unique(vgsales$Publisher))



################tidyr demo##################
#handling missing data
aq <- datasets::airquality

mean(aq$Ozone)
mean(aq$Ozone, na.rm = TRUE)

aq_drop <- drop_na(aq)

aq_fill <- replace_na(aq, replace = list(Ozone=mean(aq$Ozone, na.rm = T),
                                         Solar.R=mean(aq$Solar.R, na.rm = T)))

#reorganizing data
titanic <- data.frame(datasets::Titanic)
t_wide <- pivot_wider(titanic, names_from = Survived, values_from = Freq)
t_tidy <- pivot_longer(t_wide, cols = c(No, Yes), names_to = 'Survived', values_to = 'Freq')

cars <- datasets::mtcars
cars <- rownames_to_column(cars, var='car')
cars <- separate(cars, car, into=c('Make', 'Model'), sep = ' ', extra = 'merge')

################dplyr demo#################
#the pipe operator
df <- datasets::iris

filter(df, Species=="setosa")
df %>% filter(Species=="setosa")

length(unique(df$Species))
df$Species %>% unique() %>% length()

#manilupating data
df <- data.frame(datasets::mtcars) %>%
  rownames_to_column(var = "car") %>%
  separate(car, c('Make', 'Model'), sep = " ", extra = 'merge')

df2 <- df %>%
  group_by(Make) %>%
  summarise(Count = n(),
            Ave_Horsepower = mean(hp))



#Create the tibbles
sales <- tibble(
  SKU = c(1111,1111,1234,1234,4569,1234,1111,1111,1234,7890,1111,1234,7890),
  Customer = c("Planet Express", "Mom Co", "Mom Co", "Slurm Bev", "Planet Express",
               "Mom Co", "Planet Express", "Mom Co", "Mom Co", "Mom Co",
               "Slurm Bev", "Slurm Bev", "Slurm Bev"),
  Sales = c(1700,2555,345,1087,900,385,850,1925,380,4785,1280,1780,8100),
  Quantity = c(34,73,276,483,20,308,17,55,304,87,32,791,81)
)

deals <- tibble(
  SKU = c(1111,1111,1234,4569,1234,4569,7890,1111,7890,1234),
  Customer = c("Planet Express", "Mom Co", "Mom Co", "Mom Co", "Slurm Bev", "Planet Express",
               "Mom Co", "Slurm Bev", "Slurm Bev", "Planet Express"),
  Deal = c("No", "Yes", "Yes", "Yes", "Yes", "No", "Yes", "Yes", "No", "No")
)


#Merge the dataframes
merged <- left_join(sales, deals)
merged <- left_join(sales, deals, by=c('SKU', 'Customer'))

#analyze the merged data
results <- merged %>% 
  group_by(Customer, Deal) %>% 
  summarise(Total_Sales = sum(Sales)) %>%
  ungroup() %>%
  group_by(Customer) %>%
  mutate(Percent_Deal = Total_Sales / sum(Total_Sales))




#########tidyr and dplyr practice###########
#Instructions
#  1. What are total sales by Year? By Year and Region?
#  2. Which Genre has the highest sales?
#  3. Since 2010, which Publisher has the highest sales?
#  4. Which Publisher has been the most succesful in each Region?  



#  Challenge:
# Find the games that were released on more than one platform. 
# On how many platforms was it released? On which did it sell the best? 
# (Hint: look into the arrange(), slice() and n_distinct() functions)



 



#Solutions
#1
vgsales %>% group_by(Year) %>% summarise(Total_Sales = sum(Sales))
vgsales %>% group_by(Year, Region) %>% summarise(Total_Sales = sum(Sales))
  
#2
vgsales %>% 
  group_by(Genre) %>% 
  summarise(Total_Sales = sum(Sales)) %>%
  arrange(desc(Total_Sales)) %>%
  slice(1)

#3
vgsales %>% 
  filter(Year >= 2010) %>%
  group_by(Publisher) %>%
  summarise(Total_Sales = sum(Sales)) %>%
  arrange(desc(Total_Sales)) %>%
  slice(1)

#4
vgsales %>% 
  group_by(Publisher, Region) %>%
  summarise(Total_Sales = sum(Sales)) %>%
  arrange(desc(Total_Sales)) %>%
  ungroup() %>%
  group_by(Region) %>%
  slice(1)


#Challenge
vgsales %>% 
  group_by(Name, Platform) %>%
  summarise(Total_Sales = sum(Sales)) %>%
  ungroup() %>%
  group_by(Name) %>%
  mutate(n_platforms = n_distinct(Platform)) %>%
  filter(n_platforms > 1) %>%
  arrange(desc(Total_Sales)) %>%
  slice(1)

########Data Visualization Section###########
#Get data for the basic plots
###Basic Plotting########

#this funciton will pull the history for stock prices. Enter the ticker symbol and the number of days to look back.
GetStockHistory <- function(ticker, n_days){
  #get the stock history
  dat <- getSymbols(ticker, from=Sys.Date()-n_days, to=Sys.Date(), env = NULL)
  
  #Reformat into tidy data
  df<- rownames_to_column(data.frame(dat), 'Date')
  colnames(df) <- c('Date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
  df$Date <- lubridate::as_date(df$Date)
  df <- df %>% select(c(Date, Open, Close)) %>%
    pivot_longer(c(Open, Close), names_to = 'Open.Close', values_to = 'Price')
  return(df)
}

stock_df <- GetStockHistory('TSLA', 3650)


#ggplot2 version
ggplot(data=stock_df, aes(x=Date, y=Price, color=Open.Close)) +
  geom_line() +
  scale_y_continuous(labels = dollar) +
  labs(title="Stock Price Open and Close",
       subtitle="Last 10 Years",
       x=NULL,
       y="Stock Price") +
  theme_minimal() +
  theme(legend.position = c(0.1, 0.8))

#plotly version
plot_ly(data=stock_df, 
        x=~Date, y=~Price, color=~Open.Close, 
        type='scatter', mode='lines',
        opacity=0.5) %>%
  layout(title="Stock Price Open and Close",
         xaxis=list(title=""),
         yaxis=list(title="Stock Price", tickprefix="$"),
         legend=list(x=0.02, y=1)) 


#bar plots
#ggplot2 version
t <- data.frame(datasets::Titanic)
ggplot(t, aes(x = Sex, y = Freq, fill = Survived)) +
  geom_col(position = 'dodge')

#plotly version
plot_ly(t, x = ~Sex, y = ~Freq, color = ~Survived, type = 'bar')


#scatter plots
#ggplot2 version
ggplot(mtcars, aes(x = hp, y = qsec, color = disp)) +
  geom_point(size = 2)


#plotly version
plot_ly(mtcars, x = ~hp, y = ~qsec, color = ~disp, type = 'scatter', mode = 'markers', size = 2)


#faceted plots
#ggplot2 version
g <- ggplot(t, aes(x = Sex, y = Freq, fill = Survived)) +
  geom_col(position = 'dodge') +
  facet_wrap(~Class, nrow = 1)
g

ggplot(t, aes(x = Survived, y = Freq, fill = Survived)) +
  geom_col(position = 'dodge') +
  facet_grid(Sex ~ Class)

#plotly version
#option 1
ggplotly(g)

#option 2
subplot(
  plot_ly(t[t$Class=='1st',], x=~Sex, y=~Freq, color=~Survived, type='bar'),
  plot_ly(t[t$Class=='2nd',], x=~Sex, y=~Freq, color=~Survived, type='bar'),
  plot_ly(t[t$Class=='3rd',], x=~Sex, y=~Freq, color=~Survived, type='bar'),
  plot_ly(t[t$Class=='Crew',], x=~Sex, y=~Freq, color=~Survived, type='bar'),
  nrows = 1
)

#option 3
plot_ly(t, x=~Sex, y=~Freq, color=~Survived, frame=~Class, type='bar')

#box plots
#ggplot2
ggplot(mtcars, aes(x = cyl, y = hp, fill = cyl,group = cyl)) +
  geom_boxplot()

#plotly
plot_ly(mtcars, x = ~factor(cyl), y = ~hp, color = ~factor(cyl), type = 'box')


#########Plotting Practice#############
#  1. With the 'diamonds' dataset, make a scatter plot of carat vs price and colored by clarity
#  2. With 'diamonds' make a box plot carat on y, cut on x. (Break out by color for extra practice)
#  3. Add axis labels and titles to one of the previous charts. (You can use the stock charts above as a reference)
#  4. With 'diamonds' make any chart you'd like, but incorporate some sort of faceting.


#Challenge:
#Convert the 'UCBAdmissions' dataset to a data frame (like with Titanic). 
#Create a visualization that shows the number of students admitted or rejected by gender and department.






#solutions

#1
ggplot(diamonds, aes(x = carat, y = price, color = clarity)) +
  geom_point()

plot_ly(diamonds, x=~carat, y=~price, color=~clarity, type='scatter', mode='markers')

#2
ggplot(diamonds, aes(x = cut, y = carat, fill = color)) +
  geom_boxplot()

plot_ly(diamonds, x=~cut, y=~carat, color=~color, type='box') %>%
  layout(boxmode = 'group')

#3
ggplot(diamonds, aes(x = cut, y = carat, fill = color)) +
  geom_boxplot() +
  labs(title = 'Diamonds Box Plot Example',
       x = 'Diamond Cut',
       y = 'Number of Carats')

plot_ly(diamonds, x=~cut, y=~carat, color=~color, type='box') %>%
  layout(boxmode = 'group',
         title = 'Diamonds Box Plot Example',
         xaxis = list(title = 'Diamonds Cut'),
         yaxis = list(title = 'Number of Carats'))

#4
ggplot(diamonds, aes(x = carat, y = price, color = color)) +
  geom_point() +
  facet_grid(cut ~ clarity)

plot_ly(diamonds, x=~carat, y=~price, color=~color, frame=~cut, type='scatter', mode='markers')


#challenge
berkley <- data.frame(UCBAdmissions)

ggplot(berkley, aes(x = Dept, y = Freq, fill = Admit)) +
  geom_col(position = 'dodge') +
  facet_wrap(~Gender, nrow = 2)

plot_ly(berkley, x = ~Gender, y = ~Freq, color = ~Admit, frame = ~Dept, textposition = 'auto', type = 'bar')





########Advanced Visualization Examples##########
vgsales <- read_csv("~/R Scripts/vgsales.csv")
#Pareto Chart
ParetoPlot <- function(df, x, y){
  x <- enquo(x)
  y <- enquo(y)
  
  t <- df %>%
    mutate(x_value = !!x) %>%
    group_by(x_value) %>%
    summarise(Total = sum(!!y)) %>%
    arrange(desc(Total)) %>%
    mutate(Percent = Total / sum(Total),
           Cum_Percent = cumsum(Percent)) %>%
    mutate(x_value = factor(x_value, levels = x_value))
  
  plot_ly() %>%
    add_bars(data=t, x=~x_value, y=~Total, name="Total") %>%
    add_lines(data=t, x=~x_value, y=~Cum_Percent, yaxis='y2', name="Percent") %>%
    layout(yaxis2=list(overlaying = 'y', side='right', title="Cumulative Percent", tickformat="%"),
           yaxis=list(title=paste("Total", as_label(y))),
           xaxis=list(title=paste(as_label(x))),
           margin=list(r=100),
           showlegend=FALSE)  
}

vgsales %>% ParetoPlot(Publisher, Sales)

vgsales %>% filter(Publisher == 'Nintendo') %>% ParetoPlot(Name, Sales)

#Sankey Diagram
#mock email marketing campaign success data
plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = c("Sent", "Opened", "Clicked", "Engaged", "Purchase", "No Purchase"),
    color = c("grey", "blue", "blue", "blue", "green", "red"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = c(0,0,1,1,2,3,3),
    target = c(1,5,2,5,3,5,4),
    value =  c(24,76,12,12,8,4,4)
  )
) %>% 
  layout(
    title = "Email Marketing Campaign",
    font = list(
      size = 10
    )
  )

#Sankey Diagram with an alternate package
install.packages('networkD3')
library(networkD3)
links <- data.frame(
  source=c('Email Sent', 'Email Sent', 'Opened', 'Opened', 'Clicked', 'Clicked', 'Engaged', 'Engaged'), 
  target=c('No Sales', 'Opened', 'No Sales', 'Clicked', 'No Sales', 'Engaged', 'No Sales', 'Made Purchase'), 
  value=c(76, 24, 12, 12, 4, 8, 4, 4)
)

nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)

#Funnel Diagram showing the same data
plot_ly(type = 'funnel',
        y = c('Emails Sent', 'Emails Opened', 'Link Clicked', 'Engaged with Content', 'Made Purchase'),
        x = c(100, 24, 12, 8, 4)
) %>%
  layout(yaxis = list(categoryarray = c('Emails Sent', 'Emails Opened', 'Link Clicked',
                                        'Engaged with Content', 'Made Purchase')))


#Treemap Diagram
install.packages('treemap')
library(treemap)

df <- vgsales %>%
  group_by(Platform, Publisher) %>%
  summarise(Sales = sum(Sales))

treemap(df,
        index = c('Platform', 'Publisher'),
        vSize = 'Sales',
        type = 'index'
)


#Waterfall chart
x= list("PY Sales", "Churn", "Sales Growth", "Marketing Campaigns", "New Products", "Est NY Sales")
measure= c("relative", "relative", "relative", "relative", "relative", "total")
text= c("PY", "-15", "20", "40", "10", "Est NY")
y= c(200, -15, 20, 40, 10, 0)
data = data.frame(x=factor(x,levels=x),measure,text,y)

plot_ly(
  data, type = "waterfall", measure = ~measure,
  x = ~x, textposition = "outside", y= ~y, text =~text,
  connector = list(line = list(color= "rgb(63, 63, 63)"))) %>%
  layout(title = "Next Year Program Impact",
         xaxis = list(title = ""),
         yaxis = list(title = ""),
         autosize = TRUE,
         showlegend = FALSE)


#Sunburst Chart
#I confess, I borrowed the code here from plotly's website
d2 <- read.csv('https://raw.githubusercontent.com/plotly/datasets/718417069ead87650b90472464c7565dc8c2cb1c/sunburst-coffee-flavors-complete.csv')
plot_ly(
    ids = d2$ids,
    labels = d2$labels,
    parents = d2$parents,
    type = 'sunburst',
    maxdepth = 3,
    domain = list(column = 1)
  ) %>%
  layout(
    margin = list(l = 0, r = 0, b = 0, t = 0))

