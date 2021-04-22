setwd("D:/Personal/Projects/EDA")
asean=readxl::read_xlsx("D:/Personal/Projects/EDA/ASEANDecade.xlsx")

library(plotly)
library(ggplot2)
library(gganimate)
library(corrplot)
library(maps)

# Bubble
asean$time=as.character(asean$time)
p=plot_ly(
  x = asean$`Civil liberties index`, 
  y = asean$`Political participation index`, 
  size = asean$`Democracy index`, 
  color = asean$name, 
  frame = asean$time, 
  text = asean$name,
  sizes = c(10,1000),
  hoverinfo = "text",
  type = 'scatter',
  mode = 'markers'
)

x=list(title="Civil Liberties Index")
y=list(title="Political Participation Index")
p=p%>%layout(title="Bubble Plot by Countries",
             xaxis=x,yaxis=y)
print(p)

# Line
asean$time=as.character.numeric_version(asean$time)
ggplot(asean,aes(time,`Democracy index`,color=name,group=name))+
  geom_line()+
  ggtitle('Democracy Index by Year')+
  xlab("Year")

# Violin
ggplot(asean,aes(x=name,y=`Democracy index`,fill=name))+
  geom_violin(scale = 3,trim = F)+
  stat_summary(fun.y = median,geom = "point",size=1,col="red")+
  labs(title = 'Violin Plot of Democracy Index',subtitle = 'by Countries')+
  xlab("Country")

# Boxplot
ggplot(asean,aes(x=name,y=`Democracy index`,fill=name))+geom_boxplot()+
  ggtitle('Boxplot of Democracy Index',subtitle = "by Year")+
  xlab("Country")

# Correlation Plot
corrdata = asean[, c(4, 6:10)]
m=cor(corrdata)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(m, method="color", col=col(200),type = 'lower',
         addCoef.col = "black",
         tl.col = 'black',tl.srt = 20,cl.pos = 'b',
         number.cex = 0.8)

# Scatterplot with jittered
ggplot(asean,aes(asean$`Civil liberties index`,`Democracy index`,col=name))+
  geom_point()+
  geom_jitter()+
  geom_smooth(col="red",method = "lm",se = F)+
  ggtitle("Scatter Plot with Jittered Points",subtitle = "Democracy Index vs Civil Liberties Index")+
  xlab("Civil Liberties Index")

ggplot(asean,aes(asean$`Electoral pluralism index`,`Democracy index`,col=name))+
  geom_point()+
  geom_jitter()+
  geom_smooth(col="red",method = "lm",se = F)+
  ggtitle("Scatter Plot with Jittered Points",subtitle = "Democracy Index vs Electoral Pluralism Index")+
  xlab("Electoral Pluralism Index")

ggplot(asean,aes(asean$`Government index`,`Democracy index`,col=name))+
  geom_point()+
  geom_jitter()+
  geom_smooth(col="red",method = "lm",se = F)+
  ggtitle("Scatter Plot with Jittered Points",subtitle = "Democracy Index vs Government Index")+
  xlab("Government Index")

ggplot(asean,aes(asean$`Political culture index`,`Democracy index`,col=name))+
  geom_point()+
  geom_jitter()+
  geom_smooth(col="red",method = "lm",se = F)+
  ggtitle("Scatter Plot with Jittered Points",subtitle = "Democracy Index vs Political Culture Index")+
  xlab("Political Culture Index")



## Map
# Data 
asean_map=map_data('world',region = c('Indonesia','Laos','Malaysia','Philippines',
                                  'Thailand','Myanmar','Cambodia','Vietnam','Singapore'))
head(asean_map)

data2019 = asean[asean$time==2019,]

#Merge
data=merge(asean_map,data2019,
           by.x = 'region',
           by.y = 'name')

#Plot
map=ggplot(data,aes(long,lat,group=group,fill=`Democracy index`))+
  geom_polygon(col="grey")+
  ggtitle('Democracy Index',subtitle = 'Year 2019')+
  scale_fill_gradient2(mid = '#960800' ,high = '#FF554D')+
  scale_x_log10()+
  xlab(" ")+ylab(" ")

map
