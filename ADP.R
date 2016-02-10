#help(ChickWeight)
data("ChickWeight")
data(ChickWeight)
head(ChickWeight)

#library("ggplot2")
ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick))+geom_line()
ggplot(ChickWeight, aes(x=Time, y=wieght, colour=Diet))+geom_line()
ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet))+geom_line()
ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet))+geom_point(alpha=.3)
ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet))+geom_point(alpha=.3)+geom_smooth(alpha=.2, size=1)

h<-ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet))
h+geom_point(alpha=.3)
h+geom_smooth(alpha=.2, size=1)
ggplot(subset(ChickWeight, Time==21), aes(x=weight, colour=Diet))+geom_density()

ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet))+geom_histogram(colour="black",binwidth=50)+facet_grid(Diet~.)
ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet))+geom_histogram(colour="black",binwidth=50)+facet_grid(.~Diet)

# help(mtcars)
data(mtcars)
head(mtcars)
p<-qplot(wt,mpg,colour=hp, data = mtcars) #mpg = miles/gallon
p+coord_cartesian(ylim=c(0, 40))
p+scale_colour_continuous(breaks = c(100,300)) # setting the hp's range
p+guides(colour = "colourbar")

#when the data is really big, we can't find what is in here, so we extract 10 observation from the front, but there is a risk that it could be bias.
m <-mtcars[1:10,]
p%+% m


c<-ggplot(mtcars, aes(factor(cyl)))
c+geom_bar()
c+geom_bar(fill="red")
c+geom_bar(colour="red")
c+geom_bar(fill="red",colour="red")
k<- ggplot(mtcars, aes(factor(cyl), fill=factor(vs)))
k+geom_bar()

data(economics)
head(economics)
b<-ggplot(economics, aes(x=date, y=unemploy))
b+geom_line()
b+geom_line(colour="red")
b+geom_line(colour="red", size=3)

b+geom_line(linetype=1)
b+geom_line(linetype=2)
b+geom_line(linetype=3)
b+geom_line(linetype=4)

df<-data.frame(x= rnorm(5000), y = rnorm(5000))
h<- ggplot(df, aes(x,y))
h + geom_point()
h + geom_point(alpha = 0.5)
h + geom_point(alpha = 1/10)

p<-ggplot(mtcars, aes(wt,mpg))
p + geom_point(size =4)
p + geom_point(aes(colour=factor(cyl)),size =4)
p + geom_point(aes(shape = factor(cyl)), size =4)

#library(reshape2)
#library(plyr)

rescale01<- function(x) (x -min(x)) / diff(range(x))
ec_scaled <- data.frame( date= economics $date, colwise(rescale01)(economics[,-(1:2)]))
ecm<-melt(ec_scaled, id ="date")

f<-ggplot(ecm, aes(date, value))
f+geom_line((aes(linetype = variable)))

data(diamonds)
head(diamonds)
k<-ggplot(diamonds, aes(carat))+geom_histogram(binwidth = 0.2)
k + facet_grid(.~cut)

w<- ggplot(diamonds, aes(clarity, fill =cut))
w+geom_bar()
w+geom_bar(aes(order = desc(cut)))

df<-data.frame(x = 1:10, y=1:10)
f<-ggplot(df, aes(x=x, y=y))
f+ geom_line(linetype =2)
f+ geom_line(linetype ="dotdash")

p<-ggplot(mtcars, aes(wt, mpg))
p + geom_point(size=4)
p + geom_point(aes(size=qsec))

p + geom_point(size = 2.5) + geom_hline(yintercept = 25, size = 3.5)
p + geom_point()
p + geom_point(shape = 5)
p + geom_point(shape = "k", size = 3)

p+geom_point(shape = ".", size = 10)
p + geom_point(shape = NA)
df2 <- data.frame(x = 1:5 , y = 1:25 , z = 1:25)
s<- ggplot(df2, aes(x = x ,y =y))
s+ geom_point(aes(shape =z), size =4) +scale_shape_identity()
dmod <- lm( price ~cut, data = diamonds)
cuts <- data.frame(cut = unique(diamonds$cut), predict(dmod, data.frame(cut = unique(diamonds$cut)), se = TRUE)[c("fit","se.fit")])

se <-ggplot(cuts, aes( x= cut, y= fit, ymin = fit - se.fit, ymax=fit + se.fit, colour = cut))
se+ geom_pointrange()
q<- ggplot(mtcars, aes(wt,mpg)) + geom_point()
q+annotate("rect", xmin=2, xmax = 3.5 , ymin=2, ymax= 25 , fill = "dark grey", alpha=.5)
q<- qplot(disp, wt, data=mtcars) + geom_smooth()
q + scale_x_continuous(limits= c(325,500))
d<-ggplot(diamonds, aes(carat,price))
d+stat_bin2d(bins = 25, colour = "grey50")
d+scale_x_continuous(limits=c(0,2))
qplot(cut, price, data=diamonds, geom="boxplot")
last_plot()+coord_flip()
qplot(cut, data=diamonds, geom="bar")
last_plot()+scale_x_reverse()

time<-seq(7000,3400,-200)
pop<-c(200,400,450,500,300,100,400,700,830,120,400,350,200,700,370,800,200,100,120)
grp<-c(2,5,8,3,2,2,4,7,9,4,4,2,2,7,5,12,5,4,4)
med<-c(1.2,1.3,1.2,0.9,2.1,1.4,2.9,3.4,2.1,1.1,1.2,1.5,1.2,0.9,0.5,3.3,2.2,1.1,1.2)
par(mar=c(5,12,4,4)+0.1)
plot(time,pop,axes=F,ylim=c(0,max(pop)),xlab="",ylab="", type = "l", col="black", main="",xlim=c(7000,3400))
points(time,pop,pch=20,col="black")
axis(2, ylim=c(0,max(pop)),col="black",lwd=2)
mtext(2, text = "Population",line=2)

par(new=T)
plot(time, med, axes=F, ylim=c(0,max(med)), xlab="",ylab = "",type="l",lty=2,main="",xlim=c(7000,3400),lwd=2)
axis(2, ylim=c(0,max(med)),lwd=2,line=3.5)
points(time,med,pch=20)
mtext(2,text="Median Group Size",line=5.5)

par(new=T)
plot(time, grp, axes=F, ylim=c(0,max(grp)), xlab="",ylab = "",type="l",lty=3,main="",xlim=c(7000,3400),lwd=2)
axis(2, ylim=c(0,max(grp)),lwd=2,line=7)
points(time,grp,pch=20)
mtext(2,text="Number of Groups", line=9)
axis(1,pretty(range(time),10))
mtext("cal BP", side=1, col="black",line=2)
legend(x=7000,y=12,legend=c("Population","Median Group Size","Number of Groups"),lty=c(1,2,3))

#library(aplpack)
score <- c(1,2,3,4,10,2,30,42,31,50,80,76,90,87,21,43,65,76,32,12,34,54)
stem.leaf(score)
faces(WorldPhones)
stars(WorldPhones)



suppressPackageStartupMessages(library(googleVis))
data(Fruits)
head(Fruits)
M1<-gvisMotionChart(Fruits,idvar="Fruit", timevar="Year")
plot(M1)
#gvisGeoChart(data, locationvar = "", colorvar="", sizevar="", hovervar="", options = list(),chartid)
data(Exports)
head(Exports)
G1<-gvisGeoChart(Exports,locationvar = 'Country', colorvar = 'Profit')
plot(G1)
G2<-gvisGeoChart(Exports, "Country", "Profit", options=list(region="150"))
plot(G2)

require(datasets)
states<-data.frame(state.name, state.x77)
head(states)
G3<-gvisGeoChart(states, "state.name", "Illiteracy", options=list(region="US", displayMode="regions", resolution="provinces", width=600, height=400))
plot(G3)

data(CityPopularity)
head(CityPopularity)
G4<-gvisGeoChart(CityPopularity, locationvar='City', colorvar='Popularity', options=list(region='US', height=350, displayMode='markers', colorAxis="{values:[200,400,600,800],
                                                                                         colors:[\'red',\'pink\', \'orange',\'green']}"))
plot(G4)
#HURRICANE
G5<- gvisGeoChart(Andrew, "LatLong", colorvar='Speed_kt', options=list(region="US"))
plot(G5)

G6<-gvisGeoChart(Andrew,"LatLong", sizevar='Speed_kt', colorvar = "Pressure_mb", options=list(region="US"))
plot(G6)

require(stats)
data(quakes)
head(quakes)
quakes$latlong<-paste(quakes$lat, quakes$long, sep=":")
head(quakes$latlong)
G7<- gvisGeoChart(quakes,"latlong", "depth", "mag", options=list(displayMode="Markers", region="009", colorAxis="{colors:['red', 'grey']}", backgroundColor="lightblue"))
plot(G7)

library(XML)
url<-"http://en.wikipedia.org/wiki/List_of_countries_by_credit_rating"
x<- readHTMLTable(readLines(url),which=3)
levels(x$Rating)<- substring(levels(x$Rating),4, nchar(levels(x$Rating)))
x$Ranking<-x$Rating
levels(x$Ranking)<-nlevels(x$Rating):1
x$Ranking<-as.character(x$Ranking)
x$Rating<-paste(x$Country, x$Rating, sep=": ")
G8<- gvisGeoChart(x,"Country", "Ranking", hovervar="Rating", options=list(gvis.editor="S&P", colorAxis="{colors:['#91BFDB', 'FC8D59']}"))
plot(G8)


library(vcd)
library(datasets)
data(Titanic)
str(Titanic)
mosaic(Titanic)
mosaic(Titanic, shade = TRUE, legend=TRUE)
mosaic(HairEyeColor, shade = TRUE, legend=TRUE)
strucplot(Titanic, pop = FALSE)
grid.edit("rect:Class=1st,Sex=Male,Age=Adult,Survived=Yes",gp=gpar(fill="red"))
