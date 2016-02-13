#library(reshape)
data(airquality)
head(airquality,10)

names(airquality)
names(airquality)<-tolower(names(airquality)) #uppercase to lowercase
names(airquality)


aqm<-melt(airquality, id=c("month", "day"), na.rm=T)
head(aqm)

a<-cast(aqm,day~month~variable) #put one dataframe consisted of 'day~month' into each variable's dimension
#library(reshape2)
b<-dcast(aqm,day+month~variable) #put all variables including day and month into one dataframe

c<-cast(aqm,month~variable,mean)
d<-cast(aqm, month~ .|variable,mean) # "|variable" shows the result by each variable
e<-cast(aqm, month ~ variable, mean, margins=c("grand_row","grand_col")) #the option "margins" shows sum of each row and col
f<-cast(aqm, day~month, mean, subset=variable=='ozone') # subset is useful when we need/want to process 'selectively' some of variables
g<-cast(aqm, month ~ variable, range) #range shows you min & max, and at the min's suffix and max's suffix, '_X1" and '_X2' are added respectively

set.seed(1)

o<-data.frame(year = rep(2000:2002, each = 6), count = round(runif(9,0,20)))
ddply(o, "year", function(x) {
mean.count <- mean(x$count)
sd.count<-sd(x$count)
cv<-sd.count/mean.count
data.frame(cv.count=cv)
})
ddply(o, "year", summarise, mean.count = mean(count))
ddply(o,"year",transform, total.count = sum(count))

x<-c(1:10)
wait<-function(i) Sys.sleep(0.1)
system.time(llply(x, wait))
system.time (sapply(x, wait))

library(doParallel)
registerDoParallel(cores=2)
system.time(llply(x,wait, .parallel = TRUE))

library(data.table)
DT = data.table(x=c("b","b","b","a","a"),v=rnorm(5))
data(cars)
head(cars)
CARS<-data.table(cars)
head(CARS)
tables()
sapply(CARS,class)
setkey(DT,x) # setkey function marks which variable should be sorted

DT["b",] #"," is optional
DT["b",mult="first"]
DT["b",mult="last"]

grpsize<-ceiling(1e7/26^2)
tt<-system.time(DF<-data.frame(x=rep(LETTERS,each=26*grpsize), y=rep(letters,each=grpsize), v=runif(grpsize*26^2),stringsAsFactors = FALSE))
tt<-system.time(ans1 <- DF[DF$x=="R" & DF$y =="h",])
head(ans1,3)
dim(ans1)
DT<-data.table(DF)
setkey(DT,x,y)
ss<-system.time(ans2<-DT[J("R","h")]) #binary search
head(ans2,3)
identical(ans1$v,ans2$v)

#bad case for using data.table
system.time(ans2<-DF[DF$x=="R" & DF$y=="h",])

DT[,sum(v)]
DT[,sum(v),by=x]
ttt<-system.time(tt<-tapply(DT$v,DT$x,sum))
ttt
sss<-system.time(ss<-DT[,sum(v),by=x])#faster
sss
