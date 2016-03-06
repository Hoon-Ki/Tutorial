library(car)
library(lattice)
dodgers <-read.csv("C:\\Users\\Administrator\\Desktop\\DataScience\\CODE\\R\\Chapter_2\\Chapter_2\\dodgers.csv")
print(str(dodgers))

dodgers$ordered_day_of_week <- with(data=dodgers, ifelse((day_of_week=="Monday"),1,ifelse((day_of_week=="Tuesday"),2,ifelse((day_of_week=="Wednesday"),3,ifelse((day_of_week=="Thursday"),4,ifelse((day_of_week=="Friday"),5,ifelse((day_of_week=="Saturday"),6,7)))))))
dodgers$ordered_day_of_week<- factor(dodgers$ordered_day_of_week, levels=1:7, labels=c("Mon","Tue","Wed","Thur","Fri","Sat",
                                                                                      "Sun"))

with(data=dodgers, plot(ordered_day_of_week, attend/1000,xlab = "Day of Week", ylab = "Attendance (thousands)", col ="violet", las=1))
with(dodgers, table(bobblehead, ordered_day_of_week))

dodgers$ordered_month <- with(data=dodgers, ifelse((month== "APR"),4,ifelse((month== "MAY"),5,ifelse((month== "JUN"),6,ifelse((month== "JUL"),7,ifelse((month== "AUG"),8,ifelse((month== "SEP"),9,10)))))))
dodgers$ordered_month<-factor(dodgers$ordered_month,levels=4:10,labels = c("April","May","June","July","Aug","Sept","Oct"))
with(data=dodgers,plot(ordered_month, attend/1000,xlab = "Month", ylab = "Attendance (thousands)", col ="light blue", las=1))
labels = c("April", "May", "June","July","Aug","Sept","Oct")

with(data=dodgers,plot(ordered_month, attend/1000, xlab= "Month", ylab= "Attendance (Thousands)", col = "light blue", las=1))
        
group.labels <- c("No Fireworks", "Fireworks")
group.symbols <- c(21,24)
group.colors <- c("black", "black")
group.fill <-c("black","red")

#"|" means conditions in formula like p(y|x),  pch() is size of points, rev() is reverse function.
# strip function is for conditional atrributes which are "| skies + day_night" and the detailed options are just graphical things.
# aspect is  physical aspect ratio of the panels, which is usually the same for all the panels. It can be specified as a ratio (vertical size/horizontal size) or as a character string
# in short, aspect is about the ratio of x and y axis ,which mean the panels' ratio.
# trying to put in and out each one option is a good way to know about this fuction.

xyplot(attend/1000 ~ temp | skies + day_night, data= dodgers, groups = fireworks, pch = group.symbols, aspect = 1, cex = 1.5 , col = group.colors, fill = group.fill,
       layout = c(2,2), type=c("p","g"),strip = strip.custom(strip.levels =T,strip.names=F,style=1),xlab="Temperature (Degrees Fahrenheit)",ylab ="Attendance (thousands)",
       key = list(space="top", text=list(rev(group.labels),col= rev(group.colors)),points =list(pch =rev(group.symbols),col=rev(group.colors),fill=rev(group.fill))))

group.labels <- c("Day","Night")
group.symbols <- c(1,20)
group.symbols.size <- c(2,2.75)

#box-and-whisker plot 
#panel.grid is drawing a grid on the background. v=-1 means (I guess) the number of vertical lines are matched with x axis' scales.

bwplot (opponent ~attend/1000, data =dodgers, groups = day_night,
        xlab = "Attendance (thousands)",
        panel = function(x,y,groups, subscripts,..) {panel.grid(h = (length(levels(dodgers$opponent))-1), v=-1)
          panel.stripplot(x,y,groups =groups, subscripts=subscripts, cex = group.symbols.size, pch = group.symbols, col= "darkblue")
          }, key = list(space = "top",text= list(group.labels,col="black"),points = list(pch = group.symbols, cex = group.symbols.size, col = "darkblue")))

my.model <- {attend ~ ordered_month + ordered_day_of_week + bobblehead}
set.seed(1234)
training_test <- c(rep (1, length = trunc((2/3)*nrow(dodgers))),
                        rep(2,length = (nrow(dodgers)- trunc((2/3)*nrow(dodgers)))))
dodgers$training_test <- sample(training_test)
dodgers$training_test <- factor(dodgers$training_test,
                                levels=c(1,2), labels =c("TRAIN","TEST"))
dodgers.train <- subset(dodgers, training_test =="TRAIN")
print(str(dodgers.train))
dodgers.test <- subset(dodgers, training_test =="TEST")
print(str(dodgers.test))


train.model.fit <-lm(my.model, data = dodgers.train)
dodgers.train$predict_attend<-predict(train.model.fit)
dodgers.test$predict_attend <- predict( train.model.fit, newdata = dodgers.test)

# cat is useful for producing output in user-defined functions.
cat("\n","Proportion of Test Set Variance Accounted for : ",
    round((with(dodgers.test, cor(attend, predict_attend)^2)),digits=3),"\n",sep="")

dodgers.plotting.frame <- rbind(dodgers.train,dodgers.test)

group.labels <- c("No Bobbleheads", "Bobbleheads")
group.symbols <- c(21,24)
group.colors <- c("black","black")
group.fill <- c("black","red")
xyplot(predict_attend/1000~ attend/1000 | training_test, data= dodgers.plotting.frame, groups = bobblehead , cex =2, pch = group.symbols,
       col = group.colors, fill = group.fill, layout = c(2,1), xlim = c(20,65), ylim = c(20,65), aspect =1 ,type = c("p","g"),
       panel = function (x,y, ...)
         {panel.xyplot(x,y,...)
         panel.segments(25,25,60,60,col="black",cex=2)}
       , strip = function(...) strip.default(..., style=1),
       xlab = "Actual Attendance (thousands)",
       ylab = "Predicted Attendance (thousands)",
       key = list(space = "top",
                  text = list(rev(group.labels), col = rev(group.colors)),
                  points = list(pch = rev(group.symbols),
                  col = rev(group.colors),
                  fill=rev(group.fill))))

my.model.fit <- lm(my.model, data = dodgers)
print(summary(my.model.fit))
print(anova(my.model.fit))

cat("\n", "Estimated Effect of Bobblehead Promotion on Attendance : ", round(my.model.fit$coefficients[length(my.model.fit$coefficients)],
                                                                             digits = 0),"\n",sep="")
plot(my.model.fit)

library(car)
residualPlots(my.model.fit)
marginalModelPlot(my.model.fit)
print(outlierTest(my.model.fit))
