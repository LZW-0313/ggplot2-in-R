#################################ggplot2可视化###########################################
library(ggplot2)

###R语言基本绘图指令###
#散点图#
plot(mtcars$wt,mtcars$mpg)
#线图#
plot(pressure$temperature,pressure$pressure,type="l")
points(pressure$temperature,pressure$pressure) #线上加点

plot(pressure$temperature,pressure$pressure)
lines(pressure$temperature,pressure$pressure) #点上加线

lines(pressure$temperature,pressure$pressure/2,col="red")#添加新线
points(pressure$temperature,pressure$pressure/2,col="red")#添加新点

#柱状图#
barplot(table(mtcars$cyl))

#频率直方图#
hist(mtcars$mpg,breaks=10)

#箱线图#
boxplot(mtcars$mpg)

#画函数#
curve(x^2+1,from=-1,to=1)
myfunction<-function(x){
  x^3+1
}
curve(myfunction,from=-1,to=1)

###ggplot2基本绘图指令 qplot函数###
dat<-diamonds
View(dat)
qplot(log(carat),log(price),data=diamonds)
qplot(carat,x*y*z,data=diamonds)

#color,size,shape and other aethetic attributes
dsmall=diamonds[sample(nrow(diamonds),100),]
qplot(carat,price,data=dsmall,colour=color)
qplot(carat,price,data=dsmall,shape=cut)
#半透明
qplot(carat,price,data=diamonds,alpha=I(1/100))

qplot(carat,price,data=dsmall,geom=c("point","smooth"))

library(splines)
qplot(carat,price,data=dsmall,geom=c("point","smooth"),method='lm')
qplot(carat,price,data=dsmall,geom=c("point","smooth"),method='lm',formula=y~ns(x,5))

#color,fill
qplot(color,price,data=dsmall,geom="boxplot")
qplot(color,price,data=dsmall,geom="boxplot",fill=I("blue"))
qplot(color,price,data=dsmall,geom="boxplot",size=2)
#geom_boxplot()
qplot(color,price,data=dsmall,geom="boxplot")+geom_boxplot(outlier.colour = "green",
                                                           outlier.size = 10,fill="red",
                                                           colour="1",
                                                           size=2)
#颜色频率\密度图
qplot(carat,data=diamonds,geom="histogram")
qplot(carat,data=diamonds,geom="histogram",colour=color)
qplot(carat,data=diamonds,geom="histogram",colour=color,fill=color)
qplot(carat,data=diamonds,geom="density")
qplot(carat,data=diamonds,geom="density",colour=color)
qplot(carat,data=diamonds,geom="density",colour=color,fill=color)

#颜色柱形图
qplot(color,data=diamonds,geom="bar",fill=color)

qplot(date,unemploy/pop,data=economics,geom="line")
qplot(date,uempmed,data=economics,geom="line")

qplot(unemploy/pop,uempmed,data=economics,geom=c("point","path"))
year=function(x) as.POSIXlt(x)$year+1900
qplot(unemploy/pop,uempmed,data=economics,
      geom="path",colour=year(date))

attach(mpg)
head(mpg)

#Review
qplot(displ,hwy,data=mpg,colour=factor(cyl))
qplot(displ,hwy,data=mpg,
      colour=factor(cyl),
      geom=c("smooth","point"),
      method="lm")
qplot(displ,hwy,data=mpg,facets=.~year)+geom_smooth() #分年份画

#储存图
getwd()
p=qplot(displ,hwy,data=mpg,colour=factor(cyl))
summary(p)
save(p,file="plot.rdata")
load("plot.rdata")
ggsave("plot.png",width=5,height=5)

##一层一层构建 ggplot函数##
p=ggplot(diamonds,aes(carat,price,colour=cut))
p
p=p+geom_point()
p

p<-ggplot(diamonds,aes(x=carat))
p<-p+geom_bar(
  geom_params=list(fill="steelblue"),
  stat="bin",
  stat_params=list(binwidth=0.5)
)
p

##qplot 与 ggplot##
#例1
ggplot(msleep,aes(sleep_rem/sleep_total,awake))+geom_point()
#等价于
qplot(sleep_rem/sleep_total,awake,data=msleep)

#例2
qplot(sleep_rem/sleep_total,awake,data=msleep,geom=c("point","smooth"))
#等价于
ggplot(msleep,aes(sleep_rem/sleep_total,awake))+geom_point()+geom_smooth()
#等价于
plot=ggplot(msleep,aes(sleep_rem/sleep_total,awake))
plot=plot+geom_point()+geom_smooth()
plot

#例3
bestfit=geom_smooth(method='lm',
                    se=T,
                    color="steelblue",
                    alpha=0.5,
                    size=2)
qplot(sleep_rem,sleep_total,data=msleep)+bestfit
qplot(displ,hwy,data=mpg,facets=.~year)+bestfit

#在图基础上改动数据
p=ggplot(mtcars,aes(mpg,wt,color=cyl))+geom_point()
p
mtcars=transform(mtcars,mpg=mpg^2)
p%+%mtcars

#Plots and Layers
p=ggplot(mtcars,aes(x=mpg,y=wt))
p+geom_point()
p+geom_point(aes(color=factor(cyl)))
p+geom_point(aes(y=disp))

#setting and mapping
p+geom_point(color="green")
p+geom_point(aes(color="blue"))

#Grouping
library(nlme)
data(package="nlme") #查看包里内置数据
?Oxboys              #查看某一数据具体信息
head(Oxboys)
str(Oxboys)
p=ggplot(Oxboys,aes(age,height,group=Subject))
p+geom_line(color="blue")
ggplot(Oxboys,aes(age,
                  height,
                  group=1))+geom_line(color="blue") #用一根线连起来，改动1也如此

#different groups on different layers
p+geom_smooth(aes(group=Subject),method="lm",se=F)
p+geom_smooth(aes(group=1),method="lm",se=F,size=2)

#Overriding the default grouping
boysbox=ggplot(Oxboys,aes(Occasion,height))+geom_boxplot()
boysbox
boysbox+geom_line(aes(group=Subject),color="blue")

#position adjustments
ggplot(diamonds,
       aes(clarity,fill=cut))+geom_bar(position="stack")
ggplot(diamonds,
       aes(clarity,fill=cut))+geom_bar(position="fill")
ggplot(diamonds,
       aes(clarity,fill=cut))+geom_bar(position="dodge")


library(gcookbook) #加载新包
data(package="gcookbook")
ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat="identity")
?BOD
ggplot(BOD,aes(x=Time,y=demand))+geom_bar(stat="identity")
ggplot(BOD,aes(x=factor(Time),y=demand))+geom_bar(stat="identity") #去空操作
ggplot(pg_mean,aes(x=group,y=weight))+
  geom_bar(stat="identity",fill="lightblue",color="black")

#Grouping Bars Together
?cabbage_exp
cabbage_exp
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(position="dodge")        #不显示内容
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(position="dodge",stat="identity")
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(position="identity",stat="identity")

#Making a Bar Graph of Counts
ggplot(diamonds,aes(x=cut))+geom_bar()

#Using Colors in a bar Graph
str(uspopchange)
upc=subset(uspopchange,rank(Change)>40)
ggplot(upc,aes(x=Abb,y=Change,fill=Region))+geom_bar(stat="identity")
ggplot(upc,aes(x=reorder(Abb,Change),y=Change,fill=Region))+
  geom_bar(stat="identity",color="black")            #排序

#coloring Negative and Positive Bars Differently
?climate
str(climate)
csub=subset(climate,Source="Berkeley"&Year>=1900)
csub$pos=csub$Anomaly10y>=0
str(csub)
ggplot(csub,aes(x=Year,y=Anomaly10y,fill=pos))+
  geom_bar(stat="identity",position="identity")+
  scale_fill_manual(values=c("#669933","#FFCC66"),guide=FALSE)

#Adjusting Bar width and Spacing
ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat="identity")
ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat="identity",width=0.5)
ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat="identity",width=1)

#Grouped data
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat="identity")+
  geom_text(aes(label=Weight),vjust=1.5,color="white")

#Adding Labels to a Bar Graph
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+
  geom_bar(stat="identity")+
  geom_text(aes(label=Weight),vjust=1.5,color="white")

#Above the top
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+
  geom_bar(stat="identity")+
  geom_text(aes(label=Weight),vjust=-0.2)

#Map y positions slightly above bar top
#-y range of plot will auto-adjust
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+
  geom_bar(stat="identity")+
  geom_text(aes(y=Weight+0.1,label=Weight))

ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat="identity",position="dodge")+
  geom_text(aes(label=Weight),vjust=1.5,color="black",
            position=position_dodge(0.9),size=5)


