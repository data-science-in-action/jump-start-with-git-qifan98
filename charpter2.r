#3.3.2数据的输入和简单展示
getwd() #查看目前的工作目录
setwd("C:\\Users\\Ly\\Desktop\\data")
w=read.csv("ag.csv")
#如果不设工作目录，可用w=read.csv("C:\\Users\\Ly\\Desktop\\data")读入数据
head(w)#查看数据的前几行(默认6行)

#3.3.4变量的种类
for (i in c(6:10,12)) w[,i]=factor(w[,i]) #把哑元标记为分类变量


w=read.csv("ag.csv")
summary(w[,c(6:10,12)])

for (i in c(6:10,12)) w[,i]=factor(w[,i])
summary(w[,c(6:10,12)])

set.seed(1010)
df=data.frame(sex=sample(c("Male","Female"),10,r=T),income=sample(c("High","Middle","Low"),10,r=T))
fastDummies::dummy_cols(df) #实行转换，需要程序包fastDummies

#3.3.2汇总统计量的计算
summary(w$sell) #给出极大，Q1，Q2，Q3
mean(w$sell)#均值
median(w$sell)#中位数(Q2)
(q1=quantile(w$sell,0.25)) #Q1
(q3=quantile(w$sell,0.75)) #Q3
sd(w$sell)#标准差
mad(w$sell)#中位数绝对差
q3-q1 #四分位间距
diff(range(w$sell)) #极差，这里range()给出极小极大值
e1071::skewness(w$sell) #偏度(默认的第三类峰度),用了程序包额1071
e1071::kurtosis(w$sell) #峰度(默认的第三类峰度),用了程序包额1071
cor(w$sell,w$lot)#两个变量的线性相关系数

#3.3.4 例3.1数据的简单汇总
summary(w)
str(w)

#3.4.2 散点图
w=read.csv("ag.csv")
for (i in c(6:10,12)) w[,i]=factor(w[,i]) #把哑元标记为分类变量
plot(sell~lot,data=w)  #也可以写成plot(w$lot,w$sell)


library(ggplot2)
ggplot(data = w, mapping = aes(x=lot,y=sell)) + geom_point()

#R的基本画图代码：
with(w,scatter.smooth(lot,sell)) #散点图及loess曲线

##ggplot代码
ggplot(data=w,mapping=aes(x=lot,y=sell)) + 
  geom_point()+   #散点图
  geom_smooth(method=loess,se=FALSE)  #加上一层loess曲线


#3.4.3直方图
par(mfrow=c(1,3))  #1行3列
for (i in c(9,25,50)) {
  hist(w$sell,breaks = i,main = paste0("Histogram of sell,breaks=",i))
  rug(w$sell) #在图下标出真实数据点的位置
}

par(mfrow=c(1,1))
#R的基本画图代码：
hist(w$sell[w$reg==0],30,col=2,ylim=c(0,50),main="",xlab="sell")
par(new=TRUE)
hist(w$sell[w$reg==1],30,col=4,ylim=c(0,50),main="",xaxt="n",xlab="")
legend("topright",c("reg=0","reg=1"),col=c(2,4),lwd=4)

##ggplot代码
ggplot(w,aes(x=sell,color=reg))+
  geom_histogram(fill="blue",alpha=0.5,position = "identity")


#3.4.4盒形图
boxplot(w$sell,horizontal = TRUE)#水平放着的盒形图


#R的基本画图代码：
plot(sell~reg,w)

##ggplot代码
ggplot(w,aes(reg,sell)) +geom_boxplot()

#3.4.5条形图
(a=table(w$sty))
barplot(a,xlab="sty",ylab="count")

#R的基本画图代码：
barplot(rbind(table(w[w$reg==0,]$sty),
              table(w[w$reg==1,]$sty)),
        xlab = "sty",ylab = "count",beside = TRUE)
legend("topright",paste0("reg=",0:1),lwd=1,col=c("black","gray"))

##ggplot代码
ggplot(w,aes(sty))+
  geom_bar(aes(fill=reg),position = "dodge")

#3.4.6三维曲面图及等高线图
library(lattice)
library(ggplot2)

#通过lattice包绘制三维曲面图及等高线图
dev.off()
wireframe(volcano)#三维曲面图
contour(volcano)#登高线图

#地图
library(rgdal)
x<-readOGR("bou2_4p.shp")  #读取地图
plot(x)
#使用ggplot2绘制中国地图
library(ggplot2)
china_map<-fortify(x)  #转化为数据框
ggplot()+
  geom_polygon(data=china_map,aes(x=long,y=lat,group=group),
               fill="white",colour="black")

#数据科学维恩图
library(ggplot2) 
#数据准备 
circle = function (x, y, radius, nv = 100) {
  angle.inc <- 2 * pi/nv  
  angles <- seq(0, 2 * pi, by = angle.inc)
  xv <- cos(angles)*radius + x  
  yv <- sin(angles)*radius + y  
  data.frame(x = xv, y = yv) } 
  c1 = circle(2.5,5,1)
  c2 = circle(3.5,5,1)
  c3 = circle(3,4.1,1)
  c1$id = 'c1'
  c2$id = 'c2' 
  c3$id = 'c3'
  c = rbind(rbind(c1,c2),c3)
  c$id = as.factor(c$id) 
  lab  = data.frame(x = c(2, 3, 3.6, 2.3, 3, 3.5, 3),
                    y = c(5.4, 5.4, 5.4, 4.3, 4.6, 4.3, 3.7),
                    name= c('计算机科学', '机器学习', '统计学',
                            '商业智能', '数据科学', '传统数据分析',
                            '邻域知识'))
  #图形绘制 
  ggplot(c, aes(x,y,group=id))+  
    geom_polygon(aes(fill = id),alpha = .4)+  
    geom_path(aes(colour = id),alpha = .4)+  
    annotate('text',x=2,y=5.1,label='计算机科学',size=6)+  
    annotate('text',x=3,y=5.4,label='机器学习',size=6)+  
    annotate('text',x=3.9,y=5.1,label='统计学',size=5)+  
    annotate('text',x=2.3,y=4.3,label='商业智能',size=6)+  
    annotate('text',x=3,y=4.6,label='数据科学',size=6)+  
    annotate('text',x=3.6,y=4.3,label='传统数据\n分析',size=6)+  
    annotate('text',x=3,y=3.7,label='邻域知识',size=6)+  
    theme(panel.grid = element_blank(), 
          panel.background = element_blank(),    
          panel.border = element_rect(colour = 'white',fill=NA),    
          legend.position = 'none' )+xlab(' ')+ylab(' ')+ggtitle('学科图')+
          theme(plot.title = element_text(size = rel(2), face = 'bold'),        
                axis.title.x = element_blank(),        
                axis.title.y = element_blank(),        
                axis.text.x=element_blank(),        
                axis.text.y=element_blank(),        
                axis.ticks = element_blank())+  
                scale_fill_manual(values = c('yellow', 'blue','green'))+  
                scale_colour_manual(values = c('yellow','blue','green'))
################################
  library(VennDiagram)
  foo <- c('a','b','c','d') 
  baa <- c('a','e','c','g')
  dbb <- c('a','b','e','m') 
  v <- venn.diagram(list(foo=foo, baa=baa, dbb =dbb),                  
                    main = "学科图",                  
                    fill = c("orange", "blue",'green'),                  
                    lty = "blank",                  
                    alpha = c(0.5, 0.5, 0.5), cat.cex = 1.5, cex=1.5,                  
                    filename=NULL) 
  v[[7]]$label  <- '计算机科学' 
  v[[8]]$label <- '机器学习' 
  v[[9]]$label <- '统计学' 
  v[[10]]$label <-  '商业智能' 
  v[[11]]$label <- '数据科学' 
  v[[12]]$label <- '传统数据分析' 
  v[[13]]$label <- '邻域知识' 
  v[[14]]$label <-        ' ' 
  v[[15]]$label <-        ' ' 
  v[[16]]$label <-        ' ' # plot   grid.newpage() grid.draw(v)
  

#4.1GDP数据案例
library(tidyverse)

GDP1=read.csv("GDP1.csv")
levels(GDP1[,3])=paste0("V",1:9) #简化名字
tb=as.tibble(GDP1)


#4.1.1 形成某年各国的多指标数据
tb2016 <-tb[tb$Year==2016,-2]%>%  #选中2016之后去掉Year列
  spread(key = "Item",value = "Value") #转换Item元素为列，Value为值

head(tb2016,3)

tb%>%  #原始数据
  subset(Year==2016)%>%  #选择2016年
  select(-Year)%>%      #选择Year之外的变量，也可以用下面一行    
  # select(Country.or.Area, Item, Value)%>% #选择Year之外的变量
  spread(key = "Item",value ="Value")%>%  #做转换
  ggplot(.,aes(x=V4,y=V5))+           #选V4，V5作图
  geom_point()+   #散点图
  geom_smooth()   #拟合loess曲线
  
#4.1.2形成某国各个时期不同指标的多元时间序列
library(magrittr) #用于调用算子%$%
#第一种方法：固有R函数画图。前四行产生数据，最后三行画图
tb%>%
  subset(Country.or.Area=="Japan")%>% #选定日本
  select(-Country.or.Area)%>%         #选择除国家或地区之外的变量
  spread(key="Item",value = "Value")%>% #经济指标为列，时间为行
  select(-Year)%>%
  ts(start = 1970,end = 2016)%>%      #标为时间序列
  plot(plot.type="single",col=11:19,lty=1:9)%>%  #时间序列图
  legend("topleft",paste0("V",1:9),col=11:19,lty=1:9,cex=.7) #图例

#第二种方法：ggplot画图。前两行产生数据，最后三行画图
tb%>%
  subset(Country.or.Area=="Japan")%>%
  ggplot(aes(x=Year,y=Value))+
  geom_line(aes(color=Item),size=1)+
  theme_minimal()

#4.1.3形成某指标的各个时期不同国家的多元时间序列
#产生以时间为行，以国家为列的只有GDP(V6)一个指标的数据
GDP=tb %>%
  subset(Item=="V6")%>%
  select(-Item)%>%
  spread(key = "Country.or.Area",value = "Value")

#准备选择BRICS国家，下面重设国家名字(原数据是很长的国家全名)
BS=c("Brazil","China","India","Russia","South Africa")

GDP[,c(28,44,95,163,182)]%>% #只取金砖国家的列
  ts(start=1970,end = 2016)%>%
  plot(plot.type="single",col=1:5,lty=1:5)%$%
  legend("topleft",BS,col=1:5,lty=1:5,cex=.7)



#4.2世界卫生组织案例之一
#数据提取
NN=c("D1","D12","D13","D15","D16","D17","D18","D2","D5","D6","D7")
Files=paste0(NN,".csv")

DF=lapply(Files,read.csv)

lapply(DF,names)

#4.2.2删除多余的行和列
U=NULL
for (i in 1:length(DF) ) U=unique(c(U,unique(as.character(DF[[i]][,1]))))

Trash=c("1","2","3","4","footnoteSeqID")
U=setdiff(U,Trash)#集合差，去掉Trash

DF=lapply(DF, function(x) x[x[,1] %in% U,]) #只取Trash之外的部分
DF=lapply(DF, function(x) select(x,-Value.Footnotes)) #去掉注释列

DP=list()
for(i in 1:length(NN)) {
  Area=DF[[i]][,1] %>% unique()
  y=data.frame()
  for(j in 1:length(Area)){
    Y=max(as.character(DF[[i]][DF[[i]][,1] == Area[j],2]))
    V=max(DF[[i]][DF[[i]][,1] == Area[j] & DF[[i]][,2] == Y,3])
    y=rbind(y, data.frame(Area[j],V))
  }
  names(y)=c("Country.or.Area",NN[i])
  DP[[i]]=y
}
lapply(DP,dim)


#把多个数据合并成一个数据
x=DP[[1]]
for (i in 2:length(NN)){
  x=merge.data.frame(x, DP[[i]],by="Country.or.Area")
}
write.csv(x,"DP.csv",row.names=FALSE)#把合并的结果数据存成csv文件

names(x)
dim(x)

#4.3世界卫生组织案例之二
#输入并识别数据
library(stringr)
RFiles=c("R1.csv","R10.csv","R11.csv","R13.csv","R14.csv",
         "R15.csv","R2.csv","R6.csv","R8.csv","R9.csv")
RN=str_sub(RFiles,1,-5)#选取没有扩展名“.csv”的字符串：第一到倒数第五个(-5)

RS=lapply(RFiles,function(x) read.csv(x,na.strings = "-"))


#删除多余的行和列
library(tidyverse)
Trash=c("1","footnoteSeqID","10","11","2","3","4","5","6","7","8","9")
'%!in%'<-function(x,y) !('%in%'(x,y))#等价于!(x%in%y)

RS=lapply(RS,function(x) x[x[,1] %!in% Trash,])#只取Trash之外的行
RS=lapply(RS,function(x) select(x,-Value.Footnotes))#去掉注释列

#4.3.3合并数据
IA=c(2,3,6,7)#没有性别或区域的4个变量RN[IA]="R10""R11""R15""R2"
IG=c(1,4,5,8)#3个GENDER,将转换成12个变量RN[IG]="R1""R13""R14""R6"
IR=c(9:10)#2个RESIDENCE.AREA(不考虑总和”Total"),
#IR将转换成4个变量RN[IR]="R8""R9"
S=c("Female","Male","Both sexes")#标记GENDER的水平
R=c("Rural","Urban")#标记RESIDENCE.AREA的水平

          
#第一部分,由4个文件组成的4个变量:"R10""R11""R15""R2"无再生变量
R0=list();k=1
for(i in IA){
  T=data.frame()
    DF=RS[[i]]
    CA=DF[,1] %>% unique()#CA是相应数据的国家和地区名字向量
    for(c in CA){
      V=DF[DF[,1]==c,]
      V=tail(V[V[,2] == max(as.character(V[,2])),],1) #每个c取最大年份
      T=rbind(T,V)#纵向组合
    }
    T=T%>%select(-Year.s.)#去掉年变量
    names(T)[2]=RN[i]#把变量名"Value"改成文件名
    R0[[k]]=T
    k=k+1
}    
  
RR=R0[[1]]
for (i in 2:length(R0))
  RR=merge.data.frame(RR, R0[[i]],by="Country.or.Area")#横向合并数据

##第二部分,由4个文件组成的4个变量"R1""R13""R14""R6"
##将再生成为12个变量:"R1F""R1M""R1B"R13F""R13M"R13B"
##"R13M""R14F""R14M""R14B""R6F""R6M""R6B"
  for(i in IG) {
    T=data.frame()
    DF=RS[[i]]
    CA=DF[,1]%>%unique()
    for(c in CA){
      for(s in S){  #S为性别类型("Female","Male","Both sexes")
        V=DF[(DF[,1] == c)&(DF[,3] ==s),]
        V=tail(V[V[,2] == max(as.character(V[,2])),],1)
        T=rbind(T,V)
       }
    }
    T=T%>%select(-Year.s.)
    for(s in S){  #每种性别一个单独数据
    TT=T%>%
        subset(GENDER==s)%>%
        select(-GENDER)
    names(TT)[2]=paste0(RN[i],str_sub(s,1,1))#改变变量名字
    RR=merge.data.frame(RR,TT,by="Country.or.Area")#合并数据
    }
}


##第三部分,由2个文件组成的2个变量:"R8""R9"
##将再生成为4个变量:"R8R""R8U""R9R""R9U
for (i in IR){
  T=data.frame()
  DF=RS[[i]]
  CA=DF[,1]%>%unique()
  for(c in CA){
    for(r in R){
      V=DF[(DF[,1]==c)&(DF[,3]==r),]
      if(!(identical(V[,2], integer(0)))){ #此条件语句处理不存在的组合
          V=tail(V[V[,2]==max(as.character(V[,2])),],1)
          T=rbind(T,V)} else{
          V=DF[DF[,1]==c,]%>%tail(1)
          V[,3]=r;V[,4]=NA #不存在组合设为NA
          T=rbind(T,V)
          }
    }
  }    
T=T %>% select(-Year.s.)
  for(r in R){
    TT=T%>%
    subset(RESIDENCE.AREA==r)%>%
    select(-RESIDENCE.AREA)
    names(TT)[2]=paste0(RN[i], str_sub(r,1,1))
    RR=merge.data.frame(RR,TT,by="Country.or.Area")
  }
}
names(RR)
dim(RR)


#4.3.4清理数据中的其他问题
levels(RR$R2)[2]=str_sub(levels(RR$R2)[2],2,4)#去掉第二个水平中的"<"
RR$R2=as.numeric(levels(RR$R2))[RR$R2]#标记成数量变量
  
#通过自编的函数舍弃6到14列的以方括号显示的区间(保留前面数字)
ZZ=function(x){
    Z=as.character(x)
    P=gregexpr(pattern='[',Z,fixed=TRUE)#找到"["的位置
    for (i in 1:length(Z))
      Z[i]=str_sub(Z[i],1,P[[i]][[1]][1]-1)#保留"["的位置前面的内容
    Z=Z %>% as.character %>% as.numeric()
    return(Z)
}
 for(i in 6:14) RR[,i] = ZZ(RR[,i])
names(RR)
dim(RR)
write.csv(RR, "RR.csV", row.names=FALSE)


#4.4数据中的缺失值
DRmix=merge.data.frame(read.csv("DP.csv"),read.csv("RR.csv"),
                       by="Country.or.Area")
write.csv(DRmix,"DRmix.csv",row.names = FALSE)

#4.4.1缺失值的审视与标记
library(mice)
md.pattern(DRmix[,26:32])

library(VIM)
aggr_plot<-aggr(DRmix[,26:32],col=c('navyblue',"red"),number=TRUE,
                sortVars=TRUE,cex.axis=.5,gap=3,
                ylab=c("Histogram of missing data","Pattern"))

x=c(1,5,NA,7,9); y=c("Happy","Worry",NA,"Calm",NA)
is.na(x)
which(is.na(y))
z=data.frame(x,y)
is.na(z)
apply(is.na(z),2,sum)

na_sum=apply(is.na(DRmix),2,sum)
na_sum[na_sum>0]
sum(na_sum) #总共91个缺失值

(na_rows=(apply(DRmix,1,function(x)sum(is.na(x)))))

DRmix$Country.or.Area[na_rows==3]

#缺失值为表明的情况
w=read.csv("R6.csv");summary(w[,3:4])
w=read.csv("R6.csv",na.string="-");summary(w[,3:4])


#4.4.2删除缺失值的方法及问题
#函数是否默认删除
plot(DRmix[,26:28])

x=c(3,7,4.5,6,21,NA,-5)
mean(x)
mean(x,na.rm=TRUE)

#直接删除相关的行或列
library(tidyverse)
DRmix=read_csv("DRmix.csv") #以tibble形式

Total=DRmix[,-1]%>%dim()%>%prod()%>%print()#数据数据总数
#删除相应缺失行后剩余数目：
Row_omit=DRmix[,-1]%>%na.omit()%>%dim()%>%prod()%>%print()
#删除3个缺失列后剩余数目：
Col_omit=DRmix[,-1]%>%select(-R6M,-R6F,-R6B)%>%dim()%>%prod()%>%print()
Total-Row_omit #删除相应缺失行后实际删除的数目
Total-Col_omit #删除相应3个列后实际删除的数目


Total=print(prod(dim(DRmix[,-1])))
Row_omit=print(prod(dim(na.omit(DRmix[,-1]))))
Col_omit=print(prod(dim(DRmix[,-c(1,30:32)])))

Total%>%"-"(c(Row_omit,Col_omit))

#4.43用认定值或某些准则来填补缺失值
#用认定的值来填补
name=c("Lucy","Barbara","Tom","Zina","John","Janet")
height=c(1.6,1.7,NA,1.75,NA,1.8)
gender=c("Female","Female","Male",NA,"Male","Female")
(z=data.frame(name,height,gender))
z$height[is.na(z$height)]=c(1.85,1.9)
z$gender[is.na(z$gender)]="Female"
z

#用相应列的均值、中位数来填补
library(Hmisc)
x=c(3,7,NA,21,NA,10000)
impute(x,fun=mean)#用均值填补
impute(x,median)#用中位数填补

library(Hmisc);library(mice)
sapply(DRmix,impute,mean) %>% md.pattern()

#4.4.4利用可预测模型来填补缺失值
w=read.table("dermatology.data",sep = ",",header = FALSE,na.strings = "?")
for (i in (1:ncol(w))[-34]) w[,i]=factor(w[,i])
library(VIM)
aggr_plot<-aggr(w,col=c('navyblue','red'),numbers=TRUE,sortVars=TRUE,
                cex.axis=.25,gap=3,ylab=c("Histogram of missing data","Pattern"))

sapply(w,function(x)sum(is.na(x)))

#K最临近方法填补
library(VIM)
library(laeken)
w=read.table("dermatology.data",sep = ",",header = FALSE,na.strings = "?")
for (i in (1:ncol(w))[-34]) w[,i]=factor(w[,i])
a=kNN(w,numFun=weightedMean,weightDist=TRUE)
a[,1:35]

#程序包mice的填补法
library(mice)
b=mice(w)
b$data  #填补后的数据

#利用随机森林方法来填补缺失值
library(missForest)
miss.w=missForest(w)
sum(is.na(miss.w$ximp))