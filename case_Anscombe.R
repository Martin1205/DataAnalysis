#anscombe dataset
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)

anscombe
summary(anscombe)
str(anscombe)

apply(anscombe,2,mean)

# 改变数据结构
#方法一 R BASE 函数

#1

d1=NULL
d1$x<-anscombe[,1]
d1$y<-anscombe[,5]
d1$type<-"a"
d1<-as.data.frame(d1)

d2=NULL
d2$x<-anscombe[,2]
d2$y<-anscombe[,6]
d2$type<-"b"
d2<-as.data.frame(d2)

#....
d<-rbind(d1,d2)

#2
#数据框里面的赋值如果用<= 则该列的名称就会是anscombe[,1] 
#这会造成后面d<-rbind(d1,d2,d3,d4)无法运行
d1<-data.frame(
  x=anscombe[,1],   
  y=anscombe[,5],
  type="a"
  )
d2<-data.frame(
  x=anscombe[,2],
  y=anscombe[,6],
  type="b"
)
d3<-data.frame(
  x=anscombe[,3],
  y=anscombe[,7],
  type="c"
)
d4<-data.frame(
  x=anscombe[,4],
  y=anscombe[,8],
  type="d"
)
d<-rbind(d1,d2,d3,d4)

#方法二 使用dplyr包函数
d1=select(anscombe, x=x1,y=y1)
d2=select(anscombe, x=x2,y=y2)
d3=select(anscombe, x=x3,y=y3)
d4=select(anscombe, x=x4,y=y4)

d1$group ='a'
d2$group ='b'
d3$group ='c'
d4$group ='d'

d<-rbind(d1,d2,d3,d4)

#方法三 for循环

t<-c("a","b","c","d")

for (i in 1:4){
d<-paste("d",i,sep="")  
  data=data_frame(
    x=anscombe[,i],
    y=anscombe[,i+4],
    type=t[i]
    )
  assign(d,data)
}
d<-rbind(d1,d2,d3,d4)

#思考下下面赋值方法为什么会出错
t<-c("a","b","c","d")
ds<-paste("d",1:4,sep="")
for (i in 1:4){
  ds[i]<-data_frame(
    x<-anscombe[,i],
    y<-anscombe[,i+4],
    type<-t[i]
  )
}

#ds[i]是一个字符串


#方法四（必须是data.table包1.9.5以上版本）
detach("package:reshape2")
library(data.table)
melt(as.data.table(anscombe), 
     measure.vars = patterns("x", "y"), 
     value.name=c('x', 'y'), 
     variable.name = "s")

#方法五 dplyr+tidyr
ans <- anscombe
ans$id <- as.numeric(rownames(anscombe))
ans <- ans %>% gather(key,value,-id) %>%
  extract (key,c("var","group"),"(.)(.)") %>%
  spread(var,value)
ans$group <- factor(ans$group)
ans <- ans %>% arrange(group)

g <- ggplot(ans,aes(x,y))
g + geom_point() + facet_wrap(~group) +
  coord_fixed(ratio = 1.3) +
  ggtitle("Anscombe's Quartet") + theme_bw(16)


#绘图

d%>%
  group_by(type) %>%
  summarize(mean(x), sd(x), mean(y), sd(y), cor(x,y))

p <- ggplot(d, aes(x, y)) + geom_point()
p <- p + geom_smooth(method = lm, se = FALSE)
p <- p + facet_wrap(~type)
p

#参考R语言自带的anscombe绘图
?anscombe
