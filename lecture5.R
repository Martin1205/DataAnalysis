#2016年11月14日

data<-read.table(file="data/survey2014_student.csv",sep=";",header=T, fileEncoding="GB2312")#data<-read.table(file="data\\survey2014_student.csv",sep=";",header=T, fileEncoding="GB2312")#正常斜杠用两个
#对数据进行处理
##错误
##missing value
##duplicate
##outline
##类型处理

options(digits=3)
options(scipen=4)#取消科学计数法
View(data)
#判断缺失数据
is.na(data)
#统计缺失值个数
sum(is.na(data))

#快速查看数据集是否有问题
lapply(data[,3:6],boxplot)#boxplot(data$weight);hist(data[,5])

##清洗身高异常
dataCleaned<-data[data$high>50 & data$high<200,]#驼峰编码
View(dataCleaned)
dataCleaned<-dataCleaned[dataCleaned$high!=111,]
dataCleaned<-dataCleaned[dataCleaned$mother!=15,]
dataCleaned<-na.omit(dataCleaned)
lapply(dataCleaned[,3:6],summary)#summary(data)#显示每个变量的缺失值数量

#data1=data[complete.cases(data),]




#dataCleaned$bmi<-round(dataCleaned$weight/2/(dataCleaned$high*0.01)^2,1);
dataCleaned$bmi1<-dataCleaned$weight/2/(dataCleaned$high*0.01)^2
a<-dataCleaned[dataCleaned$sex=="男" & dataCleaned$high>=180,"weight"]
hist(a)

dataCleaned$weight<-ifelse(dataCleaned$bmi<15,dataCleaned$weight*2,dataCleaned$weight)
dataCleaned$bmi2<-dataCleaned$weight/2/(dataCleaned$high*0.01)^2
