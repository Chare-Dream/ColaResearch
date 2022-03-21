#Group 11
#############################################################################
#Step One#
##Data Preparation##
if (!require("forecast")){install.packages("forecast")};library(forecast) 
if (!require("plm")){install.packages("plm")};library(plm) 
if (!require("ggplot2")){install.packages("ggplot2")};library(ggplot2) 
if (!require("ggpubr")){install.packages("ggpubr")};library(ggpubr) 
if (!require("lmtest")){install.packages("lmtest")};library(lmtest)
if (!require("MLmetrics")){install.packages("MLmetrics")};library(MLmetrics)
if (!require("plotrix")){install.packages("plotrix")};library(plotrix)
if (!require("tseries")){install.packages("tseries")};library(tseries)
if (!require("mtest")){install.packages("mtest")};library(mtest)
if (!require("car")){install.packages("car")};library(car)
df<-read.csv("Data_Cola_1.csv") #load the data

#############################################################################
#Step Two#
#Exploring Data
##Sensitive Analysis
#To see each region's sales 
#1# point graph
plot1<-ggplot(df,aes(x=time,y=sales,colour=region))+
  geom_point()+
  geom_smooth(method = "lm",)+
  facet_wrap(~region,nrow = 1)+
  theme(legend.position = "top",plot.title = element_text(face="bold",hjust = 0.5),plot.subtitle = element_text(colour="blue",face = "italic",hjust = 0.5))+
  scale_x_time(breaks = c(1,10,20,30,36),labels = c("1","10","20","30","36"))+
  labs(x="Time(month)",y="Sales",title="The Sales of Each Region",subtitle = "Primary Data")
plot1 #plot sales data in point graph

#2# box graph
plot2<-ggplot(df,aes(y=sales,colour=region))+
  geom_boxplot(aes(fill = region))+
  facet_wrap(~region,nrow = 1)
plot2 #plot sales data in box graph
#combine Plot1 and plot2 for comparison
p1<-ggarrange(plot1,plot2,nrow=2,labels = "AUTO")
p1#extreme values (outliers) spotted

#############################################################################
#To have a better understanding of the data set by plotting line graph
#plot the relationship between time and sales for different region
plot3<-ggplot(df,aes(x = time, y = sales, group = region)) +
  geom_line(aes(linetype=region,color=region))+
  geom_point(aes(color=region))+
  theme(legend.position = "top",plot.title = element_text(face="bold",hjust = 0.5),plot.subtitle = element_text(colour="blue",face = "italic",hjust = 0.5))+
  scale_x_time(breaks = c(1,10,20,30,36),labels = c("1","10","20","30","36"))+
  labs(x="Time(month)",y="Sales",title="The Sales of Each Region",subtitle = "Primary Data")
plot3

#Transform the sales into logsales as the variance of sales is too large, then draw the relationship again
df$logsales<-log(df$sales)
plot4<-ggplot(df,aes(x = time, y = logsales, group = region)) +
  geom_line(aes(linetype=region,color=region))+
  geom_point(aes(color=region))+
  theme(legend.position = "top",plot.title = element_text(face="bold",hjust = 0.5),plot.subtitle = element_text(colour="blue",face = "italic",hjust = 0.5))+
  scale_x_time(breaks = c(1,10,20,30,36),labels = c("1","10","20","30","36"))+
  labs(x="Time(month)",y="Log Sales",title="The Log Sales of Each Region",subtitle = "Primary Data")
plot4
#combine Plot3 and plot4 for comparison
p2<-ggarrange(plot3,plot4,nrow=2,labels = "AUTO")
p2

#############################################################################
#point and box graph for logsales
plot5<-ggplot(df,aes(x=time,y=logsales,colour=region))+
  geom_point()+
  geom_smooth(method = "lm",)+
  facet_wrap(~region,nrow = 1)+
  theme(legend.position = "top",plot.title = element_text(face="bold",hjust = 0.5),plot.subtitle = element_text(colour="blue",face = "italic",hjust = 0.5))+
  scale_x_time(breaks = c(1,10,20,30,36),labels = c("1","10","20","30","36"))+
  labs(x="Time(month)",y="Sales",title="The Log Sales of Each Region",subtitle = "Primary Data")
plot5 #plot sales data in point graph

plot6<-ggplot(df,aes(y=logsales))+
  geom_boxplot(aes(fill = region))+
  facet_wrap(~region,nrow = 1)+
  theme(legend.position = "top")
plot6 #plot sales data in box graph
#combine Plot1 and plot2 for comparison
p3<-ggarrange(plot3, plot1,plot4,plot5,nrow=2,ncol = 2,labels = "AUTO")
p3


#############################################################################
#Declare the dataset as panel data
df<-pdata.frame(df,index = c("region","time"),row.names = TRUE)

#dickey-fuller test---stationary
#Null hypothesis: having a unit root
adf.test(df$sales,k=0)
#p-value < 0.05 
#reject the null hypothesis
#the series is stationary 

#############################################################################
#Step
#modelling
#model0 primitive
model0.ols<-plm(sales~price+ad1+ad2+prom,data = df,model = "pooling")
summary(model0.ols) #Adj. R-Squared: 0.0096082

#############################################################################
#model1 with linear-log
df$logprice<-log(df$price)
df$logad1<-log(df$ad1)
df$logad2<-log(df$ad2)
df$logprom<-log(df$prom)
df$logprice[df$logprice==-Inf]<-0#replacing -inf with 0
df$logad1[df$logad1==-Inf]<-0#replacing -inf with 0
df$logad2[df$logad2==-Inf]<-0#replacing -inf with 0
df$logprom[df$logprom==-Inf]<-0#replacing -inf with 0
model1.ols<-plm(sales~logprice+logad1+logad2+logprom,data = df,model = "pooling")
summary(model1.ols)#Adj. R-Squared: 0.0065238

#############################################################################
#model2 with log-log
df$logsales<-log(df$sales)
model2.ols<-plm(logsales~logprice+logad1+logad2+logprom,data = df,model = "pooling")
summary(model2.ols) #Adj. R-Squared: 0.085313
# R-Squared is too low, which means our model is not significant enough to explain the fluctuations of sales

#############################################################################
#According to MMM approach, in ideal situation, advertisements shall contain decay,lag, shape effects
# lag effect is unlikely to present in this data set, as the dataset is recorded in monthly basis
# shape effect is sound in this data set, however, it would be too complicated as the limited knowledge and time we are having,
#
#decay effect, however, would be applicable into this case,which could be converted into mathematical expression as the following function below:
#model3 with adstock ratio 0.5 

adstock<-function(data,rate){return(as.numeric(filter(x=data,filter=rate,method="recursive")))}
df$logad1stock<-rep(log(adstock(df$ad1[1:36],0.5)),5)
df$logad2stock<-rep(log(adstock(df$ad2[1:36],0.5)),5)
df$logad1stock[df$logad1stock==-Inf]<-0#replacing -inf with 0
df$logad2stock[df$logad2stock==-Inf]<-0#replacing -inf with 0
model3.ols<-plm(logsales~logprice+logad1stock+logad2stock+logprom,data = df,model = "pooling")
summary(model3.ols) #Adj. R-Squared: 0.12398

#forecast
df$predict<-exp(predict(model3.ols))
df$predictl<-predict(model3.ols)

#plot the relationship between time and sales for different regions
plot7<-ggplot(df,aes(x = time, y = predict, group = region)) +
  geom_line(aes(linetype=region,color=region))+
  theme(legend.position = "top",plot.title = element_text(face="bold",hjust = 0.5),plot.subtitle = element_text(colour="blue",face = "italic",hjust = 0.5))+
  scale_x_time(breaks = c(1,10,20,30,36),labels = c("1","10","20","30","36"))+
  labs(x="Time(month)",y="Predict Sales",title="The Predicted Sales of Each Region",subtitle = "Model 3 OLS")
plot7

#Transform the sales into logsales as the variance of sales is too large, then draw the relationship again
plot8<-ggplot(df,aes(x = time, y = predictl, group = region)) +
  geom_line(aes(linetype=region,color=region))+
  theme(legend.position = "top",plot.title = element_text(face="bold",hjust = 0.5),plot.subtitle = element_text(colour="blue",face = "italic",hjust = 0.5))+
  scale_x_time(breaks = c(1,10,20,30,36),labels = c("1","10","20","30","36"))+
  labs(x="Time(month)",y="Predict logSales",title="The Predicted Log Sales of Each Region",subtitle = "Model 3 OLS ")
plot8

#combine Plot3 and plot4 for comparison
p4<-ggarrange(plot3,plot4,plot7,plot8,nrow=2,ncol = 2,labels = "AUTO")
p4

############################################################################

#Test for Random effects
#Null hypothesis: no panel effect
plmtest(model3.ols,type = c("bp"))
#p-value = 1.313e-09 
#reject the null hypothesis
#Random effect is present

model3.fe<-plm(logsales~logprice+logad1stock+logad2stock+logprom,data = df,model = "within")
model3.re<-plm(logsales~logprice+logad1stock+logad2stock+logprom,data = df,model = "random")
summary(model3.fe)  #Adj. R-Squared: 0.12317
summary(model3.re)  #Adj. R-Squared: 0.14075

#############################################################################
#Test for individual effects
#Null hypothesis: OLS better than fixed
pFtest(model3.fe,model3.ols)
#p-value = 6.29e-05
#reject the null hypothesis
#fixed effect is a better choice

#############################################################################
#model4 dynamic panel model with laglogsales
df$laglogsales<-lag(df$logsales)
df$laglogsales[is.na(df$laglogsales)]<-0
model4.fe<-plm(logsales~laglogsales+logprice+logad1stock+logad2stock+logprom,data = df,model = "within")
model4.re<-plm(logsales~laglogsales+logprice+logad1stock+logad2stock+logprom,data = df,model = "random")
summary(model4.fe) #Adj. R-Squared: 0.42451
summary(model4.re) #Adj. R-Squared: 0.48262

#Step for choosing regression models from random or fixed effects
#Run a Hausman test comparing random and fixed effects
#Null hypothesis: unique errors are correlated with aggressors
phtest(model4.fe,model4.re)
#p-value = 0.3393  
#fail to reject the null hypothesis
#choose Random Effect model

#############################################################################
#model5 dynamic panel model with laglogsales
#with adstock ratio 0.5+month(dummy variable)
model5.fe<-plm(logsales~laglogsales+logprice+logad1stock+logad2stock+logprom+factor(month),data = df,model = "within")
model5.re<-plm(logsales~laglogsales+logprice+logad1stock+logad2stock+logprom+factor(month),data = df,model = "random")
summary(model5.fe)#Adj. R-Squared: 0.90802
summary(model5.re)#Adj. R-Squared: 0.88158

#Step for testing time-fixed effects
#Null hypothesis: no time-fixed effect needed
phtest(model5.re,model4.re)
plmtest(model4.re,c("time"),type = ("bp"))
#Both results show that p-value < 0.05
#Reject the null hypothesis
#Time-effects is required


#############################################################################
#Theoretically, the decay effect ratio could be any number from 0 to 1
#In this study, assuming that the decay effect ratio of TV and Banner page within threshold from 0 to 1, by sequence of 0.1
#which means there would be 11x11= 121 adstocks combinations based on model6
#############################################################################
#Model selection would be based on the better R^2 
#And the significance of independent variables
#selecting the best model7 models 
#by sorting the highest R^2 with independent variables significance level of 95% 
var_matrix <-matrix(ncol=3)
adstock<-function(data,rate){return(as.numeric(filter(x=data,filter=rate,method="recursive")))}
for(Tvad_decay_ratio in seq(0,1,by=0.1)){
  df$logad1stock<-rep(log(adstock(df$ad1[1:36],Tvad_decay_ratio)),5)
  df$logad1stock[df$logad1stock==-Inf]<-0
  for(Bannerad_decay_ratio  in seq(0,1,by=0.1)){
    df$logad2stock<-rep(log(adstock(df$ad2[1:36],Bannerad_decay_ratio)),5)
    df$logad2stock[df$logad2stock==-Inf]<-0
    df$laglogsales<-lag(df$logsales)
    df$laglogsales[is.na(df$laglogsales)]<-0
    model_loop<-plm(logsales~laglogsales+logprice+logad1stock+logad2stock+logprom+factor(month),data = df,model = "random")
    if(all(summary(model_loop)$coefficients[c(1,2,4,5,6),'Estimate'] > 0)){
      var_matrix <- rbind(var_matrix, c(Tvad_decay_ratio,Bannerad_decay_ratio,as.numeric(summary(model_loop)$r.squared["adjrsq"]))) 
    } 
  }
}
colnames(var_matrix) <- c("Tvad_decay_ratio", "Bannerad_decay_ratio","Adj. R-Squared")
var_matrix <- var_matrix[-1,]
sd(var_matrix[,"Adj. R-Squared"])
R_Squared_mean <- mean(var_matrix[,"Adj. R-Squared"])
abs_difference <- abs(var_matrix[,"Adj. R-Squared"]-R_Squared_mean)
var_matrix <- cbind(var_matrix,abs_difference)
var_matrix <- var_matrix[order(var_matrix[,"abs_difference"],decreasing = FALSE),]
var_matrix

#According to the model selection result
#the best model combination is 
#TV     adstock with ratio 0.6 
#Banner adstock with ratio 0.2
#Thus, following is the final model:
df$logad1stockf<-rep(log(adstock(df$ad1[1:36],0.6)),5)
df$logad2stockf<-rep(log(adstock(df$ad2[1:36],0.2)),5)
df$logad1stockf[df$logad1stockf==-Inf]<-0#replacing -inf with 0
df$logad2stockf[df$logad2stockf==-Inf]<-0#replacing -inf with 0
modelfinal.re<-plm(logsales~laglogsales+logprice+logad1stockf+logad2stockf+logprom+factor(month),data = df,model = "random")
summary(modelfinal.re)#Adj. R-Squared: 0.88478

#############################################################################
#Hypothesis testing
#test for cross-sectional dependence
#Null hypothesis: residuals across entities are not correlated
pcdtest(modelfinal.re,test = c("lm"))
pcdtest(modelfinal.re,test = c("cd"))
#p-value < 2.2e-16 
#reject the null hypothesis
#According to Baltagi, cross-sectional dependence is not a problem in micro panels(few years and large number of cases)

#test for serial correlation
#Null hypothesis: no serial correlation
pbgtest(modelfinal.re)
#p-value =  2.182e-12
#reject the null hypothesis
#Not an issue in micro panel(with very few years)

#VIF TEST
car::vif(modelfinal.re)
#VIF DOES NOT EXCEED 5 except the dummy variable, NO STRONG MULTICOLLINEARITY ISSUE

#test for heteroskedasticity
#Residual plot
df$predictmodelfinal<-exp(predict(modelfinal.re,newdata = df))
df$predictLmodelfinal<-predict(modelfinal.re,newdata = df)

df$resi <- modelfinal.re$residuals
ggplot(data = df, aes(y = resi, x = predictLmodelfinal)) +
  geom_point(col = 'blue') + 
  geom_abline(slope = 0)
#No evident pattern present


#Null hypothesis: covariance is constant(Homoskedasticity)
bptest(modelfinal.re,studentize = F)
#p-value = 2.709e-06
#reject the null hypothesis
#covariance is not constant(Heteroskedasticity detected)

#Robust test for Heteroskedasticity
#cluster standard errors at the individual level
robust_test<-coeftest(modelfinal.re,vcovHC(modelfinal.re,type = "HC0",cluster = "group"))
robust_test
#No significant differences between two sets of standard error, thus it would be safe to say the result of modelfinal.re is homokedasticity

#############################################################################
#Step 
#Forecast
plot9<-ggplot(df,aes(x = time, y = predictmodelfinal, group = region)) +
  geom_line(aes(linetype=region,color=region))+
  theme(legend.position = "top",plot.title = element_text(face="bold",hjust = 0.5),plot.subtitle = element_text(colour="blue",face = "italic",hjust = 0.5))+
  scale_x_time(breaks = c(1,10,20,30,36),labels = c("1","10","20","30","36"))+
  labs(x="Time(month)",y="Predict Sales",title="The Predicted Sales of Each Region",subtitle = "Model Final")
plot9
  
  
plot10<-ggplot(df,aes(x = time, y = predictLmodelfinal, group = region)) +
  geom_line(aes(linetype=region,color=region))+
  theme(legend.position = "top",plot.title = element_text(face="bold",hjust = 0.5),plot.subtitle = element_text(colour="blue",face = "italic",hjust = 0.5))+
  scale_x_time(breaks = c(1,10,20,30,36),labels = c("1","10","20","30","36"))+
  labs(x="Time(month)",y="Predict logSales",title="The Predicted Log Sales of Each Region",subtitle = "Model Final")
plot10

p5<-ggarrange(plot3,plot4,plot7,plot8,plot9,plot10,nrow=3,ncol = 2,labels = "AUTO")
p5

P6<-ggarrange(plot3,plot4,plot9,plot10,nrow=2,ncol = 2,labels = "AUTO")
P6

MAPE_logFinal<-MAPE(df$predictLmodelfinal,df$logsales) 
MAPE_Final<-MAPE(df$predictmodelfinal,df$sales) 
MAPE_logFinal#[1] 0.1310012
MAPE_Final   #[1] 1.860086

# MAPE improvement matrix
#model2
df$predictmodel2.ols<-exp(predict(model2.ols))
df$predictLmodel2.ols<-predict(model2.ols)

MAPE_Model2<-MAPE(df$predictmodel2.ols,df$sales)
MAPE_Model2log<-MAPE(df$predictLmodel2.ols,df$logsales)

#model3
df$predictmodel3.re<-exp(predict(model3.re))
df$predictLmodel3.re<-predict(model3.re)

MAPE_Model3<-MAPE(df$predictmodel3.re,df$sales)
MAPE_Model3log<-MAPE(df$predictLmodel3.re,df$logsales)

#model4
df$predictmodel4.re<-exp(predict(model4.re))
df$predictLmodel4.re<-predict(model4.re)

MAPE_Model4<-MAPE(df$predictmodel4.re,df$sales)
MAPE_Model4log<-MAPE(df$predictLmodel4.re,df$logsales)

#model5
MAPE_Model5<-df$predictmodel5.re<-exp(predict(model5.re))
MAPE_Model5log<-df$predictLmodel5.re<-predict(model5.re)

MAPE_Model5<-MAPE(df$predictmodel5.re,df$sales)
MAPE_Model5log<-MAPE(df$predictLmodel5.re,df$logsales)


MAPE_Matrix<-matrix(ncol=2)
MAPE_prediction<-c(MAPE_Model2,MAPE_Model3,MAPE_Model4,MAPE_Model5,MAPE_Final)
MAPE_logprediction<-c(MAPE_Model2log,MAPE_Model3log,MAPE_Model4log,MAPE_Model5log,MAPE_logFinal)
model <- c("MAPE_Model2","MAPE_Model3","MAPE_Model4","MAPE_Model5","MAPE_Final") 
MAPE_Matrix<-cbind(MAPE_prediction, MAPE_logprediction)

colnames(MAPE_Matrix) <- c("MAPE_prediction","MAPE_logprediction")
rownames(MAPE_Matrix) <- c("MAPE_Model2","MAPE_Model3","MAPE_Model4","MAPE_Model5","MAPE_Final") 
MAPE_Matrix

par(mar =c(5,5,2,5))
twoord.plot(c(1:5),MAPE_prediction,c(1:5),MAPE_logprediction, xticklab = model,
            lcol=4, rcol=2,type = c("l","l"), lwd = 2, 
            xlab = "Model",ylab = "MAPE_prediction", rylab = "MAPE_logprediction",
            main = "MAPE Improvement")

#The reason behind of this is that the points of data we have are very limited, and our model is based on log-log model, which means we can predict log form dependent variable with fair amount of accuracy, however, transform the log prediction into standard(exponential)form will affect the model overall forecast accuracy.
#The more obvious effect of this issue is the MAPE value.
#In this case, MAPE would have less significance in regards to exam the model's fitness by comparing the actual sales prediction and actual sales. 
#which means our model has sound ability to predict the overall sales trend instead of forecasting the actual sales due to unobserved variables and reasons.
#

df$predictmodelfinal<-format(df$predictmodelfinal,scientific = FALSE)
prediction<-subset(df,select = c(sales,predictmodelfinal,logsales,predictLmodelfinal))
View(prediction)
