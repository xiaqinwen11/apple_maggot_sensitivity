library(readxl)
library(ggplot2)
library(Rmisc)
library(dplyr)
library(MASS)


data <- read_excel("Q:/Qinwen/Rhagoletis/Ecdysteroid sensitivity/Rhagoletis_sensitivity/Rhagoletis_sensitivity.xlsx", sheet = "Sheet1")
data_no_nt<-subset(data,data$dose!="nt")

#fit decay in ggplot
ggplot(data_no_nt, aes(x = log(dose+1), y = days,color=race)) + geom_point() + 
  stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = F)+facet_grid(.~phase)


#regression  rcace seperately first to get the k and C for each race
#then test the race effect on C and K
#prewinter
pre=subset(data_no_nt, phase=="pre")
ggplot(pre,aes(x=log(dose+1),y=days,color=race))+geom_point()+
  stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = F)

coef(nls(days ~ C * exp(-k*log(dose+1)), data = pre[pre$race=="apple",], start = list(C=33, k=1)))
coef(nls(days ~ C * exp(-k*log(dose+1)), data = pre[pre$race=="haw",], start = list(C=53, k=1)))

library(nlme)
fit <- gnls(days ~ C * exp(-k*log(dose+1)), 
            data = pre[,-c(2,4,5,7,8)], 
            params = list(C ~ race, k ~ race), 
            start = list(C = c(34, 15), k = c(0.2, 0.13)),
            weights = varExp(-0.1, form = ~log(dose+1)),
            control = gnlsControl(nlsTol = 0.1))

summary(fit)
plot(fit)





#fit overwinter

over_winter=subset(data_no_nt, phase=="overwinter2")

coef(nls(days ~ C * exp(-k*log(dose+1)), data = over_winter[over_winter$race=="apple",], start = list(C=33.5, k=1)))
coef(nls(days ~ C * exp(-k*log(dose+1)), data = over_winter[over_winter$race=="haw",], start = list(C=50, k=1)))

library(nlme)
fit <- gnls(days ~ C * exp(-k*log(dose+1)), 
            data = over_winter[,-c(2,4,5,7,8)], 
            params = list(C ~ race, k ~ race), 
            start = list(C = c(33.5, 16.5), k = c(0.32, 0.09)),
            weights = varExp(-0.1, form = ~log(dose+1)),
            control = gnlsControl(nlsTol = 0.1))

summary(fit)
plot(fit)






#fit pw10

post_10=subset(data_no_nt, phase=="pw10")

coef(nls(days ~ C * exp(-k*log(dose+1)), data = post_10[post_10$race=="apple",], start = list(C=40, k=0.3)))
coef(nls(days ~ C * exp(-k*log(dose+1)), data = post_10[post_10$race=="haw",], start = list(C=55, k=0.3)))

library(nlme)
fit <- gnls(days ~ C * exp(-k*log(dose+1)), 
            data = post_10[,-c(2,4,5,7,8)], 
            params = list(C ~ race, k ~ race), 
            start = list(C = c(40.5, 15), k = c(0.31, 0.06)),
            weights = varExp(-0.1, form = ~log(dose+1)),
            control = gnlsControl(nlsTol = 0.1))

summary(fit)
plot(fit)



#fit pw20

post_20=subset(data_no_nt, phase=="pw20")

coef(nls(days ~ C * exp(-k*log(dose+1)), data = post_20[post_20$race=="apple",], start = list(C=24, k=0.3)))
coef(nls(days ~ C * exp(-k*log(dose+1)), data = post_20[post_20$race=="haw",], start = list(C=50, k=0.3)))

library(nlme)
fit <- gnls(days ~ C * exp(-k*log(dose+1)), 
            data = post_20[,-c(2,4,5,7,8)], 
            params = list(C ~ race, k ~ race), 
            start = list(C = c(23.5, 16.6), k = c(0.17, 0.05)),
            weights = varExp(-0.1, form = ~log(dose+1)),
            control = gnlsControl(nlsTol = 0.1))

summary(fit)
plot(fit)




#fit pw30

post_30=subset(data_no_nt, phase=="pw30")

coef(nls(days ~ C * exp(-k*log(dose+1)), data = post_30[post_30$race=="apple",], start = list(C=16.2, k=0.3)))
coef(nls(days ~ C * exp(-k*log(dose+1)), data = post_30[post_30$race=="haw",], start = list(C=35, k=0.3)))

library(nlme)
fit <- gnls(days ~ C * exp(-k*log(dose+1)), 
            data = post_30[,-c(2,4,5,7,8)], 
            params = list(C ~ race, k ~ race), 
            start = list(C = c(16.2, 17.3), k = c(0.11, 0.04)),
            weights = varExp(-0.1, form = ~log(dose+1)),
            control = gnlsControl(nlsTol = 0.1))

summary(fit)
plot(fit)