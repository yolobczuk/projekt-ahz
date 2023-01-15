

library(survival)
library(gtools)
library(survMisc)
data <- survival::diabetic

# Kategoryzacja AGE
data$Age_cat=quantcut(data$age,q=4)
summary(data)

km = survfit(Surv(time, status) ~ trt, conf.type="plain", data=diabetic) # estymator KM
summary(km)
plot(km)

test.1=survdiff(Surv(time, status) ~ trt, data=diabetic, rho=1) # test Peto i Peto
test.1 # wniosek: zmienna inst nie jest istotnym predyktorem (na poziomie istotnosci alfa=0,05)
test.1=survdiff(Surv(time, status) ~ trt, rho=0, data=diabetic) # test log-rank
test.1

km = survfit(Surv(time, status) ~ laser, conf.type="plain", data=diabetic) # estymator KM
summary(km)
plot(km)

#estymator Nelsona-Aalena
Nels=survfit(Surv(time, status) ~ trt, conf.type ="log-log", type="fleming-harrington",data=diabetic) # estymator NA
summary(Nels)
lines(Nels,col="blue")
Nels2=survfit(Surv(time,cens) ~ 1, conf.type ="plain", type="fleming-harrington",data=diabetic) # estymator NA
lines(Nels2,col="grey")

#Test dla zmiennej laser. Widac, ze wartosc p jest wysoka. Zmienne nie sa rozne i nie wplywa na dlugosc zycia.
test.1=survdiff(Surv(time, status) ~ laser, data=diabetic, rho=1) # test Peto i Peto
test.1 # wniosek: zmienna inst nie jest istotnym predyktorem (na poziomie istotnoÅci alfa=0,05)
test.1=survdiff(Surv(time, status) ~ laser, rho=0, data=diabetic) # test log-rank
test.1

# To powyzej powtarzyc dla wszystkich predyktorow

#library(survMisc) # zbior testÃow dla roznych wag
t1=survfit(Surv(time, status) ~ trt,data=diabetic)
comp(ten(t1),p=c(0,1,1,0.5,0.5),q=c(1,0,1,0.5,2)) # roz¼ne wagi w testach Fleminga-Harringtona
# jako druga lista w przypadku zmiennej dychotomicznej sÄ wyÅwietlane testy Renyi
#kategoryzacja zmiennej wedÅug kwantyli i porÃ³wnywanie krzywych przeÅ¼ycia dla grup kwartylowych
t2 = ten(t1)
comp(t2,p=c(0,1,1,0.5,0.5),q=c(1,0,1,0.5,2))
b = attr(t2, "lrt") #lrw - obliczenia pomocnicze, lrt - test wilcoxona, sup - test supremum (koÅmogorowa, typu reni), tft - testy dla trendu dla zmiennej w skali porzÄdkowej
c = attr(t2, "sup")
cbind(b$W, b$pNorm, c$pSupBr)

#lrt bo mamy dla zmiennej skale dychotomiczna, tft dla porzadkowej, uzywamy 2 kolumny jesli sie nie przecinaja, trzeciej jesli sie przecinaja
#Dla modelu Cox robimy dla wszystkich istotnych zmiennych

### PORÓWNANIE RISK PARAMI
t2=ten(survfit(Surv(time,status)~risk, data=data[data$risk==9 | data$risk==8,]))
comp(t2,p=c(0,1,1,0.5,0.5),q=c(1,0,1,0.5,2))
b = attr(t2, 'lrt')
c = attr(t2, 'sup')
cbind(b$W, b$pChisq, c$pSupBr)
plot(survfit(Surv(time,status)~risk, data=data[data$risk==9 | data$risk==8,]))


Cox=coxph(Surv(time, status) ~ trt+laser+age+eye+risk, data=diabetic)
summary(Cox)
library(MASS)
stepAIC(Cox,direction="backward")
#Najlepiej wychodzi zestawinie zmiennych trt, eyeright, risk. Wartosc p jest najnizsza.

Cox=coxph(Surv(time,status)~trt+eye+risk, data=diabetic)
summary(Cox)

library(survMisc)
#Sa to miary r2. Im blizej 1 tym lepiej.
rsq(Cox)

#Wygooglac co one znacza
Cox$coefficients

Prop=cox.zph(Cox,transform="identity") # weryfikacja zalolenia proporcjonalno?ci
print(Prop)
plot(Prop)

#H0 - hazard jest proporcjonalny dla danej zmiennej w czasie

Prop=cox.zph(Cox,transform="rank") # weryfikacja za?o?enia proporcjonalno?ci
print(Prop)

Prop=cox.zph(Cox,transform="km") # weryfikacja zalozenia proporcjonalnoli
print(Prop)

reszty=resid(Cox, type="scaledsch") # graficzna weryfikacja zalozenia proporcjonalno?ci
Time=as.numeric(rownames(reszty))
zmienne=c("trt","eye","risk")
for (i in 1:3) 
{plot(log(Time), reszty[,i], xlab="ln(Czas)",main=zmienne[i],
      ylab="Skalowane reszty Schoenfelda",pch=20, cex=0.7)
  lines( smooth.spline(log(Time), reszty[,i] ), lwd=3 )
}

deviance=residuals(Cox,type="deviance")# jednostki odstajace
s=Cox$linear.predictors
plot(s,deviance,xlab="Liniowy predyktor",ylab="Reszty odchylen",cex=0.5, pch=20)
abline(h=3,lty=3)
data$deviance=deviance
c1=which(data$deviance>3)

# jednostki wplywowe
dfb=residuals(Cox,type="dfbeta")
n=dim(dfb)[1]
obs.nr=c(1:n)
for (j in 1:3) {
  plot(obs.nr,dfb[,j],xlab="Numer jednostki",ylab="Przyrost oceny parametru",
      main=zmienne[j])
  }
a1=which(dfb[,1]>(0.004)) #usunac te jednostki
a2=which(abs(dfb[,2])>(0.06))
a3=which(abs(dfb[,3])>(0.1))
