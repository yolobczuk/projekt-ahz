

library(survival)
library(gtools)
library(survMisc)
data <- survival::data


###########################################################################################
### Zmienna AGE
#kategoryzacja zmiennej
data$Age_cat=quantcut(data$age,q=4)
summary(data)

#Estymator Kaplana-Meiera
km = survfit(Surv(time, status) ~ Age_cat, conf.type="plain", data=data) # estymator KM
summary(km)
plot(km, col=c("blue","pink","red","green"))
legend("topright",c("[1,10]","(10,16]","(16,30]","(30,58]"),lty=1,col=c("blue","pink","red","green"),bty="n")
#estymator Nelsona-Aalena
Nels=survfit(Surv(time, status) ~ Age_cat, conf.type ="log-log", type="fleming-harrington",data=data) # estymator NA
summary(Nels)
lines(Nels,plot(km, col=c("blue","pink","red","green")))
Nels2=survfit(Surv(time,cens) ~ 1, conf.type ="plain", type="fleming-harrington",data=data) # estymator NA
lines(Nels2,col="grey")

#testy istotności
test_petopeto=survdiff(Surv(time, status) ~ Age_cat, data=data, rho=1) # test Peto i Peto
test_petopeto # wniosek: Przedział wiekowy nie jest istotnym predyktorem możliwości wystąpienia ślepoty (na poziomie istotnosci alfa=0,05)
test_logrank=survdiff(Surv(time, status) ~ Age_cat, rho=0, data=data) # test log-rank
test_logrank


###########################################################################################
### ZMIENNA TREATMENT - czy w ogóle było leczenie czy 0 -no treatment 1 - leczenie laserem
#Estymator Kaplana-Meiera
km = survfit(Surv(time, status) ~ trt, conf.type="plain", data=data) # estymator KM
summary(km)
plot(km)

test_petopeto=survdiff(Surv(time, status) ~ trt, data=data, rho=1) # test Peto i Peto
test_petopeto # wniosek: zmienna trt nie jest istotnym predyktorem (na poziomie istotnosci alfa=0,05)
test_logrank=survdiff(Surv(time, status) ~ trt, rho=0, data=data) # test log-rank
test_logrank

#Obydwa testy (Peto Peto oraz log rank) przy poziomie istotności = 0.05 wskazują, że leczenie oka laserem wpływa istotnie
# na nie wystąpienie ślepoty w leczonym oku. 
#estymator Nelsona-Aalena
Nels=survfit(Surv(time, status) ~ trt, conf.type ="log-log", type="fleming-harrington",data=data) # estymator NA
summary(Nels)
lines(Nels,col="blue")
Nels2=survfit(Surv(time,cens) ~ 1, conf.type ="plain", type="fleming-harrington",data=data) # estymator NA
lines(Nels2,col="grey")


############################################################################################
### ZMIENNA LASER - typ laseru xenon albo argon

km = survfit(Surv(time, status) ~ laser, conf.type="plain", data=data) # estymator KM
summary(km)
plot(km)

#estymator Nelsona-Aalena
Nels=survfit(Surv(time, status) ~ laser, conf.type ="log-log", type="fleming-harrington",data=data) # estymator NA
summary(Nels)
lines(Nels,col="blue")
Nels2=survfit(Surv(time,cens) ~ 1, conf.type ="plain", type="fleming-harrington",data=data) # estymator NA
lines(Nels2,col="grey")

#Test dla zmiennej laser. Widac, ze wartosc p jest wysoka. oznacza to, że typ użytego lasera nie wpływa istotnie na różnice w wystąpieniu ślepoty.
test_petopeto=survdiff(Surv(time, status) ~ laser, data=data, rho=1) # test Peto i Peto
test_petopeto # wniosek: zmienna inst nie jest istotnym predyktorem (na poziomie istotnoÅci alfa=0,05)
test_logrank=survdiff(Surv(time, status) ~ laser, rho=0, data=data) # test log-rank
test_logrank


#############################################################################################
### ZMIENNA EYE - które oko było leczone

km = survfit(Surv(time, status) ~ eye, conf.type="plain", data=data) # estymator KM
summary(km)
plot(km, col=c("blue","pink","red","green","yellow","black"))
legend("topright",c("6","8","9","10","11","12"),lty=1,col=c("blue","pink","red","green","yellow","black"),bty="n")

#estymator Nelsona-Aalena
Nels=survfit(Surv(time, status) ~ eye, conf.type ="log-log", type="fleming-harrington",data=data) # estymator NA
lines(Nels2,col="grey")
lines(Nels,col=c("blue","pink","red","green","yellow","black"))     ### NIE WIEM JAK TO WYKONAĆ PÓKI CO
Nels2=survfit(Surv(time,cens) ~ 1, conf.type ="plain", type="fleming-harrington",data=data) # estymator NA
lines(Nels2,col="grey")

#W przypadku obydwóch testów, przy przyjętym poziomie istotności - 0.05 nie wystąpiła statystycznie istotna różnica
# pomiędzy leczeniem prawo a lewego oka 
test_petopeto=survdiff(Surv(time, status) ~ eye, data=data, rho=1) # test Peto i Peto
test_petopeto # wniosek: zmienna inst nie jest istotnym predyktorem (na poziomie istotnoÅci alfa=0,05)
test_logrank=survdiff(Surv(time, status) ~ eye, rho=0, data=data) # test log-rank
test_logrank

#############################################################################################
### ZMIENNA RISK - grupa ryzyka

km = survfit(Surv(time, status) ~ risk, conf.type="plain", data=data) # estymator KM
summary(km)
plot(km)

#estymator Nelsona-Aalena
Nels=survfit(Surv(time, status) ~ risk, conf.type ="log-log", type="fleming-harrington",data=data) # estymator NA
summary(Nels)
lines(Nels)
Nels2=survfit(Surv(time,cens) ~ 1, conf.type ="plain", type="fleming-harrington",data=data) # estymator NA
lines(Nels2)

# Przy przyjętym poziomie istotności = 0.05 grupa ryzyka miała istotny wpływ na wystąpienie ślepoty w oku
test_petopeto=survdiff(Surv(time, status) ~ risk, data=data, rho=1) # test Peto i Peto
test_petopeto # wniosek: zmienna inst nie jest istotnym predyktorem (na poziomie istotnoÅci alfa=0,05)
test_logrank=survdiff(Surv(time, status) ~ risk, rho=0, data=data) # test log-rank
test_logrank


#library(survMisc) # zbior testÃow dla roznych wag
t1=survfit(Surv(time, status) ~ trt,data=data)
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


Cox=coxph(Surv(time, status) ~ trt+laser+age+eye+risk, data=data)
summary(Cox)
library(MASS)
stepAIC(Cox,direction="backward")
#Najlepiej wychodzi zestawinie zmiennych trt, eyeright, risk. Wartosc p jest najnizsza.

Cox=coxph(Surv(time,status)~trt+eye+risk, data=data)
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
