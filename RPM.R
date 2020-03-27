install.packages("riskParityPortfolio")
library(riskParityPortfolio)
?riskParityPortfolio



weight<-matrix(1/ncol(dax.ret),nrow=1,ncol=ncol(dax.ret)) #Gewichtung
p.ret<-(weight) %*% t(dax.ret)  
demean <- scale(dax.ret, center=TRUE, scale=FALSE)
Sigma <- cov(demean) # create covariance matrix


# risk parity portfolio (mit gleichverteilten Risiken)
RPP <- riskParityPortfolio(Sigma)
names(RPP)
RPP$w
RPP$relative_risk_contribution
par(mfrow=c(1,1))
barplot(RPP$w,xlim=NULL,ylab="Gewicht",main = "Risk Parity Portfolio")

# risk budggeting portfolio (vorbestimmte Risikoverteilung)
RPP.risk <- riskParityPortfolio(Sigma, b = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.05, 0.05, 0.05, 0.05, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01))
RPP.risk$w
RPP.risk$relative_risk_contribution
plot(RPP.risk$w,xlab="Komponente",ylab="Gewicht",main = "Risk Parity Portfolio")
barplot(RPP.risk$w,ylab="Gewicht",main = "Risk Parity Portfolio")
