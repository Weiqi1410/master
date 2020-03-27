############## PCA

weight<-matrix(1/ncol(dax.ret),nrow=1,ncol=ncol(dax.ret)) #Gewichtung
p.ret<-(weight) %*% t(dax.ret) 


#PCA Foundational
demean <- scale(dax.ret, center=FALSE, scale=FALSE)
covm <- cov(demean)
evec1 <- eigen(cov(demean), symmetric=TRUE)$vector[] #Eigenvektoren
eval1 <- eigen(cov(demean), symmetric=TRUE)$values #Eigenwerte
inv.evec1 <- solve(evec1) #Inverse der Eigenvektoren
pc.port1 <- inv.evec1 %*% t(dax.ret)
attach(mtcars)
par(mfrow=c(2,2))
plot(pc.port1[1,], main="PC1", type = "l",xlab = "Timeline", ylab = "Rendite")
plot(pc.port1[10,], main="PC10", type = "l",xlab = "Timeline", ylab = "Rendite")
plot(pc.port1[20,], main="PC20", type = "l",xlab = "Timeline", ylab = "Rendite")
plot(pc.port1[30,], main="PC30", type = "l",xlab = "Timeline", ylab = "Rendite")
par(mfrow=c(1,1))




#PCA Functional
pca <- prcomp(dax.ret,center=TRUE, scale=TRUE)
summary(pca)
plot(pca,type="l")
#biplot(pca)
#str(pca)
#pca$x
evec <- pca$rotation[] #Eigenvektoren
eval <- pca$sdev^2 #Eigenwerte
#diag(t(evec) %*% covm %*% evec) #Nachprüfung
inv.evec <- solve(evec) #Inverse der Eigenvektoren
pc.port <- inv.evec %*% t(dax.ret)

attach(mtcars)
par(mfrow=c(2,2))
plot(pc.port[1,], main="PC1", type = "l",xlab = "Timeline", ylab = "Rendite")
plot(pc.port[10,], main="PC10", type = "l",xlab = "Timeline", ylab = "Rendite")
plot(pc.port[20,], main="PC20", type = "l",xlab = "Timeline", ylab = "Rendite")
plot(pc.port[30,], main="PC30", type = "l",xlab = "Timeline", ylab = "Rendite")
par(mfrow=c(1,1))






######################
# Arbeiten mit dem wichtigsten Faktor
inv.evec2 <- inv.evec[1,]
inv.evec2.q <- inv.evec2^2
sum(inv.evec2.q)  #Prüfung Summe=1
pc.port2 <- inv.evec2.q %*% t(dax.ret)
cor(dax.ret,t(pc.port2)) # Korrelationen zwischen Daten und PCP


