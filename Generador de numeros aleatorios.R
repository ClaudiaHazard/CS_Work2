#a)
x <- function(i,a,b,d){
  return ((a*i+b)%%d)
}

u <- function(i,m){
  return (i/m)
}

GCL <- function(a,b,d,n,s){
  algo=c(s)
  ui=numeric()
  for(i in 1:(n-1)){
    algo=c(algo,x(s,a,b,d))
    ui=c(ui,u(s,d))
    s=x(s,a,b,d)
  }
  ui=c(ui,u(s,d))
  return(data.frame(X=algo,U=ui))
}

#b)
GLC64=GCL(191,401,997,64,11)
print(GLC64$X)


GLC1000=GCL(191,401,997,1000,11)
print(GLC1000$X)


GLC2000=GCL(191,401,997,2000,11)
print(GLC2000$X)


#c)

dfGLC <- function(n,v){
  y1= numeric()
  for (i in 1:n){
    y1<-c(y1,i)
  }
  df <- data.frame(x=y1, y=v, stringsAsFactors = FALSE)
  return (df)
}


par(mfrow=c(1,3))
lala = dfGLC(64,GLC64$X)
plot(lala,xlab="n",ylab="x")

lala2=dfGLC(1000,GLC1000$X)
plot(lala2,xlab="n",ylab="x")

lala3=dfGLC(2000,GLC2000$X)
plot(lala3,xlab="n",ylab="x")

#d)

hist(GLC64$U,main="n=64",xlab="U_Values")
hist(GLC1000$U,main="n=1000",xlab="U_Values")
hist(GLC2000$U,main="n=2000",xlab="U_Values")

#e) ---------

#f)

attach(mtcars)
par(mfrow=c(2,3))
set.seed(1)
hist(runif(10, min = 0, max = 1),main="n=10",xlab = "number")
hist(runif(100, min = 0, max = 1),main="n=100",xlab = "number")
hist(runif(1000, min = 0, max = 1),main="n=1000",xlab = "number")
hist(runif(10000, min = 0, max = 1),main="n=10000",xlab = "number")
hist(runif(100000, min = 0, max = 1),main="n=100000",xlab = "number")

attach(mtcars)
par(mfrow=c(2,3))
hist(GCL(191,401,997,10,11)$U,main="n=10",xlab = "number")
hist(GCL(191,401,997,100,11)$U,main="n=100",xlab = "number")
hist(GCL(191,401,997,1000,11)$U,main="n=1000",xlab = "number")
hist(GCL(191,401,997,10000,11)$U,main="n=10000",xlab = "number")
hist(GCL(191,401,997,100000,11)$U,main="n=100000",xlab = "number")

#g)

g64=GCL(33,13,64,64,57)
g1000=GCL(33,13,64,1000,57)
g2000=GCL(33,13,64,2000,57)

#64
par(mfrow=c(1,2))
lala = dfGLC(64,GLC64$X)
plot(lala,xlab="n",ylab="x")

lala64=dfGLC(64,g64$X)
plot(lala64,xlab="n",ylab="x")

#1000
lala2=dfGLC(1000,GLC1000$X)
plot(lala2,xlab="n",ylab="x")

lala1000=dfGLC(1000,g1000$X)
plot(lala1000,xlab="n",ylab="x")


#2000
lala3=dfGLC(2000,GLC2000$X)
plot(lala3,xlab="",ylab="x")

lala2000=dfGLC(2000,g2000$X)
plot(lala2000,xlab="",ylab="x")

par(mfrow=c(1,1))



#h)





