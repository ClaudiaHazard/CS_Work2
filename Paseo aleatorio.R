#a) Varianza=1

par(mfrow=c(1,1))

#Unidimencional

set.seed(26)
x<-cumsum(rnorm(n=10,0,1))
print (x)
plot(data.frame(y=1:10,x=x),type="b",col="red",xlab="Numero de paso en el recorrido [n]",ylab="Posición [m]",main="Paseo Aleatorio 1D")

#b)
set.seed(26)
x<-cumsum(rnorm(n=10,0,1))
print (x)
y<-cumsum(rnorm(n=10,0,1))
print(y)
la=data.frame(x=x,y=y)
plot(la,type="c",col="red",xlab="Posicion en el eje x [m]", "Posicion en el eje y [m]",main="Paseo Aleatorio 2D")
text(la$x,la$y,label=c(1,2,3,4,5,6,7,8,9,10),col='red')


set.seed(33)
x<-cumsum(rnorm(n=10,0,1))
print (x)
y<-cumsum(rnorm(n=10,0,1))
print(y)
la=data.frame(x=x,y=y)
plot(la,type="c",col="red",xlab="Posicion en el eje x [m]", "Posicion en el eje y [m]",main="Paseo Aleatorio 2D")
text(la$x,la$y,label=c(1,2,3,4,5,6,7,8,9,10),col='red')
