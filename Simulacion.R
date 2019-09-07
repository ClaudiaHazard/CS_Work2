library(xtable)
#Pregunta 1
#a)
simulacion <- function(n)
{
  vector= sample(0:1,n, replace=T, prob=c(0.37,0.63))
  return (vector)
}

#b)

pEmpirica <- function(v){
  c=0
  g=0
  for (i in v){
    c=c+1
    if (i == 1){
      g=g+1
    }
  }
  return(g/c)
}

for (i in c(10,50,500,1000,10000)){
  set.seed(10)
  print(paste("probabilidad emprica de",i ,"es", pEmpirica(simulacion(i))))
}


#c)
vect=numeric()
for (i in seq(5, 10000, by=25)){
  set.seed(10)
  vect=c(vect,pEmpirica(simulacion(i)))
}
plot(data.frame(n=seq(5, 10000, by=25),prob=vect),ylim=c(0.6,0.8),main="Probabilidad Empírica V/S Tamaño")


#d)
rachas <- function(v){
  flag=FALSE
  c=0
  rachas=numeric()
  for (i in v){
    if (i == 1){
      flag=TRUE
    } else {
      flag=FALSE
    }
    if (flag==TRUE){
      c=c+1
    }
    if ((flag==FALSE) & !(c==0)){
      rachas=c(rachas,c)
      c=0
    }
  }
  if (flag==TRUE){
    rachas=c(rachas,c)
  } 
  
  return(rachas)
}

largos <- function(v){
  n= length(v)
  df <- data.frame(x = numeric(), y = numeric(), stringsAsFactors = FALSE)
  for (i in v){
    df <- rbind(df,data.frame(x=i,y=1))
  }
  return(df)
}

ganar=numeric()
for (i in c(200,1000,5000)){
  set.seed(10)
  rach=rachas(simulacion(i))
  print (paste("Para ",i,"se obtuvieron ",length(rach)," cantidad de rachas de largos"))
  df <- largos(rach)
  df <- aggregate(y~x,data=df,FUN=sum)
  print (df)
  print(paste("Rachas mayoreso iguales a 5 son",sum(df[df$x>=5,]$y)))
  ganar=c(ganar,sum(df[df$x>=5,]$y*df[df$x>=5,]$x)/i)
  
}

#e)
print(ganar)

set.seed(10)
rach=rachas(simulacion(10000))
df <- largos(rach)
df <- aggregate(y~x,data=df,FUN=sum)
print (df)
print(paste("Rachas mayoreso iguales a 5 son",sum(df[df$x>=5,]$y)))
print( print (sum(df[df$x>=5,]$y*df[df$x>=5,]$x)/10000))


#f)
set.seed(10)
df <- largos(rachas(simulacion(1000)))
df <- aggregate(y~x,data=df,FUN=sum)
z=numeric()
for(i in df$y*df$x){
  z=c(z,i/1000)
}
df$z <- z  

print(df)

print(xtable(df))

