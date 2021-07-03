library(tidyverse)
library(dplyr)
#1. Calcula los valores numéricos aproximados de----

#a
0.3*0.15/(0.3*0.15+0.2*0.8+0.5*0.12)

#b
(5^6/factorial(6))*exp(-5)

#c
choose(5,2)*(0.4^7)*(0.69^13)

#2. Realizar la siguiente suma ----

#a
c<-(1:1000)
sum(c)

#b
a <- 1
r <- 2
n <- 1024

S <- (n*r - a)/(r-1) + 1
S

#3. El vector grupo representa el grupo----
#al que pertenece una serie de alumnos

grupo
length(grupo)
summary(grupo)

grupo== "A" 
table(grupo== "A")

x <- c(grupo)
which(x == "A")

#4. El vector nota representa la nota ----
#de un examen de los alumnos que están en los grupos del vector grupo

#a
nota
sum(nota)

#b
mean(nota)

#c #FALTA----
Q <- filter (df02, nota > 7)
which(Q$nota)

#d
sort(nota, TRUE)

#e
n <- max(nota) 
which(nota == n)

#5 A partir de los vectores grupo y nota definidos----

#a
head(nota,10)
sum(head(nota,10))

#a2
sno<-nota[1:10]
sum(sno)

#b
grupo== "C"
table(grupo== "C")

#b2
length(grupo[grupo=="C"])

#c
nota>=5.5
table(nota>=5.5)

#c2
x<-c(nota)
A <- which(x >= 5.5)
length(A)

#c3
length(nota[nota>=5.5])

#d
df <- data.frame(grupo,nota)
df02<- as_tibble(df)
df02
nrow(filter(df02, grupo == "B", nota >= 5.5))

#d2
y <- nota [which( grupo == "B" )]
A <- which(y >= 5.5)
length(A)

#d3
gn<-data.frame(Grupo=grupo,Notas=nota)
head(gn)
n_ab<-(gn[gn$Grupo=="B"& gn$Notas>=5.5, ])
nrow(n_ab)

#d4
which( grupo == "B" )
notasB <- nota [which( grupo == "B" )] # notas de los alumnos del grupo B
which(notasB >= 5.5)
length(which(notasB >= 5.5))

#e
nc <- filter(df02, grupo == "C", nota >= 5.5)
nrow(nc)
table(grupo == "C")
(20/39)*100 #51.28%

#e3
which( grupo == "C" )
notasC <- nota [which( grupo == "C" )] # notas de los alumnos del grupo C
which(notasB >= 5.5)
NaprobadosC <- length(which(notasB >= 5.5)) # NÂº alumnos C aprobados
TotalC <- length(which( grupo == "C") ) # NÂº total alumnos C
porc <- (aprobadosC/TotalC)*100
print(porc)

#observacion----

#f
Y<- nota
max(Y) -> Z
Z
min(Y) -> R
R
A <- grupo [which( nota == Z )]
A
B <- grupo [which( nota == R )]
B

#f2 observacion----
df02[max(df02$nota),]
df02[min(df02$nota),]

#g ¿?----
GA <- filter(df02, grupo == "A", nota >= 5.5)
length(GA)
GB <- filter(df02, grupo == "B", nota >= 5.5)
length(GB)

mean(GB$nota) 
mean(GA$nota)

#g2
g_a<-gn[gn$Grupo==c("A","B") & gn$Notas>=5.5, ]
mean(g_a$Notas)

#g3
notasA <- nota [which(grupo == "A")] # notas de los alumnos del grupo A
AprobadasA <- notasA[which(notasA >= 5.5)] # Notas aprobadas de A

notasB <- nota [which(grupo == "B")] # notas de los alumnos del grupo B
AprobadasB <- notasB[which(notasB >= 5.5)] # Notas aprobadas de B

c <- c(AprobadasA, AprobadasB)

mean(c)

#g4
y <- nota [which( grupo == "A" )]
y
A <- which(y >= 5.5)
A
a <- y[c(A)] 
sum(a) ->Y
Y
length(A) -> M
M

x <- nota [which( grupo == "B" )]
x
B <- which(x >= 5.5)
B
b <- x[c(B)] 
sum(b) ->X
X
length(B) -> N
N
((X + Y)/(M + N))

# 6. Calcula el percentil 66 de las notas de todos----
#s alumnos, y también de los alumnos del grupo C.

#a Todos los alumnos
perc <- df$nota 
quantile(perc, .66)

#b Aumnos del grupo C

GC <- filter(df02, grupo == "C")
GC
perc2 <- GC$nota
quantile(perc2, .66)

#6.2 Calc
Y<-c(nota)
Y

quantile(Y, .66)

x <- nota [which( grupo == "C" )]
x

quantile(x, .66)
#7. ----

c = 4.9 

table (nota <= c )
(c/90)*100

table (nota >= c )
(c/108)*100

#7.2

x<-c(nota)
A <- which(x <= 4.9)
C <- length(A)
A
B <- length(nota)
(C*100/B)

x<-c(nota)
A <- which(x >= 4.9)
C <- length(A)
A
B <- length(nota)
(C*100/B)

#7.3 observacion----
menor<-(sum(gn$Notas<=4.9)/sum(gn$Notas))*100
mayor<-(sum(gn$Notas>=4.9)/sum(gn$Notas))*100

#8 Hacer los gráficos

GA2 <- filter(df02, grupo == "A")
GB2 <- filter(df02, grupo == "B")
GC2 <- filter(df02, grupo == "C")
GD2 <- filter(df02, grupo == "D")
GE2 <- filter(df02, grupo == "E")

boxplot(GA2$nota)
boxplot(GB2$nota)
boxplot(GC2$nota)
boxplot(GD2$nota)
boxplot(GE2$nota)

#8.2

N <- c(nota)
N
A <- nota [which( grupo == "A" )]
A
B <- nota [which( grupo == "B" )]
B
C <- nota [which( grupo == "C" )]
C
D <- nota [which( grupo == "D" )]
D
E <- nota [which( grupo == "E" )]
E
boxplot (A, B, C, D, E, main = "NOTAS DE CADA GRUPO",
         xlab = "GRUPOS", ylab = "NOTAS",
         col = c("orange3", "yellow3", "green3", "grey", "yellow3"))

#8.2
boxplot(`Notas` ~ `Grupo`,gn,col = palette(rainbow(2)))

#9. Si la variable conc recoge la concentración----
#de plomo (en ppm) en el aire de cierta zona durante un día completo

#a
max(conc)

#b
conc > 40.0
table(conc > 40.0)

#c
mean(conc) #Los datos mostrados son por dias?

#d
m<-sort(conc)
head(m<-sort(conc), 10)

#e CERAS----
Y<-c(conc)
Z <- max(Y)
A <-which(Y == Z)
A

B <- length(Y)
B
# UN DIA EQUIVALE A 24 HORAS

X <-(24*A)/B
X

#e #47.34 MILI---- 
hora<-seq(as.POSIXct("00:00", format="%H:%M"),
          as.POSIXct("23:59", format="%H:%M"), by="5 min")
h_conc<-data.frame(hora=hora,Medida=conc)
h_conc[max(h_conc$Medida),]

h_max<-h_conc %>% select(Medida) %>% summarise(max(Medida))

