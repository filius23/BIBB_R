# ------------------- #
# Kapitel 1: Einstieg
# ------------------- #


# Taschenrechner -----

2+5
3-4
5*6
7/8

2+3*2
(2+3)*2

4^2 ## 4²
sqrt(4) ## Wurzel
exp(1) ## Exponentialfunktion (Eulersche Zahl)
log(5) ## Natürlicher Logarithmus
log(exp(5)) ## log und exp heben sich gegenseitig auf

# Zahlenreihen -------
2:6 # 2  bis 6
seq(2,11,3) # Schritte angeben

# Objekte  -----------
x <- 4/2
x


y <- x * 5
y

# Objekte mit mehreren Elementen ---------
x1 <- c(1,2,3)
x1

x2 <- c(3:9)
x2

length(x2)

x1 * 2

# mit Objekten rechnen -----------------
y1 <- c(10,11,9)

y1
x1

y1/x1

# löschen --------------
rm( x1 )
x1


rm(list = ls())

# wie funktioniert rm(list = ls()) ?
  # rm() # "remove" Befehl --> wir wollen etwas löschen
  ls() # 'liste' alle Objekte
  ls(pattern = "x") # 'liste' alle Objekte mit x im Namen
  
  rm(list = ls()) # also: remove alle Objekte, die ls() findet

# Kommentare -------------
2+ 5 # hier steht ein Kommentar

2+ # auch hier kann ein Kommentar stehen
  5

( 2 + # ein
    3) * # kommentar
  2 # über mehrere Zeilen



# Überschrift 1 ----

## Abschnit 1.1 ----
3+2*4
3+2*3
## Abschnit 1.2 ----
3+2*sqrt(3)

# Überschrift 2 ------
x <- c(2,6,8,2,35)
y <- seq(2,10,2)

y/x


