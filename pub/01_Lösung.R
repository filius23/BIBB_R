# ------------------- #
# Kapitel 1: Erste Schritte
# Lösung
# ------------------- #



# Legen Sie die Anzahl der Studierenden an der Uni Oldenburg (15643) unter stud ab. ---------
stud <- 15643
# Legen Sie die Anzahl der Professuren an der Uni Oldenburg (210) unter prof ab. ---------
prof <- 210

# Berechnen Sie die Anzahl der Studierenden pro Professur an der Uni Oldenburg indem Sie die Objekte stud und prof verwenden. ---------
stud/prof

# Legen Sie das Ergebnis unter studprof ab und rufen Sie das das Objekt noch einmal auf! ---------
stud_prof <- stud/prof

stud_prof

# Sehen Sie die erstellten Variablen im Environment-Fenster? ---------


# Legen Sie die Studierendenzahlen der Uni Bremen (19173), Uni Vechta (5333) und Uni Oldenburg (15643) zusammen unter studs ab. ---------
studs <- c(19173, 5333, 15643)

# Legen Sie die Zahl der Profs der Uni Bremen (322), Uni Vechta (67) und Uni Oldenburg (210) zusammen unter profs ab. ---------
profs <- c(322,67,210)

# Berechnen die Anzahl der Studierenden pro Professur für alle drei Universitäten. ---------
studs/profs

# Sie möchten zusätzlich die Zahl der Studierenden (14000) und Professuren (217) der Uni Osnabrück in studs und profs ablegen. Wie gehen Sie vor? ---------

studs1 <- c(19173, 5333, 15643,14000)
profs1 <- c(322, 67, 210, 217)

studs2 <- c(studs,14000)
profs2 <- c(profs,217)

# profs <- c(profs,217) # wächst ins unendliche
# profs 

# Berechnen Sie für alle vier Universitäten das Verhältnis von Studierenden und Professuren! ---------
studs2/profs2

# Löschen Sie das Objekt stud. Woran erkennen Sie, dass das funktioniert hat? ---------
rm(stud)

# Löschen Sie alle Objekte aus dem Environment. Woran erkennen Sie, dass das funktioniert hat? ---------
rm(list=ls())  



# dies ist eine Überschrift ----
# dies ist keine Überschrift weil nur 3 striche --- 
