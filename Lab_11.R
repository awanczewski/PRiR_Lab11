library(ggplot2)
library(PBImisc)
library(PogromcyDanych)
install.packages("data.table")
library(data.table)
library(dplyr)
#Zadanie 1
#a) SprawdŸ (przedstaw graficznie) jak zmienia³a siê w czasie funkcja nerki po przeszczepie mierzona w MDRD, jakie czynniki wp³ywa³y na sprawnoœæ nerki?

days <- sub("MDRD", "", colnames(kidney)[9: 16])
means <- colMeans(kidney[9: 16])
data <- data.table(name = days, value = means)
ggplot(data, aes(x = days, y = means)) + geom_bar(stat = "identity", na.rm = TRUE)

#b) Dla zbioru eden, nale¿y pokazaæ zró¿nicowanie poziomu objawów psychopatologicznych (obszary:
# mania, depresja, symptomy pozytywne i negatywne), na wykresie przedstaw jakie czynniki wp³ywaj¹ na
# poziom objawów psychopatologicznych.

data = data.frame(value = eden$BPRS.Maniac)
ggplot(data, aes(x = value)) + geom_histogram() + ggtitle("Obszar: mania")

data = data.frame(value = eden$BPRS.Negative)
ggplot(data, aes(x = value)) + geom_histogram() + ggtitle("Obszar: symptomy negatywne")

data = data.frame(value = eden$BPRS.Positive)
ggplot(data, aes(x = value)) + geom_histogram() + ggtitle("Obszar: symptomy pozytywne")

data = data.frame(value = eden$BPRS.Depression) 
ggplot(data, aes(x = value)) + geom_histogram() + ggtitle("Obszar: depresja")


#Zadanie 2

#1)dane dla grupy kobiet, pogrupuj wzglêdem markera TP53 znajdŸ pacjentki które mia³y badanie
#wczeœniej ni¿ 700 dni przed œmierci¹ (tzw. kliniczny punkt koñcowy), uzyskany zbiór zapisz do pliku

my_TCGA_BRCA <- TCGA_BRCA %>% filter(plec == 'female', dni.do.smierci > 700) %>% group_by(TP53)
write.table(my_TCGA_BRCA, file = "zad2.txt")

#2) sprawdŸ czy w kolumnie dni.do.smierci brakuje danych wykorzystaj funkcjê is.na()
#3) oblicz ile jest takich brakuj¹cych danych w/w kolumnie

table(is.na(TCGA_BRCA$dni.do.smierci))

#4) wszystkie brakuj¹ce dane zast¹p liczb¹ 0, wykorzystaj w tym celu funkcje gsub() jeœli NA jest zmienn¹
#typu String jeœli nie to zastosuj maskê i funkcjê is.na (patrz przyk³ad poni¿ej)

my_TCGA_BRCA <- TCGA_BRCA
my_TCGA_BRCA$dni.do.smierci[is.na(my_TCGA_BRCA$dni.do.smierci)] <- 0

#5) nastêpnie podstaw nowe wartoœci w miejsce poprzednich korzystaj¹c z funkcji mutate() w kolumnie
#dni.do.smierci

#JUZ ZROBIONE SPOSOBEM WYZEJ


#Zadanie 3


#a) z pliku seria1.txt dla ka¿dego pacjenta (identyfikator to !Series_sample_id) (1 wiersz 1 pacjent) w
#kolejnych kolumnach wyœwietl nastêpuj¹ce informacje: p³eæ (F lub M), wiek pacjenta (>18 months
#lub <18 months), stadium nowotworu (Stage: 3)


Series_sample_id <- read.table("seria1.txt", sep="",dec=".", fill = TRUE) %>% filter(V1 == "!Series_sample_id")
ids <- data.frame(strsplit(Series_sample_id$V2, " "))

seria1 <- read.table("seria1.txt", sep="" , dec=".", fill = TRUE, skip = 39)

characteristics <- seria1 %>% filter(V1 == "!Sample_characteristics_ch1")
genders <- data.frame(t(characteristics[1,2:length(characteristics)])) %>% sapply(substring, 6, 7)
ages <- data.frame(t(characteristics[4,2:length(characteristics)])) %>% sapply(substring, 37, 38)
stages <- data.frame(t(characteristics[3,2:length(characteristics)]))

for (i in 1:nrow(ids[1])){
  if(ages[i, 1] == "1"){
    ages[i, 1] <- "< 18 miesiecy"
  } else {
    ages[i, 1] <- "> 18 miesiecy"
  }
  print(paste("ID Pacjenta:", ids[i,1], "Plec:", genders[i,1], "Wiek:", ages[i, 1], stages[i,1]))
}

### Nie wiedzialem czy trzeba wypisac czy zrobic z tego tabele wiec zrobilem na dwa sposoby

patients <- data.frame(as.vector(ids), as.vector(genders), as.vector(ages), as.vector(stages))
colnames(patients) <- c("id", "gender", "age", "stage")

#b) z plików seria1.txt i seria2.txt wydziel dane tabelaryczne:
  
data1 <- read.table("seria1.txt", sep="", header=TRUE, skip=74,
                   dec=".", fill = TRUE)

data2 <- read.table("seria2.txt", sep="", header=TRUE, skip=89,
                             dec=".", fill = TRUE)

data_merged <- merge(data1, data2, by="ID_REF")





