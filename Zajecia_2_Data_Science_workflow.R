
#1) Import ----
kraje_1 = read.table("kraje_makro_1.csv", header=TRUE, sep=",", dec=".")
kraje_2 = read.table("kraje_makro_2.csv", header=TRUE, sep=",", dec=".")


#2) Przygotowanie danych ----
#2.1 Podgląd danych ----

# Pierwsze/ostatnie wiersze
head(kraje_1)	# pierwsze 6 wierszy (obserwacji)
head(kraje_2)      

head(kraje_1, 10)	# pierwsze 10 wierszy (obserwacji)
head(kraje_2, 10)

tail(kraje_1, 5)	# ostatnie 5 wierszy (obserwacji)
tail(kraje_2, 5)


# Podstawowe statystyki wszystkich kolumn (zmiennych)
summary(kraje_1)	# min, max, średnia, mediana, kwantyle
summary(kraje_2)

# Statystyki pojedynczej kolumny (zmiennej)
mean(kraje_1$Przyrost_populacji)		# średnia
median(kraje_1$Przyrost_populacji)	# mediana
min(kraje_1$Przyrost_populacji)		# minimum
max(kraje_1$Przyrost_populacji)		# maksimum


#2.2 Porządkowanie nazw kolumn (zmiennych) ----

# Usuwanie zbędnej kolumny
kraje_1$X = NULL
kraje_2$X = NULL

# Zmiana nazw kolumn z angielskich na polskie
colnames(kraje_2) = c("Kod_kraju", "Nazwa", "Region", "Urbanizacja_proc.", "Internet_proc.")


#2.3 Porządkowanie typów danych ----

# W ramce danych kraje_2 sprawdź typ zmiennej Region 
is.numeric(kraje_2$Region) 	# czy zmienna jest liczbowa? Odp. Nie.
is.character(kraje_2$Region) 	# czy zmienna jest tekstowa? Odp. Tak.

# Region to zmienna kategorialna, więc nadajemy jej typ factor:
kraje_2$Region = as.factor(kraje_2$Region)

# Sprawdzenie kategorii:
summary(kraje_2)
levels(kraje_2$Region)

# Teraz widać, że jest 7 kategorii regionów, na których operuje zmienna Region.


#2.4 Porządkowanie braków danych ----

# Szybka kontrola braków danych we wszystkich kolumnach:
colSums(is.na(kraje_1))	# nie ma braków danych
colSums(is.na(kraje_2))	# są 4 braki danych w kolumnie (zmiennej) Internet_proc.

# Liczba braków w konkretnej kolumnie:
sum(is.na(kraje_2$Internet_proc.)) 	# 4 braki


# Zobaczmy te 4 wiersze, w których brakuje wartości:
kraje_2[is.na(kraje_2$Internet_proc.), ]


# Braki danych są częścią rzeczywistości ekonomisty, dlatego trzeba umieć je obsłużyć i # podjąć decyzję analityczną:
# OPCJA 1 - Pozostawić (teraz tak postąpimy)
# OPCJA 2 - Usunąć obserwacje z brakami (czy usunięcie tych obserwacji zmieni analizę?)
# OPCJA 3 - Uzupełnić braki (np. imputacja medianą)


#2.5 Czyszczenie danych ----
# W ramce danych kraje_2, w kolumnie Region są kategorie, w których nazwie jest znak &:
levels(kraje_2$Region)
# [1] "East Asia & Pacific"       "Europe & Central Asia"    
# [3] "Latin America & Caribbean" "Middle East & North Africa"
# [5] "North America"             "South Asia"               
# [7] "Sub-Saharan Africa"

# Znak & bywa problematyczny przy dalszym przetwarzaniu, dlatego zastąp go słownym spójnikiem "and".
# Funkcja gsub() działa jak "Znajdź i zamień" (Ctrl+H) w Excelu. 
# Zamienia wszystkie wystąpienia tekstu na inny tekst
# Przykładowo: gsub("stary_tekst", "nowy_tekst", ramka$kolumna)

# W naszym przypadku wykonamy następujący kod:
kraje_2$Region <- gsub("&", "and", kraje_2$Region)

# Sprawdzenie (po zamianie ponownie ustawiamy typ factor):
kraje_2$Region = as.factor(kraje_2$Region)
levels(kraje_2$Region)

#   Łączenie (scalanie) ramek danych kraje_1 i kraje_2
kraje = merge(kraje_1, kraje_2, by.x="Kod", by.y="Kod_kraju")


# Usuwanie zbędnej kolumny po połączeniu
kraje$Nazwa = NULL


# Zobacz ramkę danych po scaleniu
summary(kraje)
str(kraje)

#3) Podstawowa analiza danych ----

#3.1 mutate() – tworzenie nowych zmiennych na bazie istniejących ----
#install.packages("dplyr")
library(dplyr)


# Tworzenie nowej zmiennej Populacja_w_mln w dplyr:
kraje = kraje %>%
  mutate(Populacja_mln = Populacja / 1e6)

# Równoważny kod w base R:
kraje$Populacja_mln = kraje$Populacja / 1e6


# 1e6 to zapis miliona w R (1 razy 10 do potęgi 6)
# 1e9  = 1 000 000 000 (miliard)
# 1e12 = 1 000 000 000 000 (bilion)


# Tworzenie nowej zmiennej PKB_per_capita w dplyr:
kraje = kraje %>%
  mutate(PKB_per_capita = PKB / Populacja)

# Równoważny kod w base R:
kraje$PKB_per_capita = kraje$PKB / kraje$Populacja


#3.2 filter() – wybieranie wierszy ----
#3.3 select() – wybieranie kolumn ----

# Wyświetl kraje, w których % poziom urbanizacji jest większy niż 50
kraje %>%
  filter(Urbanizacja_proc. > 50)

# Równoważny kod w base R:
kraje[kraje$Urbanizacja_proc. > 50, ]


# Wyświetl tylko dane pokazujące zmienne Panstwo, Region, PKB, Populacja_mln
kraje %>%
  select(Panstwo, Region, PKB, Populacja_mln)

# Równoważny kod w base R:
kraje[, c("Panstwo", "Region", "PKB", "Populacja_mln")]

#3.4 arrange() – sortowanie ----

# Posortuj kraje według przyrostu populacji rosnąco
kraje %>%
  arrange(Przyrost_populacji)


# Posortuj kraje według przyrostu populacji malejąco
kraje %>%
  arrange(desc(Przyrost_populacji))

# Równoważny kod w base R:
kraje[order(kraje$Przyrost_populacji), ]  # rosnąco
kraje[order(kraje$Przyrost_populacji, decreasing = TRUE), ]  # malejąco


# Wybierz kraje z PKB większym niż 1 bilion, posortuj je rosnąco względem PKB 
# i wyświetl nazwę państwa, PKB i PKB per capita. Ile jest takich krajów?
kraje %>%
  filter(PKB > 1e12) %>%
  arrange(PKB) %>%
  select(Panstwo, PKB, PKB_per_capita)


# Równoważny kod w base R:

# Krok 1: Filtrowanie
kraje_filtr = kraje[kraje$PKB > 1e12, ]

# Krok 2: Sortowanie
kraje_sort = kraje_filtr[order(kraje_filtr$PKB), ]

# Krok 3: Wybór kolumn
kraje_sort[, c("Panstwo", "PKB", "PKB_per_capita")]

# Wniosek: dplyr jest bardziej czytelny przy wielu operacjach.



# Wybierz kraje z regionu Afryki Subsaharyjskiej, 
# wybierz zmienne Panstwo, PKB_per_capita, Populacja_mln, Urbanizacja,
# a następnie posortuj malejąco po PKB per capita
kraje %>%
  filter(Region == "Sub-Saharan Africa") %>%
  select(Panstwo, PKB_per_capita, Populacja_mln, Urbanizacja_proc.) %>%
  arrange(desc(PKB_per_capita))


# Równoważny kod w base R:
# Krok 1: Filtrowanie i wybór kolumn
kraje_reg = kraje[kraje$Region == "Sub-Saharan Africa", c("Panstwo", "PKB_per_capita", "Populacja_mln", "Urbanizacja_proc.")]

# Krok 2: Sortowanie
kraje_reg[order(kraje_reg$PKB_per_capita, decreasing = TRUE), ]


#3.5 group_by() – grupowanie ----
#3.6 summarise() - obliczanie wartości zagregowanych (np. średnich, sum) ----

# Wyświetl tylko te kraje, które są bogatsze niż średnia regionu
bogate = kraje %>%
  group_by(Region) %>%
  filter(PKB_per_capita > mean(PKB_per_capita, na.rm = TRUE))



# Równoważny kod w base R:

bogate = kraje[kraje$PKB_per_capita > ave(kraje$PKB_per_capita, kraje$Region, 
                                          FUN = mean, na.rm = TRUE), ]

# ave() liczy średnią wewnątrz grup i zwraca wektor tej samej długości co dane.



# Znajdź największą wartość PKB per capita w całym zbiorze krajów
kraje %>%
  summarise(max_PKB_per_capita = max(PKB_per_capita, na.rm = TRUE))


# Równoważny kod w base R:

max(kraje$PKB_per_capita, na.rm = TRUE)



# Znajdź największą i najmniejszą wartość Populacji w mln w całym zbiorze krajów
kraje %>%
  summarise(
    min_populacja = min(Populacja_mln, na.rm = TRUE),
    max_populacja = max(Populacja_mln, na.rm = TRUE))


# Równoważny kod w base R:
min(kraje$Populacja_mln, na.rm = TRUE)
max(kraje$Populacja_mln, na.rm = TRUE)


# Oblicz średnią populację w całym zbiorze krajów (jedna liczba dla całej ramki)
kraje %>%
  summarise(srednia_populacja = mean(Populacja_mln, na.rm = TRUE))

# Równoważny kod w base R:
mean(kraje$Populacja_mln, na.rm = TRUE)

# Ile krajów jest w całym zbiorze danych?
kraje %>%
  summarise(liczba_krajow = n())


# Równoważny kod w base R:

nrow(kraje)



# Policz, ile krajów jest w każdym regionie
kraje %>%
  group_by(Region) %>%
  summarise(liczba_krajow = n())


# Równoważny kod w base R:

table(kraje$Region)



# Dla każdego regionu świata: oblicz liczbę krajów (n), średni % dostęp do internetu i średni % poziom urbanizacji, a następnie posortuj regiony malejąco wg średniego % dostępu do internetu
kraje %>%
  group_by(Region) %>%
  summarise(
    liczba_krajow = n(),
    sredni_internet = mean(Internet_proc., na.rm = TRUE),
    srednia_urbanizacja = mean(Urbanizacja_proc., na.rm = TRUE)
  ) %>%
  arrange(desc(sredni_internet))


# Równoważny kod w base R:
{
  wynik = aggregate(cbind(Internet_proc., Urbanizacja_proc.) ~ Region,
                    kraje, mean, na.rm = TRUE)
  wynik$liczba_krajow = as.vector(table(kraje$Region)[wynik$Region])
  colnames(wynik) = c("Region", "sredni_internet", "srednia_urbanizacja", "liczba_krajow")
  wynik[order(-wynik$sredni_internet), ]
  }


# UWAGA!
# Wszystkie zaprezentowane działania da się zrobić w base R (czystym R bez pakietów), 
# ale w wielu przykładach użycie funkcji z pakietu dplyr jest bardziej czytelne i szybsze.

# Wizualizacja danych także pozwala zidentyfikować wzorce i zależności w zbiorze danych.

#4) Wizualizacja ----

#install.packages("ggplot2", type = "binary")
library(ggplot2)



# 4.1. Prosty wykres punktowy: urbanizacja a PKB per capita ----
ggplot(kraje, aes(x = Urbanizacja_proc., y = PKB_per_capita)) +
  geom_point() +
  labs(
    title = "Urbanizacja a PKB per capita",
    x = "Urbanizacja (%)",
    y = "PKB per capita")



# 4.2. Zaawansowany wykres punktowy: urbanizacja a PKB per capita ----
ggplot(kraje, aes(x = Urbanizacja_proc., y = PKB_per_capita, color = Region)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Urbanizacja a PKB per capita",
    subtitle = "Czy bardziej zurbanizowane kraje są bogatsze?",
    x = "Urbanizacja (% ludności miejskiej)",
    y = "PKB per capita (USD, skala log)",
    color = "Region świata"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom")



# 4.3. Zaawansowany wykres punktowy: rozmiar gospodarki a populacja ----

ggplot(kraje, aes(x = Populacja_mln, y = PKB, size = PKB_per_capita, color = Region)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Skala gospodarki i demografia",
    x = "Populacja (mln, log10)",
    y = "PKB (USD, log10)",
    size = "PKB per capita"
  ) +
  theme_minimal()



# 4.4. Prosty wykres słupkowy: liczba krajów w regionach ----
ggplot(kraje, aes(x = Region)) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(
    title = "Liczba krajów w regionach świata",
    x = "Region",
    y = "Liczba krajów"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5))


# 4.5. Zaawansowany wykres słupkowy poziomy: TOP 15 najbogatszych krajów ----
kraje %>%
  arrange(desc(PKB_per_capita)) %>%
  head(15) %>%
  ggplot(aes(x = reorder(Panstwo, PKB_per_capita), y = PKB_per_capita, fill = Region)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "TOP 15 najbogatszych krajów świata (2016)",
    subtitle = "PKB per capita w USD",
    x = NULL,
    y = "PKB per capita (USD)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10))


# 4.6. Wykres pudełkowy (boxplot): dostęp do internetu według regionów ----
ggplot(kraje, aes(x = reorder(Region, Internet_proc., FUN = median), 
                  y = Internet_proc., fill = Region)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  coord_flip() +
  labs(
    title = "Dostęp do internetu według regionów świata",
    subtitle = "(punkty to poszczególne kraje)",
    x = NULL,
    y = "Dostęp do internetu (% populacji)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none")
# 4.7. Wykres pudełkowy (boxplot): przyrost populacji według regionów ----
# (mediana, rozrzut i obserwacje odstające)
ggplot(kraje, aes(x = Region, y = Przyrost_populacji)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_jitter(width = 0.15, alpha = 0.5) +
  coord_flip() +
  labs(
    title = "Tempo przyrostu populacji w regionach świata",
    subtitle = "(punkty to poszczególne kraje, linia przerywana = 0%)",
    x = "Region",
    y = "Przyrost populacji (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14))

# Zapisanie ramki danych do pliku CSV
write.csv(kraje, "kraje_analiza.csv") 



# Zapisanie ramki danych do pliku Excel wymaga pakietu writexl:
#install.packages("writexl")
library(writexl)

write_xlsx(kraje, "kraje_wynik.xlsx")



#5) Eksport ----

# Zapisz wszystkie wykresy – prawe dolne okno, zakładka Plots:
# Export -> Save as image

# Niestety każdy wykres trzeba zapisać ręcznie
# nie ma funkcji do masowego eksportu wykresów.
