# MOW
Generacja drzewa decyzyjnego za pomocą algorytmu genetycznego
Tworzenie drzewa decyzyjnego przy pomocy algorytmu ewolucyjnego.
Zwykle klasyfikatory budowane są w oparciu o metodę zachłanną - w kolejnym kroku wybieramy lokalnie najlepszy podział.
Takie podejście jest bardzo szybkie jednak nie zawsze prowadzi do utworzenia optymalnej struktury drzewa.
Budowa drzewa będzie przebiegała od korzenia. W standardowym algorytmie budowy drzewa oceniamy kolejne węzły dodawane do drzewa natomiast w tworzonym przez nas algorytmie będziemy oceniać drzewo jako całość.
 
Algorytm kończy pracę w momencie, gdy przez wyznaczoną liczbę iteracji algorytmu, np. 50, nie zostanie znalezione lepsze drzewo, czy znalezione drzewo będzie lepsze ale w bardzo niewielkim stopniu.
Do badań wykorzystamy dane ze strony: https://archive.ics.uci.edu/ml/index.php.
Rozważamy użycie danych dotyczących oceny jakości wina na podstawie jego właściwości fizyczno-chemicznych (https://archive.ics.uci.edu/ml/datasets/Wine+Quality).
