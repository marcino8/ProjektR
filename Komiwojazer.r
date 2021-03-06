liczba_miast<- 18

Macierz_odleglosci<-matrix(1:324 ,nrow = liczba_miast ,ncol=liczba_miast)
rownames(Macierz_odleglosci)<-c("Bialystok","Bydgoszcz","Gdańsk","Gorzów Wielkopolski","Katowice","Kielce","Kraków","Lublin","Łódz","Olsztyn","Opole","Poznań","Rzeszów","Szczecin","Toruń","Warszawa","Wroclaw","Zielona Góra")
colnames(Macierz_odleglosci)<-c("Bialystok","Bydgoszcz","Gdańsk","Gorzów Wielkopolski","Katowice","Kielce","Kraków","Lublin","Łódz","Olsztyn","Opole","Poznań","Rzeszów","Szczecin","Toruń","Warszawa","Wroclaw","Zielona Góra")

#Reczne wprowadzenie macierzy odleglosci
Macierz_odleglosci[,1]=c(0,398,387,622,487,373,470,261,327,142,502,480,422,649,353,191,541,600)
Macierz_odleglosci[,2]=c(398,0,170,224,378,349,452,419,203,201,323,129,523,260,45,257,265,261)
Macierz_odleglosci[,3]=c(387,170,0,318,525,470,584,509,330,165,514,304,655,318,179,346,481,415)
Macierz_odleglosci[,4]=c(622,224,318,0,454,489,525,586,353,431,351,133,663,103,286,437,264,105)
Macierz_odleglosci[,5]=c(487,378,525,454,0,164,75,326,197,474,112,321,237,557,353,294,197,353)
Macierz_odleglosci[,6]=c(373,349,470,489,164,0,122,177,139,400,224,347,174,583,302,180,311,452)
Macierz_odleglosci[,7]=c(470,452,584,525,75,122,0,253,257,492,169,403,166,616,380,299,256,473)
Macierz_odleglosci[,8]=c(261,419,509,586,326,177,253,0,262,365,380,453,271,677,370,226,424,541)
Macierz_odleglosci[,9]=c(327,203,330,353,197,139,257,262,0,368,184,205,313,441,156,134,207,302)
Macierz_odleglosci[,10]=c(142,201,165,431,474,400,492,365,368,0,453,328,313,459,181,220,445,467)
Macierz_odleglosci[,11]=c(502,323,514,351,112,224,169,380,184,453,0,328,489,454,302,310,85,241)
Macierz_odleglosci[,12]=c(480,129,304,133,321,347,403,453,205,328,328,0,513,236,147,304,173,132)
Macierz_odleglosci[,13]=c(422,523,655,663,237,174,166,271,313,313,489,513,0,749,468,302,418,626)
Macierz_odleglosci[,14]=c(649,260,318,103,557,583,616,677,441,459,454,236,749,0,308,517,367,215)
Macierz_odleglosci[,15]=c(353,45,179,286,353,302,380,370,156,181,302,147,468,308,0,209,280,279)
Macierz_odleglosci[,16]=c(191,257,346,437,294,180,299,226,134,220,310,304,302,517,209,0,341,413)
Macierz_odleglosci[,17]=c(541,265,481,197,311,256,424,207,445,85,173,418,367,280,209,341,0,156)
Macierz_odleglosci[,18]=c(600,261,415,105,353,452,473,541,302,467,241,132,626,215,279,413,156,0)

#Wprowadzenie koordynatow do dzialania markerow na mapie i oblicznia odleglosci
city_coords<-matrix(1:72, ncol=4, byrow=TRUE)
colnames(city_coords)<-c("Miasto","X","Y", "ID")
city_coords[,1] <- c("Bialystok","Bydgoszcz","Gdańsk","Gorzów Wielkopolski","Katowice","Kielce","Kraków","Lublin","Łódz","Olsztyn","Opole",  "Poznań", "Rzeszów","Szczecin","Toruń","Warszawa","Wroclaw","Zielona Góra")
city_coords[,2] <- c(23.0844,     18.00,      18.667,    15.25 ,               19.023,  20.6167, 19.944,  22.552,  19.448, 20.46,   17.911,   16.921,   22.01,    14.546,    18.61,   21.014,     17.039,     15.50)
city_coords[,3] <- c(53.0807,     53.15,      54.35,     52.733,               50.264,  50.8833, 50.057,  51.252,  51.76,  53.77,   50.674,   52.405,   50.03,    53.441,    53.018,  52.241,     51.105,     51.94)
city_coords[,4] <- 1:18
city_coords <-as.data.frame(city_coords)
city_coords$X<-as.numeric(city_coords$X)
city_coords$Y<-as.numeric(city_coords$Y)
city_coords$ID<-as.numeric(city_coords$ID)
library("ompr")
library("knitr")
library("dplyr")
library(ggplot2)

#Funkcja obliczjakaca dystans miedzy miastami, korzystajaca 
#z recznie wprowadzonej macierzy odleglosci miedzy miastami
dist_fun <- function(i, j) {
  vapply(seq_along(i), function(k) Macierz_odleglosci[i[k], j[k]], numeric(1L))
}

#Model rozwiazanie problemu komiwojażera
model <- MIPModel() %>%
  #Stworzenie zmiennej o wartoci 1 dla podrozy z miasta i do j
  add_variable(x[i, j], i = 1:liczba_miast, j = 1:liczba_miast, 
               type = "integer", lb = 0, ub = 1) %>%
  
  #zmienna pomocnicza
  add_variable(u[i], i = 1:liczba_miast, lb = 1, ub = liczba_miast) %>% 
  
  #Minimalizacja odleglosci
  set_objective(sum_expr(dist_fun(i, j) * x[i, j], i = 1:liczba_miast, j = 1:liczba_miast), "min") %>%
  
  #Dodajemy warunki
  #Nie mozna przejechac przez miasto dwa rady
  set_bounds(x[i, i], ub = 0, i = 1:liczba_miast) %>%
  
  # Trzeba opuscic kazde miasto
  add_constraint(sum_expr(x[i, j], j = 1:liczba_miast) == 1, i = 1:liczba_miast) %>%
  #
  #Trzeba byc w kazdym miescie
  add_constraint(sum_expr(x[i, j], i = 1:liczba_miast) == 1, j = 1:liczba_miast) %>%
  
  #Brak mozliwosci tworzenia "drogi na skroty"
  add_constraint(u[i] >= 2, i = 2:liczba_miast) %>% 
  add_constraint(u[i] - u[j] + 1 >= (liczba_miast - 1) * (1 - x[i, j]), i = 2:liczba_miast, j = 2:liczba_miast)
model

#Obliczenie macierzy odleglosci z podanych wspolrzednych geograficznych
Odleglosc <- as.matrix(stats::dist(select(city_coords, X, Y), diag = TRUE, upper = TRUE))

library("ompr.roi")
library("ROI.plugin.glpk")


#Przykladowe rozwiazanie, test dzialania
result <- solve_model(model,   with_ROI(solver = "glpk", verbose = TRUE))
solution <- get_solution(result, x[i, j]) %>% 
  filter(value > 0) 
kable(head(solution, 3))

         drogi <- select(solution, i, j) %>% 
  rename(from = i, to = j) %>% 
  mutate(id_podrozy=row_number()) %>% 
  tidyr::gather(property, idx_val, from:to) %>% 
  mutate(idx_val = as.integer(idx_val)) %>% 
  inner_join(city_coords, by = c("idx_val" = "ID"))
kable(head(arrange(drogi,id_podrozy), 4))


################################################

#SHINY SECTION
library(shiny)
library(leaflet)
library(RColorBrewer)
########################SHINY UI############################
ui <- fluidPage(
  titlePanel("Problem komiwojażera"),
  mainPanel(
    leafletOutput("map"),
    fluidRow(
      column(5,
             p("Wybierz miasta"),
             selectizeInput("cities", "MIASTA", city_coords$Miasto, multiple=TRUE, width="100%",
                            options = list(maxItems=18, maxOptions=100, placeholder="Wpisz aby wybrać",
                            selectOnTab=TRUE, openOnFocus=FALSE, hideSelected=TRUE)),
      )
    ),
  actionButton("btn2_click", label="Oblicz trase"),
  textOutput("txf")
)
)
###################SHINY SERVER################################
server <- function(input, output, session) {
  #Stworzenie mapy i dodanie na niej markerow odpowiadajacych miastom
  output$map <- renderLeaflet({
    leaflet(city_coords) %>% addTiles() %>%
      #ustawienie mapy na Polske przy starcie programu
      fitBounds(~min(18.012100), ~min(49.985842), ~max(20.846025), ~max(54.435947)) %>% 
      addMarkers(~X, ~Y, popup = ~as.character(Miasto), layerId = city_coords$ID, label=city_coords$Miasto)
      
  })
  #Obliczanie odleglosci i wypisywanie informacji zwrotnej uzytkownikowi
  observeEvent(input$btn2_click,{
    isolate({
      miasta <- subset(city_coords, Miasto %in% input$cities)
      #### W TYM MIEJSCU WYWOŁAĆ SOLVER OD miasta i wypisać niżej

      output$txf<-renderText({
        paste0("Najkrótsza odlegosc: ", )####TU DOPISAC WYNIK
      })
    })
  })
  
}
#Start aplikacji
shinyApp(ui, server)
