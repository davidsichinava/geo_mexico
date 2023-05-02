library(readxl)
library(tidyverse)
library(extrafont)
setwd("D:\\Dropbox (Personal)\\My projects\\scratchpad\\mexico")


download.file(
  "https://www.datatur.sectur.gob.mx/Documentos%20compartidos/DatosAbiertos_SIOM_NAC.zip",
  "mx_data.zip"
)


destfile <- tempfile()

unzip("mx_data.zip")

mx <- read_excel()

loadfonts(device = "win")

theme_ef <- function () { 
  theme_minimal(base_size=12) %+replace%
    theme(
      axis.title = element_blank(),
      title = element_blank(),
      # axis.text.x = element_blank(),
      # panel.grid = element_blank(),
      strip.text = element_text(family = "FiraGO", face="bold", size = 12),
      text = element_text(family= "FiraGO"),
      plot.title = element_text(size=14, face="bold", family="FiraGO", hjust = 0),
      plot.subtitle = element_text(size=12, family="FiraGO", hjust=0),
      axis.text = element_text(size=12, family="FiraGO", color = "black"),
      legend.position = "none"
    )
}


mx |> 
  select(Año, Mes, Nacionalidad, Entradas) |> 
  filter(Nacionalidad %in% c("Armenia", "Georgia", "Azerbaiyán")) |> 
  group_by(Año, Mes, Nacionalidad) |> 
  mutate(
    Mes = factor(Mes,
                 levels = c(
                    "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"
                          ), 
                 labels = c(
#                   "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"
                   "იან", "თებ", "მარ", "აპრ", "მაი", "ივნ", "ივლ", "აგვ", "სექ", "ოქტ", "ნოე", "დეკ"
                 )
    )
  ) |> 
  summarize(
    Entradas = sum(Entradas, na.rm = T)
  ) |> 
  # group_by(Año, Nacionalidad) |> 
  # summarize(
  #   Entradas = sum(Entradas, na.rm = T)
  # ) |> View()
  filter(Nacionalidad == "Georgia") |> 
  ggplot(
    aes(
      fct_rev(Mes), Entradas, label = Entradas, group = Nacionalidad, fill = Nacionalidad
    )
  )+
  geom_col()+
  geom_text(hjust = -1.01, nudge_x = 0.1, family="Lato", size=2, fontface = "bold")+
  ylim(0, 1000)+
  facet_wrap(~Año)+
  coord_flip()+
  scale_fill_manual(values = c("#1b998b"))+
  theme_ef()+
  labs(
    title = "საქართველოს მოქალაქეების მიერ საჰაერო გზით მექსიკის ტერიტორიაზე შესვლა",
    subtitle = "წყარო: DATATUR, მექსიკის მთავრობა"
  )+
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9)
  )

ggsave("geo_mx.png", device = "png", height=6, width=13)  

# http://www.politicamigratoria.gob.mx/es/PoliticaMigratoria/Cuadros_MyH?Anual=2022&Secc=1