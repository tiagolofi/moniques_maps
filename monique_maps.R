
#### PACOTES E DIRETORIOS ####

setwd('C:/Users/usuario/Documents/Ambientes/MONIQUE/moniques_maps')

require(magrittr)

#### CARREGANDO DADOS ####

municipality = geobr::read_municipality(21, 2018)
municipality$code_muni %<>% stringr::str_sub(1, 6) %>% as.numeric() 

dados = readxl::read_excel('PLANILHA PARA MAPAS.xlsx')

municipality %<>%
  dplyr::left_join(
    dados, by = c('code_muni' = 'CÓDIGO')
  )

#### BAIXANDO FONTES ####

showtextdb::font_install(showtextdb::google_fonts('Fira Sans'))
showtext::showtext_auto()

#### CRIANDO FAIXAS ####

municipality %<>% 
  dplyr::mutate(
    `Faixa EGS 2005` = factor(
      dplyr::case_when(
        dplyr::between(municipality$`EGS 2005`, 0.005, 0.91499999) ~ "0,005 a 0,914", 
        dplyr::between(municipality$`EGS 2005`, 0.9149999, 1.2199999) ~ "0,915 a 1,219",
        dplyr::between(municipality$`EGS 2005`, 1.2209999, 1.5599999) ~ "1,220 a 1,559", 
        dplyr::between(municipality$`EGS 2005`, 1.5609999, 1.9409999) ~ "1,560 a 1,940", 
        dplyr::between(municipality$`EGS 2005`, 1.9419999, 8.5) ~ "acima de 1,941",
        is.na(municipality$`EGS 2005`) ~ 'Sem info',
      ),
      levels = c(
        "Sem info",
        "0,005 a 0,914", 
        "0,915 a 1,219",
        "1,220 a 1,559", 
        "1,560 a 1,940", 
        "acima de 1,941"
      )
    ),
    `Faixa EGS 2015` = factor(
      dplyr::case_when(
        dplyr::between(municipality$`EGS 2005`, 0.005, 0.672999999) ~ "0,005 a 0,672", 
        dplyr::between(municipality$`EGS 2005`, 0.673999999, 0.858999999) ~ "0,673 a 0,858",
        dplyr::between(municipality$`EGS 2005`, 0.859999999, 1.1399999) ~ "0.859 a 1,139", 
        dplyr::between(municipality$`EGS 2005`, 1.1409999, 1.46199999) ~ "1,140 a 1,461", 
        dplyr::between(municipality$`EGS 2005`, 1.46299999, 289) ~ "acima de 1,462",
        is.na(municipality$`EGS 2005`) ~ 'Sem info',
      ),
      levels = c(
        "Sem info",
        "0,005 a 0,672", 
        "0,673 a 0,858",
        "0.859 a 1,139", 
        "1,140 a 1,461",  
        "acima de 1,462"
      )
    )
  )

#### CRIANDO MAPAS ####

gg1 = municipality %>% 
  ggplot2::ggplot()+
  ggplot2::geom_sf(ggplot2::aes(fill=`Faixa EGS 2005`), col='grey45')+
  ggplot2::scale_fill_brewer(
    palette='YlGnBu'
  )+
  ggplot2::theme_minimal()+
  ggplot2::theme(
    text = ggplot2::element_text(family='Fira Sans', size = 90, face='bold'),
    axis.text = ggplot2::element_blank(),
    legend.spacing.y = ggplot2::unit(0.5, 'cm'),
    legend.spacing.x = ggplot2::unit(0.25, 'cm'),
    panel.border = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(gg1, filename='egs2005.jpg', dpi=600, height = 10, width = 8, limitsize = FALSE)

gg2 = municipality %>% 
  ggplot2::ggplot()+
  ggplot2::geom_sf(ggplot2::aes(fill=`Faixa EGS 2015`), col='grey45')+
  ggplot2::scale_fill_brewer(
    palette='YlGnBu'
  )+
  ggplot2::theme_minimal()+
  ggplot2::theme(
    text = ggplot2::element_text(family='Fira Sans', size = 90, face='bold'),
    axis.text = ggplot2::element_blank(),
    legend.spacing.y = ggplot2::unit(0.5, 'cm'),
    legend.spacing.x = ggplot2::unit(0.25, 'cm'),
    panel.border = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(gg2, filename='egs2015.jpg', dpi=600, height = 10, width = 8, limitsize = FALSE)
