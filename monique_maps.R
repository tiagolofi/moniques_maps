
setwd('C:/Users/usuario/Documents/Ambientes/MONIQUE/moniques_maps')

require(magrittr)

municipality = geobr::read_municipality(21, 2018)
municipality$code_muni %<>% stringr::str_sub(1, 6) %>% as.numeric() 

dados = readxl::read_excel('PLANILHA PARA MAPAS.xlsx')

municipality %<>%
  dplyr::left_join(
    dados, by = c('code_muni' = 'CÓDIGO')
  )

showtextdb::font_install(showtextdb::google_fonts('Fira Sans'))
showtext::showtext_auto()

municipality %>% 
  ggplot2::ggplot()+
  ggplot2::geom_sf(ggplot2::aes(fill=`EGS 2005`), col='white')+
  ggplot2::scale_fill_gradient2(
    low='red4',
    high='blue4',
    midpoint=1.1,
    limits = c(0, 4.5),
    na.value = 'beige')+
  ggplot2::theme_minimal()+
  ggplot2::theme(
    text = ggplot2::element_text(family='Fira Sans', face='bold'),
    axis.text = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )

municipality %>% 
  ggplot2::ggplot()+
  ggplot2::geom_sf(ggplot2::aes(fill=`EGS 2015`), col='white')+
  ggplot2::scale_fill_distiller(
    palette='Blues',
    direction = 1,
    limits = c(0, 4.5),
    na.value = 'beige')+
  ggplot2::theme_minimal()+
  ggplot2::theme(
    text = ggplot2::element_text(family='Fira Sans', face='bold'),
    axis.text = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )
