
#### PACOTES E DIRETORIOS ####

setwd('C:/Users/usuario/Documents/Ambientes/MONIQUE/moniques_maps')

require(magrittr)

#### CARREGANDO DADOS ####

## adicionando mesorregioes ###

meso <- readxl::read_excel('MUNICÍPIOS POR REGIÃO.xlsx')

meso %<>%
  tidyr::gather(key='regiao', value='municipio') %>% 
  dplyr::filter(!is.na(municipio))

meso$municipio %<>% stringr::str_replace_all('Conceição do Lago Açu', 'Conceição do Lago-Açu')
meso$municipio %<>% stringr::str_replace_all('Governador Edson Lobão', 'Governador Edison Lobão')
meso$municipio %<>% stringr::str_replace_all('Itapecuru-Mirim', 'Itapecuru Mirim')
meso$municipio %<>% stringr::str_replace_all('São João do Caru', 'São João do Carú')
meso$municipio %<>% stringr::str_replace_all('São José dos Patos', 'São João dos Patos')
meso$municipio %<>% stringr::str_replace_all('Sucupira do Riação', 'Sucupira do Riachão')

dados = readxl::read_excel('PLANILHA PARA MAPAS.xlsx')

dados %<>% dplyr::left_join(
  meso, 
  by = c('MUNICÍPIO' = 'municipio')
)

dados$regiao[61] <- 'OESTE'

## baixando mapa

municipality = geobr::read_municipality(21, 2018)
municipality$code_muni %<>% stringr::str_sub(1, 6) %>% as.numeric() 

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
        municipality$`EGS 2005` < 1 ~ "Ineficiente", 
        municipality$`EGS 2005` >= 1 ~ "Eficiente",
        is.na(municipality$`EGS 2005`) ~ "Sem info",
      ),
      levels = c(
        "Sem info",
        "Ineficiente",
        "Eficiente"
      )
    ),
    `Faixa EGS 2015` = factor(
      dplyr::case_when(
        municipality$`EGS 2015` < 1 ~ "Ineficiente", 
        municipality$`EGS 2015` >= 1 ~ "Eficiente",
        is.na(municipality$`EGS 2005`) ~ "Sem info",
      ),
      levels = c(
        "Sem info",
        "Ineficiente",
        "Eficiente"
      )
    )
  )

#### CRIANDO MAPAS ####

gg1 = municipality %>% 
  ggplot2::ggplot()+
  ggplot2::geom_sf(ggplot2::aes(fill=`Faixa EGS 2005`), col='grey30')+
  ggplot2::scale_fill_manual(
    values=c("beige", "coral2", "cyan4")
  )+
  ggplot2::labs(
    fill="EGSi 2005"
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

ggplot2::ggsave(gg1, filename='egs2005.png', dpi=600, height = 10, width = 8, limitsize = FALSE)

gg2 = municipality %>% 
  ggplot2::ggplot()+
  ggplot2::geom_sf(ggplot2::aes(fill=`Faixa EGS 2015`), col='grey30')+
  ggplot2::scale_fill_manual(
    values=c("beige", "coral2", "cyan4")
  )+
  ggplot2::labs(
    fill="EGSi 2015"
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

ggplot2::ggsave(gg2, filename='egs2015.png', dpi=600, height = 10, width = 8, limitsize = FALSE)

#### tabela ####

tabela <- municipality %>% 
  dplyr::select(regiao, `Faixa EGS 2005`, `Faixa EGS 2015`) %>% 
  dplyr::filter(!is.na(regiao)) %>% 
  dplyr::group_by(regiao) %>% 
  dplyr::summarise(
    eficientes_2005 = sum(`Faixa EGS 2005` == 'Eficiente'),
    ineficientes_2005 = sum(`Faixa EGS 2005` == 'Ineficiente'),
    eficientes_2015 = sum(`Faixa EGS 2015` == 'Eficiente'),
    ineficientes_2015 = sum(`Faixa EGS 2015` == 'Ineficiente'),
    .groups = 'drop'
  ) 

tabela['geom'] = NULL

openxlsx::write.xlsx(tabela, file='EFICIENCIA POR MESORREGIAO.xlsx', row.names = FALSE)
