# Limpando base de dados ----

rm(list = ls())
graphics.off()

# Carregando Pacotes ------------------------------------------------------

library(sidrar)
library(tidyverse)
library(scales)
library(writexl)
library(lubridate)
library(glue)
library(memoise)

# Período -----------------------------------------------------------------

periodo <- "202012"

# Parâmetros --------------------------------------------------------------

my_cache_folder <- memoise::cache_filesystem(path = 'data/mem')

mem_get_sidra <- memoise(
  f = get_sidra,
  cache = my_cache_folder
)


data_pub <- lubridate::ymd(paste0(periodo, "01"))
estados_grafico1 <- '21,22,23,24,25,26,27,28,29,31,32'
estados1 <- '21,22,23,24,25,26,27,28,29,31,32'
# variavel_tabs <- "90670" # Acumulado de 12 meses
variavel_tabs <- "90669" # Acumulado do ano

# API's utilizadas para download dos dados

API_tab3416 <- paste0(
  "/t/3416/n1/all/n3/", 
  estados1, 
  "/v/564/p/", 
  periodo, 
  "/c11046/",
  variavel_tabs,"/d/v565%201"
)

API_tab3416_2 <- paste0(
  "/t/3416/n1/all/n3/",
  estados1, "/v/564/p/",
  periodo, 
  "/c11046/33534,90668,90669,90670/d/v564%201"
)


API_tab3416_grafico <- paste0(
  "/t/3416/n1/all/n3/",
  estados_grafico1, 
  "/v/564/p/", 
  periodo, 
  "/c11046/", 
  variavel_tabs,
  "/d/v564%201"
)


API_tab3417 <- paste0(
  "/t/3417/n1/all/n3/", 
  estados1, "/v/1186/p/", 
  periodo, "/c11046/",
  variavel_tabs,
  "/d/v1186%201"
)

API_tab3417_2 <- paste0(
  "/t/3417/n1/all/n3/",
  estados1, "/v/1186/p/",
  periodo, 
  "/c11046/33534,90668,90669,90670/d/v1186%201"
)


API_tab3417_grafico <- paste0(
  "/t/3417/n1/all/n3/", 
  estados_grafico1, 
  "/v/1186/p/",
  periodo,
  "/c11046/", 
  variavel_tabs,
  "/d/v1186%201"
)


API_tab_3418 <- paste0(
  "/t/3418/n1/all/n3/",
  estados1, "/v/564/p/",
  periodo, "/c11046/", 
  variavel_tabs, 
  "/c85/all/d/v564%201"
)

API_tab_3419 <- paste0(
  "/t/3419/n1/all/n3/",
  estados1, 
  "/v/1186/p/", 
  periodo, 
  "/c11046/",
  variavel_tabs,
  "/c85/2762,103159/d/v1186%201"
)

# Parâmetros do Gráfico

# Cores para o gráfico

cor1 <- rgb(166, 25, 60, maxColorValue = 250)
cor2 <- rgb(244, 177, 131, maxColorValue = 250)
cores <- c(cor1, cor2)
black <- rgb(0, 0, 0, maxColorValue = 1) # Cor Datas Labels

# Modificando rótulos da tabela 2

mes_atual <- 
  month(str_sub(periodo, start = 5) %>% as.numeric(),label = T, abbr = F) %>% 
  as.character() %>% 
  str_to_sentence()


mes_anterior <-
  month(
    ifelse(
      as.numeric(str_sub(periodo, start = 5)) == 1, 12, 
      as.numeric(str_sub(periodo, start = 5)) - 1
      ),
    label = T, 
    abbr = F) %>% 
  as.character() %>% 
  str_to_sentence()

ano_atual <- str_sub(periodo, end = 4) %>% as.numeric()

variacao_mensal <- glue::glue(
  "{mes_atual} {ano_atual} / {str_to_lower(mes_anterior)} {ifelse(mes_anterior == 'Dezembro', ano_atual - 1, ano_atual)}*"
  )

variacao_interanual <- glue::glue(
  "{mes_atual} {ano_atual} / {ifelse(mes_anterior == 'Dezembro', 'janeiro', str_to_lower(mes_atual))} {ano_atual - 1})"
  ) 


variacao_interanual <- str_remove(variacao_interanual, "[)]")


acumulado_ano <- glue::glue("Acumulado em {ano_atual}")

# Download dos Dados ------------------------------------------------------

dados_3416 <- mem_get_sidra(api = API_tab3416) %>%
  select(`Brasil e Unidade da Federação`, Valor) %>%
  spread(key = `Brasil e Unidade da Federação`, value = Valor) %>%
  mutate(atividades = c("Comércio varejista")) %>%
  select(
    "atividades",
    "Brasil",
    "Ceará",
    "Pernambuco",
    "Bahia",
    "Minas Gerais",
    "Espírito Santo"
  )


dados_3418 <- mem_get_sidra(api = API_tab_3418) %>% 
  select(`Brasil e Unidade da Federação`, Atividades, Valor) %>% 
  rename("atividades" = "Atividades") %>% 
  pivot_wider(names_from = `Brasil e Unidade da Federação`, values_from = Valor)


dados_3417 <- mem_get_sidra(api = API_tab3417) %>%
  select(`Brasil e Unidade da Federação`, Valor) %>%
  mutate(atividades = c("Comércio varejista ampliado")) %>% 
  pivot_wider(names_from = `Brasil e Unidade da Federação`, values_from = Valor) %>% 
  select(
    "atividades",
    "Brasil",
    "Ceará",
    "Pernambuco",
    "Bahia",
    "Minas Gerais",
    "Espírito Santo"
  )

dados_3419 <- mem_get_sidra(api = API_tab_3419) %>%
  select(`Brasil e Unidade da Federação`, Atividades, Valor) %>%
  rename("atividades" = "Atividades") %>%
  pivot_wider(names_from = `Brasil e Unidade da Federação`, values_from = Valor) %>% 
  arrange(desc(atividades))


# Download da Tabela 3416 para a construção da Tabela 2

dados_3416_2 <- mem_get_sidra(api = API_tab3416_2) %>% 
  select(`Brasil e Unidade da Federação`, `Tipos de índice`, Valor) %>% 
  pivot_wider(names_from = `Brasil e Unidade da Federação`, values_from = Valor)

# Download da Tabela 3417 para a construção da Tabela 2

dados_3417_2 <- mem_get_sidra(api = API_tab3417_2) %>% 
  select(`Brasil e Unidade da Federação`, `Tipos de índice`, Valor) %>% 
  pivot_wider(names_from = `Brasil e Unidade da Federação`, values_from = Valor)


# Download dados para o gráfico 3416 


dados_3416_grafico <- mem_get_sidra(api = API_tab3416_grafico) %>% 
  select(`Brasil e Unidade da Federação`, Valor) %>% 
  rename("BR_UF" = "Brasil e Unidade da Federação", 'Value' = 'Valor') %>% 
  mutate(Type = "Varejo Restrito") %>% 
  arrange(BR_UF)

# Download dados para o gráfico 3417

dados_3417_grafico <- mem_get_sidra(api = API_tab3417_grafico) %>% 
  select(`Brasil e Unidade da Federação`, Valor) %>% 
  rename("BR_UF" = "Brasil e Unidade da Federação", 'Value' = 'Valor') %>% 
  mutate(Type = "Varejo Ampliado") %>% 
  arrange(BR_UF)

# Tabela 1 ----------------------------------------------------------------

tabela_1 <- dados_3416 %>% 
  bind_rows(dados_3418, dados_3417, dados_3419) %>% 
  as_tibble() %>% 
  select(atividades:`Espírito Santo`)

# Tabela 2 ----------------------------------------------------------------

tabela_2 <- dados_3416_2 %>% 
  bind_cols(dados_3417_2) %>% 
  janitor::clean_names() %>% 
  rename(
    "indice" = "tipos_de_indice_1",
    "brasil_restrito" = "brasil_2",
    "brasil_ampliado" = "brasil_15",
    "maranhao_restrito" = "maranhao_3",
    "maranhao_ampliado" = "maranhao_16",
    "piaui_restrito" = "piaui_4",
    "piaui_ampliado" = "piaui_17",
    "ceara_restrito" = "ceara_5",
    "ceara_ampliado" = "ceara_18", 
    "rio_grande_do_norte_restrito" = "rio_grande_do_norte_6",
    "rio_grande_do_norte_ampliado" = "rio_grande_do_norte_19",
    "paraiba_restrito" = "paraiba_7",
    "paraiba_ampliado" = "paraiba_20",
    "pernambuco_restrito" = "pernambuco_8",
    "pernambuco_ampliado" = "pernambuco_21",
    "alagoas_restrito" = "alagoas_9",
    "alagoas_ampliado" = "alagoas_22",
    "sergipe_restrito" = "sergipe_10",
    "sergipe_ampliado" = "sergipe_23",
    "bahia_restrito" = "bahia_11",
    "bahia_ampliado" = "bahia_24",
    "minas_gerais_restrito" = "minas_gerais_12", 
    "minas_gerais_ampliado" = "minas_gerais_25",
    "espirito_santo_restrito" = "espirito_santo_13", 
    "espirito_santo_ampliado" = "espirito_santo_26"
  ) %>% 
  select(
    "indice",
    "brasil_restrito",
    "brasil_ampliado",
    "maranhao_restrito",
    "maranhao_ampliado",
    "piaui_restrito",
    "piaui_ampliado",
    "ceara_restrito",
    "ceara_ampliado", 
    "rio_grande_do_norte_restrito",
    "rio_grande_do_norte_ampliado",
    "paraiba_restrito",
    "paraiba_ampliado",
    "pernambuco_restrito",
    "pernambuco_ampliado",
    "alagoas_restrito",
    "alagoas_ampliado",
    "sergipe_restrito",
    "sergipe_ampliado",
    "bahia_restrito",
    "bahia_ampliado",
    "minas_gerais_restrito", 
    "minas_gerais_ampliado",
    "espirito_santo_restrito", 
    "espirito_santo_ampliado"
  )  %>% 
  mutate(
    indice = recode(
      indice,
      "Variação mês / mês anterior com ajuste sazonal" = variacao_mensal,
      "Variação mensal (base: igual mês do ano anterior)" = variacao_interanual,
      "Variação acumulada no ano (base: igual período do ano anterior)" = acumulado_ano,
      "Variação acumulada de 12 meses" = "Acumulado em 12 meses"
    ),
    indice = as.character(indice)
  )

# Tabela do Gráfico 1 -----------------------------------------------------

tabela_grafico <- dados_3416_grafico %>% 
  bind_rows(dados_3417_grafico) %>%
  pivot_wider(names_from = Type, values_from = Value)


# Gráfico 1 ---------------------------------------------------------------

tab.grph <- tabela_grafico %>% 
  pivot_longer(
    cols = `Varejo Restrito`:`Varejo Ampliado`,
    names_to = "comercio",
    values_to = "valor"
  ) %>% 
  mutate(
    comercio = factor(
      comercio, levels=c("Varejo Restrito", "Varejo Ampliado")
    )
  )

max_tab <- max(tab.grph$valor/100)
min_tab <- min(tab.grph$valor/100)


## Recoding tab.grph$BR_UF
tab.grph$BR_UF <- recode(tab.grph$BR_UF,
                         "Espírito Santo" = "Espírito\nSanto",
                         "Minas Gerais" = "Minas\nGerais",
                         "Rio Grande do Norte" = "Rio Grande\ndo Norte"
)

grafico_1 <- tab.grph %>% 
  mutate(
    BR_UF <- recode(
      BR_UF,
      "Espírito Santo" = "Espírito\nSanto",
      "Minas Gerais" = "Minas\nGerais",
      "Rio Grande do Norte" = "Rio Grande\ndo Norte"
    )
  ) %>% 
  ggplot(aes(x = reorder(BR_UF, desc(valor/100)), y = valor/100, fill = comercio)) + 
  geom_bar(stat = "identity", position = position_dodge(), width = 0.65) +
  geom_text(
    aes(
      label = percent(valor/100, accuracy = 0.1, big.mark = ".", decimal.mark = ",")
    ),
    position = position_dodge(width = 0.9),
    vjust = -ifelse(tab.grph$valor > 0, 0.4, -1.4),
    hjust = 0.4,
    col = ifelse(tab.grph$valor / 100 > 0, "black", "red"),
    size = 4
  ) + 
  geom_hline(yintercept = 0, col = "gray") +
  theme_bw() + 
  scale_y_continuous(
    "",
    labels = percent_format(big.mark = ".", decimal.mark = ","),
    limits = c(min_tab - 0.02, max_tab + 0.02)
  ) + 
  scale_x_discrete("") + 
  scale_fill_manual(
    "",
    values = c("#A6193C", "#F4B084")
  ) +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.box = "vertical",
    text = element_text(size = 15.5)
  )

grafico_1

# Salvando Dados ----------------------------------------------------------

DataCombine::rmExcept(c("tabela_1", "tabela_2", "tabela_grafico", "grafico_1"))

writexl::write_xlsx(
  list(
    "tabela_1" = tabela_1,
    "tabela_2" = tabela_2,
    "tabela_grafico" = tabela_grafico
    ), 
  "data/tabela_graficos.xlsx"
  )

# Salvando Gráfico --------------------------------------------------------

ggsave(
  filename = "grafico1.png",
  plot = grafico_1,
  device = "png",
  path = "figs",
  width = 32,
  height = 20,
  units = "cm"
)


