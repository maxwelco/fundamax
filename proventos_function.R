library(tidyverse)
library(rvest)
fundamentus2 <- read_html("https://www.fundamentus.com.br/resultado.php") %>%
  html_node("table") %>% # selector = table
  html_table(fill = TRUE) %>% # get a table
  arrange(Papel)

# funcao de proventos - site fundamentus
proventos_function <- function(papel) {

  url <- paste0("https://www.fundamentus.com.br/proventos.php?papel=",papel,"&tipo=2")

  #papel will change by each papel code

  # Read url
  papel <- read_html(url)

  # Extract herbicide resistance data
  papel1 <- papel %>%
    html_node("table") # selector


  # Algumas empresas não pagam dividendos
  if (class(papel1) == "xml_missing") {
    stop("Sem proventos!")
  }

  papel1 <- papel1 %>%
    html_table(fill = TRUE) # get the table


  # Tidy dataset
  papel2 <- papel1 %>%
    janitor::clean_names() %>%
    as_tibble() %>% #str_replace_all(., "\\-", "NA")) %>%
    mutate(data_de_pagamento = if_else(
      data_de_pagamento == "-", "", data_de_pagamento)) %>%
    mutate_if(is_character, ~ str_replace_all(., "\\/", "\\-")) %>%
    mutate_at("valor",
              ~ as.double(str_replace_all(., ",", "\\."))) %>%
    rename(data_com = data) %>%
    mutate(data_com = dmy(data_com),
           data_de_pagamento = dmy(data_de_pagamento),
           tipo = as_factor(tipo),
           `Mês` = month(data_com, label = TRUE),
           `Mês` = fct_recode(`Mês`,
                              "Fev" = "Feb",
                              "Abr" = "Apr",
                              "Mai" = "May",
                              "Ago" = "Aug",
                              "Set" = "Sep",
                              "Out" = "Oct",
                              "Dez" = "Dec"),
           Semana = week(data_com),
           Sem_dia = wday(data_com, label = TRUE),
           Sem_dia = fct_recode(Sem_dia,
                                "Seg" = "Mon",
                                "Ter" = "Tue",
                                "Qua" = "Wed",
                                "Qui" = "Thu",
                                "Sex" = "Fri"),
           Ano = factor(year(data_com)),
           Dia = day(data_com)) %>%
    mutate(Tipo = str_to_title(tipo),
           Tipo = fct_recode(Tipo,
                             "Jrs Cap Próprio" = "Jrs Cap Proprio")) %>%
    mutate(Valor = valor / por_quantas_acoes) %>%
    dplyr::select(data_com, `Mês`, Semana, Dia, Valor, everything())
  papel2
}
