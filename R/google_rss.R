# PACOTES ----------------------------------------------------------------------

library(tidyverse) # conjunto de pacotes de manipulação de dados no R
library(lubridate) # para formatos de datas
library(xml2)      # para raspar os dados dos feeds
library(httr)      # para raspar os dados dos feeds

# FUNÇÃO -----------------------------------------------------------------------

google_rss <- function(termo, frase_exata = TRUE, periodo = NULL, n_periodo = NULL) {
  
  # checa se é palavra-chave ou se é o link RSS:
  res <- try(read_xml(termo), silent = T)
  
  # Se for palavra-chave, o R vai montar a URL:
  if (isTRUE(class(res) == "try-error")) {
    
    # . customizando o período especificado:
    periodo <- ifelse(is.null(periodo), "dia", periodo)
    n_periodo <- ifelse(is.null(n_periodo), 1, n_periodo)  
    
    prd <- tibble(periodo = periodo, n_periodo = n_periodo) %>%
      mutate(
        when = case_when(
          periodo == "dia" ~ paste0(n_periodo, "d"),
          periodo == "hora" ~ paste0(n_periodo, "h"),
          TRUE ~ "1d"
        )
      )
    
    # . preparando o termo para entrar na url
    termo <- url_escape(termo)
    if(frase_exata) termo <- paste('%22', termo, '%22', sep = '')
    
    # . finaliza URL
    rss_url <- paste0(
      'https://news.google.com/rss/search?q=',
      termo,
      '%20when%3A',
      prd$when,
      '&hl=pt-BR&gl=BR&ceid=BR%3Apt-419'
    )
    
  } else {
    
    # se for uma URL, fica fácil =)
    rss_url <- termo 
    
  }
  
  # Raspa os dados:
  if (str_detect(termo, "https://www.google.com/alerts")) {
    
    # . parent node (conteúdo do rss):
    item <- rss_url %>% read_xml() %>% xml_ns_strip() %>% xml_children()
    
    # . childrens (colunas)
    termo <- item  %>% 
      xml_parent() %>%
      xml_find_first('//title') %>% 
      xml_text()
    
    # . formato de data apropriado
    data_alerta <- item  %>%
      xml_find_first('.//updated') %>%
      xml_text() %>%
      str_replace_all("[[:alpha:]]", " ") %>% 
      str_trim() %>% 
      as_datetime() %>% 
      na.omit()
    
    titulo_da_materia <- item %>% 
      xml_find_all(".//title") %>% 
      xml_text(trim = TRUE) %>% 
      gsub("<.*?>", "", .)
    
    data_da_materia <- item  %>% 
      xml_find_first('.//published') %>%
      xml_text() %>%
      str_replace_all("[[:alpha:]]", " ") %>% 
      str_trim() %>% 
      as_datetime() %>% 
      na.omit()
    
    link <- item %>% 
      xml_find_all(".//link/@href") %>% 
      xml_text() %>% 
      map_chr(~gsub("(^https.+)(https.+)(\\&.+$)", "\\2", .x))
    
    veiculo <- link %>% 
      map(httr::GET) %>% 
      map(httr::content) %>% 
      map(~ xml_find_all(.x, '//meta[@property="og:site_name"]/@content')) %>% 
      map(xml_text) %>% 
      map(~ tibble(value = .[1])) %>% 
      reduce(~ bind_rows(.x, .y)) %>% 
      pull(value)
    
    host <- map_chr(link, ~ parse_url(.x)$hostname)
    
  } else {
    
    # . parent node (conteúdo do rss):
    item <- rss_url %>% 
      read_xml() %>% 
      xml_find_all('//channel') %>% 
      xml_find_all('.//item')
    
    # . childrens (colunas):
    termo <-  item %>% 
      xml_find_first('//title') %>% 
      xml_text() %>%
      gsub("(^.+)(when.+$)", "\\1", .) %>% 
      str_remove_all('\\"') %>% 
      str_squish()
    
    # . precisei ajustar o fuso horário nas datas
    data_alerta <- item %>% 
      xml_find_all('//lastBuildDate') %>% 
      xml_text() %>%
      parse_datetime("%a, %d %b %Y %H:%M:%S GMT") %>%
      with_tz("America/Sao_Paulo")
    
    titulo_da_materia <- item %>% 
      xml_find_all('.//title') %>%
      xml_text()
    
    veiculo <- item %>%
      xml_find_all('.//source') %>%
      xml_text()
    
    link <- item %>%
      xml_find_all('.//link') %>%
      xml_text()
    
    host <- map_chr(link, ~ parse_url(.x)$hostname)
    
    data_da_materia <- item %>% 
      xml_find_all('.//pubDate') %>% 
      xml_text() %>% 
      parse_datetime("%a, %d %b %Y %H:%M:%S GMT") %>%
      with_tz("America/Sao_Paulo")
  }
  
  # output: mensagem
  if (isTRUE(class(res) == "try-error")) {
    
    cat(
      paste(
        "\n\nO link que você pesquisou foi:\n",
        rss_url,
        "\n",
        "\n\nExibindo resultados do período:\n",
        n_periodo,
        periodo
      )
    )
    
  } else {
    
    cat(paste("\n\nLink direto de RSS:\n", rss_url,"\n\n"))
    
  }
  
  # output: tabela
  rss_tbl <- tibble(
    termo = termo,
    data_alerta = data_alerta,
    titulo_da_materia = titulo_da_materia,
    veiculo = veiculo,
    data_da_materia,
    dia = day(data_da_materia),
    mes = month(data_da_materia),
    ano = year(data_da_materia),
    dia_semana = format(data_da_materia, "%A"),
    link = link,
    host = host
  )
  
  return(rss_tbl)
}