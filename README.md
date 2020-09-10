Coletando as menções nos feeds do Google
================

  - [Introdução](#introdução)
  - [Baixe o script](#baixe-o-script)
  - [Pacotes necessários](#pacotes-necessários)
  - [Função `google_rss()`](#função-google_rss)
      - [Sintaxe](#sintaxe)
      - [Parâmetros:](#parâmetros)
      - [Corpo da função](#corpo-da-função)
  - [Exemplos de uso:](#exemplos-de-uso)
      - [Pesquisa com link direto:](#pesquisa-com-link-direto)
          - [Google Alert](#google-alert)
          - [Google News RSS](#google-news-rss)
      - [Pesquisa com palavras-chave](#pesquisa-com-palavras-chave)
          - [Usando os parâmetros](#usando-os-parâmetros)
  - [Pesquisando palavras e links em um
    *loop*](#pesquisando-palavras-e-links-em-um-loop)
  - [Salve os dados em uma planilha:](#salve-os-dados-em-uma-planilha)
      - [Excel](#excel)
      - [Google Spreadsheets:](#google-spreadsheets)

# Introdução

Este repositório apresenta uma função em `R` que armazena os [*feeds
RSS*](https://pt.wikipedia.org/wiki/RSS) do [Google
Notícias](https://news.google.com/) ou do [Google
Alerts](https://www.google.com.br/alerts) em uma planilha estruturada.

# Baixe o script

Você pode baixar o script [CLICANDO
AQUI](https://github.com/rdurl0/google-rss-feeds-com-r/blob/master/R/google_rss.R).

# Pacotes necessários

Para rodar a função em seu computador, você vai precisar das seguintes
bibliotecas:

``` r
library(tidyverse) # conjunto de pacotes de manipulação de dados no R
library(lubridate) # para formatos de datas
library(xml2)      # para raspar os dados dos feeds
library(httr)      # para raspar os dados dos feeds
library(xlsx)      # salva em Excel
library(googlesheets4) # salva em Google Spreadsheet
```

# Função `google_rss()`

## Sintaxe

``` r
google_rss(
  termo,
  frase_exata = TRUE,
  periodo = NULL,
  n_periodo = NULL
)
```

## Parâmetros:

O parâmetro principal é o `termo`

  - **`termo`**: Uma palavra-chave de sua preferência, ou o link direto
    do RSS do Google.

Os parâmetros abaixo não precisam ser especificados se o seu `termo` for
um link direto para o feed (se o termo for um link eles serão
ignorados).

Se o `termo` for uma palavra-chave, utilize os parâmetros abaixo para
ajustar a sua pesquisa. Se nenhum desses parâmetros for fornecido, por
*default* a função te retornará uma pesquisa por correspondência exata
das últimas 24h.

  - **`frase_exata`**: use `TRUE` se quiser correspondência exata na
    busca do termo (ele será pesquisado entre aspas) ou `FALSE` se
    quiser correspondência ampla.
  - **`periodo`**: use `"hora"` ou `"dia"` para definir o intervalo de
    tempo das últimas menções.
  - **`n_periodo`**: um número inteiro para retornar a quantidade de
    `hora` ou `dia` desejadas para a pesquisa.

## Corpo da função

``` r
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
```

# Exemplos de uso:

## Pesquisa com link direto:

### Google Alert

ATENÇÃO: Esse link expira a cada 24 horas.

``` r
# link direto de um feed recebido pelo e-mail do Google Alerts:
ex1 <- google_rss("https://www.google.com/alerts/feeds/14690018069938359893/7021405697253714962")
#> 
#> 
#> Link direto de RSS:
#>  https://www.google.com/alerts/feeds/14690018069938359893/7021405697253714962

glimpse(ex1)
#> Rows: 1
#> Columns: 11
#> $ termo             <chr> "Google Alert - \"Open Knowledge Brasil\""
#> $ data_alerta       <dttm> 2020-09-09 22:30:00
#> $ titulo_da_materia <chr> "RS alcança topo do ranking que avalia o índice d...
#> $ veiculo           <chr> NA
#> $ data_da_materia   <dttm> 2020-09-09 22:30:00
#> $ dia               <int> 9
#> $ mes               <dbl> 9
#> $ ano               <dbl> 2020
#> $ dia_semana        <chr> "quarta-feira"
#> $ link              <chr> "https://www.jornalnh.com.br/noticias/rio_grande_...
#> $ host              <chr> "www.jornalnh.com.br"
```

### Google News RSS

O RSS do Google News fica em uma URL específica, com padrão:
`https://news.google.com/rss/search?q={_termo_de_pesquisa_}`.

Os resultados são os mesmos conteúdos de uma busca comum no [Google
News](https://news.google.com). A diferença é que o RSS fica em uma
[página
XML](https://news.google.com/rss/search?q=%22bob%20marley%22when%3A1d&hl=pt-BR&gl=BR&ceid=BR%3Apt-419)
e os resultados do [Google News
tradicional](https://news.google.com/search?q=%22bob%20marley%22when%3A1d&hl=pt-BR&gl=BR&ceid=BR%3Apt-419)
é uma interface para o público interagir.

``` r
# link direto de um feed do Google News:
ex2 <- google_rss("https://news.google.com/rss/search?q=%22bob%20marley%22when%3A1d&hl=pt-BR&gl=BR&ceid=BR%3Apt-419")
#> 
#> 
#> Link direto de RSS:
#>  https://news.google.com/rss/search?q=%22bob%20marley%22when%3A1d&hl=pt-BR&gl=BR&ceid=BR%3Apt-419

glimpse(ex2)
#> Rows: 5
#> Columns: 11
#> $ termo             <chr> "bob marley", "bob marley", "bob marley", "bob ma...
#> $ data_alerta       <dttm> 2020-09-10 11:01:51, 2020-09-10 11:01:51, 2020-0...
#> $ titulo_da_materia <chr> "Notícias da semana no Reggae Point - MiranteFM.c...
#> $ veiculo           <chr> "Imirante.com", "radar amazonico", "Contigo!", "O...
#> $ data_da_materia   <dttm> 2020-09-10 05:59:00, 2020-09-10 09:17:17, 2020-0...
#> $ dia               <int> 10, 10, 10, 9, 9
#> $ mes               <dbl> 9, 9, 9, 9, 9
#> $ ano               <dbl> 2020, 2020, 2020, 2020, 2020
#> $ dia_semana        <chr> "quinta-feira", "quinta-feira", "quinta-feira", "...
#> $ link              <chr> "https://imirante.com/mirantefm/noticias/2020/09/...
#> $ host              <chr> "imirante.com", "radaramazonico.com.br", "contigo...
```

## Pesquisa com palavras-chave

### Usando os parâmetros

Buscando por um termo, você obtém os principais resultados de sua
correspondência exata nas últimas 24 horas (1 dia):

``` r
ex3 <- google_rss("black lives matter")
#> 
#> 
#> O link que você pesquisou foi:
#>  https://news.google.com/rss/search?q=%22black%20lives%20matter%22%20when%3A1d&hl=pt-BR&gl=BR&ceid=BR%3Apt-419 
#>  
#> 
#> Exibindo resultados do período:
#>  1 dia

glimpse(ex3)
#> Rows: 9
#> Columns: 11
#> $ termo             <chr> "black lives matter", "black lives matter", "blac...
#> $ data_alerta       <dttm> 2020-09-10 11:01:52, 2020-09-10 11:01:52, 2020-0...
#> $ titulo_da_materia <chr> "American Airlines vai confeccionar broches do “B...
#> $ veiculo           <chr> "Estudos Nacionais", "RTP", "Educação Estadão", "...
#> $ data_da_materia   <dttm> 2020-09-09 17:16:33, 2020-09-10 03:26:00, 2020-0...
#> $ dia               <int> 9, 10, 9, 9, 9, 9, 9, 9, 9
#> $ mes               <dbl> 9, 9, 9, 9, 9, 9, 9, 9, 9
#> $ ano               <dbl> 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020
#> $ dia_semana        <chr> "quarta-feira", "quinta-feira", "quarta-feira", "...
#> $ link              <chr> "https://www.estudosnacionais.com/28389/american-...
#> $ host              <chr> "www.estudosnacionais.com", "www.rtp.pt", "educac...
```

Veja a diferença entre uma pesquisa de frase exata e outra com
correspondência ampla:

``` r
# pesquisa exata (frase_exata = TRUE)
ex4 <- google_rss("NBA jogadores greve", periodo = "dia", n_periodo = 15, frase_exata = TRUE)
#> 
#> 
#> O link que você pesquisou foi:
#>  https://news.google.com/rss/search?q=%22NBA%20jogadores%20greve%22%20when%3A15d&hl=pt-BR&gl=BR&ceid=BR%3Apt-419 
#>  
#> 
#> Exibindo resultados do período:
#>  15 dia

glimpse(ex4)
#> Rows: 0
#> Columns: 11
#> $ termo             <chr> 
#> $ data_alerta       <dttm> 
#> $ titulo_da_materia <chr> 
#> $ veiculo           <chr> 
#> $ data_da_materia   <dttm> 
#> $ dia               <int> 
#> $ mes               <dbl> 
#> $ ano               <dbl> 
#> $ dia_semana        <chr> 
#> $ link              <chr> 
#> $ host              <chr>
```

``` r
# pesquisa ampla (frase_exata = FALSE)
ex5 <- google_rss("NBA jogadores greve", periodo = "dia", n_periodo = 15, frase_exata = FALSE)
#> 
#> 
#> O link que você pesquisou foi:
#>  https://news.google.com/rss/search?q=NBA%20jogadores%20greve%20when%3A15d&hl=pt-BR&gl=BR&ceid=BR%3Apt-419 
#>  
#> 
#> Exibindo resultados do período:
#>  15 dia

glimpse(ex5)
#> Rows: 24
#> Columns: 11
#> $ termo             <chr> "NBA jogadores greve", "NBA jogadores greve", "NB...
#> $ data_alerta       <dttm> 2020-09-10 11:01:53, 2020-09-10 11:01:53, 2020-0...
#> $ titulo_da_materia <chr> "Funcionários fazem greve e cobram a NBA contra r...
#> $ veiculo           <chr> "globoesporte.com", "Revista Fórum", "Sputnik Bra...
#> $ data_da_materia   <dttm> 2020-08-28 04:00:00, 2020-08-27 04:00:00, 2020-0...
#> $ dia               <int> 28, 27, 27, 3, 27, 3, 28, 4, 4, 2, 4, 28, 29, 27,...
#> $ mes               <dbl> 8, 8, 8, 9, 8, 9, 8, 9, 9, 9, 9, 8, 8, 8, 9, 8, 9...
#> $ ano               <dbl> 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2...
#> $ dia_semana        <chr> "sexta-feira", "quinta-feira", "quinta-feira", "q...
#> $ link              <chr> "https://globoesporte.globo.com/basquete/nba/noti...
#> $ host              <chr> "globoesporte.globo.com", "revistaforum.com.br", ...
```

Buscas nas últimas 5 horas:

``` r
ex6 <- google_rss("Fabrício Queiroz", periodo = "hora", n_periodo = 5, frase_exata = TRUE)
#> 
#> 
#> O link que você pesquisou foi:
#>  https://news.google.com/rss/search?q=%22Fabr%C3%ADcio%20Queiroz%22%20when%3A5h&hl=pt-BR&gl=BR&ceid=BR%3Apt-419 
#>  
#> 
#> Exibindo resultados do período:
#>  5 hora

glimpse(ex6)
#> Rows: 4
#> Columns: 11
#> $ termo             <chr> "Fabrício Queiroz", "Fabrício Queiroz", "Fabrício...
#> $ data_alerta       <dttm> 2020-09-10 11:01:54, 2020-09-10 11:01:54, 2020-0...
#> $ titulo_da_materia <chr> "Detonautas Roque Clube lança música que satiriza...
#> $ veiculo           <chr> "OFuxico", "Revista Seleções", "Mais Goiás", "Os ...
#> $ data_da_materia   <dttm> 2020-09-10 09:25:27, 2020-09-10 06:39:25, 2020-0...
#> $ dia               <int> 10, 10, 10, 10
#> $ mes               <dbl> 9, 9, 9, 9
#> $ ano               <dbl> 2020, 2020, 2020, 2020
#> $ dia_semana        <chr> "quinta-feira", "quinta-feira", "quinta-feira", "...
#> $ link              <chr> "https://www.ofuxico.com.br/noticias-sobre-famoso...
#> $ host              <chr> "www.ofuxico.com.br", "www.selecoes.com.br", "www...
```

# Pesquisando palavras e links em um *loop*

Faça várias buscas ao mesmo tempo:

  - Por palavras-chave:

<!-- end list -->

``` r
# Crie uma lista com os termos que deseja pesquisar:
lista_de_palavras <- c(
  "Transparência Brasil",
  "Manoel Galdino",
  "Juliana Sakai"
   # (...)
)

# rode a lista nesse loop:
pesquisas_por_palavras <- lista_de_palavras %>% 
  map_df(~ google_rss(.x, frase_exata = T, periodo = "dia", n_periodo = 7))
#> 
#> 
#> O link que você pesquisou foi:
#>  https://news.google.com/rss/search?q=%22Transpar%C3%AAncia%20Brasil%22%20when%3A7d&hl=pt-BR&gl=BR&ceid=BR%3Apt-419 
#>  
#> 
#> Exibindo resultados do período:
#>  7 dia
#> 
#> O link que você pesquisou foi:
#>  https://news.google.com/rss/search?q=%22Manoel%20Galdino%22%20when%3A7d&hl=pt-BR&gl=BR&ceid=BR%3Apt-419 
#>  
#> 
#> Exibindo resultados do período:
#>  7 dia
#> 
#> O link que você pesquisou foi:
#>  https://news.google.com/rss/search?q=%22Juliana%20Sakai%22%20when%3A7d&hl=pt-BR&gl=BR&ceid=BR%3Apt-419 
#>  
#> 
#> Exibindo resultados do período:
#>  7 dia

# veja como ficou:
glimpse(pesquisas_por_palavras)
#> Rows: 11
#> Columns: 11
#> $ termo             <chr> "Transparência Brasil", "Transparência Brasil", "...
#> $ data_alerta       <dttm> 2020-09-10 11:01:55, 2020-09-10 11:01:55, 2020-0...
#> $ titulo_da_materia <chr> "Transparência Brasil mostra ranking de ações de ...
#> $ veiculo           <chr> "Portal a12", "MixVale", "The Intercept Brasil", ...
#> $ data_da_materia   <dttm> 2020-09-03 14:57:10, 2020-09-09 13:31:40, 2020-0...
#> $ dia               <int> 3, 9, 8, 3, 4, 4, 4, 5, 9, 5, 8
#> $ mes               <dbl> 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
#> $ ano               <dbl> 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2...
#> $ dia_semana        <chr> "quinta-feira", "quarta-feira", "terça-feira", "q...
#> $ link              <chr> "https://www.a12.com/radio/noticias/transparencia...
#> $ host              <chr> "www.a12.com", "www.mixvale.com.br", "theintercep...
```

  - Por links RSS:

<!-- end list -->

``` r
# faça o mesmo com os links:
lista_de_links <- c(
  "https://news.google.com/rss/search?q=%20%22Covid-19%22%20when%3A1h&hl=pt-BR&gl=BR&ceid=BR%3Apt-419",
  "https://news.google.com/rss/search?q=%22Palmeiras%22%20when%3A1h&hl=pt-BR&gl=BR&ceid=BR%3Apt-419",
  "https://news.google.com/rss/search?q=%22bob%20marley%22when%3A1d&hl=pt-BR&gl=BR&ceid=BR%3Apt-419"
   # (...)
)

# rode a lista nesse loop:
pesquisas_por_links <- lista_de_links %>% map_df(~ google_rss(.x))
#> 
#> 
#> Link direto de RSS:
#>  https://news.google.com/rss/search?q=%20%22Covid-19%22%20when%3A1h&hl=pt-BR&gl=BR&ceid=BR%3Apt-419 
#> 
#> 
#> 
#> Link direto de RSS:
#>  https://news.google.com/rss/search?q=%22Palmeiras%22%20when%3A1h&hl=pt-BR&gl=BR&ceid=BR%3Apt-419 
#> 
#> 
#> 
#> Link direto de RSS:
#>  https://news.google.com/rss/search?q=%22bob%20marley%22when%3A1d&hl=pt-BR&gl=BR&ceid=BR%3Apt-419

# veja como ficou:
glimpse(pesquisas_por_links)
#> Rows: 78
#> Columns: 11
#> $ termo             <chr> "Covid-19", "Covid-19", "Covid-19", "Covid-19", "...
#> $ data_alerta       <dttm> 2020-09-10 11:01:58, 2020-09-10 11:01:58, 2020-0...
#> $ titulo_da_materia <chr> "Covid persistente: os sintomas e as sequelas mai...
#> $ veiculo           <chr> "UOL", "ISTOÉ", "Jornal Correio do Povo", "ISTOÉ"...
#> $ data_da_materia   <dttm> 2020-09-10 10:39:48, 2020-09-10 10:27:00, 2020-0...
#> $ dia               <int> 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 1...
#> $ mes               <dbl> 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9...
#> $ ano               <dbl> 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2...
#> $ dia_semana        <chr> "quinta-feira", "quinta-feira", "quinta-feira", "...
#> $ link              <chr> "https://www.uol.com.br/vivabem/noticias/bbc/2020...
#> $ host              <chr> "www.uol.com.br", "istoe.com.br", "www.correiodop...
```

# Salve os dados em uma planilha:

## Excel

O problema aqui é que você só pode reescrever a planilha adicionando
nova aba. Caso contrário sempre terá que criar outra.

``` r
# Cria as abas, sempre tem que criar uma aba nova:
aba_plvr_chave <- paste("Palavras-chave em", today())
aba_links <- paste("Links em", today())

# palavras
write.xlsx(as.data.frame(pesquisas_por_palavras),
           file = "./data/varias_pesquisas.xlsx",
           # insere os dados em uma aba sem apagar o que já tinha antes:
           sheetName = aba_plvr_chave,
           append = TRUE)

# links
write.xlsx(as.data.frame(pesquisas_por_links),
           file = "./data/varias_pesquisas.xlsx",
           sheetName = aba_links,
           append = TRUE)
```

Veja o resultado
[AQUI](https://github.com/rdurl0/google-rss-feeds-com-r/blob/master/data/varias_pesquisas.xlsx)

## Google Spreadsheets:

Siga os passos (atenção ao item 3\!):

1.  Vá no Google Drive e **crie uma planilha em branco**;
2.  Copie o **link da planilha**;

<!-- end list -->

``` r
# link da planilha criada no drive:
link_da_planilha <- "https://docs.google.com/spreadsheets/d/1-sjp0oF3RXDMYwT3IKWEF7y6Cp-ahBkD_MYti5ZejX8/edit?usp=sharing"
```

3.  Crie uma aba nova com
    [`sheet_write()`](https://googlesheets4.tidyverse.org/reference/sheet_write.html)
    e **não volte a usar essa função** (senão você reescreve em cima da
    que estava salva);

<!-- end list -->

``` r
# USE SOMENTE UMA VEZ PARA CRIAR CADA ABA DA PLANILHA:
sheet_write(data = pesquisas_por_palavras,
            ss = link_da_planilha,
            sheet = "Palavras-chave")

sheet_write(data = pesquisas_por_links,
            ss = link_da_planilha,
            sheet = "Links")
```

4.  Atualiza a planilha com novos dados com
    [`sheet_append()`](https://googlesheets4.tidyverse.org/reference/sheet_append.html).

<!-- end list -->

``` reval
# SEMPRE QUE FOR ATUALIZAR USA ESTA: 
sheet_append(data = ex5, # adiciona novos dados
             ss = link_da_planilha,
             sheet = "Palavras-chave")

sheet_append(data = ex2, # adiciona novos dados
             ss = link_da_planilha,
             sheet = "Links")
```

veja o resultado
[AQUI](https://docs.google.com/spreadsheets/d/1-sjp0oF3RXDMYwT3IKWEF7y6Cp-ahBkD_MYti5ZejX8/edit?usp=sharing)

FIM

:smile:
