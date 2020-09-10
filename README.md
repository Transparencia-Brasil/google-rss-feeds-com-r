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
          - [Link go Google Alert](#link-go-google-alert)
          - [Google News RSS](#google-news-rss)
      - [Pesquisa com palavras-chave](#pesquisa-com-palavras-chave)
          - [Usando os parâmetros](#usando-os-parâmetros)
          - [Pesquisando várias palavras-chave em um
            *loop*](#pesquisando-várias-palavras-chave-em-um-loop)
  - [Salve os dados em uma planilha:](#salve-os-dados-em-uma-planilha)
      - [Excel](#excel)
      - [Google Spreadsheets:](#google-spreadsheets)

# Introdução

Este repositório apresenta uma função em `R` que armazena os [*feeds
RSS*](https://pt.wikipedia.org/wiki/RSS) do [Google
Notícias](https://news.google.com/) ou do [Google
Alerts](https://www.google.com.br/alerts) em uma planilha estruturada.

# Baixe o script

Você pode baixar o script [CLICANDO AQUI](.R/google_rss.R).

# Pacotes necessários

Para rodar a função em seu computador, você vai precisar das seguintes
bibliotecas:

``` r
library(tidyverse) # conjunto de pacotes de manipulação de dados no R
library(lubridate) # para formatos de datas
library(xml2)      # para raspar os dados dos feeds
library(httr)      # para raspar os dados dos feeds
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

### Link go Google Alert

ATENÇÃO: Esse link expira a cada 24 horas.

``` r
# link direto de um feed recebido pelo e-mail do Google Alerts:
ex1 <- google_rss("https://www.google.com/alerts/feeds/05043731044875902072/152558739277222634")

glimpse(ex1)
```

### Google News RSS

O RSS do Google News fica em uma URL específica, com padrão:
`https://news.google.com/rss/search?q={_termo_de_pesquisa_}`. Os
resultados são os mesmos conteúdos de uma busca comum no [Google
News](https://news.google.com). A diferença é que o RSS fica em uma
página XML.

``` r
# link direto de um feed do Google News:
ex2 <- google_rss("https://news.google.com/rss/search?q=%22bob%20marley%22when%3A1d&hl=pt-BR&gl=BR&ceid=BR%3Apt-419")
#> 
#> 
#> Link direto de RSS:
#>  https://news.google.com/rss/search?q=%22bob%20marley%22when%3A1d&hl=pt-BR&gl=BR&ceid=BR%3Apt-419

glimpse(ex2)
#> Rows: 3
#> Columns: 11
#> $ termo             <chr> "bob marley", "bob marley", "bob marley"
#> $ data_alerta       <dttm> 2020-09-10 00:27:13, 2020-09-10 00:27:13, 2020-0...
#> $ titulo_da_materia <chr> "Festival Internacional de Cerveja e Cultura será...
#> $ veiculo           <chr> "O Tempo", "Portal R10", "Virou Pauta"
#> $ data_da_materia   <dttm> 2020-09-09 19:59:00, 2020-09-09 10:31:00, 2020-0...
#> $ dia               <int> 9, 9, 9
#> $ mes               <dbl> 9, 9, 9
#> $ ano               <dbl> 2020, 2020, 2020
#> $ dia_semana        <chr> "quarta-feira", "quarta-feira", "quarta-feira"
#> $ link              <chr> "https://www.otempo.com.br/opiniao/cerveja-e-cult...
#> $ host              <chr> "www.otempo.com.br", "www.portalr10.com", "viroup...
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
#> Rows: 10
#> Columns: 11
#> $ termo             <chr> "black lives matter", "black lives matter", "blac...
#> $ data_alerta       <dttm> 2020-09-10 00:27:14, 2020-09-10 00:27:14, 2020-0...
#> $ titulo_da_materia <chr> "American Airlines libera pin do Black Lives Matt...
#> $ veiculo           <chr> "AEROIN", "Aventuras na História", "Ciência Estad...
#> $ data_da_materia   <dttm> 2020-09-09 08:57:00, 2020-09-09 08:49:46, 2020-0...
#> $ dia               <int> 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
#> $ mes               <dbl> 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
#> $ ano               <dbl> 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2...
#> $ dia_semana        <chr> "quarta-feira", "quarta-feira", "quarta-feira", "...
#> $ link              <chr> "https://www.aeroin.net/american-airlines-criara-...
#> $ host              <chr> "www.aeroin.net", "aventurasnahistoria.uol.com.br...
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
#> $ data_alerta       <dttm> 2020-09-10 00:27:15, 2020-09-10 00:27:15, 2020-0...
#> $ titulo_da_materia <chr> "Funcionários fazem greve e cobram a NBA contra r...
#> $ veiculo           <chr> "globoesporte.com", "Revista Fórum", "Sputnik Bra...
#> $ data_da_materia   <dttm> 2020-08-28 04:00:00, 2020-08-27 04:00:00, 2020-0...
#> $ dia               <int> 28, 27, 27, 3, 27, 3, 28, 4, 2, 4, 28, 28, 29, 27...
#> $ mes               <dbl> 8, 8, 8, 9, 8, 9, 8, 9, 9, 9, 8, 8, 8, 8, 9, 9, 8...
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
#> Rows: 3
#> Columns: 11
#> $ termo             <chr> "Fabrício Queiroz", "Fabrício Queiroz", "Fabrício...
#> $ data_alerta       <dttm> 2020-09-10 00:27:16, 2020-09-10 00:27:16, 2020-0...
#> $ titulo_da_materia <chr> "Lava Jato diz que Wassef recebeu R$ 2,6 mi desvi...
#> $ veiculo           <chr> "UOL Notícias", "Tribuna do Paraná", "Jornal Toda...
#> $ data_da_materia   <dttm> 2020-09-09 21:35:59, 2020-09-09 21:38:00, 2020-0...
#> $ dia               <int> 9, 9, 9
#> $ mes               <dbl> 9, 9, 9
#> $ ano               <dbl> 2020, 2020, 2020
#> $ dia_semana        <chr> "quarta-feira", "quarta-feira", "quarta-feira"
#> $ link              <chr> "https://noticias.uol.com.br/politica/ultimas-not...
#> $ host              <chr> "noticias.uol.com.br", "www.tribunapr.com.br", "w...
```

### Pesquisando várias palavras-chave em um *loop*

Faça várias buscas ao mesmo tempo (também funciona com links):

``` r
# Crie uma lista com os termos que deseja pesquisar:
lista_de_termos <- c(
  "Transparência Brasil",
  "Manoel Galdino",
  "Juliana Sakai"
)

# rode a lista nesse loop:
varias_pesquisas <- lista_de_termos %>% 
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
varias_pesquisas
#> # A tibble: 11 x 11
#>    termo data_alerta         titulo_da_mater~ veiculo data_da_materia       dia
#>    <chr> <dttm>              <chr>            <chr>   <dttm>              <int>
#>  1 Tran~ 2020-09-10 00:27:16 Transparência B~ Portal~ 2020-09-03 14:57:10     3
#>  2 Tran~ 2020-09-10 00:27:16 Cancelamento do~ MixVale 2020-09-09 13:31:40     9
#>  3 Tran~ 2020-09-10 00:27:16 Desembargadora ~ The In~ 2020-09-08 02:14:00     8
#>  4 Tran~ 2020-09-10 00:27:16 Nota de R$ 200 ~ Revist~ 2020-09-03 12:09:09     3
#>  5 Tran~ 2020-09-10 00:27:16 Coordenador da ~ G1      2020-09-04 22:07:00     4
#>  6 Tran~ 2020-09-10 00:27:16 Brasil atinge m~ Jornal~ 2020-09-04 12:24:00     4
#>  7 Tran~ 2020-09-10 00:27:16 Cegos vão ao Co~ O Popu~ 2020-09-04 18:38:00     4
#>  8 Tran~ 2020-09-10 00:27:16 Regra geral par~ MixVale 2020-09-05 17:30:58     5
#>  9 Mano~ 2020-09-10 00:27:17 Cancelamento do~ MixVale 2020-09-09 13:31:40     9
#> 10 Mano~ 2020-09-10 00:27:17 Regra geral par~ MixVale 2020-09-05 17:30:58     5
#> 11 Juli~ 2020-09-10 00:27:17 Desembargadora ~ The In~ 2020-09-08 02:14:00     8
#> # ... with 5 more variables: mes <dbl>, ano <dbl>, dia_semana <chr>,
#> #   link <chr>, host <chr>
```

# Salve os dados em uma planilha:

## Excel

O problema aqui é que você só pode reescrever a planilha adicionando
nova aba. Caso contrário sempre terá que criar outra.

``` r
# Salvando em excel:
nova_aba <- paste("Pesquisa em", Sys.Date())

library(xlsx)

write.xlsx(as.data.frame(varias_pesquisas),
           file = "./data/varias_pesquisas.xlsx",
           # insere os dados em uma aba sem apagar o que já tinha antes:
           sheetName = nova_aba,
           append = TRUE)
```

## Google Spreadsheets:

Siga os passos (atenção ao item 3\!):

1.  Vá no Google Drive e **crie uma planilha em branco**;
2.  Copie o **link da planilha**;
3.  Crie uma aba nova com
    [`sheet_write()`](https://googlesheets4.tidyverse.org/reference/sheet_write.html)
    e **não volte a usar essa função** (senão você reescreve em cima da
    que estava salva);
4.  Atualiza a planilha com novos dados com
    [`sheet_append()`](https://googlesheets4.tidyverse.org/reference/sheet_append.html).

<!-- end list -->

``` r
library(googlesheets4)

# link da planilha criada no drive:
link_da_planilha <- "https://docs.google.com/spreadsheets/d/1-sjp0oF3RXDMYwT3IKWEF7y6Cp-ahBkD_MYti5ZejX8/edit?usp=sharing"

# USE SOMENTE UMA VEZ PARA CRIAR A PLANILHA:
sheet_write(data = varias_pesquisas,
            ss = link_da_planilha,
            sheet = "Pesquisa")

# SEMPRE QUE FOR ATUALIZAR USA ESTA: 
sheet_append(data = ex2,
             ss = link_da_planilha,
             sheet = "Pesquisa")
```

FIM :smile:
