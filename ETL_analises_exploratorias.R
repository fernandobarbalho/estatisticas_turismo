library(tidyverse)
library(ggbeeswarm)



endereco<- "https://dados.turismo.gov.br/gl/dataset/184e0ddd-7eaf-488d-ad84-0331219d6e99/resource/53dd37d0-42fe-4c23-9a9a-8248191d06e8/download/chegadas-2025.csv"


download.file(endereco, destfile = "chegadas_turismo_2025.csv", mode = "wb")

chegadas_turismo_2025 <- read_delim("chegadas_turismo_2025.csv", 
                                    delim = ";", escape_double = FALSE, locale = locale(encoding = "LATIN1"), 
                                    trim_ws = TRUE) %>%
  janitor::clean_names()

total<- sum(chegadas_turismo_2025$chegadas)


chegadas_turismo_2025 %>%
  filter(nome_pais_correto == "Venezuela") %>%
  summarise(sum(chegadas),
            sum(chegadas)/total)

169346/9287196

chegadas_turismo_2025 %>%
  summarise(quantidade = sum(chegadas),
            participacao =  quantidade/total,
            .by = nome_pais_correto) %>%
  slice_max(n=20, order_by = quantidade)





chegadas_2024 <- read_delim("chegadas_2024.csv", 
                            delim = ";", escape_double = FALSE, locale = locale(encoding = "LATIN1"), 
                            trim_ws = TRUE) %>%
  janitor::clean_names()


total_2024<-
  sum(chegadas_2024$chegadas)


(169346-8637)/(total - total_2024)

(total/total_2024)-1

chegadas_2024 %>%
  summarise(quantidade = sum(chegadas),
            participacao = quantidade/total_2024,
            .by = pais) %>%
  slice_max(n=10, order_by = quantidade)





chegadas_turismo_2025 %>%
  summarise(quantidade = sum(chegadas),
            .by = nome_pais_correto) %>%
  rename(pais =nome_pais_correto ) %>%
  mutate(ano =as.factor(2025)  ) %>%
  filter(quantidade < 1e6) %>%
  slice_max(n=40, order_by = quantidade) %>%
  bind_rows(
    chegadas_2024 %>%
      summarise(quantidade = sum(chegadas),
                .by = pais) %>%
      mutate(ano =as.factor(2024)  )%>%
      filter(quantidade < 1e6) %>%
      slice_max(n=40, order_by = quantidade)
  ) %>%
  ggplot(aes(x= ano, y= quantidade)) +
  geom_violin(fill= NA) +
  geom_boxplot(fill= NA) 


###Tratamento de STI

endereco<- "https://servicos.dpf.gov.br/dadosabertos/STI/STI_MOVIMENTO_2024_01.csv"

download.file(endereco, destfile = "sti_janeiro_2024.csv", mode = "wb", method = "auto")

sti_janeiro_2024 <- read_delim("sti_janeiro_2024.csv", 
                               delim = ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                               trim_ws = TRUE) %>%
  janitor::clean_names()


glimpse(sti_janeiro_2024)

sti_janeiro_2024 %>%
  filter(nacionalidade == "VENEZUELA") %>%
  summarise(quantidade = sum(total),
            .by = classificacao)


dados_sti_2024_2025<-
  
  purrr::map_dfr(2024:2025, function(ano){
    purrr::map_dfr(str_pad("1":"12",width = 2, side = "left", pad = "0" ), function(mes){
      
      print(ano)
      print(mes)
      
      endereco<- paste0("https://servicos.dpf.gov.br/dadosabertos/STI/STI_MOVIMENTO_",ano,"_", mes, ".csv")  
      
      download.file(endereco, destfile = "dados_sti.csv", mode = "wb", method = "auto")
      
      dados_sti <- 
        read_delim("dados_sti.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                   trim_ws = TRUE) %>%
        janitor::clean_names()
      
      dados_sti$mes<- mes
      dados_sti$ano<- ano
      dados_sti
      
    })
  })

dados_sti_2024_2025 %>%
  filter(nacionalidade == "VENEZUELA",
         tipo == "ENTRADA") %>%
  summarise(quantidade = sum(total),
            .by =  c(ano, classificacao) )



dados_sti_2024_2025 %>%
  filter(nacionalidade == "ARGENTINA",
         tipo == "ENTRADA") %>%
  summarise(quantidade = sum(total),
            .by =  c(ano, classificacao) )



chegadas_turismo_2024_2025<-
  chegadas_turismo_2025 %>%
  summarise(quantidade = sum(chegadas),
            .by = nome_pais_correto) %>%
  rename(pais =nome_pais_correto ) %>%
  mutate(ano =as.factor(2025) ) %>%
  bind_rows(
    chegadas_2024 %>%
      summarise(quantidade = sum(chegadas),
                .by = pais) %>%
      mutate(ano =as.factor(2024)  )  ) 


dados_pf_tur<-
  chegadas_turismo_2024_2025 %>%
  mutate(pais =  str_to_upper(pais),
         ano = as.character(ano),
         ano = as.numeric(ano)) %>%
  inner_join(
    dados_sti_2024_2025 %>%
      rename(pais = nacionalidade) %>% 
      filter(tipo == "ENTRADA",
             classificacao == "VISITANTE") %>%
      summarise(quantidade_entrada_pf = sum(total),
                .by = c(ano, pais))
  ) %>%
  mutate(proporcao_tur_pf = quantidade/quantidade_entrada_pf)


dados_pf_tur %>%
  #filter(quantidade > 1000) %>%
  mutate(ano = factor(ano)) %>%
  ggplot(aes(x= ano, y= proporcao_tur_pf)) +
  geom_boxplot(fill= NA,
               outliers = FALSE) +
  geom_quasirandom()



chegadas_turismo_2024_2025 %>%
  ggplot(aes(x= ano, y= quantidade)) +
  geom_boxplot(fill= NA,
               outliers = FALSE) +
  geom_quasirandom() +
  scale_y_log10()


chegadas_turismo_2024_2025 %>%
  summarise(sum(quantidade),
            mean(quantidade),
            median(quantidade),
            n(),
            .by = ano)

fab<-
  chegadas_turismo_2024_2025 %>%
  #group_by(ano) %>%
  #slice_max(n=40, order_by = quantidade) %>%
  pivot_wider(names_from = ano, values_from = quantidade, names_prefix = "ano_") %>%
  mutate(variacao = (ano_2025/ano_2024)-1  )