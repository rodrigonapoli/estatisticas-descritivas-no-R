# Análises Telma (nutrição) - dados "novos" de setembro de 2022
# Aqui há dados pré-operatórios da Telma e da Marilia
# Análises descritivas

library("dplyr")
library ("ggplot2")
library("ggpubr")
library("psych")
library("writexl")
library("tidyverse")
library("readxl") #para ler o arquivo em excel

#abrindo o arquivo em excel
dados <- read_excel("/Users/rodrigonapoli/Dropbox/ Assessoria/ 2022/Telma (Nutri)/Novas analises (Set_22)/DADOS BIO TELMA 25.09.xlsx", sheet = 3)

#corrigindo os tipos de variável
dados$paciente <- NULL
dados$sexo <- as.factor(dados$sexo)
dados$grupo <- as.factor(dados$grupo)
dados$diabetes <- as.factor(dados$diabetes)
dados$esteatose <- as.factor(dados$esteatose)
dados$esteatose_dic <- as.factor(dados$esteatose_dic)
dados$bard <- as.factor(dados$bard)
dados$risco <- as.factor(dados$risco)

str(dados)


##############################
##         GRAFICOS         ##
##############################  

#gráficos preto e branco

grafico <- ggboxplot(dados, x = "esteatose", y = "hemoglobina", 
          ylab = "Hemoglobina", xlab = "Grau de Esteatose",)
gráfico

#gráfico colorido
peso <- ggboxplot(dados, x = "esteatose", y = "peso", 
                 color = "esteatose", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                 ylab = "peso", xlab = "Grau de Esteatose")

imc <- ggboxplot(dados, x = "esteatose", y = "imc", 
                         color = "esteatose", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                         ylab = "imc", xlab = "Grau de Esteatose")

rce <- ggboxplot(dados, x = "esteatose", y = "rce", 
                 color = "esteatose", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                 ylab = "rce", xlab = "Grau de Esteatose")

cp <- ggboxplot(dados, x = "esteatose", y = "cp", 
                 color = "esteatose", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                 ylab = "cp", xlab = "Grau de Esteatose")

gordura <- ggboxplot(dados, x = "esteatose", y = "gordura", 
                color = "esteatose", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                ylab = "gordura", xlab = "Grau de Esteatose")

tri <- ggboxplot(dados, x = "esteatose", y = "tri", 
                     color = "esteatose", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     ylab = "tri", xlab = "Grau de Esteatose")
vitamina_d <- ggboxplot(dados, x = "esteatose", y = "vitamina_d", 
                 color = "esteatose", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                 ylab = "vitamina_d", xlab = "Grau de Esteatose")

ferritina <- ggboxplot(dados, x = "esteatose", y = "ferritina", 
                 color = "esteatose", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                 ylab = "ferritina", xlab = "Grau de Esteatose")

tgo <- ggboxplot(dados, x = "esteatose", y = "tgo", 
                       color = "esteatose", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                       ylab = "tgo", xlab = "Grau de Esteatose")

tgp <- ggboxplot(dados, x = "esteatose", y = "tgp", 
                       color = "esteatose", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                       ylab = "tgp", xlab = "Grau de Esteatose")

tgo_tgp <- ggboxplot(dados, x = "esteatose", y = "tgo_tgp", 
                 color = "esteatose", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                 ylab = "tgo_tgp", xlab = "Grau de Esteatose")

insulina <- ggboxplot(dados, x = "esteatose", y = "insulina", 
                     color = "esteatose", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     ylab = "insulina", xlab = "Grau de Esteatose")

homa <- ggboxplot(dados, x = "esteatose", y = "homa", 
                     color = "esteatose", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     ylab = "homa", xlab = "Grau de Esteatose")

glicose <- ggboxplot(dados, x = "esteatose", y = "glicose", 
                  color = "esteatose", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                  ylab = "glicose", xlab = "Grau de Esteatose")

hemoglobina <- ggboxplot(dados, x = "esteatose", y = "hemoglobina", 
                     color = "esteatose", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     ylab = "hemoglobina", xlab = "Grau de Esteatose")

conicidade <- ggboxplot(dados, x = "esteatose", y = "conicidade", 
                     color = "esteatose", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("Leve", "Moderado", "Severo"),
                     ylab = "conicidade", xlab = "Grau de Esteatose")

# 2 figures arranged in 2 rows and 4 columns
ggarrange(peso, imc, rce, cp, gordura, tri, vitamina_d, ferritina + 
            rremove("x.text"), 
          ncol = 4, nrow = 2)

ggarrange(tgo, tgp, tgo_tgp, insulina, homa, glicose, hemoglobina, conicidade + 
            rremove("x.text"), 
          ncol = 4, nrow = 2)

# 4 figures arranged in 2 rows and 2 columns
ggarrange(peso, imc, rce, cp,
          common.legend = TRUE,
          legend = "none",
          ncol = 2, nrow = 2)

ggarrange(gordura, tri, vitamina_d, ferritina + 
          common.legend = TRUE,
          legend = "none",
          ncol = 2, nrow = 2)

ggarrange(tgo, tgp, tgo_tgp, insulina + 
            rremove("x.text"),
          common.legend = TRUE,
          legend = "none",
          ncol = 2, nrow = 2)

ggarrange(homa, glicose, hemoglobina, conicidade + 
            rremove("x.text"), 
          common.legend = TRUE,
          legend = "none",
          ncol = 2, nrow = 2)


#ggplot(dados, aes(x=esteatose, y=peso, fill=esteatose)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour='black')


##############################
##      QUANTITATIVAS      ##
##############################  
  
#criando um banco de dados somente com as variáveis quantitativas (e a esteatose como fator)
quantitativas <- dados %>% 
  select_if(is.numeric)
quantitativas$esteatose <- dados$esteatose
quantitativas$paciente <- NULL
str(quantitativas)
quantitativas <- as.data.frame(quantitativas)

#descrevendo as variáveis quantitativas (sem agrupamentos)
descritivas_quanti <- pastecs::stat.desc(quantitativas)
descritivas_quanti_var <- tibble::rownames_to_column(descritivas_quanti, "variável")
descritivas_quanti_var = descritivas_quanti_var[c(4,5,8,9,10),]
descritivas_quanti_var
# Exportando um data frame para o excel
writexl::write_xlsx(descritivas_quanti_var,"/Users/rodrigonapoli/Dropbox/ Assessoria/ 2022/Telma (Nutri)/Novas analises (Set_22)/descritivas_quanti.xlsx")


##############################
##      QUALITATIVAS        ##
##############################

#descrevendo as variáveis qualitativas (sem agrupamento)
qualitativas <- dados %>% 
  select_if(negate(is.numeric))
str(qualitativas)
qualitativas

#criando resultados de frequencias das variáveis qualitativas
descritivas_quali <- apply((qualitativas), 2, tabyl)
descritivas_quali_df <- ldply (descritivas_quali, data.frame) 
descritivas_quali_df$valid_percent <- NULL 
descritivas_quali_df

# Exportando um data frame para o excel
writexl::write_xlsx(descritivas_quali_df,"/Users/rodrigonapoli/Dropbox/ Assessoria/ 2022/Telma (Nutri)/Novas analises (Set_22)/descritivas_quali.xlsx")


##############################
##  POR ESTEATOSE - QUANTI  ##
##############################  
#descrevendo as variáveis segundo graus de esteatose
a <- psych::describeBy(quantitativas,
                  mat=TRUE,
                  digits=2,
                  group="esteatose")

b <- row.names(a)
a
descritivas_quanti_esteatose <- as.data.frame(b)
descritivas_quanti_esteatose <- plyr::rename(descritivas_quanti_esteatose, c("b" = "variavel")) #para renomear a coluna
descritivas_quanti_esteatose$esteatose <- a$group1
descritivas_quanti_esteatose$mean <- a$mean
descritivas_quanti_esteatose$sd <- a$sd
descritivas_quanti_esteatose$median <- a$median
descritivas_quanti_esteatose$min <- a$min
descritivas_quanti_esteatose$max <- a$max
descritivas_quanti_esteatose

# Exportando um data frame para o excel
writexl::write_xlsx(descritivas_quanti_esteatose,"/Users/rodrigonapoli/Dropbox/ Assessoria/ 2022/Telma (Nutri)/Novas analises (Set_22)/descritivas_quanti_por_esteatose.xlsx")


#gráfico de frequencias
ggplot(dados, aes(x=esteatose, fill=sexo))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_dodge(0.9), vjust=-0.5)+
  ylab('Percentagem (%)')
  #scale_y_continuous(labels = scales::percent) +
  #theme(panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA))
