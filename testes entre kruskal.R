# Análises Telma (nutrição) - dados "novos" de setembro de 2022
# Aqui há dados pré-operatórios da Telma e da Marilia
# Análises comparativas de kruskall-Wallis

# Bibliotecas
library ("dunn.test") #para rodar o post hoc de dunn
library("FSA") #para rodar o post hoc de dunn
library("readxl") #para ler o arquivo em excel

#abrindo o arquivo em excel
dados <- read_excel("/Users/rodrigonapoli/Dropbox/ Assessoria/ 2022/Telma (Nutri)/Novas analises (Set_22)/DADOS BIO TELMA 25.09.xlsx", sheet = 3)

#corrigindo os tipos de variável
dados$paciente <- NULL
dados$grup <- NULL
dados$grupo <- as.factor(dados$grupo)
dados$esteatose <- as.factor(dados$esteatose)
dados$esteatose_dic <- as.factor(dados$esteatose_dic)
dados$sexo <- as.factor(dados$sexo)
dados$risco <- as.factor(dados$risco)
dados$diabetes <- as.factor(dados$diabetes)
dados$bard <- as.factor(dados$diabetes)
str(dados)

# Analisando as variáveis segundo o teste de Kruskall-Wallis e criando um dataframe
# com os resultados
#peso
a <- kruskal.test(dados$peso ~ dados$esteatose)

a$p.value
resultado <- as.data.frame(a$data.name)
resultado$p <- a$p.value
resultado

#imc
a <- kruskal.test(dados$imc ~ dados$esteatose)
resultado[nrow(resultado) + 1,] <- c(a$data.name, a$p.value)

#rce
a <- kruskal.test(dados$rce ~ dados$esteatose)
resultado[nrow(resultado) + 1,] <- c(a$data.name, a$p.value)

#cp
a <- kruskal.test(dados$cp ~ dados$esteatose)
resultado[nrow(resultado) + 1,] <- c(a$data.name, a$p.value)

#gordura_%
a <- kruskal.test(dados$gordura ~ dados$esteatose)
resultado[nrow(resultado) + 1,] <- c(a$data.name, a$p.value)

#tri
a <- kruskal.test(dados$tri ~ dados$esteatose)
resultado[nrow(resultado) + 1,] <- c(a$data.name, a$p.value)

#vitamina_d
a <- kruskal.test(dados$vitamina_d ~ dados$esteatose)
resultado[nrow(resultado) + 1,] <- c(a$data.name, a$p.value)

#ferritina
a <- kruskal.test(dados$ferritina ~ dados$esteatose)
resultado[nrow(resultado) + 1,] <- c(a$data.name, a$p.value)

#tgo
a <- kruskal.test(dados$tgo ~ dados$esteatose)
resultado[nrow(resultado) + 1,] <- c(a$data.name, a$p.value)

#tgp
a <- kruskal.test(dados$tgp ~ dados$esteatose)
resultado[nrow(resultado) + 1,] <- c(a$data.name, a$p.value)


#tgo_tgp
a <- kruskal.test(dados$tgo_tgp ~ dados$esteatose)
resultado[nrow(resultado) + 1,] <- c(a$data.name, a$p.value)

#insulina
a <- kruskal.test(dados$insulina ~ dados$esteatose)
resultado[nrow(resultado) + 1,] <- c(a$data.name, a$p.value)

#homa
a <- kruskal.test(dados$homa ~ dados$esteatose)
resultado[nrow(resultado) + 1,] <- c(a$data.name, a$p.value)

#glicose
a <- kruskal.test(dados$glicose ~ dados$esteatose)
resultado[nrow(resultado) + 1,] <- c(a$data.name, a$p.value)


#hemoglobina
a <- kruskal.test(dados$hemoglobina ~ dados$esteatose)
resultado[nrow(resultado) + 1,] <- c(a$data.name, a$p.value)

#conicidade
a <- kruskal.test(dados$conicidade ~ dados$esteatose)
resultado[nrow(resultado) + 1,] <- c(a$data.name, a$p.value)

# Observando os resultados
resultado$p <- as.numeric(resultado$p)
resultado

# Enviando para o excel
writexl::write_xlsx(resultado,"/Users/rodrigonapoli/Dropbox/ Assessoria/ 2022/Telma (Nutri)/Novas analises (Set_22)/resultados2.xlsx")

#apresentado somente resulados significativos
resultado_significativo <- resultado[(resultado$p<=0.05),]
resultado_significativo  

#desenvolvendo os testes a posteriori de Dunn

pos <- dunnTest(dados$peso, dados$esteatose, method = "bonferroni")
pos$res$var <- c("peso")
multiple <- as.data.frame (pos$res)

pos <- dunnTest(dados$rce, dados$esteatose, method = "bonferroni")
pos$res$var <- c("rce")
multiple <- rbind(multiple, pos$res)

pos <- dunnTest(dados$cp, dados$esteatose, method = "bonferroni")
pos$res$var <- c("cp")
multiple <- rbind(multiple, pos$res)

pos <- dunnTest(dados$ferritina, dados$esteatose, method = "bonferroni")
pos$res$var <- c("ferritina")
multiple <- rbind(multiple, pos$res)

pos <- dunnTest(dados$tgo, dados$esteatose, method = "bonferroni")
pos$res$var <- c("tgo")
multiple <- rbind(multiple, pos$res)

pos <- dunnTest(dados$tgp, dados$esteatose, method = "bonferroni")
pos$res$var <- c("tgp")
multiple <- rbind(multiple, pos$res)

pos <- dunnTest(dados$tgo_tgp, dados$esteatose, method = "bonferroni")
pos$res$var <- c("tgo_tgp")
multiple <- rbind(multiple, pos$res)

pos <- dunnTest(dados$glicose, dados$esteatose, method = "bonferroni")
pos$res$var <- c("glicose")
multiple <- rbind(multiple, pos$res)

pos <- dunnTest(dados$hemoglobina, dados$esteatose, method = "bonferroni")
pos$res$var <- c("hemogloblina")
multiple <- rbind(multiple, pos$res)

# Enviando para o excel
writexl::write_xlsx(multiple,"/Users/rodrigonapoli/Dropbox/ Assessoria/ 2022/Telma (Nutri)/Novas analises (Set_22)/resultados3.xlsx")

