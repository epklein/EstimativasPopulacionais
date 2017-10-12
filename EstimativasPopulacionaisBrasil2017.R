#install.packages("tidyverse")

library(readxl)
library(ggplot2)

countryName <- c("Brasil")
regionName <- c("Região Norte", "Região Nordeste", "Região Sudeste",
                "Região Sul", "Região Centro-Oeste")

# ----- FUNÇÕES DE IMPORTAÇÃO DE DADOS -----
readPopulationData <- function() {
  
  # carga das estimativas populacionais e formatação
  # uso da biblioteca readxl para leitura diretamente de planilha Excel
  rawDataPop <- read_excel("estimativa_dou_2017.xls",
                     sheet <- "BRASIL E UFs",
                     range="A2:C35")
  
  # exclusão de coluna vazia e acerto dos nomes das colunas
  rawDataPop$X__1 <- NULL
  colnames(rawDataPop) <- c("Unit", "Population")
  
  # limpeza e formatação da informação de População
  rawDataPop$Population <- gsub(" \\(\\*\\)", "", rawDataPop$Population)
  rawDataPop$Population <- gsub(" \\(\\*\\*\\)", "", rawDataPop$Population)
  rawDataPop$Population <- gsub("\\.", "", rawDataPop$Population)
  
  rawDataPop$Population <- as.integer(rawDataPop$Population)

  #formatação da tabela final de populações
  
  dataPop <- NULL
  
  #região Norte
  tempDF <- rawDataPop[c(3:9),]
  tempDF$Region <- regionName[1]
  dataPop <- rbind(dataPop, tempDF)
  
  #região Nordeste
  tempDF <- rawDataPop[c(11:19),]
  tempDF$Region <- regionName[2]
  dataPop <- rbind(dataPop, tempDF)
  
  #região Sudeste
  tempDF <- rawDataPop[c(21:24),]
  tempDF$Region <- regionName[3]
  dataPop <- rbind(dataPop, tempDF)
  
  #região Sul
  tempDF <- rawDataPop[c(26:28),]
  tempDF$Region <- regionName[4]
  dataPop <- rbind(dataPop, tempDF)
  
  #região Centro-Oeste
  tempDF <- rawDataPop[c(30:33),]
  tempDF$Region <- regionName[5]
  dataPop <- rbind(dataPop, tempDF)
  
  dataPop$Region <- factor(dataPop$Region, levels=regionName)
  dataPop$Unit <- factor(dataPop$Unit)
  
  return (dataPop)
}
readAreaData <- function() {
  
  # carga das medidas de área das Unidades Federativas
  dataArea <- read.csv("area_UF_Brasil.csv", sep=";")

  # limpeza e formatação da informação de Área
  dataArea$Área..Km2. <- gsub("\\.", "", dataArea$Área..Km2.)
  dataArea$Área..Km2. <- gsub(",", "\\.", dataArea$Área..Km2.)
  dataArea$Área..Km2. <- as.numeric(dataArea$Área..Km2.)
  
  # formatação da tabela final de áreas
  dataArea$Código.UF <- NULL
  colnames(dataArea) <- c("Unit", "Area")

  return (dataArea)
}

# importação dos dados
dataPop <- readPopulationData()
dataArea <- readAreaData()

# ----- CONSOLIDAÇÃO DE DADOS -----

# data.frame das unidades federativas do país
fedUnitsDF <- merge(dataPop, dataArea)

# colunas ordenadas e filtradas para fins de plotagem de população
fedUnitsDF$UnitOrdPop <- factor(fedUnitsDF$Unit,
                                 levels=fedUnitsDF$Unit[order(fedUnitsDF$Population)], 
                                 ordered=TRUE)
fedUnitsDF$UnitNamePopPlot <- as.character(fedUnitsDF$Unit)
fedUnitsDF$UnitNamePopPlot[which(fedUnitsDF$Population < 2.5E+6)] <- NA

# colunas ordenadas e filtradas para fins de plotagem de área territorial
fedUnitsDF$UnitOrdArea <- factor(fedUnitsDF$Unit,
                                 levels=fedUnitsDF$Unit[order(fedUnitsDF$Area)], 
                                 ordered=TRUE)
fedUnitsDF$UnitNameAreaPlot <- as.character(fedUnitsDF$Unit)
fedUnitsDF$UnitNameAreaPlot[which(fedUnitsDF$Area < 1E+5)] <- NA

# data.frame das regiões do país
regionsDF <- aggregate(list(Population=fedUnitsDF$Population, Area=fedUnitsDF$Area),
                       list(Region=fedUnitsDF$Region),
                       sum)

# ----- PLOTAGEM DOS GRÁFICOS COM GGPLOT2 -----

# Gráfico das populações por região / unidade federativa

ggplot(data=fedUnitsDF, 
       aes(x=Region, weights=Population / 1E+6)) +
  geom_bar(aes(fill=UnitOrdPop), color="Black") +
  geom_text(aes(x=Region, y=Population / 1E+6, group=UnitOrdPop, label=UnitNamePopPlot),
            position = position_stack(vjust = 0.5), size=3.3) +
  guides(fill=FALSE) +
  xlab("Região do Brasil") +
  ylab("Milhões de habitantes")

# Gráfico de áreas por região / unidade federativa

ggplot(data=fedUnitsDF,
       aes(x=Region, weights=Area / 1E+3)) +
  geom_bar(aes(fill=UnitOrdArea), color="Black") +
  geom_text(aes(x=Region, y=Area / 1E+3, group=UnitOrdArea, label=UnitNameAreaPlot),
            position = position_stack(vjust = 0.5), size=3.3) +
  guides(fill=FALSE) +
  xlab("Região do Brasil") +
  ylab("Área (mil Km²)")

# Gráfico das densidades demográficas das regiões do país

ggplot(data=regionsDF, aes(x=Region)) +
  geom_col(aes(y=Population/Area, fill=Region)) +
  geom_text(aes(x=Region, y=Population / Area, label=round(Population / Area, 2)),
            size=3.3, vjust=-1) +
  guides(fill=FALSE) +
  ylim(0, 100) +
  xlab("Região do Brasil") + ylab("Densidade demográfica (Hab/Km²)")