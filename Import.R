library(readxl)

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
  dataArea <- read.csv2("area_UF_Brasil.csv", encoding="UTF-8")

  # limpeza e formatação da informação de Área
  dataArea$Área..Km2. <- gsub("\\.", "", dataArea$Área..Km2.)
  dataArea$Área..Km2. <- gsub(",", "\\.", dataArea$Área..Km2.)
  dataArea$Área..Km2. <- as.numeric(dataArea$Área..Km2.)
  
  # formatação da tabela final de áreas
  dataArea$Código.UF <- NULL
  colnames(dataArea) <- c("Unit", "Area")

  return (dataArea)
}

dataPopulation <- readPopulationData()
dataArea <- readAreaData()

dfFedUnits <- merge(dataPopulation, dataArea)
dfFedUnits <- dfFedUnits[,c(1,3,2,4)]

save(dfFedUnits, file="ibge.RData")