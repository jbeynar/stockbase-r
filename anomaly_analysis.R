library("elasticsearchr")
library("quantmod")
library("forecast")
library("TTR")

# Before start ensure variables esUrl and indexName

simulateStrategy <- function (symbol){
  # TODO move this params as functions args
  emaLength <- 20
  stdDevLength <- 90
  transactionMaxLength <- 90
  profitMargin <- 0.07
  anomalySpread <- 2.0
 
  filterQuery <- sprintf('{
          "bool": {
              "must": [
                  {
                    "term": {
                      "symbol": "%s"
                    }
                  },
                	{
                    "range":{
                      "date":{
                        "gte": "2014-01-01",
                        "lte": "2017-12-31"
                      }
                    }
                  }
              ]
          }
      }', symbol)
  
  match <- query(filterQuery)
  sort <- sort_on('{"date": "asc"}')
  
  data <- tryCatch({
    elastic(esUrl, indexName, indexName) %search% (match + sort)
  }, error = function(e){
    print(sprintf("Skip on %s %s", symbol, e))
    return(NA)
  })
  
  if(is.na(data) || nrow(data) < max(c(stdDevLength, emaLength))){
    return(NA)
  }
  
  data.xts <- xts(x = data$close, order.by = as.Date(data$date))
  ema.xts <- xts(x = EMA(data.xts, emaLength), order.by = as.Date(data$date))
  emaDiff <- ema.xts - data.xts
  movSD <-na.locf(runSD(data.xts, stdDevLength), fromLast = T)
  
  # Anomaly startPoints detection
  startPoints = index(emaDiff[emaDiff>anomalySpread*movSD])
  
  transactions <- lapply(startPoints, function(s){
    chunk <- window(data.xts, start = s, end = s+transactionMaxLength)
    sp <- first(chunk)
    targetPrice <- as.numeric(sp*(1+profitMargin))
    ep <- first(chunk[chunk>=targetPrice])
    targetPriceReached <- T
    if(!length(ep)){
      ep <- last(chunk)
      targetPriceReached <- F
    }
    yield <- coredata(ep)-coredata(sp)
    yieldp <- yield/coredata(sp)
    
    list(startPoints = sp, 
         endPoint = ep, 
         timeFrame = index(ep)-index(sp), 
         yield = yield, 
         yieldRatio = yieldp, 
         tpr = targetPriceReached)
  })
  
  # Transactions simulation
  cash <- 1.0
  for(tr in transactions){
    cash <- cash*(1 + tr$yieldRatio)
  }
  
  # Set plot placeholders as 2 rows, 1 column
  png(sprintf("plots/%s.png", symbol), width = 1920, height = 1080)
  
  par(mfrow = c(2,1))
  graph1 <- plot(data.xts, main = sprintf("%s price and anomaly (%d) detection, total yield = %.2f%%", symbol, length(startPoints), (cash-1)*100), col = "black")
  graph1 <- lines(ema.xts, col = "blue")
  graph1 <- lines(ema.xts-movSD, col = "blue", lty=2)
  graph1 <- lines(ema.xts+movSD, col = "blue", lty=2)
  graph1 <- points(data.xts[startPoints], pch=16, col="red")
  print(graph1)
  
  
  # TODO 1. save graph as png instead of printing
  
  # TODO 2. draw end points 
  
  graph2 <- plot(emaDiff, type="l", col = "blue", main= "Price spread")
  graph2 <- points(emaDiff[startPoints], pch=16, col="red")
  print(graph2)
  dev.off()
  as.numeric(cash)
}

# TODO 3. do ES query (agg or facet) for symbols
symbols = c('11-BIT-STUDIOS', 'AAT', 'ASSECO-POLAND', 'AMICA', 'APR', 'ARTIFEX', 'ATM-GRUPA', 'ATLAS-ESTATES', 'ATLANTA-POLAND', 'ATREM', 'AUGAGROUP', 'BUDIMEX', 'BORYSZEW', 'BYTOM', 'CCC', 'CYFROWY-POLSAT', 'CUBE-ITG', 'CTS', 'DOM-DEVELOPMENT', 'ELKOP', 'ENEA', 'ENEL-MED', 'E-STAR', 'FERRUM', 'FAMUR', 'FON', 'FASING', 'GRAVITON-CAPITAL', 'HELIO', 'IALBGR', 'IBSM', 'INDYGOTECH-MINERALS', 'INDATA-SOFTWARE', 'IZOSTAL', 'JWW-INVEST', 'K2-INTERNET', 'KCI', 'KDM-SHIPPING-PUBLIC-LIMITED', 'KINO-POLSKA-TV', 'KRUSZWICA', 'KETY', 'MABION', 'MCI', 'SKYSTONE', 'BANK-MILLENNIUM', 'MILKILAND', 'MNI', 'MOL-MAGYAR-OLAJ', 'MORIZON', 'ODLEWNIE-POLSKIE', 'OPEN-FINANCE', 'ORION-INVESTMENT', 'PATENTUS', 'PBG', 'PRIME-CAR-MANAGEMENT', 'ROKITA', 'PRAIRIE-MINING-LIMITED', 'POLSKA-GRUPA-ODLEWNICZA', 'PROJPRZEM', 'PKN-ORLEN', 'PKO', 'PKPCARGO', 'PRIMA-MODA', 'PEMUG', 'PROCAD', 'QUANTUM-SOFTWARE', 'QUERCUS', 'RADPOL', 'SCO-PAK', 'SETANTA-FINANCE', 'SFINKS', 'SYGNITY', 'SADOVAYA', 'SKOTAN', 'STOMIL-SANOK', 'SYNTHOS', 'SYNEKTIK', 'SOLAR-COMPANY', 'SOPHARMA', 'STALPRODUKT', 'SWISSMED-CENTRUM-ZDROWIA', 'TIM', 'TRANS-POLONIA', 'TESGAS', 'UNICREDIT', 'WADEX', 'WANDALEX', 'WISTIL', 'WARIMPEX', 'ZAMET-INDUSTRY', 'ZREMB-CHOJNICE', 'ZYWIEC', 'AVIAAM', 'ABC-DATA', 'ABE', 'AGORA', 'ALCHEMIA', 'WIND-MOBILE', 'ALMA', 'ALIOR-BANK', 'ALUMETAL', 'APLISENS', 'APATOR', 'ARH', 'ARTERIA', 'ARCTIC-PAPER', 'ZAKLADY-AZOTOWE-TARNOW', 'B3SYSTEM', 'BETACOM', 'BUDOPOL-WROCLAW', 'BENEFIT-SYSTEMS', 'BGZ-BNP-PARIBAS', 'BANK-HANDLOWY', 'BUMECH', 'INTER-CARS', 'CI-GAMES', 'CLNPHARMA', 'CPD', 'CAPITAL-PARK', 'EKO-EXPORT', 'ELZAB', 'EMPERIA', 'ENERGA', 'ENERGOINSTAL', 'EUROPEJSKIE-CENTRUM-ODSZKODOWAN', 'EVEREST-INVESTMENTS', 'FEERUM', 'FORTUNA-ENTERTAINMENT', 'POLISH-SERVICES-GROUP', 'IMMOBILE', 'GEKOPLAST', 'GLOBAL-COSMED', 'GETIN-NOBLE-BANK', 'INVESTMENT-FRIENDS', 'IMMOFINANZ', 'INDUSTRIAL-MILK-COMPANY', 'IMPEL', 'INTER-RAO-LIETUVA', 'IZO-BLOK', 'HENRYK-KANIA', 'KGL', 'KRYNICKI-RECYKLING', 'KRKA', 'KRUK', 'LABO-PRINT', 'LOTOS', 'BOGDANKA', 'MEDICALGORITHMICS', 'MOJ', 'MOSTOSTAL-PLOCK', 'NTT', 'ORZEL-BIALY', 'ORCO-PROPERTY', 'OTMUCHOW', 'PBS-FINANSE', 'PEKABEX', 'PCC-EXOL', 'PEKAO', 'PGNIG', 'PLATYNOWE-INWESTYCJE', 'POLMED', 'ZPUE', 'RAINBOW-TOURS', 'RAFAKO', 'RELPOL', 'ROBYG', 'RONSON', 'SEKO', 'AS-SILVANO-FASHION', 'SKARBIEC-HOLDING', 'SELVITA', 'STALPROFIL', 'TALEX', 'TORPOL', 'TERMO-REX', 'UNIMA-2000', 'VISTULA', 'WASKO', 'WIELTON', 'WIRTUALNA-POLSKA', 'WORK-SERVICE', 'XTB', 'ZAKLADY-AZOTOWE-PULAWY', 'ZASTAL', 'STAPORKOW', '4FUN-MEDIA', 'ABADONRE', 'ABM-SOLID', 'AAT-HOLDING', 'ALTUS', 'AMBRA', 'ASM-GROUP', 'ATENDE', 'ATM', 'AWBUD', 'BALTONA', 'BIOTON', 'BKM', 'BANK-OCHRONY-SRODOWISKA', 'BUWOG', 'CD-PROJEKT', 'CHEMOSERVIS-DWORY', 'CIECH', 'COAL-ENERGY', 'CERAMIKA-NOWA-GALA', 'CENTRUM-NOWOCZESNYCH-TECHNOLOGII', 'CZERWONA-TOREBKA', 'DECORA', 'DEKPOL', 'DGA', 'DROP', 'AMREST', 'EFEKT', 'ELEKTROTIM', 'ENERGOAPARATURA', 'ERG', 'ES-SYSTEM', 'FORTE', 'GPW', 'HYDROTOR', 'IMS', 'INC', 'INVISTA', 'INTERBUD-LUBLIN', 'JHM-DEVELOPMENT', 'JW-CONSTRUCTION', 'KOPEX', 'KREDYT-INKASO', 'KVT', 'LIBET', 'LC-CORP', 'LENA-LIGHTING', 'LKD', 'LPP', 'LARK-PL', 'LENTEX', 'LIVECHAT', 'MEDIACAP', 'MEGARON', 'MEX-POLSKA', 'MIRACULUM', 'MLP-GROUP', 'MW-TRADE', 'NETIA', 'PANOVA', 'NOVITA', 'OPONEO-PL', 'ORBIS', 'PC-GUARD', 'PGE', 'POINT-GROUP', 'POLSKI-HOLDING-NIERUCHOMOSCI', 'PLAYWAY', 'PRAGMA-INKASO', 'PROTEKTOR', 'POLWAX', 'PZU', 'REDAN', 'REMAK', 'ROPCZYCE', 'REDWOOD-HOLDING', 'RAWLPLUG', 'SECO-WARWICK', 'TRAKCJA', 'ULMA-CONSTRUCCION', 'UNIBEP', 'UNIMOT-GAZ', 'URSUS', 'VIGO-SYSTEM', 'VINDEXUS', 'VOTUM', 'VIVID-GAMES', 'WIKANA', 'WILBO', 'YOLO', '06N', 'OCTAVA', 'ATAL', 'ACTION', 'APS-ENERGIA', 'ARCUS', 'ASSECO-SOUTH-EASTERN-EUROPE', 'AVIA-SOLUTIONS-GROUP', 'ATLANTIS', 'BIK', 'BIOMED-LUBLIN', 'BRASTER', 'BRIJU', 'BSC-DRUKARNIA-OPAKOWAN', 'BEST', 'BZW', 'CDL', 'COMP', 'COMARCH', 'COGNOR', 'COLIAN', 'CAPITAL-PARTNERS', 'CORMAY', 'CALATRAVA-CAPITAL', 'DELKO', 'DREWEX', 'ESOTIQ-HENDERSON', 'ECHO-INVESTMENT', 'EUROHOLD-BULGARIA', 'EUROCASH', 'GBK', 'GRODNO', 'GORENJE', 'HARPER-HYGIENICS', 'HERKULES', 'HYPERION', 'IDEA-BANK', 'INVESTMENT-FRIENDS-CAPITAL', 'POWER-MEDIA', 'IMPERA', 'INTERFERIE', 'INSTAL-KRAKOW', 'INPRO', 'INTERNATIONAL-PERSONAL-FINANCE-PLC', 'IMPEXMETAL', 'KBD', 'KERNEL', 'KREZUS', 'LUBAWA', 'CAM-MEDIA', 'LSI-SOFTWARE', 'MAKARONY-POLSKIE', 'MBANK', 'MBR', 'MERCOR', 'MFO', 'ZETKAMA', 'MONNARI-TRADE', 'MPH', 'MIRBUD', 'MERCATOR', 'MOSTOSTAL-WARSZAWA', 'MOSTOSTAL-ZABRZE', 'MEDIATEL', 'MXC', 'MUZA', 'NORTH-COAST', 'NEWAG', 'TELL', 'OT-LOGISTICS', 'POLICE', 'PEM', 'PFLEIDERER-GRAJEWO', 'PLAZACNTR', 'PAMAPOL', 'POLNORD', 'PEPEES', 'PROCHNIK', 'PRAGMA-FAKTORING', 'PROCHEM', 'RAFAMET', 'RUBICON-PARTNERS', 'REINHOLD', 'SANTANDER', 'SARE', 'SELENA-FM', 'SERINUS', 'SOHO-DEVELOPMENT', 'SIMPLE', 'SANWIL-HOLDING', 'STELMET', 'SUWARY', 'TARCZYNSKI', 'TATRY-MOUNTAIN-RESORTS', 'TXM', 'VOXEL', 'WOJAS', 'ZEPAK', 'ASSECO-BUSINESS-SOLUTIONS', 'ACG', 'ADIUVO-INVESTMENT', 'AGROTON', 'ASBISC', 'ASTARTA', 'AIRWAY-MEDIX', 'BBI-DEVELOPMENT', 'ELEKTROCIEPLOWNIA-BEDZIN', 'BOWIM', 'BERLING', 'CEZ', 'COMPERIA-PL', 'DEBICA', 'DNP', 'DROZAPOL-PROFIL', 'ED-INVEST', 'ERGIS-EUROFILMS', 'ELEKTROBUDOWA', 'EMC', 'ELEMENTAL-HOLDING', 'ENT', 'ERBUD', 'EUROTEL', 'FAST-FINANCE', 'FERRO', 'GROCLIN', 'GOBARTO', 'GPR', 'GINO-ROSSI', 'GTC', 'GETIN-HOLDING', 'HUBSTYLE', 'HAWE', 'I2D', 'IDM', 'INDYKPOL', 'ING-BANK-SLASKI', 'INTROL', 'IPOPEMA', 'INTERSPORT', 'IQ-PARTNERS', 'IZOLACJA-JAROCIN', 'JSW-JASTRZEBSKA-SPOLKA-WEGLOWA', 'KRAKCHEMIA', 'KGHM', 'KOGENERACJA', 'KOMPAP', 'KOMPUTRONIK', 'KPD', 'KERDOS-GROUP', 'KSG-AGRO', 'KONSORCJUM-STALI', 'MARIE-BRIZARD-WINE-SPIRITS', 'MENNICA-POLSKA', 'MARVIPOL', 'NETMEDIA', 'NEUCA', 'PETROLINVEST', 'ORANGE', 'OPTEAM', 'OVOSTAR', 'PCC-INTERMODAL', 'POLENERGIA', 'PEIXIN', 'PLAST-BOX', 'POZBUD', 'PGS-SOFTWARE', 'POLIMEX', 'QUMAK', 'REGNON', 'RESBUD', 'RANK-PROGRESS', 'STARHEDGE', 'SNIEZKA', 'SKYLINE-INVESTMENT', 'SUNEX', 'SONEL', 'STALEXPORT-AUTOSTRADY', 'TNX', 'TOYA', 'TAURON', 'TRITON-DEVELOPMENT', 'UNIWHEELS', 'VANTAGE-DEVELOPMENT', 'VISTAL', 'W-INVESTMENTS', 'WTN', 'WAWEL', 'ZUE')

results = c()
for(symbol in symbols[1:4]){ # TODO adjust AFTER implement graph to png
  R <- simulateStrategy(symbol)
  results <- append(results,  R)
}
