setwd("C:/Users/fatma/Desktop/IE 492")

#library

library(ggplot2)
library(dplyr)
library(zoo)
library(readxl)
library(corrplot)
library(data.table)
library(ggcorrplot)
library(Metrics)
library(lmtest) #for granger causality test
library(rvest)
library(vars)

#data


###############################################################################
gasPrice=read.csv("data/Balancing_Gas_Price%28BGP%29_2018-09-01_2021-04-12.csv")
gasPrice=gasPrice%>%
  mutate(TSO.Purchase.Price=as.numeric(gsub(",","",TSO.Purchase.Price)),
         TSO.Sale.Price=as.numeric(gsub(",","",TSO.Sale.Price)),
         Balancing.Gas.Purchase.Price=as.numeric(gsub(",","",Balancing.Gas.Purchase.Price)),
         Balancing.Gas.Sale.Price=as.numeric(gsub(",","",Balancing.Gas.Sale.Price)),
         Gas.Day=as.Date(Gas.Day,"%d/%m/%Y"))

bilatContBid=read.csv("data/BilateralContractsBid-01012017-11042021.csv")
bilatContBid=bilatContBid%>%
  mutate(Date=as.Date(Date,"%d/%m/%Y"),
         Hour=as.numeric(gsub(":00$","",Hour)),
         Quantity..MWh.=as.numeric(gsub(",","",Quantity..MWh.)))

merged=full_join(bilatContBid,gasPrice,by = c("Date"="Gas.Day"))

capacityExAmount=read.csv("data/capacityExitAmount-01042021-12042021.csv")
capacityExAmount=capacityExAmount%>%
  mutate(Gas.Day=as.Date(Gas.Day,"%d/%m/%Y"),
         Max..Exit.Amount.Sm3.=as.numeric(gsub(",","",Max..Exit.Amount.Sm3.)))

merged=full_join(merged,capacityExAmount,by = c("Date"="Gas.Day"))

capacityMaxEntryAmount=read.csv("data/capacityMaxEntryAmount-27072018-12042021.csv")
capacityMaxEntryAmount=capacityMaxEntryAmount%>%
  mutate(Gas.Day=as.Date(Gas.Day,"%d/%m/%Y"),
         Max..Entry.Amount.Sm3.=as.numeric(gsub(",","",Max..Entry.Amount.Sm3.)))

merged=full_join(merged,capacityMaxEntryAmount,by = c("Date"="Gas.Day"))

dailyRefPrice=read.csv("data/Daily_Reference_Price%28DRP%29_2018-09-01_2021-04-12.csv")
dailyRefPrice=dailyRefPrice%>%
  mutate(Gas.Day=as.Date(Gas.Day,"%d/%m/%Y"),
         DRP=as.numeric(gsub(",","",DRP)))

merged=full_join(merged,dailyRefPrice,by = c("Date"="Gas.Day"))

imbalanceCost=read.csv("data/ImbalanceCost-01012017-12042021.csv")
imbalanceCost=imbalanceCost%>%
  mutate(Date=as.Date(Date,"%d/%m/%Y"),
         Hour=as.numeric(gsub(":00$","",Hour)),
         Pozitive.Imbalance.Cost..TL.=as.numeric(gsub(",","",Pozitive.Imbalance.Cost..TL.)),
         Negative.Imbalance.Cost..TL.=as.numeric(gsub(",","",Negative.Imbalance.Cost..TL.)))

merged=full_join(merged,imbalanceCost,by = c("Date","Hour"))

imbalanceQuantity=read.csv("data/ImbalanceQuantity-01012017-12042021.csv")
imbalanceQuantity=imbalanceQuantity%>%
  mutate(Date=as.Date(Date,"%d/%m/%Y"),
         Hour=as.numeric(gsub(":00$","",Hour)),
         Pozitive.Imbalance.Quantity..MWh.=as.numeric(gsub(",","",Pozitive.Imbalance.Quantity..MWh.)),
         Negative.Imbalance.Quantity..MWh.=as.numeric(gsub(",","",Negative.Imbalance.Quantity..MWh.)))

merged=full_join(merged,imbalanceQuantity,by = c("Date","Hour"))

matchedQuantityforDRP=read.csv("data/Matched_Quantity_for_DRP_2018-09-01_2021-04-12.csv")
matchedQuantityforDRP=matchedQuantityforDRP%>%
  mutate(Gas.Day=as.Date(Gas.Day,"%d/%m/%Y"),
         Matched.Quantity.for.DRP=as.numeric(gsub(",","",Matched.Quantity.for.DRP)))

merged=full_join(merged,matchedQuantityforDRP,by = c("Date"="Gas.Day"))

MCP=read.csv("data/MCP-01012017-12042021.csv")
MCP=MCP%>%
  mutate(Date=as.Date(Date,"%d/%m/%Y"),
         Hour=as.numeric(gsub(":00$","",Hour)))

merged=full_join(merged,MCP,by = c("Date","Hour"))

Price=read.csv("data/Price_2021-03-12_2021-04-12.csv")
Price=Price%>%
  mutate(Gas.Day=as.Date(Gas.Day,"%d/%m/%Y"),
         Day.Ahead.Price..DAP.=as.numeric(gsub(",","",Day.Ahead.Price..DAP.)),
         Intraday.Price..IDP.=as.numeric(gsub(",","",Intraday.Price..IDP.)),
         After.Day.Price..ADP.=as.numeric(gsub(",","",After.Day.Price..ADP.)),
         Weighted.Average.Price=as.numeric(gsub(",","",Weighted.Average.Price)))

merged=full_join(merged,Price,by = c("Date"="Gas.Day"))

SMP=read.csv("data/SMP-01012017-11042021.csv")
SMP=SMP%>%
  mutate(Date=as.Date(Date,"%d/%m/%Y"),
         Hour=as.numeric(gsub(":00$","",Hour)),
         SMP..TL.MWh.=as.numeric(gsub(",","",SMP..TL.MWh.)))

merged=full_join(merged,SMP,by = c("Date","Hour"))

stockAmount=read.csv("data/stockAmount-25072018-12042021.csv")
stockAmount=stockAmount%>%
  mutate(Gas.Day=as.Date(Gas.Day,"%d/%m/%Y"),
         Linepack..Sm3.=as.numeric(gsub(",","",Linepack..Sm3.)))


merged=full_join(merged,stockAmount,by = c("Date"="Gas.Day"))

exRate=read_excel("data/TL-EURO.xlsx")
str(exRate)
exRate=exRate%>%
  mutate(Date=as.Date(Tarih,"%d-%m-%Y"),
         USD.TL=as.numeric(`TP DK USD A YTL`),
         EUR.TL=as.numeric(`TP DK EUR A YTL`))

exRate=exRate[4:6]
merged=full_join(merged,exRate,by = c("Date"))


Consumption=fread("data/RealTimeConsumption-01012017-12042021.txt")
Consumption=Consumption%>%
  mutate(Date=as.Date(Date, "%d.%m.%Y"),
         Hour=as.numeric(gsub(":00$","",Hour)),
         `Consumption (MWh)`=as.numeric(gsub(",","",`Consumption (MWh)`)))

merged=full_join(merged,Consumption,by = c("Date","Hour"))



Generation=fread("data/RealTimeGeneration-01012017-14032021.txt")
Generation=Generation%>%
  mutate(Date=as.Date(Date, "%d.%m.%Y"),
         Hour=as.numeric(gsub(":00$","",Hour)),
         `G.Total (MWh)`=as.numeric(gsub(",","",`Total (MWh)`)),
         `G.Natural Gas`=as.numeric(gsub(",","",`Natural Gas`)),
         `G.Dammed Hydro`=as.numeric(gsub(",","",`Dammed Hydro`)),
         G.Lignite =as.numeric(gsub(",","",Lignite)),
         G.River =as.numeric(gsub(",","",River)),   
         `G.Import Coal` =as.numeric(gsub(",","",`Import Coal`)), 
         G.Wind=as.numeric(gsub(",","",Wind)),
         G.Solar=as.numeric(gsub(",","",Solar)),
         `G.Fuel Oil`=as.numeric(gsub(",","",`Fuel Oil`)),
         G.Geothermal=as.numeric(gsub(",","",Geothermal)),
         `G.Asphaltite Coal`=as.numeric(gsub(",","",`Asphaltite Coal`)),
         `G.Black Coal`=as.numeric(gsub(",","",`Black Coal`)),
         G.Biomass=as.numeric(gsub(",","",Biomass)),     
         G.Naphta=as.numeric(gsub(",","",Naphta)),
         G.LNG=as.numeric(gsub(",","",LNG)),
         `G.Import-Export`=as.numeric(gsub(",","",`Import-Export`)))

Generation=Generation[,c(1,2,19:34)]

merged=full_join(merged,Generation,by = c("Date","Hour"))


EAK=fread("data/EAK-01012017-29032021.txt")

EAK=EAK%>%
  mutate(Date=as.Date(Date, "%d.%m.%Y"),
         Hour=as.numeric(gsub(":00$","",Hour)),
         `EAK.Total (MWh)`=as.numeric(gsub(",","",`Total (MWh)`)),
         `EAK.Natural Gas`=as.numeric(gsub(",","",`Natural Gas`)),
         EAK.Wind=as.numeric(gsub(",","",Wind)),
         EAK.Lignite =as.numeric(gsub(",","",Lignite)),
         `EAK.Black Coal`=as.numeric(gsub(",","",`Black Coal`)),
         `EAK.Import Coal` =as.numeric(gsub(",","",`Import Coal`)), 
         `EAK.Fuel Oil`=`Fuel Oil`,
         EAK.Geothermal=as.numeric(gsub(",","",Geothermal)),
         `EAK.Dammed Hydro`=as.numeric(gsub(",","",`Dammed Hydro`)),
         EAK.Naphta=Naphta,
         EAK.Biomass=Biomass,     
         EAK.River =as.numeric(gsub(",","",River)),   
         EAK.Others=Others)

EAK=EAK[,c(1,2,16:28)]

merged=full_join(merged,EAK,by = c("Date","Hour"))

Load=fread("data/LoadForecast-01012017-29032021.txt")

Load=Load%>%
  mutate(Date=as.Date(Date, "%d/%m/%Y"),
         Hour=as.numeric(gsub(":00$","",Hour)),
         `Load Forecast (MWh)`=as.numeric(gsub(",","",`Load Forecast (MWh)`)))

merged=full_join(merged,Load,by = c("Date","Hour"))


fuelPrices=read.csv("data/FUELPRICES.csv")
fuelPrices=fuelPrices%>%
  mutate(Date=as.Date(Date,"%Y-%m-%d"))%>%
  rename(fuelPrice=fUEL_pRICE)
fuelPrices=fuelPrices[-1]

merged=full_join(merged,fuelPrices,by = c("Date"))


merged=merged%>%
  mutate(MCP_lag24=lag(MCP..TL.MWh.,24),
         MCP_lag48=lag(MCP..TL.MWh.,48),
         MCP_lag168=lag(MCP..TL.MWh.,168))


merged=merged%>%mutate(`BilateralCQ/Consumption`=Quantity..MWh./`Consumption (MWh)`,
                       `Generation/Consumption`=`G.Total (MWh)`/`Consumption (MWh)`,
                       GProportion.NatGas=`G.Natural Gas`/`G.Total (MWh)`,
                       GProportion.DammedH=`G.Dammed Hydro`/`G.Total (MWh)`,
                       GProportion.Lignite=G.Lignite/`G.Total (MWh)`,
                       GProportion.River=G.River/`G.Total (MWh)`,
                       GProportion.ImportC=`G.Import Coal`/`G.Total (MWh)`,
                       GProportion.Wind=G.Wind/`G.Total (MWh)`,
                       GProportion.Solar=G.Solar/`G.Total (MWh)`,
                       GProportion.FuelOil=`G.Fuel Oil`/`G.Total (MWh)`,
                       GProportion.Geot=G.Geothermal/`G.Total (MWh)`,
                       GProportion.AsphalC=`G.Asphaltite Coal`/`G.Total (MWh)`,
                       GProportion.BlackC=`G.Black Coal`/`G.Total (MWh)`,
                       GProportion.Biomass=G.Biomass/`G.Total (MWh)`,
                       GProportion.Naphta=G.Naphta/`G.Total (MWh)` ,
                       GProportion.LNG=G.LNG/`G.Total (MWh)` ,
                       GProportion.ImportExport=`G.Import-Export` /`G.Total (MWh)`)


merged=merged[-c(4,5,8,19:22,41,42,76,77)]


merged2018=merged%>%filter(Date>="2018-09-01")

merged2019=merged2018%>%filter(Date>="2019-01-02")

#write.csv(merged, file = "merged.csv")
#write.csv(merged2018, file = "merged2018.csv")
#write.csv(merged2019, file = "merged2019.csv")