#Ubacuvanje dataseta u RStudio
#setwd("C:/Users/Ivana/Desktop/proba-Copy/")
df <- read.csv("darknet.csv")

#ucitavanje potrebnih paketa
library(caret)
library(randomForest)
library(e1071)
library(rpart)
library(rpart.plot)

#uzorak od 5000 opservacija na samom pocetku
set.seed(1)
df <- df[sample(nrow(df), 5000), ]

#utvrdjivanje raspodele atributa. Kada se sabere broj opservacija,
#dobija se tacno 5000, sto znaci da nema nekih skrivenih NA vrednosti
table(df$Label)
table(df$Label.1)

#transformacija atributa Protocol u factor
table(df$Protocol)
df$Protocol <- as.factor(df$Protocol)
levels(df$Protocol) <- c("HOPOPT", "TCP", "UDP")

#Reimenovanje vrednosti i transformacija u faktor tip
#Razlog reimenovanja - Citav ovaj darknet dataset je nastao spajanjem dva
#dataseta. Prvi se odnosi na tor saobracaj, drugi na vpn. Sustina je da
#se u ovom detasetu, tor i vpn saobracaj smatraju darknet saobracajem zbog 
#nacina rada
df$Label[df$Label == "Tor"] <- "Darknet"
df$Label[df$Label == "VPN"] <- "Darknet"
df$Label[df$Label == "Non-Tor"] <- "Non-Darknet"
df$Label[df$Label == "NonVPN"] <- "Non-Darknet"
df$Label<-as.factor(df$Label)

#uklanjanje NA vrednosti, iskoristio sam summary da bih brzo prosao kroz 
#sve variajble. Za one numerickog tipa, pisace odmah broj NA vrednosti.
#Nakon toga, ispitivacu one koje su greskom character tipa.
summary(df)
shapiro.test(df$Flow.Bytes.s)
#p-value < 2.2e-16, posto ne vazi p > 0.05, raspodela nije normalna, tako
#da se NA vrednsti zamenjuju medianom

mediana <- median(df$Flow.Bytes.s, na.rm = TRUE)
df$Flow.Bytes.s[is.na(df$Flow.Bytes.s)] <- mediana

#Resavanje problema inf vrednosti, u pitanju je atribut Flow.Packets.s
#to se videlo koriscenjem summary funkcije sa linije 32. Pomenuti atribut 
#se nalazi odmah pored atributa Flow.Bytes.s, koji je imao NA vrednosti
bezInf <- df$Flow.Packets.s[which(df$Flow.Packets.s < Inf)]
max(bezInf)
df$Flow.Packets.s[is.infinite(df$Flow.Packets.s)] <- 2000000.0
#2e+06 je 2000000
summary(df$Flow.Packets.s)

#Sredjivanje atributa Label.1
df$Label.1[df$Label.1 == "AUDIO-STREAMING"] <- "Audio-Streaming"
df$Label.1[df$Label.1 == "File-transfer"] <- "File-Transfer"
df$Label.1[df$Label.1 == "Video-streaming"] <- "Video-Streaming"
table(df$Label.1)
df$Label.1 <- as.factor(df$Label.1)

#Transformacija timestamp atributa
#Mislio sam sa ovom which funkcijom da proverim da li ima odredjenih
#vrednosti ali dobio sam rezultat integer(0), valjda je u redu
which(df$Timestamp == "" | df$Timestamp == "*" | df$Timestamp == "-")
#install.packages("lubridate")
df$Timestamp <- as.POSIXct(df$Timestamp,format="%d/%m/%Y %I:%M:%S %p")
str(df$Timestamp)

#jos jedna varijabla POSIXlt ovog puta jer jos nisam siguran koju da
#primenjujem. Razmisljao sam da bi ipak na ovom datasetu imalo vise smisla
#posmatrati ceo timestamp kao celinu jer verujem da su podaci skupljani
#nekim redosledom, u nekim intervalima(samo pretpostavka) na primer: 
#5. septembra od 3 do 5 preko tor mreze, a sutradan od 10h do 15h normalan 
#saobracaj. Tako da mozda ne bi bilo dobro da ostavim samo h:m:s, kako sam
#na pocetku mislio
lt.tip <- as.POSIXlt(df$Timestamp,format="%d/%m/%Y %I:%M:%S %p")

#Uklanjanje atributa cije su sve vrednosti 0
#opet koristim funkciju summary nad celim datasetom
summary(df)
df$Bwd.PSH.Flags <- NULL
df$URG.Flag.Count <- NULL
df$CWE.Flag.Count <- NULL
df$ECE.Flag.Count <- NULL
df$Fwd.Bytes.Bulk.Avg <- NULL
df$Fwd.Packet.Bulk.Avg <- NULL
df$Fwd.Bulk.Rate.Avg <- NULL
df$Bwd.Bytes.Bulk.Avg <- NULL
df$Subflow.Bwd.Packets <- NULL
df$Active.Mean <- NULL
df$Active.Std <- NULL
df$Active.Max <- NULL
df$Active.Min <- NULL

#uklanjanje atributa za koje mislim da nisu relevantni
#Scr.IP i Dst.IP predstavljaju samo ip adresa uredjaja koji ucesvuju u
#komunikaciji
df$Src.IP <- NULL
df$Dst.IP <- NULL
#FlowID predstavlja kombinaciju Src.IP, Dst.IP, Src.Port, Dst.Port i 
#Protocol-a, tako da cak nije ni pravi id jer ne mora nuzno da bude 
#jedinstven
df$Flow.ID <- NULL

#U summary funkciji, na liniji 75, moglo se videti da su neki atributi
#character tipa iako se po njihovim nazivima vidi da nisu
#Sledi njihova transformacija u odgovarajuce tipove
length(unique(df$Bwd.URG.Flags))
table(df$Bwd.URG.Flags)
#Atribut Bwd.Urg.Flags ima sve vrednosti nula, tako da ga iskljucujem
#iz dataseta
df$Bwd.URG.Flags <- NULL
#isto vazi i za Fwd.Urg.Flags
length(unique(df$Fwd.URG.Flags))
table(df$Fwd.URG.Flags)
df$Fwd.URG.Flags <- NULL

#ima1420 razlicitih vrednosti, transformacija u numeric jer je u pitanju
#prosecna vrednost
length(unique(df$Packet.Length.Mean))
df$Packet.Length.Mean <- as.numeric(df$Packet.Length.Mean)

#3134 razlicitih vrednosti, transformacija u integer jer je u pitanju
#broj primljenih paketa u sekundi
length(unique(df$Bwd.Packets.s))
df$Bwd.Packets.s <- as.integer(df$Bwd.Packets.s)

#336 razlicitih, dakle nema sve nule, transformacija u int
length(unique(df$Bwd.Header.Length))
df$Bwd.Header.Length <- as.integer(df$Bwd.Header.Length)

#binarizacija atributa Subflow.Fwd.Packets i Fwd.PSH.Flags
table(df$Subflow.Fwd.Packets)
df$Subflow.Fwd.Packets <-as.factor(df$Subflow.Fwd.Packets)
table(df$Fwd.PSH.Flags)
df$Fwd.PSH.Flags <- as.factor(df$Fwd.PSH.Flags)

#reordering kolona dataframea, kako bi target labela bila poslednja

df <- df[ , c(1:65, 67, 66)]   

#Selekcija atributa koriscenjem Recursive feature elimination algoritma

control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

set.seed(1)
parts = createDataPartition(df$Label, p = .8, list = F)
trainRFE = df[parts, ]
testRFE = df[-parts, ]
X_train = trainRFE[,-67]
y_train = trainRFE[,67]

set.seed(1)
result_rfe = rfe(x = X_train, 
                 y = y_train, 
                 sizes = c(5:35),
                 rfeControl = control)

result_rfe
predictors(result_rfe)
#Selektovani atributi su:Label.1, Flow.IAT.Min, Timestamp,
#Fwd.Seg.Size.Min, Idle.Max, Flow.IAT.Max, Flow.IAT.Mean, 
#Fwd.Packets.s, Fwd.Init.Win.Bytes, Flow.Packets.s, Flow.Duration

#Kreiranje novog dataseta koji sadrzi selektovane atribute i
#izlazni atribut
dfSelected <- df[,c("Label.1", "Flow.IAT.Min", "Timestamp", "Fwd.Seg.Size.Min", "Idle.Max", "Flow.IAT.Max", "Flow.IAT.Mean", "Fwd.Packets.s", "FWD.Init.Win.Bytes", "Flow.Packets.s", "Flow.Duration", "Label")]

saveRDS(dfSelected, "preprocessed_darknet_.RDS")