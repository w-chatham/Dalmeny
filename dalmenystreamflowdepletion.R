install.packages("streamDepletr")
library (streamDepletr)
install.packages("ggplot2")
library (ggplot2)
getAnywhere(glover)
#variables#
DalmenyTrMean<- 432
DalmenyTrMin<- 130
DalmenyTrMax<- 708
DalmenySMean<- 0.00073
DalmenySMin<- 0.00039
DalmenySMax<- 0.0016
DalmenyKMean<- (86400*0.00021) #m/d#
DalmenyKMin<- (86400*0.000062)
DalmenyKMax<- (86400*0.00037)
DalmenyMeanThickness<- 20
ufaKMean<- (86400*0.00000001)
ufaMeanThickness<- 20
streambed2<- (86400*0.0000000181)
streambed2thickness<- 22
RiverWidth<- 300
MarAugMean<- (285.0803786*86400) #m3/d#
MarAugMin<- (68.5*86400)
MarAugMax<- (848*86400)
SepFebMean<- (155.1909147*86400)
SepFebMin<- (66.6*86400)
SepFebMax<- (284*86400)
#Time is 178 days, 1/2 a year. Our 2 seasons (high flow and low flow) are 6 months (178 days) long.#
time <- seq(1,178)
#glover#
#Capture fraction (Qf) result with minimum, max and mean S#
glover1<- glover(time, 5000, DalmenySMin, DalmenyTrMean)
glover2<- glover(time, 5000, DalmenySMax, DalmenyTrMean)
glover3<- glover(time, 5000, DalmenySMean, DalmenyTrMean)

#comparison plot main = "Qf with minimum, maximum, and mean Storativity using Glover's model",#
plot(glover1, col="white" ,lty=1, xlab = "Time (days)", ylab = "Qf, capture fraction" )
lines(glover1, lwd = 2, col="darkorchid",lty=1)
lines(glover2, lwd = 2, col="gold",lty=1)
lines(glover3, lwd = 2, col="olivedrab",lty=1)
legend("bottomright", legend = c(DalmenySMin, DalmenySMax, DalmenySMean),
       lwd = 2, col = c("darkorchid", "gold", "olivedrab"))

#normalisation of S#
NormSGlover<- ((glover(178, 5000, DalmenySMax, DalmenyTrMean)) - (glover(178, 5000, DalmenySMin, DalmenyTrMean))) / (DalmenySMax - DalmenySMin)
#-157.9731#

#Capture fraction (Qf) result with minimum, max and mean Tr#
glover4<- glover(time, 5000, DalmenySMean, DalmenyTrMin)
glover5<- glover(time, 5000, DalmenySMean, DalmenyTrMax)
glover6<- glover(time, 5000, DalmenySMean, DalmenyTrMean)

#comparison plot main = "Qf with minimum, maximum, and mean Transmissivity using Glover's model",#
plot(glover1, col="white" ,lty=1, xlab = "Time (days)", ylab = "Qf, capture fraction" )
lines(glover4, lwd = 2, col="darkorchid",lty=1)
lines(glover5, lwd = 2, col="gold",lty=1)
lines(glover6, lwd = 2, col="olivedrab",lty=1)
legend("bottomright", legend = c("130 m2/d", "708 m2/d", "432 m2/d"),
       lwd = 2, col = c("darkorchid", "gold", "olivedrab"))

#normalisation of Tr#
NormTrGlover<- ((glover(178, 5000, DalmenySMean, DalmenyTrMax)) - (glover(178, 5000, DalmenySMean, DalmenyTrMin))) / (DalmenyTrMax - DalmenyTrMin)
#0.0004460841#

#Capture fraction (Qf) result with d = 2500, 5000, 7500 and 10000#
glover7<- glover(time, 2500, DalmenySMean, DalmenyTrMean)
glover8<- glover(time, 5000, DalmenySMean, DalmenyTrMean)
glover9<- glover(time, 7500, DalmenySMean, DalmenyTrMean)
glover10<- glover(time, 10000, DalmenySMean, DalmenyTrMean)

#comparison plot main = "Qf with varying distance of well to stream using Glover's model"#
plot(glover7, col="white" ,lty=1, xlab = "Time (days)", ylab = "Qf, capture fraction" )
lines(glover7, lwd = 2, col="darkorchid",lty=1)
lines(glover8, lwd = 2, col="gold",lty=1)
lines(glover9, lwd = 2, col="olivedrab",lty=1)
lines(glover10, lwd = 2, col="turquoise",lty=1)
legend("bottomright", legend = c("2500m", "5000m", "7500m", "10000m"),
       lwd = 2, col = c("darkorchid", "gold", "olivedrab", "turquoise"))

#normalisation of d#
NormDGlover<- ((glover(178, 10000, DalmenySMean, DalmenyTrMean)) - (glover(178, 2500, DalmenySMean, DalmenyTrMean))) / (10000 - 2500)
#-4.965344e-05#



#hunt#
#Upperfloral streambed#
#Capture fraction (Qf) result with minimum, max and mean S#
hunt1<- hunt(time, 5000, DalmenySMin, DalmenyTrMean, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
hunt2<- hunt(time, 5000, DalmenySMax, DalmenyTrMean, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
hunt3<- hunt(time, 5000, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))

#comparison chart "Qf with minimum, maximum, and mean Storativity using Hunt's model and the upperfloral streambed"#
plot(hunt1, col="white" ,lty=1, xlab = "Time (days)", ylab = "Qf, capture fraction" )
lines(hunt1, lwd = 2, col="darkorchid",lty=1)
lines(hunt2, lwd = 2, col="gold",lty=1)
lines(hunt3, lwd = 2, col="olivedrab",lty=1)
legend("bottomright", legend = c(DalmenySMin, DalmenySMax, DalmenySMean),
       lwd = 2, col = c("darkorchid", "gold", "olivedrab"))

#normalisation of S#
NormSHuntU<- ((hunt(178, 5000, DalmenySMax, DalmenyTrMean, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))) - (hunt(178, 5000, DalmenySMin, DalmenyTrMean, (streambed_conductance(300, ufaKMean, ufaMeanThickness))))) / (DalmenySMax - DalmenySMin)
#-75.7726#

#Capture fraction (Qf) result with minimum, max and mean Tr#
hunt4<- hunt(time, 5000, DalmenySMean, DalmenyTrMin, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
hunt5<- hunt(time, 5000, DalmenySMean, DalmenyTrMax, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
hunt6<- hunt(time, 5000, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))

#comparison chart main = "Qf with minimum, maximum, and mean Transmissivity using Hunt's model and the upperfloral streambed",#
plot(hunt1, col="white" ,lty=1, xlab = "Time (days)", ylab = "Qf, capture fraction" )
lines(hunt4, lwd = 2, col="darkorchid",lty=1)
lines(hunt5, lwd = 2, col="gold",lty=1)
lines(hunt6, lwd = 2, col="olivedrab",lty=1)
legend("bottomright", legend = c("130 m2/d", "708 m2/d", "432 m2/d"),
       lwd = 2, col = c("darkorchid", "gold", "olivedrab"))

#normalisation of Tr#
NormTrHuntU<- ((hunt(178, 5000, DalmenySMean, DalmenyTrMax, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))) - (hunt(178, 5000, DalmenySMean, DalmenyTrMin, (streambed_conductance(300, ufaKMean, ufaMeanThickness))))) / (DalmenyTrMax - DalmenyTrMin)
#-3.412155e-05#

#Capture fraction (Qf) result with d = 2500, 5000, 7500 and 10000#
hunt7<- hunt(time, 2500, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
hunt8<- hunt(time, 5000, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
hunt9<- hunt(time, 7500, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
hunt10<- hunt(time, 10000, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))

#comparison plot main = "Qf with varying distance of well to stream using Hunt's model and the upperfloral streambed"#
plot(hunt7, col="white" ,lty=1, xlab = "Time (days)", ylab = "Qf, capture fraction" )
lines(hunt7, lwd = 2, col="darkorchid",lty=1)
lines(hunt8, lwd = 2, col="gold",lty=1)
lines(hunt9, lwd = 2, col="olivedrab",lty=1)
lines(hunt10, lwd = 2, col="turquoise",lty=1)
legend("bottomright", legend = c("2500m", "5000m", "7500m", "10000m"),
       lwd = 2, col = c("darkorchid", "gold", "olivedrab", "turquoise"))

#normalisation of d#
NormDHuntU<- ((hunt(178, 10000, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))) - (hunt(178, 2500, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, ufaKMean, ufaMeanThickness))))) / (10000 - 2500)
#-8.747756e-06#


#Weighted average streambed#
#Capture fraction (Qf) result with minimum, max and mean S#
hunt11<- hunt(time, 5000, DalmenySMin, DalmenyTrMean, (streambed_conductance(300, streambed2, streambed2thickness)))
hunt12<- hunt(time, 5000, DalmenySMax, DalmenyTrMean, (streambed_conductance(300, streambed2, streambed2thickness)))
hunt13<- hunt(time, 5000, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, streambed2, streambed2thickness)))

#comparison chart "Qf with minimum, maximum, and mean Storativity using Hunt's model and the weighted streambed"#
plot(hunt11, col="white" ,lty=1, xlab = "Time (days)", ylab = "Qf, capture fraction" )
lines(hunt11, lwd = 2, col="darkorchid",lty=1)
lines(hunt12, lwd = 2, col="gold",lty=1)
lines(hunt13, lwd = 2, col="olivedrab",lty=1)
legend("bottomright", legend = c(DalmenySMin, DalmenySMax, DalmenySMean),
       lwd = 2, col = c("darkorchid", "gold", "olivedrab"))

#normalisation of S#
NormSHuntW<- ((hunt(178, 5000, DalmenySMax, DalmenyTrMean, (streambed_conductance(300, streambed2, streambed2thickness)))) - (hunt(178, 5000, DalmenySMin, DalmenyTrMean, (streambed_conductance(300, streambed2, streambed2thickness))))) / (DalmenySMax - DalmenySMin)
#-110.1866#

#Capture fraction (Qf) result with minimum, max and mean Tr#
hunt14<- hunt(time, 5000, DalmenySMean, DalmenyTrMin, (streambed_conductance(300, streambed2, streambed2thickness)))
hunt15<- hunt(time, 5000, DalmenySMean, DalmenyTrMax, (streambed_conductance(300, streambed2, streambed2thickness)))
hunt16<- hunt(time, 5000, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, streambed2, streambed2thickness)))

#comparison chart main = "Qf with minimum, maximum, and mean Transmissivity using Hunt's model and the weighted streambed",#
plot(hunt14, col="white" ,lty=1, xlab = "Time (days)", ylab = "Qf, capture fraction" )
lines(hunt14, lwd = 2, col="darkorchid", lty=1)
lines(hunt15, lwd = 2, col="gold", lty=1)
lines(hunt16, lwd = 2, col="olivedrab", lty=1)
legend("bottomright", legend = c("130 m2/d", "708 m2/d", "432 m2/d"),
       lwd = 2, col = c("darkorchid", "gold", "olivedrab"))

#normalisation of Tr#
NormTrHuntW<- ((hunt(178, 5000, DalmenySMean, DalmenyTrMax, (streambed_conductance(300, streambed2, streambed2thickness)))) - (hunt(178, 5000, DalmenySMean, DalmenyTrMin, (streambed_conductance(300, streambed2, streambed2thickness))))) / (DalmenyTrMax - DalmenyTrMin)
#-3.901106e-05#

#Capture fraction (Qf) result with d = 2500, 5000, 7500 and 10000#
hunt17<- hunt(time, 2500, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, streambed2, streambed2thickness)))
hunt18<- hunt(time, 5000, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, streambed2, streambed2thickness)))
hunt19<- hunt(time, 7500, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, streambed2, streambed2thickness)))
hunt20<- hunt(time, 10000, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, streambed2, streambed2thickness)))

#comparison plot main = "Qf with varying distance of well to stream using Hunt's model and the weighted streambed"#
plot(hunt17, col="white" ,lty=1, xlab = "Time (days)", ylab = "Qf, capture fraction" )
lines(hunt17, lwd = 2, col="darkorchid",lty=1)
lines(hunt18, lwd = 2, col="gold",lty=1)
lines(hunt19, lwd = 2, col="olivedrab",lty=1)
lines(hunt20, lwd = 2, col="turquoise",lty=1)
legend("bottomright", legend = c("2500m", "5000m", "7500m", "10000m"),
       lwd = 2, col = c("darkorchid", "gold", "olivedrab", "turquoise"))

#normalisation of d#
NormDHuntW<- ((hunt(178, 10000, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, streambed2, streambed2thickness)))) - (hunt(178, 2500, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, streambed2, streambed2thickness))))) / (10000 - 2500)
#-1.324301e-05#

#Hantush#
#Upperfloral streambed#
#Capture fraction (Qf) result with minimum, max and mean S#
hantush1<- hantush(time, 5000, DalmenySMin, DalmenyKMean, DalmenyMeanThickness, ufaKMean, ufaMeanThickness)
hantush2<- hantush(time, 5000, DalmenySMax, DalmenyKMean, DalmenyMeanThickness, ufaKMean, ufaMeanThickness)
hantush3<- hantush(time, 5000, DalmenySMean, DalmenyKMean, DalmenyMeanThickness, ufaKMean, ufaMeanThickness)
#normalisation of S#
NormSHantushU<- ((hantush(178, 5000, DalmenySMax, DalmenyKMean, DalmenyMeanThickness, ufaKMean, ufaMeanThickness)) - (hantush(178, 5000, DalmenySMin, DalmenyKMean, DalmenyMeanThickness, ufaKMean, ufaMeanThickness))) / (DalmenySMax - DalmenySMin)
#-12.99986#

#comparison chart "Qf with minimum, maximum, and mean Storativity using Hantush's model and the upperfloral streambed"#
plot(hantush1, col="white" ,lty=1, xlab = "Time (days)", ylab = "Qf, capture fraction" )
lines(hantush1, lwd = 2, col="darkorchid",lty=1)
lines(hantush2, lwd = 2, col="gold",lty=1)
lines(hantush3, lwd = 2, col="olivedrab",lty=1)
legend("bottomright", legend = c(DalmenySMin, DalmenySMax, DalmenySMean),
       lwd = 2, col = c("darkorchid", "gold", "olivedrab"))

#Capture fraction (Qf) result with minimum, max and mean K#
hantush4<- hantush(time, 5000, DalmenySMean, DalmenyKMin, DalmenyMeanThickness, ufaKMean, ufaMeanThickness)
hantush5<- hantush(time, 5000, DalmenySMean, DalmenyKMax, DalmenyMeanThickness, ufaKMean, ufaMeanThickness)
hantush6<- hantush(time, 5000, DalmenySMean, DalmenyKMean, DalmenyMeanThickness, ufaKMean, ufaMeanThickness)
#normalisation of K#
NormKHantushU<- ((hantush(178, 5000, DalmenySMean, DalmenyKMax, DalmenyMeanThickness, ufaKMean, ufaMeanThickness)) - (hantush(178, 5000, DalmenySMean, DalmenyKMin, DalmenyMeanThickness, ufaKMean, ufaMeanThickness))) / (DalmenyKMax - DalmenyKMin)
#-0.0001333417#

#comparison chart main = "Qf with minimum, maximum, and mean Hydraulic conductivity using Hantush's model and the upperfloral streambed",#
plot(hantush4, col="white" ,lty=1, xlab = "Time (days)", ylab = "Qf, capture fraction" )
lines(hantush4, lwd = 2, col="darkorchid",lty=1)
lines(hantush5, lwd = 2, col="gold",lty=1)
lines(hantush6, lwd = 2, col="olivedrab",lty=1)
legend("bottomright", legend = c("5.3568 m/s", "31.968  m/s", "18.144 m/s"),
       lwd = 2, col = c("darkorchid", "gold", "olivedrab"))

#Capture fraction (Qf) result with d = 2500, 5000, 7500 and 10000#
hantush7<- hantush(time, 2500, DalmenySMean, DalmenyKMean, DalmenyMeanThickness, ufaKMean, ufaMeanThickness)
hantush8<- hantush(time, 5000, DalmenySMean, DalmenyKMean, DalmenyMeanThickness, ufaKMean, ufaMeanThickness)
hantush9<- hantush(time, 7500, DalmenySMean, DalmenyKMean, DalmenyMeanThickness, ufaKMean, ufaMeanThickness)
hantush10<- hantush(time, 10000, DalmenySMean, DalmenyKMean, DalmenyMeanThickness, ufaKMean, ufaMeanThickness)
#normalisation of d#
NormDHantushU<- ((hantush(178, 10000, DalmenySMean, DalmenyKMean, DalmenyMeanThickness, ufaKMean, ufaMeanThickness)) - (hantush(178, 2500, DalmenySMean, DalmenyKMean, DalmenyMeanThickness, ufaKMean, ufaMeanThickness))) / (10000 - 2500)
#-1.499232e-06#

#comparison plot main = "Qf with varying distance of well to stream using Hunt's model and the upperfloral streambed"#
plot(hantush7, col="white" ,lty=1, xlab = "Time (days)", ylab = "Qf, capture fraction" )
lines(hantush7, lwd = 2, col="darkorchid",lty=1)
lines(hantush8, lwd = 2, col="gold",lty=1)
lines(hantush9, lwd = 2, col="olivedrab",lty=1)
lines(hantush10, lwd = 2, col="turquoise",lty=1)
legend("bottomright", legend = c("2500m", "5000m", "7500m", "10000m"),
       lwd = 2, col = c("darkorchid", "gold", "olivedrab", "turquoise"))


#Weighted average streambed#
#Capture fraction (Qf) result with minimum, max and mean S#
hantush11<- hantush(time, 5000, DalmenySMin, DalmenyKMean, DalmenyMeanThickness, streambed2, streambed2thickness)
hantush12<- hantush(time, 5000, DalmenySMax, DalmenyKMean, DalmenyMeanThickness, streambed2, streambed2thickness)
hantush13<- hantush(time, 5000, DalmenySMean, DalmenyKMean, DalmenyMeanThickness, streambed2, streambed2thickness)

#normalisation of S#
NormSHantushW<- ((hantush(178, 5000, DalmenySMax, DalmenyKMean, DalmenyMeanThickness, streambed2, streambed2thickness)) - (hantush(178, 5000, DalmenySMin, DalmenyKMean, DalmenyMeanThickness, streambed2, streambed2thickness))) / (DalmenySMax - DalmenySMin)
#-20.97219#

#comparison chart "Qf with minimum, maximum, and mean Storativity using Hantush's model and the weighted streambed"#
plot(hantush11, col="white" ,lty=1, xlab = "Time (days)", ylab = "Qf, capture fraction" )
lines(hantush11, lwd = 2, col="darkorchid",lty=1)
lines(hantush12, lwd = 2, col="gold",lty=1)
lines(hantush13, lwd = 2, col="olivedrab",lty=1)
legend("bottomright", legend = c(DalmenySMin, DalmenySMax, DalmenySMean),
       lwd = 2, col = c("darkorchid", "gold", "olivedrab"))

#Capture fraction (Qf) result with minimum, max and mean K#
hantush14<- hantush(time, 5000, DalmenySMean, DalmenyKMin, DalmenyMeanThickness, streambed2, streambed2thickness)
hantush15<- hantush(time, 5000, DalmenySMean, DalmenyKMax, DalmenyMeanThickness, streambed2, streambed2thickness)
hantush16<- hantush(time, 5000, DalmenySMean, DalmenyKMean, DalmenyMeanThickness, streambed2, streambed2thickness)

#normalisation of K#
NormKHantushW<- ((hantush(178, 5000, DalmenySMean, DalmenyKMax, DalmenyMeanThickness, streambed2, streambed2thickness)) - (hantush(178, 5000, DalmenySMean, DalmenyKMin, DalmenyMeanThickness, streambed2, streambed2thickness))) / (DalmenyKMax - DalmenyKMin)
#-0.0002079556#

#comparison chart main = "Qf with minimum, maximum, and mean Hydraulic conductivity using Hantush's model and the weighted streambed",#
plot(hantush14, col="white" ,lty=1, xlab = "Time (days)", ylab = "Qf, capture fraction" )
lines(hantush14, lwd = 2, col="darkorchid",lty=1)
lines(hantush15, lwd = 2, col="gold",lty=1)
lines(hantush16, lwd = 2, col="olivedrab",lty=1)
legend("bottomright", legend = c("5.3568 m/s", "31.968  m/s", "18.144 m/s"),
       lwd = 2, col = c("darkorchid", "gold", "olivedrab"))

#Capture fraction (Qf) result with d = 2500, 5000, 7500 and 10000#
hantush17<- hantush(time, 2500, DalmenySMean, DalmenyKMean, DalmenyMeanThickness, streambed2, streambed2thickness)
hantush18<- hantush(time, 5000, DalmenySMean, DalmenyKMean, DalmenyMeanThickness, streambed2, streambed2thickness)
hantush19<- hantush(time, 7500, DalmenySMean, DalmenyKMean, DalmenyMeanThickness, streambed2, streambed2thickness)
hantush20<- hantush(time, 10000, DalmenySMean, DalmenyKMean, DalmenyMeanThickness, streambed2, streambed2thickness)

#normalisation of d#
NormDHantushW<- ((hantush(178, 10000, DalmenySMean, DalmenyKMean, DalmenyMeanThickness, streambed2, streambed2thickness)) - (hantush(178, 2500, DalmenySMean, DalmenyKMean, DalmenyMeanThickness, streambed2, streambed2thickness))) / (10000 - 2500)
#-2.43446e-06#

#comparison plot main = "Qf with varying distance of well to stream using Hantush's model and the weighted streambed"#
plot(hantush17, col="white" ,lty=1, xlab = "Time (days)", ylab = "Qf, capture fraction" )
lines(hantush17, lwd = 2, col="darkorchid",lty=1)
lines(hantush18, lwd = 2, col="gold",lty=1)
lines(hantush19, lwd = 2, col="olivedrab",lty=1)
lines(hantush20, lwd = 2, col="turquoise",lty=1)
legend("bottomright", legend = c("2500m", "5000m", "7500m", "10000m"),
       lwd = 2, col = c("darkorchid", "gold", "olivedrab", "turquoise"))


#comparing models#
#comparison plot main = "Comparison between Glover, Hunt, and Handush models using all mean values, d=5000m#
plot(glover3, col="white" ,lty=1, xlab = "Time (days)", ylab = "Qf, capture fraction" )
lines(glover3, lwd = 2, col="red",lty=1)
lines(hunt3, lwd = 2, col="orange",lty=1)
lines(hunt13, lwd = 2, col="yellow",lty=1)
lines(hantush3, lwd = 2, col="green",lty=1)
lines(hantush13, lwd = 2, col="blue",lty=1)
legend("topleft", legend = c("Glover", "Hunt Upperfloral", "Hunt Weighted", "Hantush Upperfloral", "Hantush Weighted"),
       lwd = 2, col = c("red", "orange", "yellow", "green", "blue"))

#comparison plot main = "Comparison between Glover, Hunt, and Handush models using all mean values, d=10000m#
plot(glover10, col="white" ,lty=1, xlab = "Time (days)", ylab = "Qf, capture fraction" )
lines(glover10, lwd = 2, col="red",lty=1)
lines(hunt10, lwd = 2, col="orange",lty=1)
lines(hunt20, lwd = 2, col="yellow",lty=1)
lines(hantush10, lwd = 2, col="green",lty=1)
lines(hantush20, lwd = 2, col="blue",lty=1)
legend("topleft", legend = c("Glover", "Hunt Upperfloral", "Hunt Weighted", "Hantush Upperfloral", "Hantush Weighted"),
       lwd = 2, col = c("red", "orange", "yellow", "green", "blue"))

#comparison plot main = "Comparison between Glover, Hunt, and Handush models using all mean values, d=2500m#
plot(glover7, col="white" ,lty=1, xlab = "Time (days)", ylab = "Qf, capture fraction" )
lines(glover7, lwd = 2, col="red",lty=1)
lines(hunt7, lwd = 2, col="orange",lty=1)
lines(hunt17, lwd = 2, col="yellow",lty=1)
lines(hantush7, lwd = 2, col="green",lty=1)
lines(hantush17, lwd = 2, col="blue",lty=1)
legend("topleft", legend = c("Glover", "Hunt Upperfloral", "Hunt Weighted", "Hantush Upperfloral", "Hantush Weighted"),
       lwd = 2, col = c("red", "orange", "yellow", "green", "blue"))


#comparison plot: As distance to well increases, it takes less time for the minimum Tr scenario to have the highest Qf#
plot(t9, col="white" ,lty=1, xlab = "Time (days)", ylab = "Qf, capture fraction" )
lines(t1, lwd = 2, col="red",lty=1)
lines(t2, lwd = 2, col="red",lty=2)
lines(t3, lwd = 2, col="red",lty=3)
lines(t4, lwd = 2, col="green",lty=1)
lines(t5, lwd = 2, col="green",lty=2)
lines(t6, lwd = 2, col="green",lty=3)
lines(t7, lwd = 2, col="blue",lty=1)
lines(t8, lwd = 2, col="blue",lty=2)
lines(t9, lwd = 2, col="blue",lty=3)
legend("topleft", legend = c("10km distance max. Tr", "10km distance mean Tr", "10km distance min. Tr", "5km distance max. Tr", "5km distance mean. Tr", "5km distance min. Tr","2.5km distance max. Tr","2.5km distance mean Tr","2.5km distance min. Tr"),
       lwd = 2, col = c("red", "red", "red", "green", "green","green","blue","blue","blue"), lty= c(1,2,3,1,2,3,1,2,3))

t1<- hunt(time, 10000, DalmenySMean, DalmenyTrMax, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
#at time=178 days:0.05812896
t2<- hunt(time, 10000, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
#at time=178 days:0.05711196
t3<- hunt(time, 10000, DalmenySMean, DalmenyTrMin, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
#at time=178 days:0.03393966
t4<- hunt(time, 5000, DalmenySMean, DalmenyTrMax, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
#at time=178 days:0.08630993
t5<- hunt(time, 5000, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
#at time=178 days:0.09696258
t6<- hunt(time, 5000, DalmenySMean, DalmenyTrMin, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
#at time=178 days:0.1060322
t7<- hunt(time, 2500, DalmenySMean, DalmenyTrMax, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
#at time=178 days:0.1033682
t8<- hunt(time, 2500, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
#at time=178 days:0.1227201
t9<- hunt(time, 2500, DalmenySMean, DalmenyTrMin, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
#at time=178 days:0.1687532





#Volumetric Depletion#
#10% of volumetric streamflow (m3/d) of river. Pumping rate must be below this to avoid significant ecological damage#
#Richter et al 2012 says 10% of natural streamflow, Richter et al 2017 says 10% of baseflow over a month, still others
#say up to 50% streamflow depletion Kennen et al 2014. We're going with 10% of streamflow.#
limitMarAugMean<- MarAugMean*0.1
limitMarAugMin<- MarAugMin*0.1
limitMarAugMax<- MarAugMax*0.1
limitSepFebMean<- SepFebMean*0.1
limitSepFebMin<- SepFebMin*0.1
limitSepFebMax<- SepFebMax*0.1

#Lowest S (of AQUIFER!) gives largest Qf#
#Highest Tr (of AQUIFER!) gives largest Qf at the start. But Qf of lower Tr scenarios quickly surpass it at lower d values#
#Lowest d gives largest Qf#
#Lowest k (of AQUIFER!) gives largest Qf (hantush)*****higher K starts with higher Qf but...
#...quickly drops off as time increases, and lower k gets highest Qf from then on#
#UFA streambed gives lower Qf than Weighted Qf#

#volumetric streamflow depletion = Qf * Qw
#10% of volumetric streamflow of river = limit for VSD (volumetric streamflow depletion)
#VSD limit = Qf * Qw, in m3/d
#Qw = VSDlimit/Qf#

#high flow season (mar-aug)#
#upperfloral#
MaxQw_High_Uf<- limitMarAugMax / hunt(time, 10000, DalmenySMax, DalmenyTrMin, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
#At 178 days: 1219039474 m3/d
MinQw_High_Uf<- limitMarAugMin / hunt(time, 2500, DalmenySMin, DalmenyTrMin, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
#At 178 days: 2454187
AverageQw_High_Uf<- limitMarAugMean / hunt(time, 5000, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
#At 178 days: 25402527

#weighted#
MaxQw_High_W<- limitMarAugMax / hunt(time, 10000, DalmenySMax, DalmenyTrMin, (streambed_conductance(300, streambed2, streambed2thickness)))
#At 178 days:175047357
MinQw_High_W<- limitMarAugMin / hunt(time, 2500, DalmenySMin, DalmenyTrMin, (streambed_conductance(300, streambed2, streambed2thickness)))
#At 178 days:1740263
AverageQw_High_W<- limitMarAugMean / hunt(time, 5000, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, streambed2, streambed2thickness)))
#At 178 days: 16557103

#nostreambed#
MaxQw_High_No<- limitMarAugMax / glover(time, 10000, DalmenySMax, DalmenyTrMin)
#At 178 days: 17215030
MinQw_High_No<- limitMarAugMin / glover(time, 2500, DalmenySMin, DalmenyTrMin)
#At 178 days: 723093.6
AverageQw_High_No<- limitMarAugMean / glover(time, 5000, DalmenySMean, DalmenyTrMean)
#At 178 days: 3371862

#comparison plot: no, weighted, and upperfloral streambeds with the max, min, and mean Qw in High flow season
plot(MaxQw_High_Uf, xlim=c(1,178),ylim=c(10e+3,10e+20),log="y", col="white" ,lty=1, xlab = "Time (days)", ylab = "Qw, pumping rate [m3/day]" )
lines(MaxQw_High_Uf, lwd = 2, col="red",lty=1)
lines(MinQw_High_Uf, lwd = 2, col="green",lty=1)
lines(AverageQw_High_Uf, lwd = 2, col="blue",lty=1)
lines(MaxQw_High_W, lwd = 2, col="red",lty=2)
lines(MinQw_High_W, lwd = 2, col="green",lty=2)
lines(AverageQw_High_W, lwd = 2, col="blue",lty=2)
lines(MaxQw_High_No, lwd = 2, col="red",lty=3)
lines(MinQw_High_No, lwd = 2, col="green",lty=3)
lines(AverageQw_High_No, lwd = 2, col="blue",lty=3)
legend("topright", legend = c("Upperfloral 10km distance max. Qw", "Upperfloral 2.5km distance min. Qw", "Upperfloral 5km distance mean Qw", "Weighted 10km distance max. Qw", "Weighted 2.5km distance min. Qw", "Weighted 5km distance mean Qw","No bed 10km distance max. Qw","No bed 2.5km distance min. Qw","No bed 5km distance mean Qw"),
       lwd = 2, col = c("red", "green", "blue", "red", "green","blue","red","green","blue"), lty = c(1,1,1,2,2,2,3,3,3))



#low flow season (sep-feb)#
#upperfloral#
MaxQw_Low_Uf<- limitSepFebMax / hunt(time, 10000, DalmenySMax, DalmenyTrMin, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
#At 178 days: 408263220
MinQw_Low_Uf<- limitSepFebMin / hunt(time, 2500, DalmenySMin, DalmenyTrMin, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
#At 178 days: 2386114
AverageQw_Low_Uf<- limitSepFebMean / hunt(time, 5000, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, ufaKMean, ufaMeanThickness)))
#At 178 days: 13828526

#weighted#
MaxQw_Low_W<- limitSepFebMax / hunt(time, 10000, DalmenySMax, DalmenyTrMin, (streambed_conductance(300, streambed2, streambed2thickness)))
#At 178 days: 262181819
MinQw_Low_W<- limitSepFebMin / hunt(time, 2500, DalmenySMin, DalmenyTrMin, (streambed_conductance(300, streambed2, streambed2thickness)))
#At 178 days: 1691993
AverageQw_Low_W<- limitSepFebMean / hunt(time, 5000, DalmenySMean, DalmenyTrMean, (streambed_conductance(300, streambed2, streambed2thickness)))
#At 178 days: 9013290

#nostreambed#
MaxQw_Low_No<- limitSepFebMax / glover(time, 10000, DalmenySMax, DalmenyTrMin)
#At 178 days: 38963273
MinQw_Low_No<- limitSepFebMin / glover(time, 2500, DalmenySMin, DalmenyTrMax)
#At 178 days: 624332.8
AverageQw_Low_No<- limitSepFebMean / glover(time, 5000, DalmenySMean, DalmenyTrMean)
#At 178 days: 1835561

#comparison plot: no, weighted, and upperfloral streambeds and the max, min, and mean Qw Low flow season
plot(MaxQw_Low_Uf, xlim=c(1,178),ylim=c(10e+3,10e+20),log="y", col="white" ,lty=1, xlab = "Time (days)", ylab = "Qw, pumping rate [m3/day]" )
lines(MaxQw_Low_Uf, lwd = 2, col="red",lty=1)
lines(MinQw_Low_Uf, lwd = 2, col="green",lty=1)
lines(AverageQw_Low_Uf, lwd = 2, col="blue",lty=1)
lines(MaxQw_Low_W, lwd = 2, col="red",lty=2)
lines(MinQw_Low_W, lwd = 2, col="green",lty=2)
lines(AverageQw_Low_W, lwd = 2, col="blue",lty=2)
lines(MaxQw_Low_No, lwd = 2, col="red",lty=3)
lines(MinQw_Low_No, lwd = 2, col="green",lty=3)
lines(AverageQw_Low_No, lwd = 2, col="blue",lty=3)
legend("topright", legend = c("Upperfloral 10km distance max. Qw", "Upperfloral 2.5km distance min. Qw", "Upperfloral 5km distance mean Qw", "Weighted 10km distance max. Qw", "Weighted 2.5km distance min. Qw", "Weighted 5km distance mean Qw","No bed 10km distance max. Qw","No bed 2.5km distance min. Qw","No bed 5km distance mean Qw"),
       lwd = 2, col = c("red", "green", "blue", "red", "green","blue","red","green","blue"), lty = c(1,1,1,2,2,2,3,3,3))

#A prettier version of the comparison plot above#
plot(MaxQw_Low_Uf, xlim=c(1,178),ylim=c(10e+3,10e+20),log="y", col="white" ,lty=1, xlab = "Time (days)", ylab = "Qw, pumping rate [m3/day]" )
lines(MaxQw_Low_Uf, lwd = 2, col="red",lty=1)
lines(MinQw_Low_Uf, lwd = 2, col="olivedrab",lty=1)
lines(AverageQw_Low_Uf, lwd = 2, col="darkorchid",lty=1)
lines(MaxQw_Low_W, lwd = 2, col="red",lty=2)
lines(MinQw_Low_W, lwd = 2, col="olivedrab",lty=2)
lines(AverageQw_Low_W, lwd = 2, col="darkorchid",lty=2)
lines(MaxQw_Low_No, lwd = 2, col="red",lty=3)
lines(MinQw_Low_No, lwd = 2, col="olivedrab",lty=3)
lines(AverageQw_Low_No, lwd = 2, col="darkorchid",lty=3)
legend("topright", legend = c("Upperfloral max. Qw", "Upperfloral min. Qw", "Upperfloral mean Qw", "Weighted max. Qw", "Weighted min. Qw", "Weighted mean Qw","No bed max. Qw","No bed min. Qw","No bed mean Qw"),
       lwd = 2, col = c("red", "olivedrab", "darkorchid", "red", "olivedrab","darkorchid","red","olivedrab","darkorchid"),  lty = c(1, 1, 1, 2, 2, 2, 3, 3, 3))


#comparison plot: no, weighted, and upperfloral streambeds and their mean Qw, for Low and High flow seasons
plot(MaxQw_Low_Uf, xlim=c(1,178),ylim=c(10e+5,10e+14),log="y", col="white" ,lty=1, xlab = "Time (days)", ylab = "Qw, pumping rate [m3/day]" )
lines(AverageQw_Low_Uf, lwd = 2, col="red",lty=1)
lines(AverageQw_Low_W, lwd = 2, col="orange",lty=1)
lines(AverageQw_Low_No, lwd = 2, col="yellow",lty=1)
lines(AverageQw_High_Uf, lwd = 2, col="purple",lty=1)
lines(AverageQw_High_W, lwd = 2, col="blue",lty=1)
lines(AverageQw_High_No, lwd = 2, col="turquoise",lty=1)
legend("topright", legend = c("Upperfloral steambed, lowflow season","Weighted steambed, lowflow season","No steambed, lowflow season","Upperfloral steambed, highflow season","Weighted steambed, highflow season","No steambed, highflow season"),
       lwd = 2, col = c("red", "orange", "yellow", "purple", "blue","turquoise"))

