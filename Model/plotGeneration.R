
setwd("C:/Users/Oliwia Kozlowska/Documents/R/RworkCoursera/Capstone/DataScienceCapstoneProject")

load("./Data/dimReduction/eval10k_train.Rdata") 
load("./Data/dimReduction/eval10k_val.Rdata") 

load("./Data/dimReduction/eval20k_train.Rdata") 
load("./Data/dimReduction/eval20k_val.Rdata") 

load("./Data/dimReduction/eval30k_train.Rdata") 
load("./Data/dimReduction/eval30k_val.Rdata") 

load("./Data/dimReduction/eval40k_train.Rdata") 
load("./Data/dimReduction/eval40k_val.Rdata") 

load("./Data/dimReduction/eval50k_train.Rdata") 
load("./Data/dimReduction/eval50k_val.Rdata") 

eval10k_train_full <- c(eval10k_train ,ts_size=864656, model_size=5035192)
eval20k_train_full <- c(eval20k_train ,ts_size=1723088, model_size=8058168)
eval30k_train_full <- c(eval30k_train ,ts_size=2578928, model_size=10389464)
eval40k_train_full <- c(eval40k_train ,ts_size=3434768, model_size=12150768)
eval50k_train_full <- c(eval50k_train ,ts_size=4289920, model_size=13472864)

data_size <- c(6361136, 3130064, 1244168, 624672, 56552)
model_size <- c(112563544, 61515704, 27818880, 14912296, 702616)
model_time <- c(15.71427064, 6.21411338, 2.35848055, 0.88785982, 0.01773076)

load("./Data/dimReduction/eval20k_test.Rdata") 

## Saving plots as png, to be included in preso

png(filename = "./Slides/figures/plot1.png",        # Output file name
    width = 800, height = 800,       # Dimensions in pixels
    units = "px",                    # Units: px, in, cm, mm
    res = 120)  

par(oma = c(2, 3, 3, 3), mgp=c(4,1,0))
plot(x = c(10000,5000,2000,1000,100),
     y = c(data_size[1]/1000000, data_size[2]/1000000, data_size[3]/1000000, 
           data_size[4]/1000000, data_size[5]/1000000), 
     type="b", xlab="Corpus size - solid line (in Mb), Model size - dashed line (in Mb)
     Time to predict next word - dotted line (in s, secondary axis)
     1s threshold - horizontal line (in s, red)", 
     ylab="Data/ model features", 
     main="Count of top scoring documents 
     vs. data/ model features",
     ylim=c(0,120),lwd=2)

lines(x = c(10000,5000,2000,1000,100),
      y = c(model_size[1]/1000000, model_size[2]/1000000, model_size[3]/1000000, 
            model_size[4]/1000000, model_size[5]/1000000),type="b", lty=2, lwd=2)

par(new = TRUE)
plot( x = c(10000,5000,2000,1000,100), 
      y = c(model_time[1], model_time[2], model_time[3], model_time[4], model_time[5]),
      type="b", lty=3, lwd = 2,
      axes = FALSE, xlab = "", ylab = "",
      ylim = c(0, 16),col="red")
abline(h=1, lwd=2,col="red")

sec_ticks <- model_time
sec_labels <- paste0(as.character(round(model_time,2))," s")
axis(side = 4, at=sec_ticks, labels = sec_labels, las = 1)

dev.off()


png(filename = "./Slides/figures//plot2.png",        # Output file name
    width = 800, height = 800,       # Dimensions in pixels
    units = "px",                    # Units: px, in, cm, mm
    res = 120)  

par(oma = c(2, 3, 1, 3), mgp=c(4,1,0))
plot(c(as.numeric(eval10k_train_full["model_size"])/1000000, as.numeric(eval20k_train_full["model_size"])/1000000, 
       as.numeric(eval30k_train_full["model_size"])/1000000, as.numeric(eval40k_train_full["model_size"])/1000000, 
       as.numeric(eval50k_train_full["model_size"])/1000000), 
     c(eval10k_train_full["top1"], eval20k_train_full["top1"], eval30k_train_full["top1"], eval40k_train_full["top1"], 
       eval50k_train_full["top1"]), 
     type="b", xlab="Model Size (Mb) vs. Accuracy, Top-1 - solid line, Top-5 - dashed line, 
     Training set - blue, Validation set - green, 
     Testing set - red (horizontal line, for selected model size)", 
     ylab="Accuracy", main="Model Size vs. Top-1/ Top-5 Accuracy",
     xlim=c(5,14), ylim=c(0,0.35), lwd=2, col="blue")
mtext("Accuracy", side = 2, line =3)

lines( x = c(as.numeric(eval10k_train_full["model_size"])/1000000, as.numeric(eval20k_train_full["model_size"])/1000000, 
             as.numeric(eval30k_train_full["model_size"])/1000000, as.numeric(eval40k_train_full["model_size"])/1000000, 
             as.numeric(eval50k_train_full["model_size"])/1000000), 
       y = c(eval10k_val["top1"], eval20k_val["top1"], eval30k_val["top1"], eval40k_val["top1"], 
             eval50k_val["top1"]),type="b",lwd=2, col="green")

lines( x = c(as.numeric(eval10k_train_full["model_size"])/1000000, as.numeric(eval20k_train_full["model_size"])/1000000, 
             as.numeric(eval30k_train_full["model_size"])/1000000, as.numeric(eval40k_train_full["model_size"])/1000000, 
             as.numeric(eval50k_train_full["model_size"])/1000000), 
       y = c(eval10k_train_full["top5"], eval20k_train_full["top5"], eval30k_train_full["top5"], eval40k_train_full["top5"], 
             eval50k_train_full["top5"]),type="b", lty=2, lwd=2, col="blue")

lines( x = c(as.numeric(eval10k_train_full["model_size"])/1000000, as.numeric(eval20k_train_full["model_size"])/1000000, 
             as.numeric(eval30k_train_full["model_size"])/1000000, as.numeric(eval40k_train_full["model_size"])/1000000, 
             as.numeric(eval50k_train_full["model_size"])/1000000), 
       y = c(eval10k_val["top5"], eval20k_val["top5"], eval30k_val["top5"], eval40k_val["top5"], 
             eval50k_val["top5"]),type="b",lwd=2, lty=2, col="green")

abline(h=eval20k_test["top1"], lwd=2, col="red")
abline(h=eval20k_test["top5"], lwd=2, lty=2, col="red")

dev.off()


png(filename = "./Slides/figures/plot3.png",        # Output file name
    width = 800, height = 800,       # Dimensions in pixels
    units = "px",                    # Units: px, in, cm, mm
    res = 120)  

par(oma = c(2, 3, 1, 3), mgp=c(4,1,0))
plot(c(as.numeric(eval10k_train_full["model_size"])/1000000, as.numeric(eval20k_train_full["model_size"])/1000000, 
       as.numeric(eval30k_train_full["model_size"])/1000000, as.numeric(eval40k_train_full["model_size"])/1000000, 
       as.numeric(eval50k_train_full["model_size"])/1000000), 
     c(eval10k_train_full["perplexity"], eval20k_train_full["perplexity"], eval30k_train_full["perplexity"], 
       eval40k_train_full["perplexity"], eval50k_train_full["perplexity"]), 
     type="b", xlab="Model size (Mb) vs. Perplexity, 
     Training set - blue, Validation set - green, 
     Testing set - red (horizontal line, for selected model size)", ylab="Perplexity", main="Model Size vs. Perplexity",
     xlim=c(5,14), ylim=c(0,800), lwd=2, col="blue")
mtext("Perplexity", side = 2, line =3)
lines( x = c(as.numeric(eval10k_train_full["model_size"])/1000000, as.numeric(eval20k_train_full["model_size"])/1000000, 
             as.numeric(eval30k_train_full["model_size"])/1000000, as.numeric(eval40k_train_full["model_size"])/1000000, 
             as.numeric(eval50k_train_full["model_size"])/1000000), 
       y = c(eval10k_val["perplexity"], eval20k_val["perplexity"], eval30k_val["perplexity"], 
             eval40k_val["perplexity"], eval50k_val["perplexity"]),type="b", lwd=2, col="green")

abline(h=eval20k_test["perplexity"], lwd=2,col="red")

dev.off()

png(filename = "./Slides/figures/plot4.png",        # Output file name
    width = 800, height = 800,       # Dimensions in pixels
    units = "px",                    # Units: px, in, cm, mm
    res = 120)  

par(oma = c(2, 3, 1, 3), mgp=c(4,1,0))
plot(c(as.numeric(eval10k_train_full["model_size"])/1000000, as.numeric(eval20k_train_full["model_size"])/1000000, 
       as.numeric(eval30k_train_full["model_size"])/1000000, as.numeric(eval40k_train_full["model_size"])/1000000, 
       as.numeric(eval50k_train_full["model_size"])/1000000), 
     c(round(as.numeric(grep("0", eval10k_train_full["pred_time"], value=TRUE)),4),
       round(as.numeric(grep("0", eval20k_train_full["pred_time"], value=TRUE)),4),
       round(as.numeric(grep("0", eval30k_train_full["pred_time"], value=TRUE)),4),
       round(as.numeric(grep("0", eval40k_train_full["pred_time"], value=TRUE)),4),
       round(as.numeric(grep("0", eval50k_train_full["pred_time"], value=TRUE)),4)),
     type="b", xlab="Model size (Mb) vs. Time to predict next word (s),
     Training set - blue, Validation set - green, 
     Testing set - red (horizontal line, for selected model size)", ylab="Time to predict next word", 
     main="Model Size vs. Time to predict next word",
     xlim=c(5,14), ylim=c(0,1), lwd=2, col="blue")
mtext("Time to predict next word (s)", side = 2, line =3)
lines( x = c(as.numeric(eval10k_train_full["model_size"])/1000000, as.numeric(eval20k_train_full["model_size"])/1000000, 
             as.numeric(eval30k_train_full["model_size"])/1000000, as.numeric(eval40k_train_full["model_size"])/1000000, 
             as.numeric(eval50k_train_full["model_size"])/1000000), 
       y = c(round(as.numeric(grep("0", eval10k_val["pred_time"], value=TRUE)),4),
             round(as.numeric(grep("0", eval20k_val["pred_time"], value=TRUE)),4),
             round(as.numeric(grep("0", eval30k_val["pred_time"], value=TRUE)),4),
             round(as.numeric(grep("0", eval40k_val["pred_time"], value=TRUE)),4),
             round(as.numeric(grep("0", eval50k_val["pred_time"], value=TRUE)),4)),type="b", lwd=2, col="green")

abline(h=round(as.numeric(grep("0", eval20k_test["pred_time"], value=TRUE)),4), lwd=2,col="red")

dev.off()

