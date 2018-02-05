################################################################################
# title: Calculating the Risk ratios for the experiments 
# author: James Margrove 



load("./models/Model.Rdata")
data <- read.table("./data/data.txt", header = TRUE)


pred1 <- expand.grid(sp = levels(data$sp), flood = c("dry","wet"), ztopo = 0, dia = mean(data[data$f.time==3,]$dia,na.rm=T), f.time = "3", blockL = 0, wl = 0)
pred1$p <- predict(s3, pred1, re.form = ~0)
preds

rr <- with(preds, tapply(m, sp, diff))
plot(rr)

riskratio_data <- data.frame(sp = levels(data$sp), rr = rr)

write.table(riskratio_data, file='./data/riskratio_data.txt')
