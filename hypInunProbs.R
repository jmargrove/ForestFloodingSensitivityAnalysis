### hypothesis on water inundaiton 

require(car)
require(arm)
require(ggplot2)
require(viridis)


x <- seq(0, 10, length = 100)
y1 <- invlogit(seq(5, -5, length = 100))
y2 <- invlogit(seq(5, 0, length = 100))
y3 <- invlogit(seq(5, 5, length = 100))

ylabs <- c("")

d1 <- data.frame(
  x = c(x, x, x),
  y = c(y1, y2, y3),
  `Species.type` = rep(c("Sensitive", "Medium", "Tollerant"),
  each = 100)
)

head(d1)
colnames(d1)[3] <- "Species type"
d1[, "Species type"] <- as.factor(d1[, "Species type"])

d1[, "Species type"] <- relevel(d1[, "Species type"], ref = c("Sensitive"))
d1[, "Species type"] <- relevel(d1[, "Species type"], ref = c("Tollerant"))

p1 <- ggplot(d1, aes(
  x,
  y,
  color = `Species type`,
  linetype = `Species type`)
)  + geom_line(size = 1) +
  ylim(0, 1) +
  scale_color_viridis(discrete = T) +
  theme_bw() +
  theme(legend.position = "top") +
  ylab("Probablity of establishment \n on the alluval plain") +
  xlab("Local density of inundation") +
  theme(axis.text.x = element_blank())

p1

ggsave(p1, file = "./graphs/hypProbAlluvail.png",
       width = 6,
       height = 4)





x <- seq(0, 10, length = 100)
y1 <- invlogit(seq(5, 0, length = 100))
y2 <- invlogit(seq(5, 2.5, length = 100))
y3 <- invlogit(seq(5, 5, length = 100))
d2 <- data.frame(x = c(x, x, x),
y = c(y1, y2, y3),
f = rep(c("y1", "y2", "y3"),
each = 100))


dt <- rbind(d1, d2)
dt$tol <- rep(c("Inundaiton Sensitive", "Inundaiton Tollerant"), each = 300)
ggplot(dt, aes(x, y, color = f, linetype = f)) + geom_line(size = 2) +
  ylim(0, 1) +
  facet_wrap(~tol) +
  scale_color_viridis(discrete = T) +
  theme_bw() +
  theme(legend.position = "top")