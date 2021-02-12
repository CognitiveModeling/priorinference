library("dplyr")
library("ggplot2")

################################################## X2 Utterance choice ##################################

# Import full csv table

full <- read.csv("X2_plots/forFigures6and7.csv")
head(full)

## See what models we have ##

stats <- full %>%
    group_by(softness,obedience, alpha, cross_validated, kl_factor,type, Nr) %>%
    summarise(n_occur = n())
ordered <- stats[order(stats$Nr),] # Sort the summary table
print.data.frame(ordered)

##########################################################################################################

# Subset data for plotting. Fill in the template

# plotData <- subset(full, softness ==  & obedience ==  & cross_validated == " " & type == "") # for simple models
# plotData <- subset(full, softness ==  & obedience ==  & alpha == & cross_validated == " " & type == "") # for full models

# Softness, obedience, and alpha are numeric
# Cross_validated can be "yes" or "no"
# Type can be "simpleRSA" or "fullRSA" or "uniform"
# Nr is model number in the order they are listed in the scatterplot file. 

##########################################################################################################

### Subsets for figures in the paper. Pick one plotData subset, then run the define variables part
### Then generate the plot either with ggplot or a simple plot

# for m1
plotData <- subset(full, softness == 0 & obedience == 0 & 
                        kl_factor == 1 &
                          cross_validated == "no" & type == "fullRSA" & 
                          alpha == 1) 


r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 14) +
  labs(title = "Full model")+
  #  labs(title = bquote(atop
  #                     (.(type) ~"," ~ r^2 == .(r2),
  #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
  #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5))
print(figure)
ggsave(figure, height = 3, width = 3, units = "in", filename = paste("x2_m", nr,".pdf", sep=""))

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)
confint(model)


# for m6
plotData <- subset(full, softness == "individually_opt" & obedience == "individually_opt" & 
                     kl_factor == "individually_opt" &
                     cross_validated == "no" & type == "fullRSA" & 
                     alpha == 1) 

# for m4
plotData <- subset(full, softness == 0 & obedience == "individually_opt" & 
                     kl_factor == "individually_opt" &
                     cross_validated == "no" & type == "fullRSA" & 
                     alpha == 1) 

# for m2
plotData <- subset(full, softness == "individually_opt" & obedience == 0 & 
                     kl_factor == 1 &
                     cross_validated == "no" & type == "fullRSA" & 
                     alpha == 1) 
# for m7
# Shortcut to do subsets. List_of_models_x2.csv says which model is which

plotData <- subset(full, Nr == 7)

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 14) +
  labs(title = "Simple model",subtitle = "Non-optimized")+
  #  labs(title = bquote(atop
  #                     (.(type) ~"," ~ r^2 == .(r2),
  #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
  #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
print(figure)
ggsave(figure, height = 3, width = 3, units = "in", filename = paste("x2_m", nr,".pdf", sep=""))

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)
confint(model)


# for m9
plotData <- subset(full, Nr == 9) 
model <- lm(formula = plotData$model~plotData$workerData)
summary(model)

# for m10
plotData <- subset(full, Nr == 10)
model <- lm(formula = plotData$model~plotData$workerData)
summary(model)

# for m11 KL individually optimized
plotData <- subset(full, Nr == 11)
model <- lm(formula = plotData$model~plotData$workerData)
summary(model)

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 14) +
  labs(title = "Simple model", subtitle = "Individually optimized")+
  #  labs(title = bquote(atop
  #                     (.(type) ~"," ~ r^2 == .(r2),
  #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
  #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
print(figure)
ggsave(figure, height = 3, width = 3, units = "in", filename = paste("x2_m", nr,".pdf", sep=""))

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)
confint(model)


# for m9

# for m13
plotData <- subset(full, Nr == 13)

# for m14
plotData <- subset(full, Nr == 14)

plotData <- subset(full, Nr == 15)
model <- lm(formula = plotData$model~plotData$workerData)
summary(model)

plotData <- subset(full, Nr == 20)

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 14) +
  labs(title = "Baseline model", subtitle = "Uniform")+
  #  labs(title = bquote(atop
  #                     (.(type) ~"," ~ r^2 == .(r2),
  #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
  #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
print(figure)
ggsave(figure, height = 3, width = 3, units = "in", filename = paste("x2_m", nr,".pdf", sep=""))

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)
confint(model)

plotData <- subset(full, Nr == 24)
model <- lm(formula = plotData$model~plotData$workerData)
summary(model)
# Define variables to pass to plot title and ggsave

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 2)
#ci <- round((confint(lm(plotData$model~plotData$workerData))), digits = 2)
#ci <- ci[c(2,4)]
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
kl_factor <- unique(as.character(plotData$kl_factor))
nr <- plotData$Nr[1]


## Scatterplot with ggplot2 ##
# Contains a title with parameter values for now ##

figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point() +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = 1) +
  theme_bw() +
  labs(title = bquote(atop
                     (.(type) ~"," ~ r^2 == .(r2),
                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience) ~ ","
                       ~ "lambda" == .(kl_factor)
                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5))
#print(figure)
ggsave(figure, filename = paste("x2_m", nr,".pdf", sep=""),width = 7, height = 7, units = "in")


######## Global optimization plot ############

plotData <- subset(full, Nr == 21)

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 14) +
  labs(title = "Simple model", subtitle = "Globally optimized")+
  #  labs(title = bquote(atop
  #                     (.(type) ~"," ~ r^2 == .(r2),
  #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
  #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
print(figure)
ggsave(figure, height = 3, width = 3, units = "in", filename = paste("x2_m", nr,".pdf", sep=""))

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)
confint(model)




## Simple plot works a little faster to look up r2 values ##

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)
confint(model)

plot(plotData$model, plotData$workerData, xlab = "model", ylab = "human data")
abline(lm(formula = plotData$model~plotData$workerData), col="red") # regression line (y~x)
lines(lowess(plotData$model,plotData$workerData), col="blue") # lowess line (x,y)

title(main = bquote(atop
      (.(type) ~"," ~ r^2 == .(r2),
        ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
        )))

#########################################################################################################


