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


############ Figure 15 #####################

### Baseline model ##

plotData <- subset(full, Nr == 20)

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure_base <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 8) +
  labs(title = "Baseline model", subtitle = "Uniform")+
  #  labs(title = bquote(atop
  #                     (.(type) ~"," ~ r^2 == .(r2),
  #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
  #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
print(figure_base)
#ggsave(figure, height = 3, width = 3, units = "in", filename = paste("X2_Plots/x2_m", nr,".pdf", sep=""))

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)
confint(model)

# for m7
# Shortcut to do subsets. List_of_models_x2.csv says which model is which

plotData <- subset(full, Nr == 7)

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure_nonopt <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 8) +
  labs(title = "Simple model",subtitle = "Non-optimized")+
  #  labs(title = bquote(atop
  #                     (.(type) ~"," ~ r^2 == .(r2),
  #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
  #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
print(figure_nonopt)

fig15 <- grid.arrange(figure_base, figure_nonopt, ncol = 2)
ggsave(fig15, height = 2.5, width = 5, units = "in", filename = "X2_plots/fig15.pdf")


######################################

########## Figure 16 #################

# for m11 KL individually optimized
plotData <- subset(full, Nr == 11)
model <- lm(formula = plotData$model~plotData$workerData)
summary(model)

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure_individual <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 8) +
  labs(title = "KL factor", subtitle = "Individually optimized")+
  #  labs(title = bquote(atop
  #                     (.(type) ~"," ~ r^2 == .(r2),
  #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
  #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
print(figure_individual)
#ggsave(figure, height = 3, width = 3, units = "in", filename = paste("x2_m", nr,".pdf", sep=""))

######## Global optimization plot ############

plotData <- subset(full, Nr == 21)

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure_global <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 8) +
  labs(title = "KL factor", subtitle = "Globally optimized")+
  #  labs(title = bquote(atop
  #                     (.(type) ~"," ~ r^2 == .(r2),
  #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
  #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
print(figure)
#ggsave(figure, height = 3, width = 3, units = "in", filename = paste("x2_m", nr,".pdf", sep=""))

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)
confint(model)

fig16 <- grid.arrange(figure_global, figure_individual, ncol = 2)
ggsave(fig16, height = 2.5, width = 5, units = "in", filename = "X2_plots/fig16.pdf")

##################################

## General code for scatterplots with ggplot2 ##
# Contains a title with parameter values ##

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


