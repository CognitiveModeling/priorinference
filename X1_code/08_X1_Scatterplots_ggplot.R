library("dplyr")
library("ggplot2")
library("magrittr")
library("gridExtra")

################################################## X 1 Prior inference ##################################

# Import full csv table

full <- read.csv("X1_data/x1_for_ggplot_scatterplots.csv")
head(full)

## See what models we have ##

stats <- full %>%
    group_by(softness,obedience, alpha, cross_validated,type, Nr) %>%
    summarise(n_occur = n())
ordered <- stats[order(stats$Nr),] # Sort the summary table
print.data.frame(ordered)

##########################################################################################################

### Subsets for figures in the paper. 

## Figure 10

################ M1 Simple non-optimized model ################
plotData <- subset(full, softness == 0 & obedience == 0 & 
                     cross_validated == "no" & type == "simpleRSA") 


r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1, size = 1) +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 8) +
  labs(title = "Simple model")+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5))
print(figure)
#ggsave(figure, height = 3, width = 3, units = "in", filename = paste("m", nr,".pdf", sep=""))


################ M19 Full non-optimized model ################

plotData <- subset(full, softness == 0 & obedience == 0 & 
                     cross_validated == "no" & type == "fullRSA" & alpha == 1) 

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure_full <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1, size = 1) +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 8) +
  labs(title = "Full model")+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5))
print(figure_full)
#ggsave(figure_full, height = 3, width = 3, units = "in", filename = paste("m", nr,".pdf", sep=""))


fig10 <- grid.arrange(figure, figure_full, ncol = 2)
ggsave(fig10, height = 2.5, width = 5, units = "in", filename = "X1_plots/fig10.pdf")

## Cognition Figure 11 ##



########## M 5 Cross-validated simple model with softness and obedience optimized individually ##########

plotData <- subset(full, softness == "individually_opt" & obedience == "individually_opt" & 
                                             cross_validated == "no" & type == "simpleRSA") 

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)
confint(model)

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
  theme_bw(base_size = 8) +
  labs(title = "Simple model", subtitle = "Non-cross-validated")+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
print(figure)
ggsave(figure, height = 3, width = 3, units = "in", filename = paste("m", nr,".pdf", sep=""))

################ for M 8 Non-cross-validated simple model with softness and obedience optimized individually ################

plotData <- subset(full, softness == "individually_opt" & obedience == "individually_opt" & 
                    cross_validated == "yes" & type == "simpleRSA") 

model <- lm(formula = plotData$model~plotData$workerData)

summary(model)
confint(model)

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure_cross <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 8) +
  labs(title = "Simple model", subtitle = "Cross-validated")+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
print(figure)
ggsave(figure, height = 3, width = 3, units = "in", filename = paste("m", nr,".pdf", sep=""))


fig11 <- grid.arrange(figure, figure_cross, ncol = 2)
ggsave(fig11, height = 2.5, width = 5, units = "in", filename = "X1_plots/fig11.pdf")


