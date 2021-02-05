library(reshape2)
library(Rmisc)
library(ggplot2)

data = read.csv("X2_data/x2pilotDataModelOptimizedSorted.csv")

colnames(data) <- c("workerid","CCode","obj1","obj2","obj3","obj1OC27","obj2OC27","obj3OC27",
                    "DPost_1","DPost_2","DPost_3","DPost_4","DPost_5",
                    "DPost_6","DPost_7","DPost_8","DPost_9",
                    "MPost1_1","MPost1_2","MPost1_3","MPost1_4",
                    "MPost1_5","MPost1_6","MPost1_7","MPost1_8","MPost1_9",
                    "MPost2_1","MPost2_2","MPost2_3","MPost2_4","MPost2_5",
                    "MPost2_6","MPost2_7","MPost2_8","MPost2_9")
summary(data$CCode)

global <- read.csv("X2_data/x2pilotDataModelOptimizedSorted_global.csv")
colnames(global) <- c("workerid","CCode","obj1","obj2","obj3","obj1OC27","obj2OC27","obj3OC27",
                    "DPost_1","DPost_2","DPost_3","DPost_4","DPost_5",
                    "DPost_6","DPost_7","DPost_8","DPost_9",
                    "MPost1_1","MPost1_2","MPost1_3","MPost1_4",
                    "MPost1_5","MPost1_6","MPost1_7","MPost1_8","MPost1_9",
                    "MPost2_1","MPost2_2","MPost2_3","MPost2_4","MPost2_5",
                    "MPost2_6","MPost2_7","MPost2_8","MPost2_9")

#sample2 <- subset(data, CCode == "22a1")
#sample3 <- subset(data, CCode == "322a")

#unsorted <- read.csv("x3pilotDataAugmV0_fixedAndD_052019.csv")
# sample <- data.frame(apply(sample, 2, function(x) {x <- gsub("\"", "", x)}))

sample <- subset(data, CCode == "22b2b")
sample_global <- subset(global, CCode == "22b2b")
#sample <- subset(data, CCode == "22a1")

# currentObjects <- c(23,4,19)
# featureValueOrder[[19]] # Order of feature values for a particular object combination of class 22b2b, we need it to label the bars

## Now reshape the table

# sample <- sample3

behavior <- melt(sample,
                 id.vars = c("CCode","obj1","obj2","obj3","obj1OC27","obj2OC27","obj3OC27"),
                 measure.vars = c("DPost_1","DPost_2","DPost_3","DPost_4","DPost_5","DPost_6","DPost_7","DPost_8","DPost_9"),
                 variable.name = "sliderNumber",value.name = "sliderValue")

modelNotOpt <- melt(sample,
                 id.vars = c("CCode","obj1","obj2","obj3","obj1OC27","obj2OC27","obj3OC27"),
                 measure.vars = c("MPost1_1","MPost1_2","MPost1_3","MPost1_4","MPost1_5","MPost1_6","MPost1_7","MPost1_8","MPost1_9"),
                 variable.name = "sliderNumber",value.name = "sliderValue")

model <- melt(sample,
                    id.vars = c("CCode","obj1","obj2","obj3","obj1OC27","obj2OC27","obj3OC27"),
                    measure.vars = c("MPost2_1","MPost2_2","MPost2_3","MPost2_4","MPost2_5","MPost2_6","MPost2_7","MPost2_8","MPost2_9"),
                    variable.name = "sliderNumber",value.name = "sliderValue")

globalOpt <- melt(sample_global,
                  id.vars = c("CCode","obj1","obj2","obj3","obj1OC27","obj2OC27","obj3OC27"),
                  measure.vars = c("MPost1_1","MPost1_2","MPost1_3","MPost1_4","MPost1_5","MPost1_6","MPost1_7","MPost1_8","MPost1_9"),
                  variable.name = "sliderNumber",value.name = "sliderValue")

behavior$sliderNumber <- gsub(paste0("DPost_",collapse = "|"),"", behavior$sliderNumber)
modelNotOpt$sliderNumber <- gsub(paste0("MPost1_",collapse = "|"),"", modelNotOpt$sliderNumber)
model$sliderNumber <- gsub(paste0("MPost2_",collapse = "|"),"", model$sliderNumber)
globalOpt$sliderNumber <- gsub(paste0("MPost1_",collapse = "|"),"", globalOpt$sliderNumber)

# behavior$sliderValue <- as.numeric(behavior$sliderValue)

statsBehavior <- summarySE(behavior, measurevar="sliderValue", groupvars="sliderNumber",na.rm = TRUE)
statsModelNotOpt <- summarySE(modelNotOpt, measurevar="sliderValue", groupvars="sliderNumber",na.rm = TRUE)
statsModel <- summarySE(model, measurevar="sliderValue", groupvars="sliderNumber",na.rm = TRUE)
statsGlobal <- summarySE(globalOpt, measurevar="sliderValue", groupvars="sliderNumber",na.rm = TRUE)

statsBehavior$type <- "behavior"
statsModel$type <- "model"
statsModelNotOpt$type <- "modelNotOpt"
statsGlobal$type <- "global"

behaviorPlot <- statsBehavior[c(1:6),]
modelPlot <- statsModel[c(1:6),]
modelNotOptPlot <- statsModelNotOpt[c(1:6),]
globalPlot <- statsGlobal[c(1:6),]

allStats <- rbind(behaviorPlot,modelPlot,modelNotOptPlot,globalPlot)

ggplot(allStats, aes(x=sliderNumber, y=sliderValue, fill = type)) +
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
#  coord_flip()+
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=sliderValue-ci, ymax=sliderValue+ci)) +
  scale_fill_manual(values = c("white","ivory2","ivory3","ivory4"), labels = c("Data","Globally opt.", "Indiv. opt", "Non-opt."), name = "") +
  theme_bw(base_size = 18)+
#  labs(title="Example of class 22b2b")+
  ylab("Slider value (normalized)")+
  xlab("")+
  scale_x_discrete(limits=c("1","2","3","4","5","6"),
                   labels=c("cloud", "circle", "striped","solid","blue","green"))+
  theme(legend.position="bottom")+
#  theme (axis.text.x=element_text(size = 16),
 #        axis.text.y=element_text(size = 16),
#         axis.title.x=element_text(size = 16),
 ##        axis.title.y=element_text(size = 16),
#         legend.text=element_text(size = 14))
  #ggsave("utterancePlot.pdf")
ggsave("X2_plots/barplot_x2_withGlobal.pdf",width=6.5,height=4.5, units = "in")
