# data merging

load("data/asym_test_data.RData") # tdat

load("data/asymmetric_conditions.RData")

tr301 = read.table("data/301_training_R.txt")
names(tr301) = c(1,2)
#tr301 = as.matrix(tr301)

conds[["301"]] = list()
conds[["301"]]$train = list(words=tr301, objs=tr301)
conds[["301"]]$Condition = "2x2 5x 24AFC"

# track down data from these conds:
for(cond in names(conds)) {
  if(length(conds[[cond]]$HumanItemAcc)==0) {
    cdat = subset(tdat, condnum==cond)
    conds[[cond]]$Nsubj = length(unique(cdat$Subject))
    acc = cdat %>% mutate(Word = as.numeric(Word)) %>%
      group_by(Word) %>% 
      summarise(Correct=mean(Correct))
    conds[[cond]]$HumanItemAcc = acc$Correct
  }
}
# 211 - 214, 217, 218, 220

# 301 - "2x2 24w 5x"

#211, 1x3.  Words appear 6 times. objects 18.  4afc.
#212, 2x3.  Words appear 6 times. objects 9.  4afc.
#213, 2x4.  Words appear 6 times. objects 12.  4afc.
#214, 3x4.  Words appear 6 times. objects 8.  4afc.

#217, 2x4.  Words appear 6 times. objects 12.  18afc.
#218, 3x4.  Words appear 6 times. objects 8.  18afc.
#219, 2x3.  Words appear 6 times. objects 9.  18afc
#220, 3x4.  Words appear 9 times. objects 12.  18afc

conds[["211"]]$Condition = "1x3 6/18 4AFC"
conds[["212"]]$Condition = "2x3 6/9 4AFC"
conds[["213"]]$Condition = "2x4 6/12 4AFC"
conds[["214"]]$Condition = "3x4 6/8 4AFC"

conds[["217"]]$Condition = "2x4 6/12"
conds[["218"]]$Condition = "3x4 6/8"
conds[["219"]]$Condition = "2x3 6/9"
conds[["220"]]$Condition = "3x4 9/12"

save(conds, file="data/asymmetric_conditions.RData")