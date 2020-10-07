order_dir = "orders/"
model_dir = "models/"
data_dir = "data/"

load("data/priming_all_trajectory.RData")
# all - trajectory data (4 blocks x 18 pairs) 11808 data points from 164 subjects
prdat <- all %>% group_by(Exp, Block, Subject) %>% summarise(Correct=mean(Correct)) %>%
  tidyboot::tidyboot_mean(Correct)
ggplot(prdat, aes(x=Block, y=mean, group=Exp, color=Exp)) + geom_point() + geom_line() + theme_bw()

prdat <- all %>% group_by(Exp, Block, TotalFreq, Subject) %>% summarise(Correct=mean(Correct)) %>%
  tidyboot::tidyboot_mean(Correct)
ggplot(prdat, aes(x=Block, y=mean, group=TotalFreq, color=TotalFreq)) + facet_wrap(. ~ Exp) +
  geom_point() + geom_line() + theme_bw()
# could combine this into below data:
# "4 Pairs/Trial 3,6,9x" = "3_x8_369_4x4" (and only 30 Ss here)
# "4 Pairs/Trial, 6x" = "orig_4x4"  (would add much to the 34 Ss there...)
# "3 Pairs/Trial 3,6,9x" = "freq369-3x3hiCD" (and only 26 Ss here..)
all$Item = rep(1:18, 4*164)

all$order = ifelse(all$Exp=="4 Pairs/Trial 3,6,9x", "3_x8_369_4x4", 
                   ifelse(all$Exp=="4 Pairs/Trial, 6x", "orig_4x4", "freq369-3x3loCD"))
# according to paper, Exp 2: 50 participants in 369 hiCD 4x4
# Exp 3: 70 participants in 369 3x3 loCD

prag <- all %>% filter(Block==1) %>%
  group_by(order, Item) %>% 
  summarise(HumanItemAcc=mean(Correct))

#cor(subset(prag, order=="orig_4x4")$HumanItemAcc, combined_data[["orig_4x4"]]$HumanItemAcc) # .42
#cor(subset(prag, order=="3_x8_369_4x4")$HumanItemAcc, combined_data[["3_x8_369_4x4"]]$HumanItemAcc) # -.21 !
#cor(subset(prag, order=="freq369-3x3loCD")$HumanItemAcc, combined_data[["freq369-3x3loCD"]]$HumanItemAcc) # -.28 !
# Block 1 has pretty low performance overall, and uncorrelated with item-level performance in the original Exp...

# load lists of many trial orderings (and some means of human performance):
load("data/master_orders.RData") # orders
load("data/asymmetric_conditions.RData") # conds
print(names(orders)) # e.g., orders[["filt0E_3L"]]
print(names(conds))

prn <- all %>% group_by(order, Subject) %>% summarise(n=n()) %>% 
  group_by(order) %>% summarise(n=n())

cc = c("orig_4x4", "3_x8_369_4x4", "freq369-3x3loCD")
for(c in cc) {
  denom = orders[[c]]$Nsubj + subset(prn, order==c)$n
  orders[[c]]$HumanItemAcc = (orders[[c]]$HumanItemAcc * orders[[c]]$Nsubj + 
                                       subset(prag, order==c)$HumanItemAcc * subset(prn, order==c)$n) / denom
  orders[[c]]$Nsubj = denom
}

load("data/filtering_item_acc.RData")
fd <- subset(acc_s, AtoA==T)
for(ord in unique(fd$order)) {
  css = subset(fd, order==ord)
  orders[[ord]]$Nsubj = css$n[1]
  orders[[ord]]$HumanItemAcc = css$Correct
}
# acc_s %>% group_by(AtoA) %>% summarise(acc=mean(Correct))

totSs = 0
for(ord in names(orders)) {
  totSs = totSs + orders[[ord]]$Nsubj
  orders[[ord]]$Condition = ord
}
# 946 subjects

#load(paste(data_dir,"asym_master.RData",sep='')) # raw


for(ord in names(conds)) {
  if(!is.na(conds[[ord]]$Nsubj)) {
    totSs = totSs + conds[[ord]]$Nsubj
  } else print(ord) 
} 
# 1696 subjects

combined_data = c(conds, orders)

save(combined_data, file="data/combined_data.RData")