#BiocManager::install("ggtree")
library("ggtree")
#install.packages("pals")
library("pals")
#install.packages("ggnetwork")
library("ggnetwork")
#install.packages("ggrepel")
library("ggrepel")
#install.packages("dplyr")
library(dplyr)

#Choose colours
#At 12, Atr 9, Hp 10, Mi 6, Ts 19, Nn 11, Mt 8, Eg 13, Vv 4, Zm 3, Os 5

#Make NRT2 all tree
tree <- read.tree("RAxML_bipartitions.nrt2_all_rn_acc_ids")
groupInfo <- split(tree$tip.label, gsub("[_-_].+|NRT2.*", "", tree$tip.label))
names(groupInfo) <- c("Arabidopsis thaliana", "Amborella trichopoda", "Eucalyptus grandis", "Hakea prostrata", "Macadamia integrifolia", "Medicago truncatula", "Nelumbo nucifera", "Oryza sativa", "Telopea speciosissima", "Vitis vinifera", "Zea mays")
tree <- groupOTU(tree, groupInfo)

#Colour species
mycol <- kelly()[c(12,9,13,10,6,8,11,5,19,4,3)]
names(mycol) <- names(groupInfo)

#Draw tree
nrt2_all_tree <- ggtree(tree, layout='rectangular', size=0.5) + 
  xlim(0, 3.8) +
  geom_tiplab(aes(subset=(grepl('NRT2', label))==TRUE,color=group), size=3.2, align=T, show.legend=FALSE, offset=0.3, geom="label", label.size=0, label.padding=unit(0.01, "lines")) +
  geom_tippoint(aes(color=group,), size=2) +
  geom_nodelabel(aes(label=as.numeric(label)), size=2.5, color = "black", alpha=0.7, nudge_x=-0.04, nudge_y=0.6, label.size=0.1, label.padding=unit(0.1,"lines")) +
  #geom_text(aes(label=node), hjust=-.3, size=3) + #for node numbers
  scale_color_manual(values=mycol, name="") +
  theme_tree2(legend.position = 'bottom', legend.background = element_rect(fill='transparent'), legend.text = element_text(face = "italic", size=9), legend.key.size=unit(0.45, 'cm')) +
  guides(color=guide_legend(ncol=2, bycol=T))
nrt2_all_tree %>% ggtree::rotate(68) %>% ggtree::rotate(72) %>% ggtree::rotate(66) %>% ggtree::rotate(59)
