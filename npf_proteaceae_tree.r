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

#Make NPF proteaceae
tree <- read.tree("RAxML_bipartitions.npf_proteaceae_rn")
groupInfo <- split(tree$tip.label, gsub("[_-_].+|NPF.*", "", tree$tip.label))
names(groupInfo) <- c("Arabidopsis thaliana", "Amborella trichopoda", "Hakea prostrata", "Macadamia integrifolia", "Telopea speciosissima")
tree <- groupOTU(tree, groupInfo)

#Colour species
mycol <- kelly()[c(12,9,10,6,19)]
names(mycol) <- names(groupInfo)

#Draw NPF proteaceae tree
npf_proteaceae_tree <- ggtree(tree, layout='circular', size=0.5, branch.length = "none") + 
  xlim(-15, NA) +
  geom_cladelabel(node=333, label="NPF6", align=TRUE, offset=10, offset.text=1, fontsize=3, geom='label', fill='grey90') +
  geom_hilight(node=333, fill="grey80") +
  geom_cladelabel(node=337, label="NPF4", align=TRUE, offset=10, offset.text=1.5, fontsize=3, geom='label', fill='grey90') +
  geom_hilight(node=337, fill="grey80") +
  geom_cladelabel(node=395, label="NPF6", align=TRUE, offset=10, offset.text=2.5, fontsize=3, geom='label', fill='grey90') +
  geom_hilight(node=395, fill="grey80") +
  geom_cladelabel(node=442, label="NPF3", align=TRUE, offset=10, offset.text=2.5, hjust=0.8, fontsize=3, geom='label', fill='grey90') +
  geom_hilight(node=442, fill="grey80") +
  geom_cladelabel(node=450, label="NPF2", align=TRUE, offset=10, offset.text=6, hjust=0.3, fontsize=3, geom='label', fill='grey90') +
  geom_hilight(node=450, fill="grey80") +
  geom_cladelabel(node=470, label="NPF1", align=TRUE, offset=10, offset.text=7.5, fontsize=3, geom='label', fill='grey90') +
  geom_hilight(node=470, fill="grey80") +
  geom_cladelabel(node=481, label="NPF2", align=TRUE, offset=10, offset.text=7.5, fontsize=3, geom='label', fill='grey90') +
  geom_hilight(node=481, fill="grey80") +
  geom_cladelabel(node=503, label="NPF7", align=TRUE, offset=10, offset.text=7.5, fontsize=3, geom='label', fill='grey90') +
  geom_hilight(node=503, fill="grey80") +
  geom_cladelabel(node=515, label="NPF8", align=TRUE, offset=10, offset.text=6.5, fontsize=3, geom='label', fill='grey90') +
  geom_hilight(node=515, fill="grey80") +
  geom_cladelabel(node=555, label="NPF5", align=TRUE, offset=10, offset.text=2, fontsize=3, geom='label', fill='grey90') +
  geom_hilight(node=555, fill="grey80") +
  geom_tiplab(aes(subset=(grepl('HpNPF|AtNPF|AtrNPF', label))==TRUE,color=group), size=1.5, align=T, show.legend=FALSE, offset = 4.5) +
  geom_tippoint(aes(color=group,), size=1.3, alpha=0.8) +
  #geom_text(aes(label=node), color="red", hjust=-.3, size=1) + #for node numbers
  scale_color_manual(values=mycol, name="") +
  theme_tree(legend.position = c(0.81,0.13), legend.background = element_rect(fill='transparent'), legend.text = element_text(face = "italic", size=8), legend.key.size=unit(0.4, 'cm')) +
  guides(color=guide_legend(ncol=1, bycol=T))
npf_proteaceae_tree %>% flip(470, 481)
