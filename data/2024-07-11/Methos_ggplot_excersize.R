#### ggplot excersize

#### check you have these packages installed.
# if not, use the following command: install.packages("name.of.package")

library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(grid)
library(cowplot)
library(corrplot)
library(Hmisc)
library(ARTool)
library(EBImage)
install.packages("EBImage")

#### photosyrvey

setwd("C:/Users/lenovo/Desktop/Research Methods B/Files for the class-20240711") # set working directory

#load the metadata- NOTE: we work at the transect level.
photo_metadata=read.csv("Photosurvey_metadata.csv", header=TRUE, row.names=1, stringsAsFactors = TRUE)
str(photo_metadata)

# load the processed and filtered coverage data by taxonomic group
photosurvey_coverage=read.csv("Photosurvey_processed_data.csv", header=TRUE, row.names=1, stringsAsFactors = TRUE)
str(photosurvey_coverage)

#make sure the order of trnasect_IDs is the same in the data and the metadata
ord=match(row.names(photosurvey_coverage), row.names(photo_metadata))
photo_metadata=photo_metadata[ord,]

#merge metadata and coverage values

Photosurvey=cbind(photo_metadata, photosurvey_coverage)
str(Photosurvey)

# let's look at the total coverage in relation to site

library(ggplot)
ggplot(Photosurvey, aes(x=site, y=Chordata))+
  geom_point()+
  theme_light()

ggplot(Photosurvey, aes(x=site, y=Chordata))+
  geom_point(position=position_jitter(width=0.2))+
  theme_light()

ggplot(Photosurvey, aes(x=site, y=Chordata))+
  geom_point(aes(color=as.factor(depth)),position=position_jitter(width=0.2))+
  theme_light()

ggplot(Photosurvey, aes(x=site, y=Chordata))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(aes(color=as.factor(depth)),position=position_jitter(width=0.2))+
  theme_light()

# now test

#1. test homogeneity of variance

bartlett.test(Photosurvey$Chordata~Photosurvey$site)

#2) test difference in coverage levels

kruskal.test(Chordata~site, data=Photosurvey)

# now let's moove to season


ggplot(Photosurvey, aes(x=season, y=Chordata))+
  geom_point()+
  theme_light()

ggplot(Photosurvey, aes(x=season, y=Chordata))+
  geom_point(position=position_jitter(width=0.2))+
  theme_light()

ggplot(Photosurvey, aes(x=season, y=Chordata))+
  geom_point(aes(color=as.factor(depth)),position=position_jitter(width=0.2))+
  theme_light()

ggplot(Photosurvey, aes(x=season, y=Chordata, fill=site))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(position=position_jitterdodge(jitter.width = 0.2), size=1)+
  theme_light()+
  facet_grid(.~depth)

### now let's test for all factors together- non-parametric anova
library(ARTool)
m=art(Chordata~site+as.factor(depth)+season+site*as.factor(depth)*season, data=Photosurvey)
anova(m)


### let's examine the sampler....

ggplot(Photosurvey, aes(x=sampler, y=Chordata))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(aes(color=as.factor(depth)),position=position_jitter(width=0.2))+
  theme_light()


#1. test homogeneity of variance

bartlett.test(Photosurvey$Chordata~Photosurvey$sampler)

#2) test difference in coverage levels

kruskal.test(Chordata~sampler, data=Photosurvey)



#### what is the relatedness between coverage by different groups? we will do correlations

library(Hmisc)
str(Photosurvey)
# variables 10 to 22 have the measurements

corrs=rcorr(as.matrix(Photosurvey[,10:22]), type="spearman")

#we need a function that will take the square matrix and turn it to long format.

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

#now we apply this function to our data
corrs_flat=flattenCorrMatrix(corrs$r, corrs$P)

#add p value adjustment using the Benjamini-Hochber method (BH)
corrs_flat$p.adj=p.adjust(corrs_flat$p, "BH")

write.csv(corrs_flat, "Taxonomic_groups_spearman.csv")

# now plot your correlation of interest e.g., my taxon of interest and live coverage
str(Photosurvey)

ggplot(Photosurvey, aes(x=live_coverage, y=Chordata))+
  geom_point()+
  geom_smooth(method='lm')+
  theme_light()

