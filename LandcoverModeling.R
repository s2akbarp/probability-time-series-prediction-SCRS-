#packages
library(rgdal)
library(caret)
library(raster)
library(mapview)
library(sf)
library(e1071)







#import Dataset


depVariables=list.files("C:/Onedrive/OneDrive - University of Waterloo/My Data/phd/data/paper1/landcover_modeling/NewBoundry/landcover/change",all.files = FALSE,pattern = "*.tif")
indepVariables=list.files("C:/Onedrive/OneDrive - University of Waterloo/My Data/phd/data/paper1/landcover_modeling/NewBoundry/data/Independent_var",all.files = FALSE,pattern = ".tif")




#data preperation


#2000 independent varisbles
Land2000=brick("Landsat2000.tif" )
cost2000=raster("cost2000.tif")
EU2000=raster("EU2000.tif")
#stack different extent
extent(ratio2000)=extent(Land2000)
cost2000=resample(cost2000,Land2000)
EU2000=resample(EU2000,Land2000)
ratio2000=resample(ratio2000,Land2000)

Indep2000=stack(Land2000,cost2000,EU2000,ratio2000)

#2010 independent varisbles
Land2010=brick("Landsat2010.tif" )
cost2010=raster("cost2010.tif")
EU2010=raster("EU2010.tif")
ratio2010=raster("ratio2010.tif")
#stack different extent
extent(ratio2010)=extent(Land2010)
cost2010=resample(cost2010,Land2010)
EU2010=resample(EU2010,Land2010)
ratio2010=resample(ratio2010,Land2010)
Indep2010=stack(Land2010,cost2010,EU2010,ratio2010)


plot(Overlay)


#dependent
land2000to2010=raster("C2000to2010.tif")
land2000to2019=raster("c2000to2019.tif")
land2010to2019=raster("c2010to2019.tif")

#final data by creating a stack of rastyer layer overlayed on each other
Variables2000to2010 <- stack(land2000to2010, Indep2000)
Variables2010to2019 <- stack(land2010to2019, Indep2010)
Variables2000to2019 <- stack(land2000to2019, Indep2000)


#partitioning the dataset
#2000to2010
Datapartion2000to2010=createDataPartition(Variables2000to2010$C2000to2010,list = FALSE,p=0.3)
trained=Overlay[Datapartion,]
test=Overlay[-Datapartion,]

trained=na.omit(trained)

# variables
DependVar="Class"
IndependVar=c("blue",  "green", "red",  "nir",  "swir", "NDVI",  "NDWI","SAVI","LST")







plot(stack1970_2000_4_bf)
#dataframe
dataframe1970_2000_4_pp=as.data.frame(stack1970_2000_4_pp, row.names=NULL, optional=FALSE, xy=TRUE, na.rm=FALSE, long=FALSE)
dataframe1970_2000_4_pB=as.data.frame(stack1970_2000_4_pB, row.names=NULL, optional=FALSE, xy=TRUE, na.rm=FALSE, long=FALSE)
dataframe1970_2000_4_pF=as.data.frame(stack1970_2000_4_pF, row.names=NULL, optional=FALSE, xy=TRUE, na.rm=FALSE, long=FALSE)
dataframe1970_2000_4_bf=as.data.frame(stack1970_2000_4_bf, row.names=NULL, optional=FALSE, xy=TRUE, na.rm=FALSE, long=FALSE)

#dataframe prediction
#1970
dataframe2000_4_pp=as.data.frame(stack2000_4_pp, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
dataframe2000_4_pB=as.data.frame(stack2000_4_pB, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
dataframe2000_4_pF=as.data.frame(stack2000_4_pF, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
dataframe2000_4_bf=as.data.frame(stack2000_4_bf, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)

dataframe1970_4_pp=as.data.frame(stack1970_4_pp, row.names=NULL, optional=FALSE, xy=TRUE, na.rm=FALSE, long=FALSE)
dataframe1970_4_pB=as.data.frame(stack1970_4_pB, row.names=NULL, optional=FALSE, xy=TRUE, na.rm=FALSE, long=FALSE)
dataframe1970_4_pF=as.data.frame(stack1970_4_pF, row.names=NULL, optional=FALSE, xy=TRUE, na.rm=FALSE, long=FALSE)
dataframe1970_4_bf=as.data.frame(stack1970_4_bf, row.names=NULL, optional=FALSE, xy=TRUE, na.rm=FALSE, long=FALSE)
#code

dataframe1970_2000_4_pp$X2=as.factor(dataframe1970_2000_4_pp$X2)
dataframe1970_2000_4_pB$X3=as.factor(dataframe1970_2000_4_pB$X3)
dataframe1970_2000_4_pF$X5=as.factor(dataframe1970_2000_4_pF$X5)
dataframe1970_2000_4_bf$X4=as.factor(dataframe1970_2000_4_bf$X4)

default_idx_pp = createDataPartition(dataframe1970_2000_4_pp$X2, p = 0.75, list = FALSE)
default_idx_pb = createDataPartition(dataframe1970_2000_4_pB$X3, p = 0.75, list = FALSE)
default_idx_bf = createDataPartition(dataframe1970_2000_4_bf$X4, p = 0.75, list = FALSE)
default_idx_pf = createDataPartition(dataframe1970_2000_4_pF$X5, p = 0.75, list = FALSE)


default_trn_pp = dataframe1970_2000_4_pp[default_idx_pp, ]
default_trn_pb = dataframe1970_2000_4_pB[default_idx_pb, ]
default_trn_bf = dataframe1970_2000_4_bf[default_idx_bf, ]
default_trn_pf = dataframe1970_2000_4_pF[default_idx_pf, ]

default_tst_pp = dataframe1970_2000_4_pp[-default_idx_pp, ]
default_tst_pb = dataframe1970_2000_4_pB[-default_idx_pb, ]
default_tst_bf = dataframe1970_2000_4_bf[-default_idx_bf, ]
default_tst_pf = dataframe1970_2000_4_pF[-default_idx_pf, ]


response_PP="X2"
response_PB="X3"
response_BF="X4"
response_PF="X5"

predictors_pp=c("Var_dis_1970",  "Var_NDVI_1970", "Var_lst_1970",  "Var_NDWI_1970")
predictors_pB=c("Var_disB_1970",  "Var_NDVI_1970", "Var_lst_1970",  "Var_NDWI_1970")
predictors_pF=c("Var_disF_1970",  "Var_NDVI_1970", "Var_lst_1970",  "Var_NDWI_1970")
predictors_bf=c("Var_cost_1970",  "Var_NDVIb_1970", "Var_lstb_1970",  "Var_NDWIb_1970")
names(predictors_pF)=c("Var_disF_1970","Var_lst_1970", "Var_NDWI_1970",   "Var_NDVI_1970")
names(predictors_pB)=c("Var_disB_1970","Var_lst_1970", "Var_NDWI_1970",   "Var_NDVI_1970")
names(predictors_pp)=c("Var_dis_1970","Var_lst_1970", "Var_NDWI_1970",   "Var_NDVI_1970")
names(predictors_bf)=c("Var_cost_1970","Var_lst_1970", "Var_NDWIb_1970",   "Var_NDVIb_1970")

fitControl <- trainControl(method = "repeatedcv", number = 10,repeats = 5,classProbs = TRUE,summaryFunction = twoClassSummary)

default_glm_PP =caret::train(default_trn_pp[,predictors_pp],default_trn_pp[,response_PP],method = "glm",family = "binomial",trControl =fitControl,na.action=na.omit,metric = "ROC")
default_glm_PB =caret::train(default_trn_pb[,predictors_pB],default_trn_pb[,response_PB],method = "glm",family = "binomial",trControl = trainControl(method = "repeatedcv",number = 10, repeats = 10),na.action=na.omit)
default_glm_BF =caret::train(default_trn_bf[,predictors_bf],default_trn_bf[,response_BF],method = "glm",family = "binomial",trControl = trainControl(method = "repeatedcv",number = 10, repeats = 10),na.action=na.omit)
default_glm_PF =caret::train(default_trn_pf[,predictors_pF],default_trn_pf[,response_PF],method = "glm",family = "binomial",trControl = trainControl(method = "repeatedcv",number = 10, repeats = 10),na.action=na.omit)
#rf
default_glm_PP_rf =caret::train(default_trn_pp[,predictors_pp],default_trn_pp[,response_PP],method = "rf",trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10),na.action=na.omit, )
default_glm_BF_rf =caret::train(default_trn_bf[,predictors_bf],default_trn_bf[,response_BF],method = "rf",trControl = trainControl(method = "repeatedcv",number = 10, repeats = 10),na.action=na.omit)
default_glm_PB_rf =caret::train(default_trn_pb[,predictors_pB],default_trn_pb[,response_PB],method = "rf",trControl = trainControl(method = "repeatedcv",number = 10, repeats = 10),na.action=na.omit)
default_glm_PF_rf =caret::train(default_trn_pf[,predictors_pF],default_trn_pf[,response_PF],method = "rf",trControl = trainControl(method = "repeatedcv",number = 10, repeats = 10),na.action=na.omit)

default_trn_pf=na.omit(default_trn_pf)





#IMPO PLOT
p1_g=ggplot(varImp(default_glm_PB))+ggtitle("P->B")
p2_g=ggplot(varImp(default_glm_BF))+ggtitle("B->F")
p3_g=ggplot(varImp(default_glm_PF))+ggtitle("P->F")
p4_g=ggplot(varImp(default_glm_PP))+ggtitle("P->P")
p1=ggplot(varImp(default_glm_PB_rf))+ggtitle("P->B")
p2=ggplot(varImp(default_glm_BF_rf))+ggtitle("B->F")
p3=ggplot(varImp(default_glm_PF_rf))+ggtitle("P->F")
p4=ggplot(varImp(default_glm_PP_rf))+ggtitle("P->P")
grid.arrange(p1, p2,p3,p4,p1_g, p2_g,p3_g,p4_g, ncol=4)

#compoare models 
results <- resamples(list(GLM=default_glm_PP, RF=default_glm_PP_rf))
p1=bwplot(results$values$`GLM~Accuracy`,,xlab = "GLM_Accuracy")
p2=bwplot(results$values$`RF~Accuracy`,xlab = "RF_Accuracy")
grid.arrange(p1, p2, ncol=2)

#prediction
names(stack2000_4_pF)=c("Var_disF_1970","Var_lst_1970", "Var_NDVI_1970",   "Var_NDWI_1970")
names(stack2000_4_pB)=c("Var_disB_1970","Var_lst_1970", "Var_NDVI_1970",   "Var_NDWI_1970")
names(stack2000_4_pp)=c("Var_dis_1970","Var_lst_1970", "Var_NDVI_1970",   "Var_NDWI_1970")
names(stack2000_4_bf)=c("Var_cost_1970","Var_lstb_1970", "Var_NDVIb_1970",   "Var_NDWIb_1970")


predictedProbs_Pf <-raster:: predict(stack1970_4_pF, default_glm_PF,type = "prob")
predictedProbs_Pb <-raster:: predict(stack1970_4_pB, default_glm_PB,type = "prob")
predictedProbs_PP <-raster:: predict(stack1970_4_pp, default_glm_PP,type = "prob")
predictedProbs_bf <-raster:: predict(stack1970_4_bf, default_glm_BF,type = "prob")

ssss=stack(predictedProbs_bf,predictedProbs_bf_rf,tr_30$X4,Var_cost_1970)


predictedProbs_Pf_rf <-raster:: predict(stack1970_4_pF, default_glm_PF_rf,type = "prob")
predictedProbs_Pb_rf <-raster:: predict(stack1970_4_pB, default_glm_PB_rf,type = "prob")
predictedProbs_PP_rf <-raster:: predict(stack1970_4_pp, default_glm_PP_rf,type = "prob")
predictedProbs_bf_rf <-raster:: predict(stack1970_4_bf, default_glm_BF_rf,type = "prob")


plot(1-ssss)
plot()

qqqq2000_4_caret_rf= stack(predictedProbs_Pf_rf,predictedProbs_Pb_rf,predictedProbs_PP_rf,predictedProbs_bf)


werree=stack(qqqq2000_4_caret_rf,qqqq2000_4_caret)

qqqq2000_4_caret= stack(predictedProbs_Pf,predictedProbs_Pb,predictedProbs_PP,predictedProbs_bf)
names(qqqq2000_4_caret_rf)=c('PF(1970-2000)','PB(1970-2000)','PP(1970-2000)','BF(1970-2000)')
rasterVis::levelplot (1-predictedProbs_bf_rf,col.regions=pal, margin = TRUE, colorkey = list(space = "bottom"), scales=list(draw=FALSE ),layout=c(1,1))


spplot(qqqq2000_4_caret)

