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


plot(Land2019)
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
land2000to2010=raster("c2000to2010.tif")
land2000to2010=layerize(land2000to2010)
land2000to2019=raster("c2000to2019.tif")
land2000to2019=layerize(land2000to2019)
land2010to2019=raster("c2010to2019.tif")
land2010to2019=layerize(land2010to2019)

#final data by creating a stack of rastyer layer overlayed on each other
Variables2000to2010 <- stack(land2000to2010, Indep2000)
Variables2010to2019 <- stack(land2010to2019, Indep2010)
Variables2000to2019 <- stack(land2000to2019, Indep2000)



      #dataframe
dataframe2000to2010=as.data.frame(Variables2000to2010, row.names=NULL, optional=FALSE,  na.rm=TRUE, long=FALSE)
dataframe2000to2019=as.data.frame(Variables2000to2019, row.names=NULL, optional=FALSE,  na.rm=TRUE, long=FALSE)
dataframe2010to2019=as.data.frame(Variables2010to2019, row.names=NULL, optional=FALSE,  na.rm=TRUE, long=FALSE)

dataframe2000to2010

#partitioning the dataset
#2000to2010
Datapartion2000to2010_pp=createDataPartition(dataframe2000to2010$X1,list = FALSE,p=0.3)
Datapartion2000to2010_pf=createDataPartition(dataframe2000to2010$X12,list = FALSE,p=0.3)
Datapartion2000to2010_pb=createDataPartition(dataframe2000to2010$X13,list = FALSE,p=0.3)
Datapartion2000to2010_bf=createDataPartition(dataframe2000to2010$X32,list = FALSE,p=0.3)
trained2000to2010_pp=dataframe2000to2010[Datapartion2000to2010_pp,]
trained2000to2010_pf=dataframe2000to2010[Datapartion2000to2010_pf,]
trained2000to2010_pb=dataframe2000to2010[Datapartion2000to2010_pb,]
trained2000to2010_bf=dataframe2000to2010[Datapartion2000to2010_bf,]
test2000to2010_pp=dataframe2000to2010[-Datapartion2000to2010_pp,]
test2000to2010_pf=dataframe2000to2010[-Datapartion2000to2010_pf,]
test2000to2010_pb=dataframe2000to2010[-Datapartion2000to2010_pb,]
test2000to2010_bf=dataframe2000to2010[-Datapartion2000to2010_bf,]

#2010to2019
Datapartion2010to2019_pp=createDataPartition(dataframe2010to2019$X1,list = FALSE,p=0.3)
Datapartion2010to2019_pf=createDataPartition(dataframe2010to2019$X12,list = FALSE,p=0.3)
Datapartion2010to2019_pb=createDataPartition(dataframe2010to2019$X13,list = FALSE,p=0.3)
Datapartion2010to2019_bf=createDataPartition(dataframe2010to2019$X32,list = FALSE,p=0.3)
trained2010to2019_pp=dataframe2010to2019[Datapartion2010to2019_pp,]
trained2010to2019_pf=dataframe2010to2019[Datapartion2010to2019_pf,]
trained2010to2019_pb=dataframe2010to2019[Datapartion2010to2019_pb,]
trained2010to2019_bf=dataframe2010to2019[Datapartion2010to2019_bf,]
test2010to2019_pp=dataframe2010to2019[-Datapartion2010to2019_pp,]
test2010to2019_pf=dataframe2010to2019[-Datapartion2010to2019_pf,]
test2010to2019_pb=dataframe2010to2019[-Datapartion2010to2019_pb,]
test2010to2019_bf=dataframe2010to2019[-Datapartion2010to2019_bf,]



#2000to2019
Datapartion2000to2019_pp=createDataPartition(dataframe2000to2019$X1,list = FALSE,p=0.3)
Datapartion2000to2019_pf=createDataPartition(dataframe2000to2019$X12,list = FALSE,p=0.3)
Datapartion2000to2019_pb=createDataPartition(dataframe2000to2019$X13,list = FALSE,p=0.3)
Datapartion2000to2019_bf=createDataPartition(dataframe2000to2019$X32,list = FALSE,p=0.3)
trained2000to2019_pp=dataframe2000to2019[Datapartion2000to2019_pp,]
trained2000to2019_pf=dataframe2000to2019[Datapartion2000to2019_pf,]
trained2000to2019_pb=dataframe2000to2019[Datapartion2000to2019_pb,]
trained2000to2019_bf=dataframe2000to2019[Datapartion2000to2019_bf,]
test2000to2019_pp=dataframe2000to2019[-Datapartion2000to2019_pp,]
test2000to2019_pf=dataframe2000to2019[-Datapartion2000to2019_pf,]
test2000to2019_pb=dataframe2000to2019[-Datapartion2000to2019_pb,]
test2000to2019_bf=dataframe2000to2019[-Datapartion2000to2019_bf,]




# variables
IndependVar2000=c("blue",  "green", "red",  "nir",  "swir", "NDVI",  "NDWI","SAVI","LST","cost2000","EU2000")
IndependVar2000_bf= c("blue",  "green", "red",  "nir",  "swir", "NDVI",  "NDWI","SAVI","LST","cost2000","Ratio2000")
IndependVar2010=c("blue",  "green", "red",  "nir",  "swir", "NDVI",  "NDWI","SAVI","LST","cost2010","EU2010")
IndependVar2010_bf=c("blue",  "green", "red",  "nir",  "swir", "NDVI",  "NDWI","SAVI","LST","cost2010","Ratio2010")








#MODELS 2000 to 2010

GLM_Model_PP_2000to2010 =caret::train(trained2000to2010_pp[,IndependVar2000],trained2000to2010_pp$X1,method = "glm",family = "binomial",trControl =fitControl,na.action=na.omit)
GLM_Model_PB =caret::train(trained2000to2010_pb[,IndependVar2000],trained2000to2010_pb[,trained2000to2010_pb$X13],method = "glm",family = "binomial",trControl = trainControl(method = "repeatedcv",number = 10, repeats = 10),na.action=na.omit)
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

