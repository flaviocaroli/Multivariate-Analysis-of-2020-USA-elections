#Libraries used

library("caret")
library("car")
library("caTools")
library("crayon")
library("dplyr")
library("effects")
library("forcats")
library("ggplot2")
library("glmnet")
library("graphics")
library("grDevices")
library("grid")
library("gridExtra")
library("lattice")
library("latticeExtra")
library("methods")
library("Matrix")
library("MASSA")
library("miscTools")
library("nortest")
library("olsrr")
library("purrr")
library("rcompanion")
library("readxl")
library("Rcmdr")
library("RcmdrMisc")
library("sandwich")
library("stats")
library("stringr")
library("splines")
library("tidyr")
library("tidyverse")
library("tibble")
library("utils")


data <- read_excel(file.choose())
View(data)
attach(data)

#MAP OF US WITH PERCENTAGE OF VOTES FOR DEMCRATIC PARTY
dataframe1<-data.frame(state,demp)
view(dataframe1)
plot_usmap(data = dataframe1, values = "demp", color = "white") + 
  labs(title = "US votes for Democratic party", color = "#4e952d") + 
  theme(panel.background = element_rect(color = "white", fill = "white"))
scale_fill_continuous(low ="#4e952d", high = "#whi", name = "Votes", label = scales::comma) + 
  theme(legend.position = "right")

#1st multilinear regression
model_1 <- lm(demp ~ boh + wg + unr + fv +rmhi +pos + ma + fp +cr + cli + pbp +vet , data = data)
summary(model_1)

#Step-up/step-down method
sel_1<-step(lm(demp~1, data),
     scope =~ boh +wg + unr + fv +rmhi +pos + 
              ma + fp +cr + cli + pbp +vet,
     direction = "forward")
sel_2<-step(lm(demp~ boh +wg + unr + fv +rmhi +pos + 
          ma + fp +cr + cli + pbp +vet, data),
     direction = "backward")
summary(sel_2)
summary(sel_1)
 
#2nd multilinear regression        
model_2 <-lm(formula = demp ~ fv + wg + boh + unr, data = data)
summary(model_2)

#Prediction with model chose by step up method
pred_model_2<-predict(model_2)
summary(pred_model_2)
summary(demp)

ggplot(data=data, mapping = aes(x = pred_model_2, y = demp))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F, color= "#4e952d")+
  labs(title= " Model_2's prediction vs Democratic votes",
       x = "Prediction",
       y= "Democratic votes")

res=residuals(model_2)

summary(lm(demp~pred_model_2, data=data))


#Multicolinearity
vif(model_2) 

vif_values <-  vif(model_2)       
#create vector of VIF values
par(mfrow =c(1,2))
barplot(vif_values,
        main = "VIF Values",
        horiz = TRUE, col = "#4e952d")
#create horizontal bar chart to display each VIF value
var <- cor(data.frame(unr,fv,boh,wg))                                        
# independent variables correlation matrix 
var_inv <- ginv(var)
# independent variables inverse correlation matrix 
colnames(var_inv) <- colnames(data.frame(unr,fv,boh,wg))                     
rownames(var_inv) <- colnames(data.frame(unr,fv,boh,wg))

corrplot(var_inv,method='number',
         col = COL1('YlOrBr', 200),
         is.corr = F) 

#Analysis of Residuals

par(mfrow=c(2,2))
plot(model_2)

res=residuals(model_2)
m<-mean(res)
std<-sqrt(var(res))

# QQ plot against normal distribution
qqnorm(res,ylab="Residuals",xlab="Normal Scores") 
qqline(res, col="#4e952d")

#check normality tests
ols_test_normality(model_2)

#Histogram with normal distribution curve
hist(res, 
     include.lowest = TRUE, right = TRUE, fuzz = 1e-7,
     density = 20, angle = 45, col = "lightgray", border = NULL,
     main = "Histogram of residuals",
     xlab = "Residuals", ylab= "Density",
)
curve(dnorm(x, mean=m, sd=std), 
      col="#4e952d", lwd=2, add=TRUE, yaxt="n")



#Cross Validation 

ctrl<-trainControl(method = "cv", number = 10)
#knn
fit.cv <- train(demp ~ fv + wg + boh + unr, data = data,
                method = "knn", 
                trControl = ctrl,
                preProcess = c("center", "scale"),
                tuneGrid = data.frame(k=seq(5,50,by = 5)))
#cross-val
ctrl_2<-trainControl(method = "cv", number = 15)
model_3 <- train(demp ~ fv + wg + boh + unr, data = data,
                method = "lm", 
                trControl = ctrl_2,
                preProcess = c("center", "scale"))

print(fit.cv)
plot(fit.cv, col ="#4e952d")
print(model_3)
summary(model_3)



#Appendix plots

p1<-ggplot(data=data, mapping = aes(x = boh, y = demp, colour = dem_cat ))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F)+
  labs(title= " Bachelor vs Democratic votes",
       x = "People with a Bachelor",
       y= "Democratic votes",
       colour= "Votes for Democratic Party")
p2<-ggplot(data=data, mapping = aes(x = wg, y = demp, colour = dem_cat ))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F)+
  labs(title= " Wage gap vs Democratic votes",
       x = "Wage gap",
       y= "Democratic votes",
       colour= "Votes for Democratic Party")
p3<-ggplot(data=data, mapping = aes(x = unr, y = demp, colour = dem_cat ))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F)+
  labs(title= " Unemployment vs Democratic votes",
       x = "Unemployment rate",
       y= "Democratic votes",
       colour= "Votes for Democratic Party")

p4<-ggplot(data=data, mapping = aes(x = rmhi, y = demp, colour = dem_cat ))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F)+
  labs(title= " Houshold income vs Democratic votes",
       x = "Real median household income",
       y= "Democratic votes",
       colour= "Votes for Democratic Party")

p5<-ggplot(data=data, mapping = aes(x = ma, y = demp, colour = dem_cat ))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F)+
  labs(title= " Age vs Democratic votes",
       x = "Median age",
       y= "Democratic votes",
       colour= "Votes for Democratic Party") 
p6<-ggplot(data=data, mapping = aes(x = vet, y = demp, colour = dem_cat ))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F)+
  labs(title= " Veterans vs Democratic votes",
       x = "Veterans percentage",
       y= "Democratic votes",
       colour= "Votes for Democratic Party")

p7<-ggplot(data=data, mapping = aes(x = pos, y = demp, colour = dem_cat ))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F)+
  labs(title= " Positives vs Democratic votes",
       x = "Positives tot",
       y= "Democratic votes",
       colour= "Votes for Democratic Party")

p8<-ggplot(data=data, mapping = aes(x = fp, y = demp, colour = dem_cat ))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F)+
  labs(title= " Firearm vs Democratic votes",
       x = "Firearm per household",
       y= "Democratic votes",
       colour= "Votes for Democratic Party")

p9<-ggplot(data=data, mapping = aes(x = cr, y = demp, colour = dem_cat ))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F)+
  labs(title= " Crimes vs Democratic votes",
       x = "Crime rate",
       y= "Democratic votes",
       colour= "Votes for Democratic Party")

p10<-ggplot(data=data, mapping = aes(x = cli, y = demp, colour = dem_cat ))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F)+
  labs(title= " Cost of living vs Democratic votes",
       x = "Cost of living index",
       y= "Democratic votes",
       colour= "Votes for Democratic Party")

p11<-ggplot(data=data, mapping = aes(x = pbp, y = demp, colour = dem_cat ))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F)+
  labs(title= " Black people vs Democratic votes",
       x = "Percentage of black people",
       y= "Democratic votes",
       colour= "Votes for Democratic Party")

p12<-ggplot(data=data, mapping = aes(x = fv, y = demp, colour = dem_cat ))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F)+
  labs(title= " Fully vaccinated vs Democratic votes",
       x = "Fully vaccinated people",
       y= "Democratic votes",
       colour= "Votes for Democratic Party")



grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,nrow=4)

g1<-ggplot(data=data, mapping = aes(x = boh, y = demp))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F, color= "#4e952d")+
  labs(title= " Bachelor vs Democratic votes",
       x = "People with a Bachelor",
       y= "Democratic votes")


g2<-ggplot(data=data, mapping = aes(x = wg, y = demp))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F, color= "#4e952d")+
  labs(title= " Wage gap vs Democratic votes",
       x = "Wage gap",
       y= "Democratic votes")

g3<-ggplot(data=data, mapping = aes(x = unr, y = demp ))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F, color= "#4e952d")+
  labs(title= " Unemployment vs Democratic votes",
       x = "Unemployment rate",
       y= "Democratic votes")

g4<-ggplot(data=data, mapping = aes(x = rmhi, y = demp ))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F, color= "#4e952d")+
  labs(title= " Houshold income vs Democratic votes",
       x = "Real median household income",
       y= "Democratic votes")


g5<-ggplot(data=data, mapping = aes(x = ma, y = demp ))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F, color= "#4e952d")+
  labs(title= " Age vs Democratic votes",
       x = "Median age",
       y= "Democratic votes")


g6<-ggplot(data=data, mapping = aes(x = pos, y = demp ))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F, color= "#4e952d")+
  labs(title= " Positives vs Democratic votes",
       x = "Positives tot",
       y= "Democratic votes")


g7<-ggplot(data=data, mapping = aes(x = fp, y = demp ))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F, color= "#4e952d")+
  labs(title= " Firearm vs Democratic votes",
       x = "Firearm per household",
       y= "Democratic votes")


g8<-ggplot(data=data, mapping = aes(x = cr, y = demp ))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F, color= "#4e952d")+
  labs(title= " Crimes vs Democratic votes",
       x = "Crime rate",
       y= "Democratic votes")


g9<-ggplot(data=data, mapping = aes(x = cli, y = demp))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F, color= "#4e952d")+
  labs(title= " Cost of living vs Democratic votes",
       x = "Cost of living index",
       y= "Democratic votes")


g10<-ggplot(data=data, mapping = aes(x = pbp, y = demp))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F, color= "#4e952d")+
  labs(title= " Black people vs Democratic votes",
       x = "Percentage of black people",
       y= "Democratic votes")

g11<-ggplot(data=data, mapping = aes(x = vet, y = demp))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F, color= "#4e952d")+
  labs(title= " Veterans vs Democratic votes",
       x = "Veterans percentage",
       y= "Democratic votes")


g12<-ggplot(data=data, mapping = aes(x = fv, y = demp))+
  geom_point(size = 1.5)+
  geom_smooth(method = lm, se = F, color= "#4e952d")+
  labs(title= " Fully vaccinated vs Democratic votes",
       x = "Fully vaccinated people",
       y= "Democratic votes")

grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,nrow=4)

scatter3d(boh,demp, fv,
          axis.col= c("black", "black", "black"),
          surface.col="#4e952d", surface.alpha=0.5,
          point.col="black", text.col="darkgreen2",
          grid.col="gray",
          fogtype=c("exp2", "linear", "exp", "none"),
          residuals=(length(model_2) == 1),
          surface=TRUE, fill=TRUE,
          grid=TRUE, grid.lines=26,
          sphere.size=1, radius=1, threshold=0.01, speed=1, fov=60,
          fit="linear", level=0.5, ellipsoid.alpha=0.1, 
          reg.function.col=surface.col[length(surface.col)], 
          mouseMode=c(none="none", left="polar", right="zoom", middle="fov", 
                      wheel="pull"))


