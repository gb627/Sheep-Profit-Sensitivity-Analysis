# Assumptions

library("randomForest")
library(varImp)
library(Hmisc)
library(clhs)
library("corrplot")
#########################################################################################################
# Models

Pricing<-read.csv("Default Dataset.csv",header = FALSE) # Live weight Sheep Prices 2021
model <- lm(V2 ~ poly(V1,20),data = Pricing)

area<-c(120,160,250,350) # Size of Farm
labour_cost<-c(285,275,200,145) # Labour costs from Farm management costing book
power<-c(175,170,165,150)# Average Power costs from Farm management costing book 
buildings<-c(20,15,15,10)# Average building maintaince costs from Farm management costing book
Genal_costs1<-c(80,80,60,45)# General costs from Farm management costing book


# Relationships between farm size and Fixed costs
Genal_costs <- lm(Genal_costs1 ~ poly(area,2)) 
power_costs <- lm(power ~ poly(area,2))
buildings_costs <- lm(buildings ~ poly(area,2))
labour_costs <- lm(labour_cost ~ poly(area,2))


########################################################################################################
startdate <- as.Date("01/01/2023","%d/%m/%Y")
ram_date<- c("2022-10-15","2022-11-15")   # date ram to ews

# Stocking Density
Ews_Per_hectare<-seq(8,12,by=2)
acres<-(seq(100,200,by=50)) # actually in hectares

# Lamb assumptions
Lambing<-seq(1.10,1.75,by=0.25) # lambing percentage
Lam_Birth_weight<-seq(3.1,3.9,by=0.4) # kg
lamb_sale_Weight<-seq(25,35,by=5) # lamb live weight (kg)


# Sales
Lamb_Loss<-seq(5,20,by=5) # as % (15% is average in Wales)

# Stock replacement
Replacement<-seq(0.18,0.25,by=0.02) # % of stock change
Barrenper<-seq(0.10,0.20,by=0.05) # percentage of barren ewes

## Daily Average Gain
weeks8<-seq(0.18,0.28,by=0.02) # 0 - 8 weeks
week8towean<-seq(0.14,0.2,by=0.02) # 8 weeks to wean
wean_onwards<-seq(0.12,0.18,by=0.04) # wean onwards


## Fixed Costs
wage<-15 # waage of worker/farmer
ew_cost<-10 # variable sundries + concentrates
ew_labour<-2*wage # Wage per ewe per year
labour_Daily<-seq(0.5,1.2,by=0.4) # For 100 lambs per day from costings book
labour_fortnightly<-seq(1,5,by=1.5) # For 100 lambs per fortnight from costings book
Vet<-seq(3,5,by=2) # vet bills
land_rent<-(seq(120,175,by=25)) # Land rent per hectare

#########################################################################################################


df<-expand.grid("Ews_Per_acre"=Ews_Per_hectare,"acres"=acres,"Lambing"=Lambing,
            "Lam_Birth_weight"=Lam_Birth_weight,
            "lamb_sale_Weight"=lamb_sale_Weight,"Lamb_Loss"=Lamb_Loss,
            "weeks8"=weeks8,"week8towean"=week8towean,"Barrenper"=Barrenper,
            "Replacement"=Replacement,"land_rent"=land_rent,"Vet"=Vet,
            "labour_fortnightly"=labour_fortnightly,"labour_Daily"=labour_Daily,
            "wean_onwards"=wean_onwards,"ram_date"=ram_date)
            

df<-df[sample(nrow(df), 3000), ]


df$acres<-abs(jitter(df$acres,amount = 30))
df$Lambing<-jitter(df$Lambing,amount = 0.25)
#df$lamb_sale_Weight<-jitter(df$lamb_sale_Weight,amount = 5)
df$land_rent<-jitter(df$land_rent,amount = 20)
df$Lamb_Loss<-jitter(df$Lamb_Loss,amount = 4)
df$ram_date<-lubridate::ymd(df$ram_date) + lubridate::days(round((jitter(rep(0,NROW(df)),amount = 15)),0))

result<-data.frame()

for(i in 1:NROW(df)){
  
  ##############################################################################
  # How many Ewes
  total_Ewsv1<-df$Ews_Per_acre[i]*df$acres[i]
  ##############################################################################
  
  Barren_Ewes<-total_Ewsv1*df$Barrenper[i]
  total_Ews<-round(total_Ewsv1-Barren_Ewes,0)
  
  # How many lambs
  Total_lambs<-round((total_Ews*df$Lambing[i]),0)
  Cost_Lambs<-Total_lambs # this is the cost element
  
  # Losses
  Total_lambs<-round(Total_lambs-(Total_lambs*(df$Lamb_Loss[i]/100)),0)
  
  Rasied_lambs<-Total_lambs
  
  Total_lambs<-round(Total_lambs-(total_Ews*df$Replacement[i]),0)
  

  
  # Weight and growth
  Lam_Birth_weight1<-jitter(rep(df$Lam_Birth_weight[i],Total_lambs),amount  = 0.3)
  weeks81<-jitter(rep(df$weeks8[i],Total_lambs),amount  = 0.015)
  week8towean1<-jitter(rep(df$week8towean[i],Total_lambs),amount  = 0.015)
  wean_onwards1<-jitter(rep(df$wean_onwards[i],Total_lambs),amount  = 0.015)
  

  #######################################################################################################################################
  
  end_lambs <- data.frame(matrix(ncol = Total_lambs, nrow = 281))
  end_lambs_Target <- data.frame()
  
  
  Lambing_Time<-as.Date(df$ram_date[i]) + 147 
  Max<-as.numeric(lubridate::ymd(df$ram_date[i])+365-Lambing_Time)
  

  for(q in 1:length(Lam_Birth_weight1)){
    
    To_8weeks<-(seq(1:53)*weeks81[q])+Lam_Birth_weight1[q]

    W8weeks_wean<-(seq(1:28)*week8towean1[q])+max(To_8weeks)
    
    wean_onwards_end<-(seq(1:200)*wean_onwards1[q])+max(W8weeks_wean)
    
    weightgain<-c(To_8weeks,W8weeks_wean,wean_onwards_end)
    
    #weightgain<-suppressWarnings(c((seq(1:53)*weeks81[q])+Lam_Birth_weight1[q],(seq(1:200)*week8towean1[q])+max((seq(1:53)*weeks81[q])+Lam_Birth_weight1[q])))
    
    
    end_lambs[q]<-c(weightgain)
    
    
    Target_Day<-which.min(abs(weightgain - df$lamb_sale_Weight[i]))
    
    Target_Day[Target_Day > Max] <- Max
    
    End_Weight<-weightgain[Target_Day]
    end_lambs_Target<-rbind(cbind(Target_Day,End_Weight),end_lambs_Target)
    
  }


 # for(l in 1:length(target)){
    
 #   if(target[l]>365){
      
 #     target[l]<-target[l]-360
      
 #   }
    

  Pricingv2<-data.frame("V1"=end_lambs_Target$Target_Day)
  Gross_Income_ewe<-(total_Ews*df$Replacement[i])*65
  
  Income_lamb<-(sum(predict(object = model,newdata =Pricingv2)*end_lambs_Target$End_Weight)/100)
  Gross_Income_Perlamb<-Income_lamb/NROW(end_lambs_Target)

  # BPS Payments
  
  if(df$acres[i]<=50){
    
    
    BPS<-(df$acres[i]*114)+df$acres[i]*120.2
    
    
    
  }
  
  if(df$acres[i]>=50.1){
    
    
    BPS<-(50*114)+df$acres[i]*120.2
    
    
    
  }
  
  
  
  Gross_Income<-Income_lamb+Gross_Income_ewe+BPS
  
  ################################################################### Costs ###################################################################
  
  # This is cost per 100 lambs daily

  
  #Labour_cost<-((Cost_Lambs/100)*median(end_lambs_Target$Target_Day)*
   # df$labour_Daily[i])*wage+(((Cost_Lambs)/100)*
    #((median(end_lambs_Target$Target_Day))/7/2)*df$labour_fortnightly[i])*wage+total_Ews*ew_cost


  # Variable costs
  perlamb<-df$labour_Daily[i]/100
  perlambweek<-df$labour_fortnightly[i]/100
 
  
  Labour_cost<-sum(((perlamb*end_lambs_Target$Target_Day))+((perlambweek*(end_lambs_Target$Target_Day/7/2))))*wage+ew_labour
  ewe_lamb_maintance_Cost<-df$Vet[i]*Cost_Lambs+ew_cost
  land_rent1<-df$land_rent[i]*df$acres[i]
  
  variable_Cost<-(land_rent1+Labour_cost+ewe_lamb_maintance_Cost)
  
  
  # Fixed Costs
  Fixed_Costs<-(predict(Genal_costs,newdata = data.frame("area"=df$acres[i]) )+  predict(power_costs,newdata = data.frame("area"=df$acres[i]))+
  predict(buildings_costs,newdata = data.frame("area"=df$acres[i]) )+predict(labour_costs,newdata = data.frame("area"=df$acres[i]) ))*df$acres[i]
  
  # Total costs
  Total_Cost<-sum(variable_Cost+Fixed_Costs)
  
  
  # Profit Margins
  
  net_profit<-Gross_Income-Total_Cost
  
  Gross_output_per_ewe<-Income_lamb/total_Ewsv1
  variable_cost_per_ewe<-variable_Cost/total_Ewsv1
  fixed_cost_per_ewe<-Fixed_Costs/total_Ewsv1
  Margin_ewe<-Gross_output_per_ewe-sum(variable_cost_per_ewe,fixed_cost_per_ewe)
  
  Gross_profit_per_ewe<-(Gross_Income_ewe+Income_lamb)/total_Ewsv1
  Gross_profit_per_lamb<-Income_lamb/NROW(end_lambs_Target)
  Gross_profit_per_hec<-net_profit/df$acres[i]
  
  
  cost_per_lamb<-Total_Cost/NROW(end_lambs_Target)

  
  # KPIS - https://www.fas.scot/downloads/an-introduction-to-benchmarking-sheep/
  Lambs_Per_ewe<-abs(Rasied_lambs/total_Ewsv1) #end lambing
  DLWG<-mean(end_lambs_Target$End_Weight/end_lambs_Target$Target_Day) # average daily weight gain
  week8_weight<-mean(as.numeric(end_lambs[56,]),na.rm = TRUE)
  Mean_Slaught_day<-mean(end_lambs_Target$Target_Day) # <150 days
  
  price_g<-mean(predict(object = model,newdata =Pricingv2))

  LW_lamb_sol<-mean(predict(object = model,newdata =Pricingv2))
  LU_HA<-(total_Ews*0.15)/df$acres[i]
  LV_KG_ha<-(LU_HA*Lambs_Per_ewe)*mean(end_lambs_Target$End_Weight) # Total kg of liveweight /ha = Stocking rate x rearing % x average weight of  lamb sold 
  

  result<-rbind(result,cbind(df[i,],cbind(Total_Cost,
                                          "Gross_Income/ha"=Gross_Income/df$acres[i],
                                           net_profit,
                                          "net_profit/ha"=net_profit/df$acres[i],
                                          Gross_profit_per_lamb,
                                          price_g,
                                          Gross_profit_per_ewe,
                                          total_Ews,
                                          Total_lambs,
                                          "BPS_percentage_income"=(BPS/Gross_Income)*100,
                                          "Fixed_Costs/ha"=as.numeric(Fixed_Costs)/df$acres[i],
                                          "variable_Costs/ha"=variable_Cost/df$acres[i],
                                          "Labour/ha"=Labour_cost/df$acres[i],
                                          Labour_cost,
                                          Lambs_Per_ewe,
                                          Lambing_Time,
                                          DLWG,
                                          week8_weight,
                                          LW_lamb_sol,
                                          Mean_Slaught_day,
                                          LU_HA,
                                          Gross_output_per_ewe,
                                          variable_cost_per_ewe,
                                          "fixed_cost_per_ewe"=as.numeric(fixed_cost_per_ewe),
                                          Margin_ewe
                                          )))
                                          
    

 
  
}

gc()
################################################################################

data <- result[with(result,order(-result$net_profit)),]
top10<-data[1:round(NROW(data)*0.1,0),]
top25<-data[round(NROW(data)*0.1,0):round(NROW(data)*0.25,0),]
top50<-data[round(NROW(data)*0.25,0):round(NROW(data)*0.50,0),]
top75<-data[round(NROW(data)*0.5,0):round(NROW(data)*0.75,0),]



rbind(round(colMeans(top10[,c(1:15,17:41)]),3),round(colMeans(top25[,c(1:15,17:41)]),3),
      round(colMeans(top50[,c(1:15,17:41)]),3), round(colMeans(top75[,c(1:15,17:41)]),3),
      round(colMeans(data[,c(1:15,17:41)]),3))



mean(top10$net_profit)
mean(top50$net_profit)
mean(data$net_profit)



mean(top50$Lambs_Per_ewe)
mean(top50$net_profit)
mean(top50$acres)
mean(top50$Margin_ewe)



resfull<-rcorr(as.matrix(result[,17:41]))
res25<-rcorr(as.matrix(top25[,17:41]))
res50<-rcorr(as.matrix(top50[,17:41]))


corrplot((res25$r), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)



plot(top50$DLWG~top50$net_profit)


plot(top50$BPS_percentage_income~top50$Margin_ewe)

pairs(top50[,c(20:40)])






corrplot((resfull$r), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)



corrplot((res50$r), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)



plot(result$DLWG~result$week8_weight)


round(res2$r,2)




 
rfdata<-cbind(result[,1:16],"net_profit"=result[,21])
jj<-randomForest(net_profit~.,data = rfdata)

