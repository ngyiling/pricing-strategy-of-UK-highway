# load llibraries ------------
library(dplyr)
library(stargazer)
library(lattice)
library(nloptr)
# step 1: simulation----------------
# load data
vehicle_data = read.csv("dft_aadf_count_point_id_27923.csv")
# drop necessary data
vehicle_data = select(vehicle_data, c(2, 19:24, 30))
# 4 types of vehicles
# 1. motorbikes
vehicle_data$motorbike = vehicle_data$pedal_cycles + vehicle_data$two_wheeled_motor_vehicles
# 2. private cars
vehicle_data$private_cars = vehicle_data$cars_and_taxis
# 3. vans
vehicle_data$vans = vehicle_data$buses_and_coaches + vehicle_data$lgvs + vehicle_data$hgvs_2_rigid_axle
# 4. goods
vehicle_data$multi_goods = vehicle_data$all_hgvs - vehicle_data$hgvs_2_rigid_axle
vehicle_data = select(vehicle_data, -c("pedal_cycles", "two_wheeled_motor_vehicles", 
                                       "cars_and_taxis", "buses_and_coaches", "lgvs", 
                                       "hgvs_2_rigid_axle", "all_hgvs"))
vehicle_data$total = vehicle_data$motorbike+vehicle_data$private_cars+vehicle_data$vans+vehicle_data$multi_goods
vehicle_data$motorbike = vehicle_data$motorbike/vehicle_data$total
vehicle_data$private_cars = vehicle_data$private_cars/vehicle_data$total
vehicle_data$vans = vehicle_data$vans/vehicle_data$total
vehicle_data$multi_goods = vehicle_data$multi_goods/vehicle_data$total
# taking 2019
vehicle_data = vehicle_data[vehicle_data$year == 2019, ]

# size of the simulated dataset of 10000 drivers
# of drivers for each vehicle type
N = 2000
vehicle_data = select(vehicle_data, c(2:5))
vehicle_data = round(vehicle_data*N)
WTP = data.frame(matrix(ncol = 25, nrow = N))
column_names = c(rep(0, 24), "vehicle_type")
for (i in 1:24){
  name = gsub(" ", "", paste("hour", as.character(i)))
  column_names[i] = name
}
colnames(WTP) = column_names
vehicle_type = c(rep("motorbike", vehicle_data$motorbike))
vehicle_type = append(vehicle_type, c(rep("private_cars", vehicle_data$private_cars)))
vehicle_type = append(vehicle_type, c(rep("vans", vehicle_data$vans)))
vehicle_type = append(vehicle_type, c(rep("multi_goods", vehicle_data$multi_goods)))
WTP$vehicle_type = vehicle_type

# simulate WTP for motorbikes
# peak hours: 8am-10am; 5-7pm
for (i in c(8:10, 17:19)){ #peak
  set.seed(i)
  WTP[WTP$vehicle_type == 'motorbike', i] = rnorm(vehicle_data$motorbike, 1*1.2, 0.2)
}
for (i in c(1:7, 11:16, 20:24)){ #non-peak
  set.seed(i)
  WTP[WTP$vehicle_type == 'motorbike', i] = rnorm(vehicle_data$motorbike, 1, 0.2)
}
# simulate WTP for private cars
# peaks hours: 8am-10am; 5-7pm
# assume they are willing to pay 50% more during peak hours
for (i in c(8:10, 17:19)){ #peak
  set.seed(i)
  WTP[WTP$vehicle_type == 'private_cars', i] = rnorm(vehicle_data$private_cars, 2.5*1.5, 0.2)
}
for (i in c(1:7, 11:16, 20:24)){ #non-peak
  set.seed(i)
  WTP[WTP$vehicle_type == 'private_cars', i] = rnorm(vehicle_data$private_cars, 2.5, 0.2)
}
# simulate WTP for vans
# peak hours: 6am-10pm
# assume they are willing to pay 25% more during peak hours
for (i in c(6:22)){ #peak
  set.seed(i)
  WTP[WTP$vehicle_type == 'vans', i] = rnorm(vehicle_data$vans, 3*1.25, 0.2)
}
for (i in c(1:5, 23:24)){ #non-peak
  set.seed(i)
  WTP[WTP$vehicle_type == 'vans', i] = rnorm(vehicle_data$vans, 3, 0.2)
}

# simulate WTP for multi-goods vehicles
for (i in c(1:24)){ #peak
  set.seed(i)
  WTP[WTP$vehicle_type == 'multi_goods', i] = rnorm(vehicle_data$multi_goods, 6, 0.2)
}

# Compute maximum willingness to pay for each client across time slots and enter it as a new column
for (i in 1:N){
  WTP$maxWTP[i]=max(WTP[i,1:24])
}

# step 2: getting demand --------------
# a. motorbike demand --------
# peak and nonpeak time: 7-9am, 4-6pm
WTP_motorbike = WTP[WTP$vehicle_type == "motorbike", ]
maxprice_motorbike = max(WTP_motorbike$maxWTP)
# increment of 5p
# minimum: 5p - max willingtopay
pricerange_mb = round((maxprice_motorbike - 0.05)/0.05)+1
# Price for all time slots (i.e., nonpeak) except for time slot 5 (5-9pm) (peak)
surplusNonPeak_mb<-rep(0,vehicle_data$motorbike)
surplusPeak_mb<-rep(0,vehicle_data$motorbike)
demandNonPeak_mb<-rep(0,pricerange_mb^2)
demandPeak_mb<-rep(0,pricerange_mb^2)
index=1
for (basePrice in seq(from = 0.05, to = round(maxprice_motorbike, 2), by = 0.05)){
  for (peakPrice in seq(from = 0.05, to = round(maxprice_motorbike, 2), by = 0.05)){
    for (i in 1:vehicle_data$motorbike){
      surplusNonPeak_mb[i]=max(WTP_motorbike[i,c(1:6,10:15,19:24)]-basePrice)
      surplusPeak_mb[i]=max(WTP_motorbike[i,c(7:9, 16:18)]-peakPrice)
    }
    demandNonPeak_mb[index]=sum((surplusNonPeak_mb>surplusPeak_mb)*(surplusNonPeak_mb>=0))
    demandPeak_mb[index]=sum((surplusPeak_mb>=surplusNonPeak_mb)*(surplusPeak_mb>=0))
    index=index+1
  }
}

# Create a data table which we will use to run the two regressions:
newdata_mb<-data.frame(matrix(nrow=pricerange_mb^2,ncol = 5))
colnames(newdata_mb)=c("index","basePrice","peakPrice","NonPeakDemand", "PeakDemand")
index=1
for (basePrice in seq(from = 0.05, to = round(maxprice_motorbike, 2), by = 0.05)){
  for (peakPrice in seq(from = 0.05, to = round(maxprice_motorbike, 2), by = 0.05)){
    newdata_mb[index,1]=index
    newdata_mb[index,2]=basePrice
    newdata_mb[index,3]=peakPrice
    newdata_mb[index,4]=demandNonPeak_mb[index]
    newdata_mb[index,5]=demandPeak_mb[index]
    index=index+1
  }
}

# Visualizing Revenue as a Function of Base and Peak Price
newdata_mb$revenue=newdata_mb$basePrice*newdata_mb$NonPeakDemand+newdata_mb$peakPrice*newdata_mb$PeakDemand
wireframe(revenue ~ basePrice * peakPrice, data=newdata_mb)
# Regression for the dependent variable NonPeakDemand
fit2NonPeak <-lm(NonPeakDemand ~ basePrice+peakPrice, data=newdata_mb)
summary(fit2NonPeak)

a1=coef(fit2NonPeak)[1]
b11=coef(fit2NonPeak)[2]
b12=coef(fit2NonPeak)[3]

# Regression for the dependent variable NonPeakDemand
fit2Peak <-lm(PeakDemand ~ basePrice+peakPrice, data=newdata_mb)
a2=coef(fit2Peak)[1]
b21=coef(fit2Peak)[2]
b22=coef(fit2Peak)[3]
stargazer(fit2NonPeak,fit2Peak, type="text")

# b. private_cars demand --------
# peak and nonpeak time: 7-9am, 4-6pm
WTP_car = WTP[WTP$vehicle_type == "private_cars", ]
maxprice_car = max(WTP_car$maxWTP)
# increment of 10p
pricerange_car = round((maxprice_car - 1.5)/0.2)+1
# Price for all time slots (i.e., nonpeak) except for time slot 5 (5-9pm) (peak)
surplusNonPeak_car<-rep(0,vehicle_data$private_car)
surplusPeak_car<-rep(0,vehicle_data$private_car)
demandNonPeak_car<-rep(0,pricerange_car^2)
demandPeak_car<-rep(0,pricerange_car^2)
index=1
for (basePrice in seq(from = 1.5, to = round(maxprice_car, 2), by = 0.2)){
  for (peakPrice in seq(from = 1.5, to = round(maxprice_car, 2), by = 0.2)){
    for (i in 1:vehicle_data$private_car){
      surplusNonPeak_car[i]=max(WTP_car[i,c(1:6,10:15,19:24)]-basePrice)
      surplusPeak_car[i]=max(WTP_car[i,c(7:9, 16:18)]-peakPrice)
    }
    demandNonPeak_car[index]=sum((surplusNonPeak_car>surplusPeak_car)*(surplusNonPeak_car>=0))
    demandPeak_car[index]=sum((surplusPeak_car>=surplusNonPeak_car)*(surplusPeak_car>=0))
    index=index+1
  }
}

# Create a data table which we will use to run the two regressions:
newdata_car<-data.frame(matrix(nrow=pricerange_car^2,ncol = 5))
colnames(newdata_car)=c("index","basePrice","peakPrice","NonPeakDemand", "PeakDemand")
index=1
for (basePrice in seq(from = 1.5, to = round(maxprice_car, 2), by = 0.2)){
  for (peakPrice in seq(from = 1.5, to = round(maxprice_car, 2), by = 0.2)){
    newdata_car[index,1]=index
    newdata_car[index,2]=basePrice
    newdata_car[index,3]=peakPrice
    newdata_car[index,4]=demandNonPeak_car[index]
    newdata_car[index,5]=demandPeak_car[index]
    index=index+1
  }
}

# Visualizing Revenue as a Function of Base and Peak Price
newdata_car$revenue=newdata_car$basePrice*newdata_car$NonPeakDemand+newdata_car$peakPrice*newdata_car$PeakDemand
wireframe(revenue ~ basePrice * peakPrice, data=newdata_car)
# Regression for the dependent variable NonPeakDemand
fit2NonPeak <-lm(NonPeakDemand ~ basePrice+peakPrice, data=newdata_car)
summary(fit2NonPeak)

a3=coef(fit2NonPeak)[1]
b31=coef(fit2NonPeak)[2]
b32=coef(fit2NonPeak)[3]

# Regression for the dependent variable NonPeakDemand
fit2Peak <-lm(PeakDemand ~ basePrice+peakPrice, data=newdata_car)
a4=coef(fit2Peak)[1]
b41=coef(fit2Peak)[2]
b42=coef(fit2Peak)[3]
stargazer(fit2NonPeak,fit2Peak, type="text")

# c. vans ---------
# peak and nonpeak time: 7-9am, 4-6pm
WTP_vans = WTP[WTP$vehicle_type == "vans", ]
maxprice_vans = max(WTP_vans$maxWTP)
# increment of 10p
pricerange_vans = round((maxprice_vans - 2.25)/0.2)+1
# Price for all time slots (i.e., nonpeak) except for time slot 5 (5-9pm) (peak)
surplusNonPeak_vans<-rep(0,vehicle_data$vans)
surplusPeak_vans<-rep(0,vehicle_data$vans)
demandNonPeak_vans<-rep(0,pricerange_vans^2)
demandPeak_vans<-rep(0,pricerange_vans^2)
index=1
for (basePrice in seq(from = 2.25, to = round(maxprice_vans, 2), by = 0.2)){
  for (peakPrice in seq(from = 2.25, to = round(maxprice_vans, 2), by = 0.2)){
    for (i in 1:vehicle_data$vans){
      surplusNonPeak_vans[i]=max(WTP_vans[i,c(1:6,10:15,19:24)]-basePrice)
      surplusPeak_vans[i]=max(WTP_vans[i,c(7:9, 16:18)]-peakPrice)
    }
    demandNonPeak_vans[index]=sum((surplusNonPeak_vans>surplusPeak_vans)*(surplusNonPeak_vans>=0))
    demandPeak_vans[index]=sum((surplusPeak_vans>=surplusNonPeak_vans)*(surplusPeak_vans>=0))
    index=index+1
  }
}

# Create a data table which we will use to run the two regressions:
newdata_vans<-data.frame(matrix(nrow=pricerange_vans^2,ncol = 5))
colnames(newdata_vans)=c("index","basePrice","peakPrice","NonPeakDemand", "PeakDemand")
index=1
for (basePrice in seq(from = 2.25, to = round(maxprice_vans, 2), by = 0.2)){
  for (peakPrice in seq(from = 2.25, to = round(maxprice_vans, 2), by = 0.2)){
    newdata_vans[index,1]=index
    newdata_vans[index,2]=basePrice
    newdata_vans[index,3]=peakPrice
    newdata_vans[index,4]=demandNonPeak_vans[index]
    newdata_vans[index,5]=demandPeak_vans[index]
    index=index+1
  }
}

# Visualizing Revenue as a Function of Base and Peak Price
newdata_vans$revenue=newdata_vans$basePrice*newdata_vans$NonPeakDemand+newdata_vans$peakPrice*newdata_vans$PeakDemand
wireframe(revenue ~ basePrice * peakPrice, data=newdata_vans)
# Regression for the dependent variable NonPeakDemand
fit2NonPeak <-lm(NonPeakDemand ~ basePrice+peakPrice, data=newdata_vans)
summary(fit2NonPeak)

a5=coef(fit2NonPeak)[1]
b51=coef(fit2NonPeak)[2]
b52=coef(fit2NonPeak)[3]

# Regression for the dependent variable NonPeakDemand
fit2Peak <-lm(PeakDemand ~ basePrice+peakPrice, data=newdata_vans)
a6=coef(fit2Peak)[1]
b61=coef(fit2Peak)[2]
b62=coef(fit2Peak)[3]
stargazer(fit2NonPeak,fit2Peak, type="text")
# d. multi-axle goods ---------
# peak and nonpeak time: 7-9am, 4-6pm
WTP_truck = WTP[WTP$vehicle_type == "multi_goods", ]
maxprice_truck = max(WTP_truck$maxWTP)
# increment of 20p
pricerange_truck = round((maxprice_truck - 5.25)/0.2)+1
# Price for all time slots (i.e., nonpeak) except for time slot 5 (5-9pm) (peak)
surplusNonPeak_truck<-rep(0,vehicle_data$multi_goods)
surplusPeak_truck<-rep(0,vehicle_data$multi_goods)
demandNonPeak_truck<-rep(0,pricerange_truck^2)
demandPeak_truck<-rep(0,pricerange_truck^2)
index=1
for (basePrice in seq(from = 5.25, to = round(maxprice_truck, 2), by = 0.2)){
  for (peakPrice in seq(from = 5.25, to = round(maxprice_truck, 2), by = 0.2)){
    for (i in 1:vehicle_data$multi_goods){
      surplusNonPeak_truck[i]=max(WTP_truck[i,c(1:6,10:15,19:24)]-basePrice)
      surplusPeak_truck[i]=max(WTP_truck[i,c(7:9, 16:18)]-peakPrice)
    }
    demandNonPeak_truck[index]=sum((surplusNonPeak_truck>surplusPeak_truck)*(surplusNonPeak_truck>=0))
    demandPeak_truck[index]=sum((surplusPeak_truck>=surplusNonPeak_truck)*(surplusPeak_truck>=0))
    index=index+1
  }
}

# Create a data table which we will use to run the two regressions:
newdata_truck<-data.frame(matrix(nrow=pricerange_truck^2,ncol = 5))
colnames(newdata_truck)=c("index","basePrice","peakPrice","NonPeakDemand", "PeakDemand")
index=1
for (basePrice in seq(from = 5.25, to = round(maxprice_truck, 2), by = 0.2)){
  for (peakPrice in seq(from = 5.25, to = round(maxprice_truck, 2), by = 0.2)){
    newdata_truck[index,1]=index
    newdata_truck[index,2]=basePrice
    newdata_truck[index,3]=peakPrice
    newdata_truck[index,4]=demandNonPeak_truck[index]
    newdata_truck[index,5]=demandPeak_truck[index]
    index=index+1
  }
}

# Visualizing Revenue as a Function of Base and Peak Price
newdata_truck$revenue=newdata_truck$basePrice*newdata_truck$NonPeakDemand+newdata_truck$peakPrice*newdata_truck$PeakDemand
wireframe(revenue ~ basePrice * peakPrice, data=newdata_truck)
# Regression for the dependent variable NonPeakDemand
fit2NonPeak <-lm(NonPeakDemand ~ basePrice+peakPrice, data=newdata_truck)
summary(fit2NonPeak)

a7=coef(fit2NonPeak)[1]
b71=coef(fit2NonPeak)[2]
b72=coef(fit2NonPeak)[3]
# Regression for the dependent variable NonPeakDemand
fit2Peak <-lm(PeakDemand ~ basePrice+peakPrice, data=newdata_truck)
a8=coef(fit2Peak)[1]
b81=coef(fit2Peak)[2]
b82=coef(fit2Peak)[3]
stargazer(fit2NonPeak,fit2Peak, type="text")
# step 3: Finding optimal prices by optimization------
# Scenario 1 - Obj: maximise revenue ---------
# scale to real volume
k = 216108/24/2000
eval_f <- function(x){
  basePrice_mb=x[1]
  peakPrice_mb=x[2]
  basePrice_car=x[3]
  peakPrice_car=x[4]
  basePrice_vans=x[5]
  peakPrice_vans=x[6]
  basePrice_truck=x[7]
  peakPrice_truck=x[8]
  
  NonPeakDemand_mb=max(0,a1+b11*basePrice_mb+b12*peakPrice_mb)*k
  PeakDemand_mb=max(0,a2+b21*basePrice_mb+b22*peakPrice_mb)*k
  NonPeakDemand_car=max(0,a3+b31*basePrice_car+b32*peakPrice_car)*k
  PeakDemand_car=max(0,a4+b41*basePrice_car+b42*peakPrice_car)*k
  NonPeakDemand_vans=max(0,a5+b51*basePrice_vans+b52*peakPrice_vans)*k
  PeakDemand_vans=max(0,a6+b61*basePrice_vans+b62*peakPrice_vans)*k
  NonPeakDemand_truck=max(0,a7+b71*basePrice_truck+b72*peakPrice_truck)*k
  PeakDemand_truck=max(0,a8+b81*basePrice_truck+b82*peakPrice_truck)*k
  
  revenue=(basePrice_mb*NonPeakDemand_mb+basePrice_car*NonPeakDemand_car+
    basePrice_vans*NonPeakDemand_vans+basePrice_truck*NonPeakDemand_truck)*20+
    (peakPrice_car*PeakDemand_car+peakPrice_vans*PeakDemand_vans+
    peakPrice_truck*PeakDemand_truck+peakPrice_mb*PeakDemand_mb)*4
  
  objfunction=-revenue
  return(objfunction)
}

eval_g_ineq <- function(x) {
  basePrice_mb=x[1]
  peakPrice_mb=x[2]
  basePrice_car=x[3]
  peakPrice_car=x[4]
  basePrice_vans=x[5]
  peakPrice_vans=x[6]
  basePrice_truck=x[7]
  peakPrice_truck=x[8]
  
  NonPeakDemand_mb=max(0,a1+b11*basePrice_mb+b12*peakPrice_mb)*k
  PeakDemand_mb=max(0,a2+b21*basePrice_mb+b22*peakPrice_mb)*k
  NonPeakDemand_car=max(0,a3+b31*basePrice_car+b32*peakPrice_car)*k
  PeakDemand_car=max(0,a4+b41*basePrice_car+b42*peakPrice_car)*k
  NonPeakDemand_vans=max(0,a5+b51*basePrice_vans+b52*peakPrice_vans)*k
  PeakDemand_vans=max(0,a6+b61*basePrice_vans+b62*peakPrice_vans)*k
  NonPeakDemand_truck=max(0,a7+b71*basePrice_truck+b72*peakPrice_truck)*k
  PeakDemand_truck=max(0,a8+b81*basePrice_truck+b82*peakPrice_truck)*k
  
  revenue=(basePrice_mb*NonPeakDemand_mb+basePrice_car*NonPeakDemand_car+
             basePrice_vans*NonPeakDemand_vans+basePrice_truck*NonPeakDemand_truck)*20+
    (peakPrice_car*PeakDemand_car+peakPrice_vans*PeakDemand_vans+
       peakPrice_truck*PeakDemand_truck+peakPrice_mb*PeakDemand_mb)*4
  
  capacity = 10000
  
  NonPeakSpeed = 48 - 0.0625*sum(NonPeakDemand_car, NonPeakDemand_mb, 
                                 NonPeakDemand_vans, NonPeakDemand_truck)/1000/18
  PeakSpeed = 48 - 0.0625*sum(PeakDemand_car, PeakDemand_mb, 
                              PeakDemand_vans, PeakDemand_truck)/1000/6
  
  NonPeakEmission_mb = ifelse(NonPeakSpeed<25, 2200*2.87/(0.35*1.48*NonPeakSpeed),
                              2200*2.87/(0.35*(45.33-0.33*NonPeakSpeed)))*NonPeakDemand_mb
  PeakEmission_mb = ifelse(PeakSpeed<25, 2200*2.87/(0.35*1.48*PeakSpeed),
                           2200*2.87/(0.35*(45.33-0.33*PeakSpeed)))*PeakDemand_mb
  NonPeakEmission_car = ifelse(NonPeakSpeed<25, 2392*2.87/(0.35*1.48*NonPeakSpeed),
                               2392*2.87/(0.35*(45.33-0.33*NonPeakSpeed)))*NonPeakDemand_car
  PeakEmission_car = ifelse(PeakSpeed<25, 2392*2.87/(0.35*1.48*PeakSpeed),
                            2392*2.87/(0.35*(45.33-0.33*PeakSpeed)))*PeakDemand_car
  NonPeakEmission_vans = ifelse(NonPeakSpeed<25, 2500*2.87/(0.35*1.48*NonPeakSpeed),
                                2500*2.87/(0.35*(45.33-0.33*NonPeakSpeed)))*NonPeakDemand_vans
  PeakEmission_vans = ifelse(PeakSpeed<25, 2500*2.87/(0.35*1.48*PeakSpeed),
                             2500*2.87/(0.35*(45.33-0.33*PeakSpeed)))*PeakDemand_vans
  NonPeakEmission_truck = ifelse(NonPeakSpeed<25, 2600*2.87/(0.35*1.48*NonPeakSpeed),
                                 2600*2.87/(0.35*(45.33-0.33*NonPeakSpeed)))*NonPeakDemand_truck
  PeakEmission_truck = ifelse(PeakSpeed<25, 2600*2.87/(0.35*1.48*PeakSpeed),
                              2600*2.87/(0.35*(45.33-0.33*PeakSpeed)))*PeakDemand_truck
  
  constraint <- c(-NonPeakDemand_mb,
                  -PeakDemand_mb,
                  -NonPeakDemand_car,
                  -PeakDemand_car,
                  -NonPeakDemand_vans,
                  -PeakDemand_vans,
                  -NonPeakDemand_truck,
                  -PeakDemand_truck,
                  # nonpeak price <= peak price
                  x[1]-x[2],
                  x[3]-x[4],
                  x[5]-x[6],
                  x[7]-x[8],
                  
                  # sum of demand <= capacity
                  NonPeakDemand_car + NonPeakDemand_mb + NonPeakDemand_vans
                  + NonPeakDemand_truck - capacity,
                  PeakDemand_car + PeakDemand_mb + PeakDemand_vans + PeakDemand_truck - capacity,
                  
                  # operational cost <= revenue
                  (102200000/365) - revenue,
                  
                  # average speed <= 48 (speed limit)
                  NonPeakSpeed - 48,
                  PeakSpeed - 48,
                  # emission <= emission allowance
                  NonPeakEmission_mb - NonPeakDemand_mb*(352775),
                  NonPeakEmission_car - NonPeakDemand_car*(352775*2),
                  NonPeakEmission_vans - NonPeakDemand_vans*(352775*10),
                  NonPeakEmission_truck - NonPeakDemand_truck*(352775*5),
                  PeakEmission_mb - PeakDemand_mb*(352775),
                  PeakEmission_car - PeakDemand_car*(352775*2),
                  PeakEmission_vans - PeakDemand_vans*(352775*10),
                  PeakEmission_truck - PeakDemand_truck*(352775*5)
  )
  return(constraint)
}

# initial values
x0 <- c(0.05, 0.05, 1.5, 1.5, 2.25, 2.25, 5.25, 5.25)
# lower and upper bounds of control
lb <- c(0.05, 0.05, 1.5, 1.5, 2.25, 2.25, 5.25, 5.25)
ub <- c(1, 2.5, 2, 4.5, 3, 6, 5.5, 10)
opts <- list( "algorithm" = "NLOPT_LN_COBYLA",
              "xtol_rel"  = 1.0e-9,
              "maxeval"   = 1000)
result <- nloptr(x0=x0,eval_f=eval_f,lb=lb,ub=ub,
                 eval_g_ineq=eval_g_ineq,opts=opts)

priceOpt<-result$solution
RevenueOpt<- -result$objective

print(paste("Optimal Base Price:",priceOpt[1]))
print(paste("Optimal Peak Price:",priceOpt[2]))
print(paste("Optimal Base Price:",priceOpt[3]))
print(paste("Optimal Peak Price:",priceOpt[4]))
print(paste("Optimal Base Price:",priceOpt[5]))
print(paste("Optimal Peak Price:",priceOpt[6]))
print(paste("Optimal Base Price:",priceOpt[7]))
print(paste("Optimal Peak Price:",priceOpt[8]))
print(paste("Optimal Revenue:",RevenueOpt))

NonPeakdemand_mb_1 = max(0,a1+b11*priceOpt[1]+b12*priceOpt[2])*k
Peakdemand_mb_1 = max(0,a2+b21*priceOpt[1]+b22*priceOpt[2])*k
NonPeakdemand_car_1 = max(0,a3+b31*priceOpt[3]+b32*priceOpt[4])*k
Peakdemand_car_1 = max(0,a4+b41*priceOpt[3]+b42*priceOpt[4])*k
NonPeakdemand_vans_1 = max(0,a5+b51*priceOpt[5]+b52*priceOpt[6])*k
Peakdemand_vans_1 = max(0,a6+b61*priceOpt[5]+b62*priceOpt[6])*k
NonPeakdemand_trucks_1 = max(0,a7+b71*priceOpt[7]+b72*priceOpt[8])*k
Peakdemand_trucks_1 = max(0,a8+b81*priceOpt[7]+b82*priceOpt[8])*k
throughput_1 = (NonPeakdemand_mb_1+NonPeakdemand_car_1+
                NonPeakdemand_vans_1+NonPeakdemand_trucks_1)*20+
  (Peakdemand_mb_1+Peakdemand_car_1+
     Peakdemand_vans_1+Peakdemand_trucks_1)*4
throughput_1
# Scenario 2 - Obj: Minimise emission ------------------
# scale to real volume
k = 216108/24/2000
eval_f <- function(x){
  basePrice_mb=x[1]
  peakPrice_mb=x[2]
  basePrice_car=x[3]
  peakPrice_car=x[4]
  basePrice_vans=x[5]
  peakPrice_vans=x[6]
  basePrice_truck=x[7]
  peakPrice_truck=x[8]
  
  NonPeakDemand_mb=max(0,a1+b11*basePrice_mb+b12*peakPrice_mb)*k
  PeakDemand_mb=max(0,a2+b21*basePrice_mb+b22*peakPrice_mb)*k
  NonPeakDemand_car=max(0,a3+b31*basePrice_car+b32*peakPrice_car)*k
  PeakDemand_car=max(0,a4+b41*basePrice_car+b42*peakPrice_car)*k
  NonPeakDemand_vans=max(0,a5+b51*basePrice_vans+b52*peakPrice_vans)*k
  PeakDemand_vans=max(0,a6+b61*basePrice_vans+b62*peakPrice_vans)*k
  NonPeakDemand_truck=max(0,a7+b71*basePrice_truck+b72*peakPrice_truck)*k
  PeakDemand_truck=max(0,a8+b81*basePrice_truck+b82*peakPrice_truck)*k
  
  revenue=(basePrice_mb*NonPeakDemand_mb+basePrice_car*NonPeakDemand_car+
             basePrice_vans*NonPeakDemand_vans+basePrice_truck*NonPeakDemand_truck)*20+
    (peakPrice_car*PeakDemand_car+peakPrice_vans*PeakDemand_vans+
       peakPrice_truck*PeakDemand_truck+peakPrice_mb*PeakDemand_mb)*4
  
  NonPeakSpeed = 48 - 0.0625*sum(NonPeakDemand_car, NonPeakDemand_mb, 
                                 NonPeakDemand_vans, NonPeakDemand_truck)/1000/18
  PeakSpeed = 48 - 0.0625*sum(PeakDemand_car, PeakDemand_mb, 
                              PeakDemand_vans, PeakDemand_truck)/1000/6
  
  NonPeakEmission_mb = ifelse(NonPeakSpeed<25, 2200*2.87/(0.35*1.48*NonPeakSpeed),
                              2200*2.87/(0.35*(45.33-0.33*NonPeakSpeed)))*NonPeakDemand_mb
  PeakEmission_mb = ifelse(PeakSpeed<25, 2200*2.87/(0.35*1.48*PeakSpeed),
                           2200*2.87/(0.35*(45.33-0.33*PeakSpeed)))*PeakDemand_mb
  NonPeakEmission_car = ifelse(NonPeakSpeed<25, 2392*2.87/(0.35*1.48*NonPeakSpeed),
                               2392*2.87/(0.35*(45.33-0.33*NonPeakSpeed)))*NonPeakDemand_car
  PeakEmission_car = ifelse(PeakSpeed<25, 2392*2.87/(0.35*1.48*PeakSpeed),
                            2392*2.87/(0.35*(45.33-0.33*PeakSpeed)))*PeakDemand_car
  NonPeakEmission_vans = ifelse(NonPeakSpeed<25, 2500*2.87/(0.35*1.48*NonPeakSpeed),
                                2500*2.87/(0.35*(45.33-0.33*NonPeakSpeed)))*NonPeakDemand_vans
  PeakEmission_vans = ifelse(PeakSpeed<25, 2500*2.87/(0.35*1.48*PeakSpeed),
                             2500*2.87/(0.35*(45.33-0.33*PeakSpeed)))*PeakDemand_vans
  NonPeakEmission_truck = ifelse(NonPeakSpeed<25, 2600*2.87/(0.35*1.48*NonPeakSpeed),
                                 2600*2.87/(0.35*(45.33-0.33*NonPeakSpeed)))*NonPeakDemand_truck
  PeakEmission_truck = ifelse(PeakSpeed<25, 2600*2.87/(0.35*1.48*PeakSpeed),
                              2600*2.87/(0.35*(45.33-0.33*PeakSpeed)))*PeakDemand_truck
  Emission = NonPeakEmission_mb+NonPeakEmission_car+NonPeakEmission_vans+NonPeakDemand_truck+
    PeakEmission_mb+PeakEmission_car+PeakEmission_vans+PeakDemand_truck
  
  objfunction=Emission
  
  return(objfunction)
}

eval_g_ineq <- function(x) {
  basePrice_mb=x[1]
  peakPrice_mb=x[2]
  basePrice_car=x[3]
  peakPrice_car=x[4]
  basePrice_vans=x[5]
  peakPrice_vans=x[6]
  basePrice_truck=x[7]
  peakPrice_truck=x[8]
  
  NonPeakDemand_mb=max(0,a1+b11*basePrice_mb+b12*peakPrice_mb)*k
  PeakDemand_mb=max(0,a2+b21*basePrice_mb+b22*peakPrice_mb)*k
  NonPeakDemand_car=max(0,a3+b31*basePrice_car+b32*peakPrice_car)*k
  PeakDemand_car=max(0,a4+b41*basePrice_car+b42*peakPrice_car)*k
  NonPeakDemand_vans=max(0,a5+b51*basePrice_vans+b52*peakPrice_vans)*k
  PeakDemand_vans=max(0,a6+b61*basePrice_vans+b62*peakPrice_vans)*k
  NonPeakDemand_truck=max(0,a7+b71*basePrice_truck+b72*peakPrice_truck)*k
  PeakDemand_truck=max(0,a8+b81*basePrice_truck+b82*peakPrice_truck)*k
  
  revenue=(basePrice_mb*NonPeakDemand_mb+basePrice_car*NonPeakDemand_car+
             basePrice_vans*NonPeakDemand_vans+basePrice_truck*NonPeakDemand_truck)*20+
    (peakPrice_car*PeakDemand_car+peakPrice_vans*PeakDemand_vans+
       peakPrice_truck*PeakDemand_truck+peakPrice_mb*PeakDemand_mb)*4
  
  capacity = 10000
  
  NonPeakSpeed = 48 - 0.0625*sum(NonPeakDemand_car, NonPeakDemand_mb, 
                                 NonPeakDemand_vans, NonPeakDemand_truck)/1000/18
  PeakSpeed = 48 - 0.0625*sum(PeakDemand_car, PeakDemand_mb, 
                              PeakDemand_vans, PeakDemand_truck)/1000/6
  
  NonPeakEmission_mb = ifelse(NonPeakSpeed<25, 2200*2.87/(0.35*1.48*NonPeakSpeed),
                              2200*2.87/(0.35*(45.33-0.33*NonPeakSpeed)))*NonPeakDemand_mb
  PeakEmission_mb = ifelse(PeakSpeed<25, 2200*2.87/(0.35*1.48*PeakSpeed),
                           2200*2.87/(0.35*(45.33-0.33*PeakSpeed)))*PeakDemand_mb
  NonPeakEmission_car = ifelse(NonPeakSpeed<25, 2392*2.87/(0.35*1.48*NonPeakSpeed),
                               2392*2.87/(0.35*(45.33-0.33*NonPeakSpeed)))*NonPeakDemand_car
  PeakEmission_car = ifelse(PeakSpeed<25, 2392*2.87/(0.35*1.48*PeakSpeed),
                            2392*2.87/(0.35*(45.33-0.33*PeakSpeed)))*PeakDemand_car
  NonPeakEmission_vans = ifelse(NonPeakSpeed<25, 2500*2.87/(0.35*1.48*NonPeakSpeed),
                                2500*2.87/(0.35*(45.33-0.33*NonPeakSpeed)))*NonPeakDemand_vans
  PeakEmission_vans = ifelse(PeakSpeed<25, 2500*2.87/(0.35*1.48*PeakSpeed),
                             2500*2.87/(0.35*(45.33-0.33*PeakSpeed)))*PeakDemand_vans
  NonPeakEmission_truck = ifelse(NonPeakSpeed<25, 2600*2.87/(0.35*1.48*NonPeakSpeed),
                                 2600*2.87/(0.35*(45.33-0.33*NonPeakSpeed)))*NonPeakDemand_truck
  PeakEmission_truck = ifelse(PeakSpeed<25, 2600*2.87/(0.35*1.48*PeakSpeed),
                              2600*2.87/(0.35*(45.33-0.33*PeakSpeed)))*PeakDemand_truck
  
  constraint <- c(-NonPeakDemand_mb,
                  -PeakDemand_mb,
                  -NonPeakDemand_car,
                  -PeakDemand_car,
                  -NonPeakDemand_vans,
                  -PeakDemand_vans,
                  -NonPeakDemand_truck,
                  -PeakDemand_truck,
                  # nonpeak price <= peak price
                  x[1]-x[2],
                  x[3]-x[4],
                  x[5]-x[6],
                  x[7]-x[8],
                  
                  # sum of demand <= capacity
                  NonPeakDemand_car + NonPeakDemand_mb + NonPeakDemand_vans
                  + NonPeakDemand_truck - capacity,
                  PeakDemand_car + PeakDemand_mb + PeakDemand_vans + PeakDemand_truck - capacity,
                  
                  # operational cost <= revenue
                  (102200000/365) - revenue,
                  
                  # average speed <= 48 (speed limit)
                  NonPeakSpeed - 48,
                  PeakSpeed - 48,
                  
                  # emission <= emission allowance
                  NonPeakEmission_mb - NonPeakDemand_mb*(352775),
                  NonPeakEmission_car - NonPeakDemand_car*(352775*2),
                  NonPeakEmission_vans - NonPeakDemand_vans*(352775*10),
                  NonPeakEmission_truck - NonPeakDemand_truck*(352775*5),
                  PeakEmission_mb - PeakDemand_mb*(352775),
                  PeakEmission_car - PeakDemand_car*(352775*2),
                  PeakEmission_vans - PeakDemand_vans*(352775*10),
                  PeakEmission_truck - PeakDemand_truck*(352775*5)
  )
  return(constraint)
}

# initial values
x0 <- c(0.05, 0.05, 1.5, 1.5, 2.25, 2.25, 5.25, 5.25)
# lower and upper bounds of control
lb <- c(0.05, 0.05, 1.5, 1.5, 2.25, 2.25, 5.25, 5.25)
ub <- c(1, 2.5, 2, 4.5, 3, 6, 5.5, 10)
opts <- list( "algorithm" = "NLOPT_LN_COBYLA",
              "xtol_rel"  = 1.0e-9,
              "maxeval"   = 1000)
result <- nloptr(x0=x0,eval_f=eval_f,lb=lb,ub=ub,
                 eval_g_ineq=eval_g_ineq,opts=opts)

priceOpt_2<-result$solution
EmissionOpt_2<- result$objective

print(paste("Optimal Base Price:",priceOpt_2[1]))
print(paste("Optimal Peak Price:",priceOpt_2[2]))
print(paste("Optimal Base Price:",priceOpt_2[3]))
print(paste("Optimal Peak Price:",priceOpt_2[4]))
print(paste("Optimal Base Price:",priceOpt_2[5]))
print(paste("Optimal Peak Price:",priceOpt_2[6]))
print(paste("Optimal Base Price:",priceOpt_2[7]))
print(paste("Optimal Peak Price:",priceOpt_2[8]))
print(paste("Optimal Emission:",EmissionOpt_2))

NonPeakdemand_mb_2 = max(0,a1+b11*priceOpt_2[1]+b12*priceOpt_2[2])*k
Peakdemand_mb_2 = max(0,a2+b21*priceOpt_2[1]+b22*priceOpt_2[2])*k
NonPeakdemand_car_2 = max(0,a3+b31*priceOpt_2[3]+b32*priceOpt_2[4])*k
Peakdemand_car_2 = max(0,a4+b41*priceOpt_2[3]+b42*priceOpt_2[4])*k
NonPeakdemand_vans_2 = max(0,a5+b51*priceOpt_2[5]+b52*priceOpt_2[6])*k
Peakdemand_vans_2 = max(0,a6+b61*priceOpt_2[5]+b62*priceOpt_2[6])*k
NonPeakdemand_trucks_2 = max(0,a7+b71*priceOpt_2[7]+b72*priceOpt_2[8])*k
Peakdemand_trucks_2 = max(0,a8+b81*priceOpt_2[7]+b82*priceOpt_2[8])*k
revenue_2=(priceOpt_2[1]*NonPeakdemand_mb_2+priceOpt_2[3]*NonPeakdemand_car_2+
             priceOpt_2[5]*NonPeakdemand_vans_2+priceOpt_2[7]*NonPeakdemand_trucks_2)*20+
  (priceOpt_2[2]*Peakdemand_mb_2+priceOpt_2[4]*Peakdemand_car_2+
     priceOpt_2[6]*Peakdemand_vans_2+priceOpt_2[8]*Peakdemand_trucks_2)*4
throughput_2 = (NonPeakdemand_mb_2+NonPeakdemand_car_2+
                  NonPeakdemand_vans_2+NonPeakdemand_trucks_2)*20+
  (Peakdemand_mb_2+Peakdemand_car_2+
     Peakdemand_vans_2+Peakdemand_trucks_2)*4
throughput_2
# Scenario 3 - Obj: Maximise throughput -------------
# scale to real volume
k = 216108/24/2000
eval_f <- function(x){
  basePrice_mb=x[1]
  peakPrice_mb=x[2]
  basePrice_car=x[3]
  peakPrice_car=x[4]
  basePrice_vans=x[5]
  peakPrice_vans=x[6]
  basePrice_truck=x[7]
  peakPrice_truck=x[8]
  
  NonPeakDemand_mb=max(0,a1+b11*basePrice_mb+b12*peakPrice_mb)*k
  PeakDemand_mb=max(0,a2+b21*basePrice_mb+b22*peakPrice_mb)*k
  NonPeakDemand_car=max(0,a3+b31*basePrice_car+b32*peakPrice_car)*k
  PeakDemand_car=max(0,a4+b41*basePrice_car+b42*peakPrice_car)*k
  NonPeakDemand_vans=max(0,a5+b51*basePrice_vans+b52*peakPrice_vans)*k
  PeakDemand_vans=max(0,a6+b61*basePrice_vans+b62*peakPrice_vans)*k
  NonPeakDemand_truck=max(0,a7+b71*basePrice_truck+b72*peakPrice_truck)*k
  PeakDemand_truck=max(0,a8+b81*basePrice_truck+b82*peakPrice_truck)*k
  
  revenue=(basePrice_mb*NonPeakDemand_mb+basePrice_car*NonPeakDemand_car+
             basePrice_vans*NonPeakDemand_vans+basePrice_truck*NonPeakDemand_truck)*20+
    (peakPrice_car*PeakDemand_car+peakPrice_vans*PeakDemand_vans+
       peakPrice_truck*PeakDemand_truck+peakPrice_mb*PeakDemand_mb)*4
  
  throughput = (NonPeakDemand_mb+NonPeakDemand_car+
                NonPeakDemand_vans+NonPeakDemand_truck)*20+
    (PeakDemand_car+PeakDemand_vans+
       PeakDemand_truck+PeakDemand_mb)*4
  
  objfunction=-throughput
  
  return(objfunction)
}

eval_g_ineq <- function(x) {
  basePrice_mb=x[1]
  peakPrice_mb=x[2]
  basePrice_car=x[3]
  peakPrice_car=x[4]
  basePrice_vans=x[5]
  peakPrice_vans=x[6]
  basePrice_truck=x[7]
  peakPrice_truck=x[8]
  
  NonPeakDemand_mb=max(0,a1+b11*basePrice_mb+b12*peakPrice_mb)*k
  PeakDemand_mb=max(0,a2+b21*basePrice_mb+b22*peakPrice_mb)*k
  NonPeakDemand_car=max(0,a3+b31*basePrice_car+b32*peakPrice_car)*k
  PeakDemand_car=max(0,a4+b41*basePrice_car+b42*peakPrice_car)*k
  NonPeakDemand_vans=max(0,a5+b51*basePrice_vans+b52*peakPrice_vans)*k
  PeakDemand_vans=max(0,a6+b61*basePrice_vans+b62*peakPrice_vans)*k
  NonPeakDemand_truck=max(0,a7+b71*basePrice_truck+b72*peakPrice_truck)*k
  PeakDemand_truck=max(0,a8+b81*basePrice_truck+b82*peakPrice_truck)*k
  
  revenue=(basePrice_mb*NonPeakDemand_mb+basePrice_car*NonPeakDemand_car+
             basePrice_vans*NonPeakDemand_vans+basePrice_truck*NonPeakDemand_truck)*20+
    (peakPrice_car*PeakDemand_car+peakPrice_vans*PeakDemand_vans+
       peakPrice_truck*PeakDemand_truck+peakPrice_mb*PeakDemand_mb)*4
  
  capacity = 10000
  
  NonPeakSpeed = 48 - 0.0625*sum(NonPeakDemand_car, NonPeakDemand_mb, 
                                 NonPeakDemand_vans, NonPeakDemand_truck)/1000/18
  PeakSpeed = 48 - 0.0625*sum(PeakDemand_car, PeakDemand_mb, 
                              PeakDemand_vans, PeakDemand_truck)/1000/6
  
  NonPeakEmission_mb = ifelse(NonPeakSpeed<25, 2200*2.87/(0.35*1.48*NonPeakSpeed),
                              2200*2.87/(0.35*(45.33-0.33*NonPeakSpeed)))*NonPeakDemand_mb
  PeakEmission_mb = ifelse(PeakSpeed<25, 2200*2.87/(0.35*1.48*PeakSpeed),
                           2200*2.87/(0.35*(45.33-0.33*PeakSpeed)))*PeakDemand_mb
  NonPeakEmission_car = ifelse(NonPeakSpeed<25, 2392*2.87/(0.35*1.48*NonPeakSpeed),
                               2392*2.87/(0.35*(45.33-0.33*NonPeakSpeed)))*NonPeakDemand_car
  PeakEmission_car = ifelse(PeakSpeed<25, 2392*2.87/(0.35*1.48*PeakSpeed),
                            2392*2.87/(0.35*(45.33-0.33*PeakSpeed)))*PeakDemand_car
  NonPeakEmission_vans = ifelse(NonPeakSpeed<25, 2500*2.87/(0.35*1.48*NonPeakSpeed),
                                2500*2.87/(0.35*(45.33-0.33*NonPeakSpeed)))*NonPeakDemand_vans
  PeakEmission_vans = ifelse(PeakSpeed<25, 2500*2.87/(0.35*1.48*PeakSpeed),
                             2500*2.87/(0.35*(45.33-0.33*PeakSpeed)))*PeakDemand_vans
  NonPeakEmission_truck = ifelse(NonPeakSpeed<25, 2600*2.87/(0.35*1.48*NonPeakSpeed),
                                 2600*2.87/(0.35*(45.33-0.33*NonPeakSpeed)))*NonPeakDemand_truck
  PeakEmission_truck = ifelse(PeakSpeed<25, 2600*2.87/(0.35*1.48*PeakSpeed),
                              2600*2.87/(0.35*(45.33-0.33*PeakSpeed)))*PeakDemand_truck
  
  constraint <- c(-NonPeakDemand_mb,
                  -PeakDemand_mb,
                  -NonPeakDemand_car,
                  -PeakDemand_car,
                  -NonPeakDemand_vans,
                  -PeakDemand_vans,
                  -NonPeakDemand_truck,
                  -PeakDemand_truck,
                  # nonpeak price <= peak price
                  x[1]-x[2],
                  x[3]-x[4],
                  x[5]-x[6],
                  x[7]-x[8],
                  
                  # sum of demand <= capacity
                  NonPeakDemand_car + NonPeakDemand_mb + NonPeakDemand_vans
                  + NonPeakDemand_truck - capacity,
                  PeakDemand_car + PeakDemand_mb + PeakDemand_vans + PeakDemand_truck - capacity,
                  
                  # operational cost <= revenue
                  (102200000/365) - revenue,
                  
                  # average speed <= 48 (speed limit)
                  NonPeakSpeed - 48,
                  PeakSpeed - 48,
                  
                  # total emission nonpeak <= emission allowance
                  NonPeakEmission_mb - NonPeakDemand_mb*(352775),
                  NonPeakEmission_car - NonPeakDemand_car*(352775*2),
                  NonPeakEmission_vans - NonPeakDemand_vans*(352775*10),
                  NonPeakEmission_truck - NonPeakDemand_truck*(352775*5),
                  PeakEmission_mb - PeakDemand_mb*(352775),
                  PeakEmission_car - PeakDemand_car*(352775*2),
                  PeakEmission_vans - PeakDemand_vans*(352775*10),
                  PeakEmission_truck - PeakDemand_truck*(352775*5)
  )
  return(constraint)
}

# initial values
x0 <- c(0.05, 0.05, 1.5, 1.5, 2.25, 2.25, 5.25, 5.25)
# lower and upper bounds of control
lb = c(0.05, 0.05, 0.5, 0.5,0.5, 0.5, 0.5, 0.5)
ub <- c(1, 2.5, 2, 4.5, 3, 6, 5.5, 10)
opts <- list( "algorithm" = "NLOPT_LN_COBYLA",
              "xtol_rel"  = 1.0e-9,
              "maxeval"   = 1000)
result <- nloptr(x0=x0,eval_f=eval_f,lb=lb,ub=ub,
                 eval_g_ineq=eval_g_ineq,opts=opts)

priceOpt_3<-result$solution
ThroughputOpt_3<- -result$objective

print(paste("Optimal Base Price:",priceOpt_3[1]))
print(paste("Optimal Peak Price:",priceOpt_3[2]))
print(paste("Optimal Base Price:",priceOpt_3[3]))
print(paste("Optimal Peak Price:",priceOpt_3[4]))
print(paste("Optimal Base Price:",priceOpt_3[5]))
print(paste("Optimal Peak Price:",priceOpt_3[6]))
print(paste("Optimal Base Price:",priceOpt_3[7]))
print(paste("Optimal Peak Price:",priceOpt_3[8]))
print(paste("Optimal Throughput:",ThroughputOpt_3))

NonPeakdemand_mb_3 = max(0,a1+b11*priceOpt_3[1]+b12*priceOpt_3[2])*k
Peakdemand_mb_3 = max(0,a2+b21*priceOpt_3[1]+b22*priceOpt_3[2])*k
NonPeakdemand_car_3 = max(0,a3+b31*priceOpt_3[3]+b32*priceOpt_3[4])*k
Peakdemand_car_3 = max(0,a4+b41*priceOpt_3[3]+b42*priceOpt_3[4])*k
NonPeakdemand_vans_3 = max(0,a5+b51*priceOpt_3[5]+b52*priceOpt_3[6])*k
Peakdemand_vans_3 = max(0,a6+b61*priceOpt_3[5]+b62*priceOpt_3[6])*k
NonPeakdemand_trucks_3 = max(0,a7+b71*priceOpt_3[7]+b72*priceOpt_3[8])*k
Peakdemand_trucks_3 = max(0,a8+b81*priceOpt_3[7]+b82*priceOpt_3[8])*k

demand_3 = c(NonPeakdemand_mb_3, Peakdemand_mb_3, NonPeakdemand_car_3, Peakdemand_car_3,
           NonPeakdemand_vans_3, Peakdemand_vans_3, NonPeakdemand_trucks_3, Peakdemand_trucks_3)
revenue_3=(priceOpt_3[1]*NonPeakdemand_mb_3+priceOpt_3[3]*NonPeakdemand_car_3+
             priceOpt_3[5]*NonPeakdemand_vans_3+priceOpt_3[7]*NonPeakdemand_trucks_3)*20+
  (priceOpt_3[2]*Peakdemand_mb_3+priceOpt_3[4]*Peakdemand_car_3+
     priceOpt_3[6]*Peakdemand_vans_3+priceOpt_3[8]*Peakdemand_trucks_3)*4

revenue_3

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
### Optimal Admission Decision for multiple occupants car and single drivers 
### with Mixed Arrivals

#Calculate emission for HOT lanes and Normal lanes
# per day
emission.per.car= 
  (2200*2.872/(0.354*(45.33 - 0.33*(48 - (0.0625 * (1.094))))) +
     2392*2.872/(0.354006*(45.33 - 0.33*(48 - 0.0625 * (165.759)))) +
     2500*2.872/(0.354*(45.33 - 0.33*(48 - 0.0625 * (30.935)))) +
     2600*2.872/(0.354*(45.33 - 0.33*(48 - 0.0625 * (18.320)))))/4

#Calculate capacity
#10000 vehicles per hour
#10000/6 to get vehicles per 10 minutes
N=round(10000/6)

#Calculate TT
# there is 1 car every 0.4s
tt=600/0.4

#calculate operating cost per 10-minute
operating.cost = 102000000/365/24/6

##### Linear programming with expected demand values ----
library(lpSolve)

n=N; # Leg 1 seat availability
TT=tt; # Length of time horizon

arrivalprob=c(0.4, 0.5);

emission=c(650,325);

totalarrivalprob=sum(arrivalprob);
noarrivalprob=1-totalarrivalprob;

obj.fun <- c(emission.per.car, emission.per.car/2);
expdemands <- arrivalprob*1500;
AllocateLessThanDLcap<-c(1,1) #capacity
cost <- c(3.5, 2.5)
AllocateLessThanDemand<-diag(1, 2, 2)
priortisex2<-c(-1,1)

constr <- rbind(AllocateLessThanDLcap,cost,
                AllocateLessThanDemand, priortisex2);

# Constraint directions:
constr.dir <- c("<=", ">",rep("<=", 2), ">");

# Constraint Right Hand Side
rhs <- c(N,operating.cost,expdemands, 1)

# Solving the LP:
optairline <- lp("min", obj.fun, constr, constr.dir, rhs, compute.sens=TRUE)

# Optimal Solution (Values for Decision Variables)
optairline$solution

#Optimal emission (Values for Decision Variables)
optairline$objval

