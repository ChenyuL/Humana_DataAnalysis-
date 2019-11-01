# install.packages("dplyr")
# install.packages("sqldf")
# install.packages("bnlearn")
#SECTION ONE####
#1. read document####
dataset<-read.csv("HMAHCC_COMP.csv")
head(dataset)
# summary(dataset)
library(sqldf)
library(dplyr)
#2. seperate dataset####
RX_Claim_Paid<-sqldf("SELECT * FROM dataset WHERE event_descr='RX Claim - Paid'")
Fully_Paid_Claim<-sqldf("SELECT * FROM dataset WHERE event_descr='Fully Paid Claim'")
#Inbound_Call_by_Mbr<-sqldf("SELECT * FROM dataset WHERE event_descr='Inbound Call by Mbr'")
#Inbound_Call_by_Other<-sqldf("SELECT * FROM dataset WHERE event_descr='Inbound Call by Other'")
#Inbound_Call_by_Prov<-sqldf("SELECT * FROM dataset WHERE event_descr='Inbound Call by Prov'")

#New_diagnosis_Top_5<-sqldf("SELECT * FROM dataset WHERE event_descr='New diagnosis - Top 5'")
Surgery<-sqldf("SELECT * FROM dataset WHERE event_descr='Surgery'")
#New_provider<-sqldf("SELECT * FROM dataset WHERE event_descr='New provider'")
New_diagnosis_CAD<-sqldf("SELECT * FROM dataset WHERE event_descr='New diagnosis - CAD'")
New_diagnosis_Diabetes<-sqldf("SELECT * FROM dataset WHERE event_descr='New diagnosis - Diabetes'")
New_diagnosis_Hypertension<-sqldf("SELECT * FROM dataset WHERE event_descr='New diagnosis - Hypertension'")
New_diagnosis_CPD<-sqldf("SELECT * FROM dataset WHERE event_descr='New diagnosis - CPD'")
New_diagnosis_CHF<-sqldf("SELECT * FROM dataset WHERE event_descr='New diagnosis - CHF'")
RX_Claim_Paid<-sqldf("SELECT * FROM dataset WHERE event_descr='RX Claim - Paid'")
RX_Claim_Rejected<-sqldf("SELECT * FROM dataset WHERE event_descr='RX Claim - Rejected'")
## RX_Claim_Rejected_id<-sqldf("select * from RX_Claim_Rejected inner join RX_Claim_New_Drug on RX_Claim_Rejected.id=RX_Claim_New_Drug.id" )
#count(RX_Claim_Rejected_id)
RX_Claim_New_Drug<-sqldf("SELECT * FROM dataset WHERE event_descr='RX Claim - New Drug'")
RX_Claim_First_Time_Mail_Order<-sqldf("SELECT * FROM dataset WHERE event_descr='RX Claim - First Time Mail Order'")
Event_descr<-sqldf("SELECT distinct(event_descr) FROM dataset")


#test_ID10776679641<-sqldf("select * from dataset where id='ID10776679641' and days=0")

# SElECT No Opoid record
#OPIOID_Paid_Record<-sqldf("select * from RX_Claim_Paid where MME is not null")
#NO_OPIOID<-sqldf("select * from Fully_Paid_Claim where id not in (select id from OPIOID_Paid_Record)")

# Opioid perscribed
#OPIOID_New_drug<-sqldf("select * from RX_Claim_New_Drug where MME is not null")
#NO_OPIOID<-sqldf("select * from Fully_Paid_Claim where id not in (select id from OPIOID_New_drug)")

#NO_patient_number<-count(NO_OPIOID%>%group_by(id))
#summary(NO_OPIOID)
#NO_patient_number
#on_hand<-sqldf("select * from RX_Claim_New_Drug where PAY_DAY_SUPPLY_CNT is not null")sqldf("select id,PAY_DAY_SUPPLY_CNT,Days from on_hand where Days!=0")
####3. First naive id ####
Naive_id<-dataset%>%group_by(id)%>%filter(Days==0)%>%filter(MME!=0)
First_Naive_records_id<-sqldf("select distinct(id) from RX_Claim_Paid where MME!=0 and Days=0") #13760
First_Naive_records<-sqldf("select * from dataset where id in First_Naive_records_id and Days>=0")
#not_opioid_supply<-sqldf("select id from Supply_Count_Total where id not in First_Naive_records_id") ##### test what data was deleted 240 patients : test_what<-sqldf("select * from dataset where id in not_opioid_supply and MME!=0")
# No_OPIOID event 
No_event<-sqldf("select * from dataset where id not in First_Naive_records_id") #240 unique id 

opioid_in_no_event<-sqldf("select * from RX_Claim_Paid
                          where id in (select id from No_event)
                          and Days>=0
                          and MME!=0")

# Minus days with Opioid claim------Yes/No 
Negative_days_id<-sqldf("select distinct(id) from RX_Claim_Paid where MME!=0 and Days<0")
#Matrix First_Naive
M_First_Naive<-cbind(id=(First_Naive_records_id),label=rep(0,length(First_Naive_records_id)))
#4. Surgery and Diagnosis data label####
#4.1 Surgery####
Surgery_before_First_Naive_id<- sqldf("select distinct(id) from Surgery where Days<=0 and id in (select id from First_Naive_records_id) " ) # 6037
Surgery_after_First_Naive_id<- sqldf("select distinct(id) from Surgery where Days>0 and id in (select id from First_Naive_records_id) " )# 6464
Surgery_both<-sqldf("select id from Surgery_before_First_Naive_id where id in (select id from Surgery_after_First_Naive_id)")#3355
Longer_Opioid_after_surgery_id_180<-sqldf("select id from Surgery_before_First_Naive_id where id in (select id from RX_Claim_Paid where MME!=0 and Days>180)")#4235
Longer_Opioid_after_surgery_both_id_180<-sqldf("select id from Surgery_both where id in (select id from RX_Claim_Paid where MME!=0 and Days>180)")#2576
DF_Surgery_Label<-sqldf("SELECT id, 
                        CASE 
                        WHEN id in (select id from Surgery_before_First_Naive_id) 
                        THEN Label=0
                        wHEN id not in (select id from Surgery_before_First_Naive_id)                             THEN Label=1
                        END AS Surgery_Label
                        FROM M_First_Naive")

##### 4.2 New diagnosis - CAD####
CAD_before_First_Naive_id<-sqldf("select distinct(id) from New_diagnosis_CAD where Days<=0 and id in (select id from First_Naive_records_id) " )# 1464 Opioid_after_CAD_diagnosed

CAD_after_First_Naive_id<-sqldf("select distinct(id) from New_diagnosis_CAD where Days>0 and id in (select id from First_Naive_records_id) " ) #1004

Longer_Opioid_after_CAD_diagnosed_id_180<-sqldf("select id from CAD_before_First_Naive_id where id in (select id from RX_Claim_Paid where MME!=0 and Days>180)")#1043
DF_CAD_Label<-sqldf("SELECT id, 
                    CASE 
                    WHEN id in (select id from CAD_before_First_Naive_id) 
                    THEN Label=0
                    wHEN id not in (select id from CAD_before_First_Naive_id)                             THEN Label=1
                    END AS CAD_Label
                    FROM M_First_Naive")

## CAD_after_Opioid_id_0<-sqldf("select id from CAD_after_First_Naive_id where id in (select id from RX_Claim_Paid where MME!=0 and Days>0)")#874
##CAD_Longer_Opioid_id_180<-sqldf("select id from CAD_after_First_Naive_id where id in (select id from RX_Claim_Paid where MME!=0 and Days>180)")#797
##
#####4.3 New diagnosis - CPD####
CPD_before_First_Naive_id<-sqldf("select distinct(id) from New_diagnosis_CPD where Days<=0 and id in (select id from First_Naive_records_id) " ) #2331
CPD_after_First_Naive_id<-sqldf("select distinct(id) from New_diagnosis_CPD where Days>0 and id in (select id from First_Naive_records_id) " )#1291
CPD_both<-sqldf("select id from CPD_before_First_Naive_id where id in (select id from CPD_after_First_Naive_id)")#0 
Longer_Opioid_after_CPD_diagnosed_id_180<-sqldf("select id from CPD_before_First_Naive_id where id in (select id from RX_Claim_Paid where MME!=0 and Days>180)")#1737
DF_CPD_Label<-sqldf("SELECT id, 
                    CASE 
                    WHEN id in (select id from CPD_before_First_Naive_id) 
                    THEN Label=0
                    wHEN id not in (select id from CPD_before_First_Naive_id)                             THEN Label=1
                    END AS CPD_Label
                    FROM M_First_Naive")

#####4.4 New diagnosis - Diabetes#### 
Diabetes_before_First_Naive_id<-sqldf("select distinct(id) from New_diagnosis_Diabetes where Days<=0 and id in (select id from First_Naive_records_id) " ) #1394
Diabetes_after_First_Naive_id<-sqldf("select distinct(id) from New_diagnosis_Diabetes where Days>0 and id in (select id from First_Naive_records_id) " )#743
Diabetes_both<-sqldf("select id from Diabetes_before_First_Naive_id where id in (select id from Diabetes_after_First_Naive_id)")#0 

Longer_Opioid_after_Diabetes_diagnosed_id_180<-sqldf("select id from Diabetes_before_First_Naive_id where id in (select id from RX_Claim_Paid where MME!=0 and Days>180)")#1023
DF_Diabetes_Label<-sqldf("SELECT id, 
                         CASE 
                         WHEN id in (select id from Diabetes_before_First_Naive_id) 
                         THEN Label=0
                         wHEN id not in (select id from Diabetes_before_First_Naive_id)                             THEN Label=1
                         END AS Diabetes_Label
                         FROM M_First_Naive")

##### 4.5 New diagnosis - Hypertension####
Hypertension_before_First_Naive_id<-sqldf("select distinct(id) from New_diagnosis_Hypertension where Days<=0 and id in (select id from First_Naive_records_id) " )#2216
Hypertension_after_First_Naive_id<-sqldf("select distinct(id) from New_diagnosis_Hypertension where Days>0 and id in (select id from First_Naive_records_id) " )#497
Longer_Opioid_after_Hypertension_diagnosed_id_180<-sqldf("select id from Hypertension_before_First_Naive_id where id in (select id from RX_Claim_Paid where MME!=0 and Days>180)")#1591
DF_Hypertension_Label<-sqldf("SELECT id, 
                             CASE 
                             WHEN id in (select id from Hypertension_before_First_Naive_id) 
                             THEN Label=0
                             wHEN id not in (select id from Hypertension_before_First_Naive_id)                             THEN Label=1
                             END AS Hypertension_Label
                             FROM M_First_Naive") # 2216

#### 4.6 New diagnosis - CHF####
CHF_before_First_Naive_id<-sqldf("select distinct(id) from New_diagnosis_CHF where Days<=0 and id in (select id from First_Naive_records_id) " )#1085
CHF_after_First_Naive_id<-sqldf("select distinct(id) from New_diagnosis_CHF where Days>0 and id in (select id from First_Naive_records_id) " )#1068
Longer_Opioid_after_CHF_diagnosed_id_180<-sqldf("select id from CHF_before_First_Naive_id where id in (select id from RX_Claim_Paid where MME!=0 and Days>180)")#796
M_First_Naive<-cbind(id=(First_Naive_records_id),label=rep(0,length(First_Naive_records_id)))
####### Label Wrong? ~~~
DF_CHF_Label<-sqldf("SELECT id, 
                    CASE 
                    WHEN id in (select id from CHF_before_First_Naive_id) 
                    THEN Label=0
                    wHEN id not in (select id from CHF_before_First_Naive_id)                             THEN Label=1
                    END AS CHF_Label
                    FROM M_First_Naive")
#5. Opioid Events label ####
##5.1 Supply count####
####5.1.1 Supply count total ####

Supply_Count_Total<-sqldf("select id,sum(PAY_DAY_SUPPLY_CNT) as Supply_Count_Total from First_Naive_records group by id order by id")
### 5.1.2. Day 0 Supply count#### 
Day0_Supply_count<-sqldf("select id,sum(PAY_DAY_SUPPLY_CNT) as Sum_Day0_Supply_count from RX_Claim_Paid where Days=0 and MME!=0 group by id order by id")
# Day 0 PAYABLE_QTY
Day0_Payable_qty<-sqldf("select id,sum(PAYABLE_QTY) as Sum_Day0_Payable_qty from RX_Claim_Paid where Days=0 and MME!=0 group by id order by id")
##5.2 Qualify event####
###5.2.1 Qualify event total####
Event_number_Total<-sqldf("select id,count(event_descr) as Event_Number_Total from dataset where id in 
                          (select id from First_Naive_records_id)
                          group by id order by id")
###5.2.1 Qualify event after first naive####
Event_number_after_firstnaive<-sqldf("select id,count(event_descr) as Event_Number_after_firstnaive from First_Naive_records group by id order by id")


##5.3 MME related####
##5.3.1 Day 0 MME ####
#MME_picture_day0<-sqldf("select id,event_descr,PAYABLE_QTY,PAY_DAY_SUPPLY_CNT,MME,Days,DRUG_TYPE,Specialty,Specialty2,Specialty3 from First_Naive_records where MME!=0 and Days=0 and event_descr='RX Claim - Paid'")
#duplicate_MME_id<-sqldf("select id,MME,Days from MME_picture_day0 group by id having count(id)>1 ")
#duplicate_MME_overview<-sqldf("select * from First_Naive_records where id in (select id from duplicate_MME_id)")
Day0_MME<-sqldf("select id,sum(MME) as Sum_Day0_MME from RX_Claim_Paid where Days=0 and MME!=0 group by id order by id")

# Average_MME<-sqldf("select id,AVG(MME) as Average_MME from First_Naive_records where Days>=0 and Days<=180 group by id order by id") # 



##5.4 First_Naive_Specialty####
First_Naive_Specialty<-sqldf("select id, Specialty,Specialty2,Specialty3 from First_Naive_records where Days=0 and MME!=0 and event_descr='RX Claim - Paid' group by id order by id")
#test_ID10180352120<-sqldf("select * from First_Naive_records where Days=0 and MME!=0 and event_descr='RX Claim - Paid'and id='ID10180352120'")
Specialty<-sqldf("select distinct(Specialty) from First_Naive_Specialty group by specialty order by specialty" )
Specialty<-cbind(Specialty,Value=(order(Specialty)))
Specialty_Value<-as.data.frame(Specialty)
Specialty_Number<-sqldf("SELECT id, Value
                        FROM First_Naive_Specialty
                        LEFT JOIN  Specialty_Value
                        ON First_Naive_Specialty.Specialty=Specialty_Value.Specialty
                        ORDER by id")

##5.5 Drug Type ####
Day0_Drug_Type_text<-sqldf("select id,group_concat(Drug_Type) As DT
                           FROM First_Naive_records
                           WHERE MME!=0 
                           AND Days=0
                           group by id
                           order by id")
Day0_Drug_Type<-sqldf(" SELECT id,
                      CASE 
                      WHEN DT='IR'   THEN 0
                      WHEN DT= 'ER' THEN 1
                      ELSE 2
                      END  AS Drug_Type
                      FROM Day0_Drug_Type_text")


#6. Add Label ####
Train_dataset<-cbind(id=(First_Naive_records_id),
                     Day0_Supply_count=(Day0_Supply_count$Sum_Day0_Supply_count),
                     Supply_Count_Total=(Supply_Count_Total$Supply_Count_Total),
                     Event_Number_Toal=(Event_number_Total$Event_Number_Total),
                     Event_Number_After_Firstnaive=(Event_number_after_firstnaive$Event_Number_after_firstnaive),
                     Day0_Payable_qty=(Day0_Payable_qty$Sum_Day0_Supply_count),
                     Day0_MME=(Day0_MME$Sum_Day0_MME),
                     Day0_Drug_Type=(Day0_Drug_Type$Drug_Type),
                     Specialty=(Specialty_Number$Value),
                     CAD_Label=(DF_CAD_Label$CAD_Label),
                     CPD_Label=(DF_CPD_Label$CPD_Label),
                     Diabetes_Label=(DF_Diabetes_Label$Diabetes_Label),
                     Hypertension_Label=(DF_Hypertension_Label$Hypertension_Label),
                     CHF_Label=(DF_CHF_Label$CHF_Label)
)
write.csv(Train_dataset,file="Train_dataset.csv")
# diagnosis_label<-cbind(id=(First_Naive_records_id),Surgery_Label=(Surgery_Label$Surgery_Label),CAD_Label=(CAD_Label$CAD_Label),CPD_Label=(CPD_Label$CPD_Label),Diabetes_Label=(Diabetes_Label$Diabetes_Label),Hypertension_Label=(Hypertension_Label$Hypertension_Label),CHF_Label=(CHF_Label$CHF_Label))

# check join id is correct 
#diagnosis_label_check<-cbind(id=(First_Naive_records_id),Surgery_Label=(Surgery_Label$id),CAD_Label=(CAD_Label$id),CPD_Label=(CPD_Label$id),Diabetes_Label=(Diabetes_Label$id),Hypertension_Label=(Hypertension_Label$id),CHF_Label=(CHF_Label$id))
#
#SECTION TWO ####
##1. read document####
dataset_test<-read.csv("HMAHCC_HOLDOUT.csv")
#head(dataset_test)
library(sqldf)
library(dplyr)
#2. seperate dataset####
t_RX_Claim_Paid<-sqldf("SELECT * FROM dataset_test WHERE event_descr='RX Claim - Paid'")
t_Fully_Paid_Claim<-sqldf("SELECT * FROM dataset_test WHERE event_descr='Fully Paid Claim'")
t_Surgery<-sqldf("SELECT * FROM dataset_test WHERE event_descr='Surgery'")
t_New_provider<-sqldf("SELECT * FROM dataset_test WHERE event_descr='New provider'")
t_New_diagnosis_CAD<-sqldf("SELECT * FROM dataset_test WHERE event_descr='New diagnosis - CAD'")
t_New_diagnosis_Diabetes<-sqldf("SELECT * FROM dataset_test WHERE event_descr='New diagnosis - Diabetes'")
t_New_diagnosis_Hypertension<-sqldf("SELECT * FROM dataset_test WHERE event_descr='New diagnosis - Hypertension'")
t_New_diagnosis_CPD<-sqldf("SELECT * FROM dataset_test WHERE event_descr='New diagnosis - CPD'")
t_New_diagnosis_CHF<-sqldf("SELECT * FROM dataset_test WHERE event_descr='New diagnosis - CHF'")
t_RX_Claim_Paid<-sqldf("SELECT * FROM dataset_test WHERE event_descr='RX Claim - Paid'")

####3. First naive id ####
t_Naive_id<-dataset_test%>%group_by(ID)%>%filter(days==0)%>%filter(MME!=0)
t_Naive_New_drug_id<-t_RX_Claim_New_Drug%>%group_by(id)%>%filter(Days==0)%>%filter(MME!=0)
t_First_Naive_records_id<-sqldf("select distinct(id) from t_RX_Claim_Paid where MME!=0 and Days=0") 
# No_OPIOID event 
t_No_event<-sqldf("select * from dataset_test where id not in t_First_Naive_records_id") #
#Matrix First_Naive
t_M_First_Naive<-cbind(id=(t_First_Naive_records_id),label=rep(0,length(t_First_Naive_records_id)))
#4. Surgery and Diagnosis data label####
#4.1 Surgery####
t_Surgery_before_First_Naive_id<- sqldf("select distinct(id) from t_Surgery where Days<=0 and id in (select id from t_First_Naive_records_id) " ) # 2889/7547
t_Surgery_after_First_Naive_id<- sqldf("select distinct(id) from t_Surgery where Days>0 and id in (select id from t_First_Naive_records_id) " )
t_Surgery_both<-sqldf("select id from t_Surgery_before_First_Naive_id where id in (select id from t_Surgery_after_First_Naive_id)")#3355
t_Longer_Opioid_after_surgery_id_180<-sqldf("select id from t_Surgery_before_First_Naive_id where id in (select id from t_RX_Claim_Paid where MME!=0 and Days>180)")#4235
t_Longer_Opioid_after_surgery_both_id_180<-sqldf("select id from Surgery_both where id in (select id from RX_Claim_Paid where MME!=0 and Days>180)")#2576
t_DF_Surgery_Label<-sqldf("SELECT id, 
                          CASE 
                          WHEN id in (select id from t_Surgery_before_First_Naive_id) 
                          THEN Label=0
                          wHEN id not in (select id from t_Surgery_before_First_Naive_id)                             THEN Label=1
                          END AS t_Surgery_Label
                          FROM t_M_First_Naive")

##### 4.2 New diagnosis - CAD####
t_CAD_before_First_Naive_id<-sqldf("select distinct(id) from t_New_diagnosis_CAD where Days<=0 and id in (select id from t_First_Naive_records_id) " )# 1464 Opioid_after_CAD_diagnosed

t_CAD_after_First_Naive_id<-sqldf("select distinct(id) from t_New_diagnosis_CAD where Days>0 and id in (select id from t_First_Naive_records_id) " ) #1004

t_Longer_Opioid_after_CAD_diagnosed_id_180<-sqldf("select id from t_CAD_before_First_Naive_id where id in (select id from t_RX_Claim_Paid where MME!=0 and Days>180)")#1043
t_DF_CAD_Label<-sqldf("SELECT id, 
                      CASE 
                      WHEN id in (select id from t_CAD_before_First_Naive_id) 
                      THEN Label=0
                      wHEN id not in (select id from t_CAD_before_First_Naive_id)                             THEN Label=1
                      END AS t_CAD_Label
                      FROM t_M_First_Naive")

#####4.3 New diagnosis - CPD####
t_CPD_before_First_Naive_id<-sqldf("select distinct(id) from t_New_diagnosis_CPD where Days<=0 and id in (select id from t_First_Naive_records_id) " ) #2331
t_CPD_after_First_Naive_id<-sqldf("select distinct(id) from t_New_diagnosis_CPD where Days>0 and id in (select id from t_First_Naive_records_id) " )#1291
t_CPD_both<-sqldf("select id from t_CPD_before_First_Naive_id where id in (select id from t_CPD_after_First_Naive_id)")#0 
t_Longer_Opioid_after_CPD_diagnosed_id_180<-sqldf("select id from CPD_before_First_Naive_id where id in (select id from t_RX_Claim_Paid where MME!=0 and Days>180)")#1737
t_DF_CPD_Label<-sqldf("SELECT id, 
                      CASE 
                      WHEN id in (select id from t_CPD_before_First_Naive_id) 
                      THEN Label=0
                      wHEN id not in (select id from t_CPD_before_First_Naive_id)                             THEN Label=1
                      END AS t_CPD_Label
                      FROM t_M_First_Naive")

#####4.4 New diagnosis - Diabetes#### 
t_Diabetes_before_First_Naive_id<-sqldf("select distinct(id) from t_New_diagnosis_Diabetes where Days<=0 and id in (select id from t_First_Naive_records_id) " ) #1394
t_Diabetes_after_First_Naive_id<-sqldf("select distinct(id) from t_New_diagnosis_Diabetes where Days>0 and id in (select id from t_First_Naive_records_id) " )#743
t_Diabetes_both<-sqldf("select id from t_Diabetes_before_First_Naive_id where id in (select id from t_Diabetes_after_First_Naive_id)")#0 

t_Longer_Opioid_after_Diabetes_diagnosed_id_180<-sqldf("select id from t_Diabetes_before_First_Naive_id where id in (select id from t_RX_Claim_Paid where MME!=0 and Days>180)")#1023
t_DF_Diabetes_Label<-sqldf("SELECT id, 
                           CASE 
                           WHEN id in (select id from t_Diabetes_before_First_Naive_id) 
                           THEN Label=0
                           wHEN id not in (select id from t_Diabetes_before_First_Naive_id)                             THEN Label=1
                           END AS t_Diabetes_Label
                           FROM t_M_First_Naive")

##### 4.5 New diagnosis - Hypertension####
t_Hypertension_before_First_Naive_id<-sqldf("select distinct(id) from t_New_diagnosis_Hypertension where Days<=0 and id in (select id from t_First_Naive_records_id) " )#2216
t_Hypertension_after_First_Naive_id<-sqldf("select distinct(id) from t_New_diagnosis_Hypertension where Days>0 and id in (select id from t_First_Naive_records_id) " )#497
t_Longer_Opioid_after_Hypertension_diagnosed_id_180<-sqldf("select id from t_Hypertension_before_First_Naive_id where id in (select id from t_RX_Claim_Paid where MME!=0 and Days>180)")#1591
t_DF_Hypertension_Label<-sqldf("SELECT id, 
                               CASE 
                               WHEN id in (select id from t_Hypertension_before_First_Naive_id) 
                               THEN Label=0
                               wHEN id not in (select id from t_Hypertension_before_First_Naive_id)                             THEN Label=1
                               END AS t_Hypertension_Label
                               FROM t_M_First_Naive") # 2216

#### 4.6 New diagnosis - CHF####
t_CHF_before_First_Naive_id<-sqldf("select distinct(id) from t_New_diagnosis_CHF where Days<=0 and id in (select id from t_First_Naive_records_id) " )
t_CHF_after_First_Naive_id<-sqldf("select distinct(id) from t_New_diagnosis_CHF where Days>0 and id in (select id from t_First_Naive_records_id) " )
t_Longer_Opioid_after_CHF_diagnosed_id_180<-sqldf("select id from t_CHF_before_First_Naive_id where id in (select id from t_RX_Claim_Paid where MME!=0 and Days>180)")

t_DF_CHF_Label<-sqldf("SELECT id, 
                      CASE 
                      WHEN id in (select id from t_CHF_before_First_Naive_id) 
                      THEN Label=0
                      wHEN id not in (select id from t_CHF_before_First_Naive_id)                             THEN Label=1
                      END AS t_CHF_Label
                      FROM t_M_First_Naive")
#5. Opioid Events label ####
##5.1 Supply count####
####5.1.1 Supply count total ####
t_First_Naive_records<-sqldf("select * from dataset_test where id in t_First_Naive_records_id and Days>=0")
t_Supply_Count_Total<-sqldf("select id,sum(PAY_DAY_SUPPLY_CNT) as Supply_Count_Total from t_First_Naive_records group by id order by id")
### 5.1.2. Day 0 Supply count#### 
t_Day0_Supply_count<-sqldf("select id,sum(PAY_DAY_SUPPLY_CNT) as Sum_Day0_Supply_count from t_RX_Claim_Paid where Days=0 and MME!=0 group by id order by id")
# Day 0 PAYABLE_QTY
t_Day0_Payable_qty<-sqldf("select id,sum(PAYABLE_QTY) as Sum_Day0_Supply_count from t_RX_Claim_Paid where Days=0 and MME!=0 group by id order by id")
##5.2 Qualify event####
###5.2.1 Qualify event total####
t_Event_number_Total<-sqldf("select id,count(event_descr) as Event_Number_Total from dataset_test where id in 
                            (select id from t_First_Naive_records_id)
                            group by id order by id")
###5.2.1 Qualify event after first naive####
t_Event_number_after_firstnaive<-sqldf("select id,count(event_descr) as Event_Number_after_firstnaive from t_First_Naive_records group by id order by id")


##5.3 MME related####
##5.3.1 Day 0 MME ####
t_Day0_MME<-sqldf("select id,sum(MME) as Sum_Day0_MME from t_RX_Claim_Paid where Days=0 and MME!=0 group by id order by id")
##5.4 First_Naive_Specialty####
t_First_Naive_Specialty<-sqldf("select id, Specialty,Specialty2,Specialty3 from t_First_Naive_records where Days=0 and MME!=0 and event_descr='RX Claim - Paid' group by id order by id")
t_Specialty<-sqldf("select distinct(Specialty) from t_First_Naive_Specialty group by specialty order by specialty" )
t_Specialty<-cbind(t_Specialty,Value=(order(t_Specialty)))
t_Specialty_Value<-as.data.frame(t_Specialty)
t_Specialty_Number<-sqldf("SELECT id, Value
                          FROM t_First_Naive_Specialty
                          LEFT JOIN  t_Specialty_Value
                          ON t_First_Naive_Specialty.SPECIALTY=t_Specialty_Value.SPECIALTY
                          ORDER by id")
##5.5 Drug Type ####
t_Day0_Drug_Type_text<-sqldf("select id,group_concat(Drug_Type) As DT
                             FROM t_First_Naive_records
                             WHERE MME!=0 
                             AND Days=0
                             group by id
                             order by id")
t_Day0_Drug_Type<-sqldf(" SELECT id,
                        CASE 
                        WHEN DT='IR'   THEN 0
                        WHEN DT= 'ER' THEN 1
                        ELSE 2
                        END  AS Drug_Type
                        FROM t_Day0_Drug_Type_text")



#6. Add Label ####
Test_dataset<-cbind(id=(t_First_Naive_records_id),
                    Day0_Supply_count=(t_Day0_Supply_count$Sum_Day0_Supply_count),
                    Supply_Count_Total=(t_Supply_Count_Total$Supply_Count_Total),
                    Event_Number_Toal=(t_Event_number_Total$Event_Number_Total),
                    Event_Number_After_Firstnaive=(t_Event_number_after_firstnaive$Event_Number_after_firstnaive),
                    Day0_Payable_qty=(t_Day0_Payable_qty$Sum_Day0_Supply_count),
                    Day0_MME=(t_Day0_MME$Sum_Day0_MME),
                    Day0_Drug_Type=(t_Day0_Drug_Type$Drug_Type),
                    Specialty=(t_Specialty_Number$Value),
                    Surgery_Label=(t_DF_Surgery_Label$t_Surgery_Label),
                    CAD_Label=(t_DF_CAD_Label$t_CAD_Label),
                    CPD_Label=(t_DF_CPD_Label$t_CPD_Label),
                    Diabetes_Label=(t_DF_Diabetes_Label$t_Diabetes_Label),
                    Hypertension_Label=(t_DF_Hypertension_Label$t_Hypertension_Label),
                    CHF_Label=(t_DF_CHF_Label$t_CHF_Label)
)
write.csv(Test_dataset,file="HOLDOUT_test.csv")

#SECTION THREE####
#1.lable LTOT####
LTOT_180<-sqldf("select id,sum(PAY_DAY_SUPPLY_CNT) as SUPPLY_CNT
                from First_Naive_records
                where days>=0 and days<=180
                group by id 
                order by id"
)

LTOT_162<-LTOT_180%>%filter(SUPPLY_CNT>=162)
# #LTOT_180_max<-sqldf("select id,Days,max(PAY_DAY_SUPPLY_CNT) as #SUPPLY_CNT                from RX_Claim_Paid
#                 where days>=0 and days<=180
#                 group by id,days
#                 order by id"
# )#
percentage<-sqldf("select count(id) from dataset where id in (select id from LTOT_162) ")
#

DF_LTOT_Label<-sqldf("SELECT id, 
                     CASE 
                     WHEN id in (select id from LTOT_162) 
                     THEN Label=0
                     wHEN id not in (select id from LTOT_162)                             THEN Label=1
                     END AS LTOT_Label
                     FROM M_First_Naive")
Train_dataset_LTOT<-cbind(Train_dataset,LTOT=(DF_LTOT_Label$LTOT_Label))
write.csv(Train_dataset_LTOT,file="Train_dataset_LTOT.csv")

# SECTION FOUR ####
# 1.Sampling population 1%####

# LTOT_1_id<-sqldf("select id from DF_LTOT_Label 
#                  WHERE LTOT_Label=1")
# LTOT_0_id<-sqldf("select id from DF_LTOT_Label 
#                  WHERE LTOT_Label=0"
Train_dataset_LTOT<-read.csv("Train_dataset_LTOT.csv")
library(dplyr)
library(sqldf)
library(bnlearn)
LTOT_1_id<-sqldf("select id from Train_dataset_LTOT where LTOT=1")
LTOT_0_id<-sqldf("select id from Train_dataset_LTOT where LTOT=0")
#Calculate sample size based on 1.04%
# N_LTOT_1=as.numeric(count(LTOT_1_id))
# N=ceiling(N_LTOT_1/0.0104)
# N_LTOT_0=N-N_LTOT_1
N=as.numeric(count(Train_dataset_LTOT))
N_LTOT_1=ceiling(N*0.0104)
N_LTOT_0=N-N_LTOT_1
set.seed(123)
LTOT_sampling_1<-sample(LTOT_1_id$id,size=N_LTOT_1,replace=TRUE)
LTOT_sampling_0<-sample(LTOT_0_id$id,size=N_LTOT_0,replace=TRUE)
LTOT_sampling_1_id<-as.data.frame(LTOT_sampling_1)
LTOT_sampling_0_id<-as.data.frame(LTOT_sampling_0)
DF_Train_dataset<-as.data.frame(Train_dataset_LTOT)
colnames(LTOT_sampling_0_id) <- c("id")
colnames(LTOT_sampling_1_id) <- c("id")
LTOT_1_data<-sqldf("select L.id,Day0_Supply_count,Supply_Count_Total,Event_Number_Toal,Event_Number_After_Firstnaive,Day0_Payable_qty,Day0_MME,Day0_Drug_Type,Specialty,CAD_Label,CPD_Label,Diabetes_Label,Hypertension_Label,CHF_Label,LTOT
                   FROM  LTOT_sampling_1_id L
                   LEFT Join DF_Train_dataset DF
                   ON DF.id=L.id")
write.csv(LTOT_1_data,file="LTOT_1_data.csv")
LTOT_0_data<-sqldf("select L.id,Day0_Supply_count,Supply_Count_Total,Event_Number_Toal,Event_Number_After_Firstnaive,Day0_Payable_qty,Day0_MME,Day0_Drug_Type,Specialty,CAD_Label,CPD_Label,Diabetes_Label,Hypertension_Label,CHF_Label,LTOT
                   FROM  LTOT_sampling_0_id L
                   LEFT Join DF_Train_dataset DF
                   ON DF.id=L.id")
write.csv(LTOT_0_data,file="LTOT_0_data.csv")


POP_dataset<-rbind(LTOT_0_data,LTOT_1_data)
write.csv(POP_dataset,file="POP_dataset.csv")

#2. sampling training dataset####
#80% training, 20%testing from POP_dataset
sample_POP<- sample(nrow(POP_dataset), 0.8*nrow(POP_dataset))
POP_dataset_training <- POP_dataset[sample_POP,]
POP_dataset_testing <- POP_dataset[-sample_POP,]
write.csv(POP_dataset_training,file="POP_training.csv")
write.csv(POP_dataset_testing,file="POP_testing.csv")
training=read.csv("POP_training.csv",header=TRUE)
testing=read.csv("POP_testing.csv",header=TRUE)

#3. LASSO regression?####

#cv.fit<-cv.glmnet(training[1:14],training[-1],family="binomial")

#4. Logistics Regression####
glm.fit=glm(LTOT~Day0_Payable_qty,data=training,family=binomial(link="logit"))
n<-length(testing)
R2<-1-exp((glm.fit$deviance-glm.fit$null.deviance)/n)
R2
cat("Cox-Snell R2=",R2,"\n")
p_testing=predict(glm.fit,testing)#
p_testing=exp(p_testing)/(1+exp(p_testing))#
testing$predict_value=1*(p_testing>0.5)
# retrieved=sum(training$predict_value)
# precision=sum(training$LTOT & training$predict_value)/retrieved
retrieved=sum(testing$predict_value)
precision=sum(testing$LTOT & testing$predict_value)/retrieved
roc1<-roc(LTOT~ p_testing,data = testing)
plot(roc1,print.auc=T, auc.polygon=T)


# see this model performance then test on HOLDOUt_test.csv
H_testing<-read.csv("HOLDOUT_Test.csv")
p=predict(glm.fit,H_testing)#
p=exp(p)/(1+exp(p))#

H_testing$predict_value=1*(p>0.5)

retrieved=sum(H_testing$predict_value)
precision=sum(H_testing$LTOT & testpredict_value)/retrieved
recall=sum(predict_value & true_value)/sum(true_value)


###SECTION FIVE #####
###1. LTOT lable in testing dataset####
t_LTOT_180<-sqldf("select id,sum(PAY_DAY_SUPPLY_CNT) as SUPPLY_CNT
                  from t_First_Naive_records
                  where days>=0 and days<=180
                  group by id 
                  order by id"
)

t_LTOT_162<-t_LTOT_180%>%filter(SUPPLY_CNT>=162)
# #LTOT_180_max<-sqldf("select id,Days,max(PAY_DAY_SUPPLY_CNT) as #SUPPLY_CNT                from RX_Claim_Paid
#                 where days>=0 and days<=180
#                 group by id,days
#                 order by id"
# )#
t_DF_LTOT_Label<-sqldf("SELECT id, 
                       CASE 
                       WHEN id in (select id from t_LTOT_162) 
                       THEN Label=0
                       wHEN id not in (select id from t_LTOT_162)                             THEN Label=1
                       END AS LTOT_Label
                       FROM t_M_First_Naive
                       ORDER BY id")
HOLDOUT_test<-read.csv("HOLDOUT_test.csv")
HOLDOUT_LTOT<-cbind(Test_dataset,LTOT=(t_DF_LTOT_Label$LTOT_Label))
write.csv(HOLDOUT_LTOT,file="HOLDOUT_LTOT.csv")
#2. manully validating_HOLDOUT_LTOT Label####
#compare HOLDOUT_LTOT with HOLDOUT_Test
HOLDOUT_LTOT<-read.csv("HOLDOUT_LTOT.csv")
n_2<-length(HOLDOUT_LTOT)
R2_2<-1-exp((glm.fit$deviance-glm.fit$null.deviance)/n_2)
R2_2
cat("Cox-Snell R2=",R2_2,"\n")
p_HOLDOUT=predict(glm.fit,HOLDOUT_LTOT)#
p_HOLDOUT=exp(p_HOLDOUT)/(1+exp(p_HOLDOUT))#
HOLDOUT_LTOT$predict_value=1*(p_HOLDOUT>0.5)
HOLDOUT_LTOT$score=p_HOLDOUT

retrieved_2=sum(HOLDOUT_LTOT$predict_value)
precision_2=sum(HOLDOUT_LTOT$LTOT & HOLDOUT_LTOT$predict_value)/retrieved_2
roc2<-roc(LTOT~ p_HOLDOUT,data = HOLDOUT_LTOT)
plot(roc2,print.auc=T, auc.polygon=T)
final_result<-sqldf("select id as ID, score as SCORE,predict_value as RANK 
                     from HOLDOUT_LTOT" )
write.csv(final_result,file="CaseCompetition_InformaticsStackAttack.csv")
anova(glm.fit,test ="Chisq")