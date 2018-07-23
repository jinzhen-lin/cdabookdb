library(icda)
library(devtools)
library(dplyr)


to_table <- function(data, formula=Freq ~ .) {
  xtabs(formula, data = data)
}

new_rep <- function(x, levels = unique(x), ...) {
  factor(rep(x, ...), levels = levels)
}

Yes <- "Yes"
No <- "No"
Y <- "Y"
N <- "N"
YesNo <- c(Yes, No)
YN <- c(Y, N)

# 是否相信来世的数据
afterlife1 <- data.frame(
  Gender = factor(rep(c("Females", "Males"), each = 2), levels = c("Females", "Males")),
  Belief = factor(rep(c("Yes", "No or Undecided"), times = 2), levels = c("Yes", "No or Undecided")),
  Freq = c(509, 116, 398, 104)
)

afterlife1 <- xtabs(Freq ~ Gender + Belief, data = afterlife1)  # 列联表版本
use_data(afterlife1, overwrite = TRUE)


# 阿司匹林数据
data("aspirin")
aspirin <- mutate(
  aspirin,
  MI = factor(MI, levels = YN),
  Group = factor(Group, levels = c("Placebo", "Aspirin"))
)
aspirin <- xtabs(Freq ~ Group + MI, data = aspirin)
use_data(aspirin, overwrite = TRUE)



# 吸烟状态与心肌梗死
smoking_mi <- data.frame(
  Smoker = factor(c("Yes", "No", "Yes", "No"), levels = YesNo),
  MI = factor(c("MI Cases", "MI Cases", "Controls", "Controls")),
  Freq = c(172, 90, 173, 346)
)
smoking_mi <- xtabs(Freq ~ Smoker + MI, data = smoking_mi)
use_data(smoking_mi, overwrite = TRUE)



# 党派与性别
gender_party <- data.frame(
  Gender = new_rep(c("Females","Males"), times = 3),
  Party = new_rep(c("Democrat", "Independent", "Republican"), each = 2),
  Freq = c(762, 484, 327, 239, 468, 477)
)
gender_party <- xtabs(Freq ~ Gender + Party, data = gender_party)
use_data(gender_party, overwrite = TRUE)



# 饮酒与胎儿畸形
data(malformation)
malformation <- xtabs(Freq ~ Alcohol + Malformation, data = malformation)
row.names(malformation) <- c("0", "<1", "1-2", "3-5", ">=6")
use_data(malformation, overwrite = TRUE)



# 死刑判决
data(deathpenalty, package = "icda")
deathpenalty1 <- deathpenalty %>%
  mutate(
    DeathPenalty = factor(DeathPenalty, levels = YesNo),
    Defendant = factor(Defendant, levels = c("White", "Black")),
    Victim = factor(Victim, levels = c("White", "Black"))
  ) %>%
  xtabs(Freq ~ Defendant + Victim + DeathPenalty, data = .)
use_data(deathpenalty1, overwrite = TRUE)
deathpenalty1.ftable <- ftable(deathpenalty1)
## use_data(deathpenalty1.ftable, overwrite = TRUE)


# 治疗方案
treatment1 <- data.frame(
  Clinic = new_rep(c("1", "2"), each = 2, times = 2),
  Treatment = new_rep(c("A", "B"), times = 4),
  Response = new_rep(c("Success", "Failure"), each = 4),
  Freq = c(18, 12, 2, 8, 12, 8, 8, 32)
)
treatment1 <- xtabs(
  Freq ~ Clinic + Treatment + Response,
  data = treatment1
)
use_data(treatment1, overwrite = TRUE)
treatment1.ftable <- ftable(treatment1)
## use_data(treatment1.ftable, overwrite = TRUE)


# 吸烟与肺癌
smoking_lungcancer <- data.frame(
  LungCancer = new_rep(c("Cases", "Controls"), each = 2),
  Smoking = new_rep(c("Yes", "No"), times = 2),
  Freq = c(688, 21, 650, 59)
)
smoking_lungcancer <- xtabs(Freq ~ Smoking + LungCancer, data = smoking_lungcancer)
use_data(smoking_lungcancer, overwrite = TRUE)



# 家庭收入与家庭幸福
data("happiness", package = "icda")
happiness1 <- to_table(happiness)
use_data(happiness1, overwrite = TRUE)



# 种族与政党认同
race_party <- data.frame(
  Race = new_rep(c("White","Black"), times = 3),
  Party = new_rep(c("Democrat", "Independent", "Republican"), each = 2),
  Freq = c(871, 302, 444, 80, 873, 43)
)
race_party <- xtabs(Freq ~ Race + Party, data = race_party)
use_data(race_party, overwrite = TRUE)



# 青少年犯罪
teenager_crime <- data.frame(
  Gender = new_rep(c("Men", "Women"), each = 3),
  Factor = new_rep(c("A", "B", "C"), times = 2),
  Freq = c(60, 81, 75, 75, 87, 86)
)
teenager_crime <- xtabs(Freq ~ Gender + Factor, data = teenager_crime)
use_data(teenager_crime, overwrite = TRUE)



# 精神病人药物治疗
psych_diag_drugs <- get(data(psychdiagdrugs)) %>%
  mutate(
    Drugs = factor(Drugs, YN),
    Diagnosis = factor(Diagnosis,
                       c("Schizophrenia", "AffectiveDisorder",
                         "Neurosis", "PersonalityDisorder", "SpecialSymptoms"))
  )

psych_diag_drugs <- xtabs(Freq ~ Diagnosis + Drugs, data = psych_diag_drugs)
use_data(psych_diag_drugs, overwrite = TRUE)



# 受教育程度与宗教信仰
religious_belief <- data.frame(
  Degree = new_rep(c("Less than high school", "High school or junior college", "Bachelor or graduate"), each = 3),
  Belief = new_rep(c("Fundamentalist", "Moderate", "Liberal"), times = 3),
  Freq = c(178, 138, 108, 570, 648, 442, 138, 252, 252)
)
religious_belief <- xtabs(Freq ~ Degree + Belief, data = religious_belief)
use_data(religious_belief, overwrite = TRUE)




# 家庭收入与教育期望
edu_aspiration <- data.frame(
  Aspiration = new_rep(c("Some high school", "High school hraduate", "Some college", "College graduate"), times = 3),
  Income= new_rep(c("Low", "Middle", "High"), each = 4),
  Freq = c(9, 44, 13, 10, 11, 52, 23, 22, 9, 41, 12, 27)
)
edu_aspiration <- xtabs(Freq ~ Aspiration + Income, data = edu_aspiration)
use_data(edu_aspiration, overwrite = TRUE)



# 喉癌治疗
larynx_cancer <- data.frame(
  Treatment = new_rep(c("Surgery", "Radiation therapy"), each = 2),
  Result = new_rep(c("Cancer Controlled", "Cancer Not Controlled"), times = 2),
  Freq = c(21, 2, 15, 3)
)
larynx_cancer <- xtabs(Freq ~ Treatment + Result, data = larynx_cancer)
use_data(larynx_cancer, overwrite = TRUE)



# 死刑案例2
deathpenalty2 <- data.frame(
  Defendant = new_rep(c("White", "Black"), each = 2, times = 2),
  Victim = new_rep(c("White", "Black"), times = 4),
  DeathPenalty = new_rep(c("Yes", "No"), each = 4),
  Freq = c(19, 0, 11, 6, 132, 9, 52, 97)
)
deathpenalty2 <- xtabs(Freq ~ Defendant + Victim + DeathPenalty, data =  deathpenalty2)
use_data(deathpenalty2, overwrite = TRUE)



# 打鼾与心脏病
snoring_level <- c("Never","Occasional","Nearly every night","Every night")
snoring_heartdisease <- data.frame(
  Heartdisease = new_rep(c("Yes", "No"), each = 4),
  Snoring = new_rep(snoring_level, times = 2),
  Freq = c(24, 35, 21, 30, 1355, 603, 192, 224)
)
snoring_heartdisease <- xtabs(Freq ~ Snoring + Heartdisease, data = snoring_heartdisease)
use_data(snoring_heartdisease, overwrite = TRUE)



# 母鲎数据
data("horseshoecrabs")
use_data(horseshoecrabs, overwrite = TRUE)



# 英国火车碰撞事故
data("traincollisions")
use_data(traincollisions, overwrite = TRUE)




# 收入与是否拥有旅行信用卡
creditcard<- read.csv(text = "
  Income,No.Cases,Creditcards
  24,1,0
  34,7,1
  48,1,0
  70,5,3
  27,1,0
  35,1,1
  49,1,0
  79,1,0
  28,5,2
  38,3,1
  50,10,2
  80,1,0
  29,3,0
  39,2,0
  52,1,0
  84,1,0
  30,9,1
  40,5,0
  59,1,0
  94,1,0
  31,5,1
  41,2,0
  60,5,2
  120,6,6
  32,8,0
  42,2,0
  65,6,6
  130,1,1
  33,1,0
  45,1,1
  68,3,3"
)
creditcard <- arrange(creditcard, Income)
use_data(creditcard, overwrite = TRUE)



# 癌症缓解
cancer_remission <- read.csv(text = "
  LI,No.Cases,No.Remission
  8,2,0
  18,1,1
  28,1,1
  10,2,0
  20,3,2
  32,1,0
  12,3,0
  22,2,1
  34,1,1
  14,3,0
  24,1,0
  38,3,2
  16,3,0
  26,1,1"
)
cancer_remission <- arrange(cancer_remission, LI)
use_data(cancer_remission, overwrite = TRUE)



# 晶片瑕疵
chip_imperfection <- data.frame(
 Imperfections = c(8, 7, 6, 6, 3, 4, 7, 2, 3, 4, 9, 9, 8, 14, 8, 13, 11, 5, 7, 6),
 Treatment = new_rep(c("A", "B"), each = 10),
 z = rep(0:1, each = 5, times = 2)
)
use_data(chip_imperfection, overwrite = TRUE)



# 足球联赛一个赛季观赛人数与被捕人数
football_arrest <- read.csv(text = '
  Team,Attendance,Arrests
  "Aston Villa",404,308
  "Shrewsbury",108,68
  "Bradford City",286,197
  "Swindon Town",210,67
  "Leeds United",443,184
  "Sheffield Utd",224,60
  "Bournemouth",169,149
  "Stoke City",211,57
  "West Brom",222,132
  "Barnsley",168,55
  "Hudderfield",150,126
  "Millwall",185,44
  "Middlesbro",321,110
  "Hull City",158,38
  "Birmingham",189,101
  "Manchester City",429,35
  "Ipswich Town",258,99
  "Plymouth",226,29
  "Leicester City",223,81
  "Reading",150,20
  "Blackburn",211,79
  "Oldham",148,19
  "Crystal Palace",215,78'
)
use_data(football_arrest, overwrite = TRUE)




# 吸烟与冠心病死亡
smoking_cd <- data.frame(
  Age = new_rep(c("35-44", "45-54", "55-64", "65-74", "75-84"), times = 4),
  Item = new_rep(c("Person-Years", "Coronary Deaths"), each = 10),
  Smoking = new_rep(c("Nonsmokers", "Smokers"), each = 5, times = 2),
  Freq = c(
    18793, 10673, 5710, 2585, 1462,
    52407, 43248, 28612, 12663, 5317,
    2, 12, 28, 28, 31,
    32, 104, 206, 186, 102
  )
)
smoking_cd <- xtabs(Freq ~ Age + Smoking + Item, data = smoking_cd)
use_data(smoking_cd, overwrite = TRUE)



# AZT的使用与AIDS
data("AZT", package = "icda")
AZT <- AZT %>% mutate(
  Race = factor(Race, c("White", "Black")),
  AZTUse = factor(AZTUse, YesNo),
  Symptoms = factor(Symptoms, YesNo)
)
AZT <- xtabs(Freq ~ Race + AZTUse + Symptoms, data = AZT)
use_data(AZT, overwrite = TRUE)



# 飞行温度与是否受热遇险
temperature_distress <- read.csv(text = "
  Ft,Temperature,TD
  1,66,0
  13,67,0
  2,70,1
  14,53,1
  3,69,0
  15,67,0
  4,68,0
  16,75,0
  5,67,0
  17,70,0
  6,72,0
  18,81,0
  7,73,0
  19,76,0
  8,70,0
  20,79,0
  9,57,1
  21,75,1
  10,63,1
  22,76,0
  11,70,1
  23,58,1
  12,78,0"
)
temperature_distress <- arrange(temperature_distress, Ft)
use_data(temperature_distress, overwrite = TRUE)



# 是否出现驼背与以月计算的年龄
kyphosis_age <- data.frame(
  Kyphosis = factor(c(rep(Yes, 18), rep(No, 22)), YesNo),
  Age = c(
    12, 15, 42, 52, 59, 73, 82, 91, 96, 105, 114, 120, 121, 128,
    130, 139, 139, 157, 1, 1, 2, 8, 11, 18, 22, 31, 37, 61, 72,
    81, 97, 112, 118, 127, 131, 140, 151, 159, 177, 206
  )
)
use_data(kyphosis_age, overwrite = TRUE)




# 绩效工资与种族
merit_pay_race <- data.frame(
  District = new_rep(c("NC", "NE", "NW", "SE", "SW"), each = 4),
  MeritPay = new_rep(c("Yes", "No"), times = 10),
  Race = new_rep(c("Black", "White"), each = 2, times = 5),
  Freq = c(24, 9, 47, 12, 10, 3, 45, 8, 5, 4, 57, 9, 16, 7, 54, 10, 7, 4, 59, 12)
)
merit_pay_race <- xtabs(Freq ~ District + Race + MeritPay, data = merit_pay_race)
use_data(merit_pay_race, overwrite = TRUE)



# Meyers-Briggs人格测验1
data("MBdrink", package = "icda")
MBtest1 <- rename(MBdrink, Freq = Count)
MBtest1 <- xtabs(Freq ~ TF + JP + EI + SN + Drink, data = MBtest1)
use_data(MBtest1, overwrite = TRUE)



# Meyers-Briggs人格测验2
MBtest2 <- data.frame(
  TF = new_rep(c("T", "F"), each = 16),
  JP = new_rep(c("J", "P"), each = 8, times = 2),
  EI = new_rep(c("E", "I"), each = 4, times = 4),
  SN = new_rep(c("S", "N"), each = 2, times = 8),
  Smoking = new_rep(YesNo, times = 16),
  Freq = c(
    13, 64, 6, 17, 32, 108, 4, 9,
    11, 31, 4, 14, 9, 43, 9, 26,
    16, 89, 6, 25, 34, 104, 4, 27,
    19, 60, 23, 57, 29, 76, 22, 57
  )
)
MBtest2 <- xtabs(Freq ~ ., data = MBtest2)
use_data(MBtest2, overwrite = TRUE)



# 治疗方案
treatment2 <- data.frame(
  Response = new_rep(c("Success", "Failure"), times = 16),
  Center = new_rep(as.character(1:8), each = 4),
  Treatment = new_rep(c("Drug", "Control"), each = 2, times = 8),
  Freq = c(11, 25, 10, 27, 16, 4, 22, 10, 14, 5, 7, 12, 2, 14, 1, 16, 6, 11, 0, 12, 1, 10, 0, 10, 1, 4, 1, 8, 4, 2, 6, 1)
)
treatment2 <- to_table(Freq ~ Center + Treatment + Response, data = treatment2)
use_data(treatment2, overwrite = TRUE)



# 喉咙痛
data("throat")
use_data(throat, overwrite = TRUE)



# 青少年与性行为
teen_sex <- data.frame(
  Race = new_rep(c("White", "Black"), each = 4),
  Gender = new_rep(c("Male", "Female"), each = 2, times = 2),
  Intercourse = new_rep(c("Yes", "No"), times = 4),
  Freq = c(43, 134, 26, 149, 29, 23, 22, 36)
)
teen_sex <- xtabs(Freq ~ Race + Gender + Intercourse, data = teen_sex)
use_data(teen_sex, overwrite = TRUE)




# 新生运动员毕业比例
athlete_graduate <- data.frame(
  Group = new_rep(c("White female", "White male", "Black female", "Black male")),
  SampleSize = c(796, 1625, 143, 660),
  Graduates = c(498, 878, 54, 197)
)
use_data(athlete_graduate, overwrite = TRUE)



# 高中毕业生的酒、香烟、大麻的使用情况
data("marijuana")
use_data(marijuana, overwrite = TRUE)



# 控制蛋白质与死亡率
albumin <- read.csv("albumin.csv")
use_data(albumin, overwrite = TRUE)



# 工作满意度与种族、性别、年龄、工作区域的关系的调查
job_satisfaction1 <- data.frame(
  Race = new_rep(c("White", "Other"), each = 6, times = 14),
  Age = new_rep(c("<35", "35-44", ">44"), each = 2, times = 28),
  Region = new_rep(c("Northeast", "Mid-Atlantic", "Southern", "Midwest", "Northwest", "Southwest", "Pacific"), each = 24),
  Satisfied = new_rep(c("Satisfied", "Not satisfied"), each = 12, times = 7),
  Gender = new_rep(c("Male", "Female"), times = 84),
  Freq = c(
    288, 60, 224, 35, 337, 70, 38, 19, 32, 22, 21, 15,
    177, 57, 166, 19, 172, 30, 33, 35, 11, 20, 8, 10,
    90, 19, 96, 12, 124, 17, 18, 13, 7, 0, 9, 1,
    45, 12, 42, 5, 39, 2, 6, 7, 2, 3, 2, 1,
    226, 88, 189, 44, 156, 70, 45, 47, 18, 13, 11, 9,
    128, 57, 117, 34, 73, 25, 31, 35, 3, 7, 2, 2,
    285, 110, 225, 53, 324, 60, 40, 66, 19, 25, 22, 11,
    179, 93, 141, 24, 140, 47, 25, 56, 11, 19, 2, 12,
    270, 176, 215, 80, 269, 110, 36, 25, 9, 11, 16, 4,
    180, 151, 108, 40, 136, 40, 20, 16, 7, 5, 3, 5,
    252, 97, 162, 47, 199, 62, 69, 45, 14, 8, 14, 2,
    126, 61, 72, 27, 93, 24, 27, 36, 7, 4, 5, 0,
    119, 62, 66, 20, 67, 25, 45, 22, 15, 10, 8, 6,
    58, 33, 20, 10, 21, 10, 16, 15, 10, 8, 6, 2
  )
)
job_satisfaction1 <- xtabs(Freq ~ ., data = job_satisfaction1)
use_data(job_satisfaction1, overwrite = TRUE)



# 佛罗里达大学的研究生入学
data("UFAdmissions")
UFAdmissions <- xtabs(Freq ~ Dept + Gender + Decision, data = UFAdmissions)
use_data(UFAdmissions, overwrite = TRUE)




# 心脏病与血压的关系
blood_pressure <- data.frame(
  BloodPressure = c(1:6 * 10 + 101.5, 176.5, 191.5),
  SampleSize = c(156, 252, 284, 271, 139, 85, 99, 43),
  ObservedDisease = c(3, 17, 12, 16, 12, 8, 16, 8)
)
use_data(blood_pressure, overwrite = TRUE)




# 治疗结果
treatment3 <- data.frame(
  Center = rep(1:5, each = 4),
  Treatment = new_rep(c("Active drug", "Placebo"), times = 10),
  Response = new_rep(c("Success", "Success", "Failure", "Failure"), times = 5),
  Freq = c(0, 0, 5, 9, 1, 0, 12, 10, 0, 0, 7, 5, 6, 2, 3, 6, 5, 2, 9, 12)
)
treatment3 <- xtabs(Freq ~ Center + Treatment + Response, treatment3)
use_data(treatment3, overwrite = TRUE)




# 晋升能力与种族
promotion_race <- data.frame(
  Race = new_rep(c("Black", "White"), times = 6),
  Promotion = new_rep(YesNo, times = 3, each = 2),
  Month = new_rep(c("July", "August", "September"), each = 4),
  Freq = c(0, 4, 7, 16,
  0, 4, 7, 13,
  0, 2, 8, 13)
)
promotion_race <- xtabs(Freq ~ Race + Month + Promotion, data = promotion_race)
use_data(promotion_race, overwrite = TRUE)




# 婚前性行为1
premarital_sex1 <- data.frame(
  ReligiousService = new_rep(c("At most a few times a year", "At least several times a year"), each = 2, times = 6),
  PoliticalViews = new_rep(c("Liberal", "Moderate", "Conservative"), each = 8),
  BirthControl = new_rep(c("Agree", "Disagree"), times = 12),
  PremaritalSex = new_rep(c("Wrong only sometimes or not wrong at all", "Always or almost always wrong"), each = 4, times = 3),
  Freq = c(
    99, 15, 73, 25, 8, 4, 24, 22,
    73, 20, 87, 37, 20, 13, 50, 60,
    51, 19, 51, 36, 6, 12, 33, 88
  )
)
premarital_sex1 <- xtabs(Freq ~ PoliticalViews + PremaritalSex + ReligiousService + BirthControl, data = premarital_sex1)
use_data(premarital_sex1, overwrite = TRUE)




# 信用评分
data(credit, package = "Fahrmeir")
credit_score <- credit
use_data(credit_score, overwrite = TRUE)


# 伦敦失踪人口
data("missingpersons")
missing_persons <- missingpersons
names(missing_persons) <- Hmisc::capitalize(names(missing_persons))
use_data(missing_persons, overwrite = TRUE)


# 中国的吸烟与肺癌数据
data("smokinglungcancer")
smoking_lungcancer_cn <- smokinglungcancer
use_data(smoking_lungcancer_cn, overwrite = TRUE)



# 加州大学伯克利分校研究生录取
data(UCBAdmissions)
use_data(UCBAdmissions, overwrite = TRUE)




# 兔子与青霉素
rabbit_penicillin <- data.frame(
  PenicillinLevel = rep(c(0.125, 0.25, 0.5, 1, 4), each = 2),
  Delay = new_rep(c("None", "1.5 h"), times = 5),
  No.Cured = c(0, 0, 3, 0, 6, 2, 5, 6, 2, 5),
  No.Died = c(6, 5, 3, 6, 0, 4, 1, 0, 0, 0)
)
use_data(rabbit_penicillin, overwrite = TRUE)




# 非转移骨肉瘤
osteosarcoma <- data.frame(
  Infiltration = new_rep(c("High", "Low"), each = 4, times = 2),
  Sex = rep(c("Female", "Male"), each = 2, times = 4),
  Pathology = new_rep(c("No", "Yes"), times = 8),
  DiseaseFree = new_rep(YesNo, each = 8),
  Freq = c(
    3, 2, 4, 1, 5, 3, 5, 6,
    0, 0, 0, 0, 0, 2, 4, 11
  )
)
osteosarcoma <- xtabs(Freq ~ Infiltration + Sex + Pathology + DiseaseFree, data = osteosarcoma)
use_data(osteosarcoma, overwrite = TRUE)




# 个人使用药物后是否失禁
incontinent <- read.csv(text = "
  y, x1, x2, x3
  0, −1.9, −5.3, −43
  0, −1.5, 3.9, −15
  0, −0.1, −5.2, −32
  0, 0.5, 27.5, 8
  0, 0.8, −3.0, −12
  0, 0.8, −1.6, −2
  0, 0.9, 3.4, 1, 0, 2.3, 23.4, 14
  1, −5.6, −13.1, −1, 1, −5.3, −19.8, −33
  1, −2.4, 1.8, −9
  1, −2.3, −7.4, 4
  1, −2.0, −5.7, −7
  1, −1.7, −3.9, 13
  1, −0.6, −2.4, −7
  1, −0.5, −14.5, −12
  1, −0.1, −10.2, −5
  1, −0.1, −9.9, −11, 1, 0.4, −17.2, −9
  1, 0.7, −10.7, −10, 1, 1.1, −4.5, −15"
)
use_data(incontinent, overwrite = TRUE)



# 短吻鳄食物选择
data(alligators1)
use_data(alligators1, overwrite = TRUE)



# 是否相信来世2
data("afterlife", package = "icda")
afterlife2 <- afterlife
afterlife2 <- xtabs(Freq ~ Race + Gender + Believe, data = afterlife2)
use_data(afterlife2, overwrite = TRUE)



# 政治意识形态与性别和隶属党派
data(ideology)
ideology <- xtabs(Freq ~ Gender + Party + Ideology, data = ideology)
use_data(ideology, overwrite = TRUE)



# 心理健康、SES与生活事件
impairment <- read.csv(
  text = "
  Subject	Impairment	SES	LifeEvents
  1	Well	1	1
  21	Mild	1	9
  2	Well	1	9
  22	Mild	0	3
  3	Well	1	4
  23	Mild	1	3
  4	Well	1	3
  24	Mild	1	1
  5	Well	0	2
  25	Moderate	0	0
  6	Well	1	0
  26	Moderate	1	4
  7	Well	0	1
  27	Moderate	0	3
  8	Well	1	3
  28	Moderate	0	9
  9	Well	1	3
  29	Moderate	1	6
  10	Well	1	7
  30	Moderate	0	4
  11	Well	0	1
  31	Moderate	0	3
  12	Well	0	2
  32	Impaired	1	8
  13	Mild	1	5
  33	Impaired	1	2
  14	Mild	0	6
  34	Impaired	1	7
  15	Mild	1	3
  35	Impaired	0	5
  16	Mild	0	1
  36	Impaired	0	4
  17	Mild	1	8
  37	Impaired	0	4
  18	Mild	1	2
  38	Impaired	1	8
  19	Mild	0	5
  39	Impaired	0	8
  20	Mild	1	5
  40	Impaired	0	9",
  sep = "\t"
)
impairment <- arrange(impairment, Subject)
impairment$Impairment <- factor(impairment$Impairment, levels = unique(impairment$Impairment))
use_data(impairment, overwrite = TRUE)




# 怀孕老鼠胎儿的发育毒性研究
response_level <- c("Non-live", "Malformation", "Normal")
toxicity <- data.frame(
  Concentration = new_rep(c(0, 62.5, 125, 250, 500), times = 3),
  Response = factor(rep(response_level, each = 5), levels = response_level),
  Freq = c(15, 17, 22, 38, 144, 1, 0, 7, 59, 132, 281, 225, 283, 202, 9)
)
toxicity <- xtabs(Freq ~ Concentration + Response, toxicity)
use_data(toxicity, overwrite = TRUE)



# 工作满意度调查2
income_num <- c(3, 10, 20, 35)
income_level <- c("<5000", "5000-15000", "15000-25000", ">25000")
income_recode <- paste0(paste(income_num, "='", income_level, "'", sep = ""), collapse = "; ")
js_level <- c("Very Dissatisfied", "A Little Satisfied", "Moderately Satisfied", "Very Satisfied")
js_recode <- paste0(paste(c(1, 3:5), "='", js_level, "'", sep = ""), collapse = "; ")
data(jobsatisfaction)
job_satisfaction2 <- jobsatisfaction
job_satisfaction2 <- job_satisfaction2 %>%
  mutate(
    Gender = car::recode(Gender, "'M'='Male'; 'F'='Female'"),
    Income = factor(car::recode(Income, income_recode), levels = income_level),
    JobSatisfaction = factor(car::recode(JobSat, js_recode), levels = js_level)
  ) %>%
  select(-JobSat)
job_satisfaction2 <- xtabs(Freq ~ Gender + Income + JobSatisfaction, data = job_satisfaction2)
use_data(job_satisfaction2, overwrite = TRUE)



# 短吻鳄食物选择2
data("alligators2", package = "icda")
alligators2 <- rename(alligators2, Freq = Count)
alligators2 <- xtabs(Freq ~ Gender + Lake + Size + Food, data = alligators2)
use_data(alligators2, overwrite = TRUE)




# 婚姻幸福与家庭收入
marital_happiness <- data.frame(
  Income = new_rep(c("Below average income", "Average income", "Above average income"), each = 3),
  Happniess = new_rep(c("Not happy", "Pretty happy", "Very happy"), times = 3),
  Freq = c(6, 43, 75, 6, 113, 178, 6, 57, 117)
)
marital_happiness <- xtabs(Freq ~ Income + Happniess, marital_happiness)
use_data(marital_happiness, overwrite = TRUE)



# 小细胞肺癌治疗
response_level <- c("Progressive Disease", "No Change", "Partial Remission", "Complete Remission")
lungcancer_treatment <- data.frame(
  Therapy = new_rep(c("Sequential", "Alternating"), each = 8),
  Gender = new_rep(c("Male", "Female"), each =4, times = 2),
  Response = factor(rep(response_level, times = 4), levels = response_level),
  Freq = c(28, 45, 29, 26, 4, 12, 5, 2, 41, 44, 20, 20, 12, 7, 3, 1)
)
lungcancer_treatment <- xtabs(Freq ~ Therapy + Gender + Response, lungcancer_treatment)
use_data(lungcancer_treatment, overwrite = TRUE)



# 幸福与宗教参与
happiness_level <- c("Not Too Happy", "Pretty Happy", "Very Happy")
religion_level <- c(
  "At most several times a year",
  "Once a month to several times a year",
  "Every week to several times a week"
)
happiness2 <- data.frame(
  Happiness = factor(rep(happiness_level, times = 3), levels = happiness_level),
  Religion = factor(rep(religion_level, each = 3), levels = religion_level),
  Freq = c(189, 908, 382, 53, 311, 180, 46, 335, 294)
)
happiness2 <- xtabs(Freq ~ Religion + Happiness, happiness2)
use_data(happiness2, overwrite = TRUE)



# LDL胆固醇研究
cholesterol_level <- c("<=3.4", "3.4-4.1", "4.1-4.9", ">4.9")
cholesterol <- data.frame(
  Group = new_rep(c("Control", "Treatment"), each = 16),
  Beginning = factor(rep(cholesterol_level, each = 4, times = 2), levels = cholesterol_level),
  Ending = factor(rep(cholesterol_level, times = 8), levels = cholesterol_level),
  Freq = c(
    18, 8, 0, 0, 16, 30, 13, 2, 0, 14, 28, 7, 0, 2, 15, 22,
    21, 4, 2, 0, 17, 25, 6, 0, 11, 35, 36, 6, 1, 5, 14, 12
  )
)
cholesterol <- xtabs(Freq ~ Beginning + Ending + Group, data = cholesterol)
use_data(cholesterol, overwrite = TRUE)



# 汽车事故与安全带
injury_level <- c(
  "Not injured",
  "Injured but not transported by emergency medical services",
  "Injured and transported by emergency medical services but not hospitalized",
  "Injured and hospitalized but did not die",
  "Injured and died"
)
accident_seatbelt1 <- data.frame(
  Gender = new_rep(c("Female", "Male"), each = 20),
  Location = new_rep(c("Urban", "Rural"), each = 10, times = 2),
  SeatBelt = new_rep(c("No", "Yes"), each = 5, times = 4),
  Injury = factor(rep(injury_level, times = 8), levels = injury_level),
  Freq = c(
    7287, 175, 720, 91, 10,
    11587, 126, 577, 48, 8,
    3246, 73, 710, 159, 31,
    6134, 94, 564, 82, 17,
    10381, 136, 566, 96, 14,
    10969, 83, 259, 37, 1,
    6123, 141, 710, 188, 45,
    6693, 74, 353, 74, 12
  )
)
accident_seatbelt1 <- xtabs(Freq ~ Gender + Location + SeatBelt + Injury, data = accident_seatbelt1)
use_data(accident_seatbelt1, overwrite = TRUE)




# 工作满意度调查3
income_level <- c("<5000", "5000-15000", "15000-25000", ">25000")
js_level <- c("Very Dissatisfied", "A Little Satisfied", "Moderately Satisfied", "Very Satisfied")
job_satisfaction3 <- data.frame(
  JobSatisfaction = factor(rep(js_level, times = 8), levels = js_level),
  Gender = rep(c("Female", "Male"), each = 16),
  Income = factor(rep(income_level, each = 4, times = 2), levels = income_level),
  Freq = c(
    3, 10, 30, 27, 7, 8, 45, 39, 8, 7, 46, 51, 4, 2, 28, 47,
    1, 4, 9, 9, 1, 2, 37, 29, 0, 10, 35, 39, 7, 14, 69, 109
  )
)
job_satisfaction3 <- xtabs(Freq ~ Gender + Income + JobSatisfaction, data = job_satisfaction3)
use_data(job_satisfaction3, overwrite = TRUE)



# 幸福感与婚姻状态
happiness_level <- c("Not too happy", "Pretty happy", "Very happy")
happiness3 <- data.frame(
  Happiness = factor(rep(happiness_level, times = 2), levels = happiness_level),
  MaritalStatus = new_rep(c("Married", "Divorced"), each = 3),
  Freq = c(67, 650, 555, 65, 276, 93)
)
happiness3 <- xtabs(Freq ~ MaritalStatus + Happiness, data = happiness3)
use_data(happiness3, overwrite = TRUE)



# 汽车事故与安全带2
accident_seatbelt2 <- accident_seatbelt1 %>%
  as.data.frame() %>%
  mutate(
    Injury = ifelse(Injury == "Not injured", "No", "Yes")
  ) %>%
  xtabs(Freq ~ ., data = .)

use_data(accident_seatbelt2, overwrite = TRUE)




# 婚前性行为2
brithcontrol_level <- c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")
premarital_sex_level <- c(
  "Always wrong",
  "Almost always wrong",
  "Wrong only sometimes",
  "Not wrong at all"
)

premarital_sex2 <- data.frame(
  BirthControl = factor(rep(brithcontrol_level, times = 4), levels = brithcontrol_level),
  PremaritalSex = factor(rep(premarital_sex_level, each = 4), levels = premarital_sex_level),
  Freq = c(
    81, 68, 60, 38,
    24, 26, 29, 14,
    18, 41, 74, 42,
    36, 57, 161, 157
  )
)
premarital_sex2 <- xtabs(Freq ~ PremaritalSex + BirthControl, data = premarital_sex2)
use_data(premarital_sex2, overwrite = TRUE)



# Myers-Briggs人格测验3
MBtest3 <- margin.table(MBtest1, 1:4)
MBtest3 <- xtabs(Freq ~ JP + EI + SN + TF, data = as.data.frame(MBtest3))
use_data(MBtest3, overwrite = TRUE)



# 白人对于黑人的接受度
white_black_acceptance <- data.frame(
  President = rep(1:2, each = 4),
  Busing = rep(1:2, each = 2, times = 2),
  Home = rep(1:2, times = 4),
  Freq = c(41, 65, 72, 175, 2, 9, 4, 55)
)
white_black_acceptance <- xtabs(Freq ~ ., data = white_black_acceptance)
use_data(white_black_acceptance, overwrite = TRUE)



# 对AIDS的处理方法 (Treatment of AIDS)
AIDS_treatment <- data.frame(
  Gender = new_rep(c("Male", "Female"), each = 4),
  InformationOpinion = new_rep(c("Support", "Oppose"), each = 2, times = 2),
  HealthOpinion = new_rep(c("Support", "Oppose"), times = 4),
  Freq = c(76, 160, 6, 25, 114, 181, 11, 48)
)
AIDS_treatment <- xtabs(Freq  ~ ., data = AIDS_treatment)
use_data(AIDS_treatment, overwrite = TRUE)



# 汽车安全事故与安全带3
accident_seatbelt3 <- data.frame(
  SafetyEquipment = new_rep(c("Seat belt", "None"), each = 4),
  Ejected = new_rep(c("Yes", "No"), each = 2, times = 2),
  Injury = new_rep(c("Nonfatal", "Fatal"), times = 4),
  Freq = c(1105, 14, 411111, 483, 4624, 497, 157342, 1008)
)
accident_seatbelt3 <- to_table(accident_seatbelt3)
use_data(accident_seatbelt3, overwrite = TRUE)



# 政府对环境、卫生、大城市补助、法律强制执行的拨款问题
response_level <- c("Too little", "About right", "Too much")
government_spending <- data.frame(
  Cities = factor(rep(response_level, each = 3, times = 9), levels = response_level),
  Law = factor(rep(response_level, times = 27), levels = response_level),
  Environment = factor(rep(response_level, each = 27), levels = response_level),
  Health = factor(rep(response_level, each = 9, times = 3), levels = response_level),
  Freq = c(
    62, 17, 5, 90, 42, 3, 74, 31, 11,
    11, 7, 0, 22, 18, 1, 19, 14, 3,
    2, 3, 1, 2, 0, 1, 1, 3, 1,
    11, 3, 0, 21, 13, 2, 20, 8, 3,
    1, 4, 0, 6, 9, 0, 6, 5, 2,
    1, 0, 1, 2, 1, 1, 4, 3, 1,
    3, 0, 0, 2, 1, 0, 9, 2, 1,
    1, 0, 0, 2, 1, 0, 4, 2, 0,
    1, 0, 0, 0, 0, 0, 1, 2, 3
  )
)
government_spending <- xtabs(Freq ~ Environment + Health + Cities + Law, data = government_spending)
use_data(government_spending, overwrite = TRUE)



# 是否应该给青少年提供避孕方法
birthcontrol_level <- c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree") %>% rev()
religion_level <- c(
  "Never",
  "Less than once a year",
  "Once or twice a year",
  "Several times a year",
  "About once a month",
  "2–3 times a month",
  "Nearly every week",
  "Every week",
  "Several times a week"
)

birth_control <- data.frame(
  Religion = factor(rep(religion_level, times = 4), levels = religion_level),
  BirthControl = factor(rep(birthcontrol_level, each = 9), levels = birthcontrol_level),
  Freq = c(
    49, 31, 46, 34, 21, 26, 8, 32, 4,
    49, 27, 55, 37, 22, 36, 16, 65, 17,
    19, 11, 25, 19, 14, 16, 15, 57, 16,
    9, 11, 8, 7, 16, 16, 11, 61, 20
  )
)
birth_control <- to_table(birth_control)
use_data(birth_control, overwrite = TRUE)




# 为了环保是否愿意付更高的税或降低生活质量
environmental_protection <- data.frame(
  PayHigherTaxes = new_rep(c("Yes", "No"), each = 2),
  CutLivingStandards = new_rep(c("Yes", "No"), times = 2),
  Freq = c(227, 132, 107, 678)
)

environmental_protection <- to_table(environmental_protection)
use_data(environmental_protection, overwrite = TRUE)
