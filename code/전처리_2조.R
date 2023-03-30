###########################################################
#######1. 디렉토리, 라이브러리, 데이터 불러오기 코드#######
###########################################################

setwd('D:/학회/방학세미나/최종')
getwd()

need_packages = c("data.table", "tidyverse", "mice", "MLmetrics","DMwR","gridExtra","reshape2","lubridate",'RColorBrewer',"rlang","corrplot")
options(warn = -1)
for(i in 1:length(need_packages)){
  if(require(need_packages[i], character.only = T) == 0){
    install.packages(need_packages[i])
    require(need_packages[i], character.only = T)
  }
  else{require(need_packages[i], character.only = T)}
}
rm(list = ls())

train = fread('train.csv',data.table = FALSE)

dim(train) # 40000 171


###################
######2.EDA########
###################

# --- 1. 컬럼별 결측치수 확인
colna <- train %>% is.na() %>% colSums() %>% data.frame()
index <- colna %>% rownames()
colna <- cbind(index , colna)
rownames(colna) <- NULL
colnames(colna) <- c('index', 'na_cnt')
colna %>% arrange(desc(na_cnt))

# 시각화
plot1 <- ggplot(colna, aes(x=reorder(index, -na_cnt), y=na_cnt, fill=na_cnt, color=na_cnt))+
  geom_bar(stat="identity", alpha=0.1)+
  theme(panel.background = element_rect(fill='white', color='black', linetype='solid'),
        axis.title.x=element_text(face="bold", size=10), 
        axis.title.y=element_text(face="bold", size=10),
        plot.title=element_text(face="bold", size=10),
        axis.text.x = element_text(size=7, face="bold", colour = "grey50"),
        axis.text.y = element_text(size=7, face="bold", colour = "grey50"),
        legend.title=element_text(face="bold", size=10),
        legend.text = element_text(face="bold", size=10))+
  scale_fill_gradient(
    high = "#E71D1D",
    low = "#47E71D",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = c("fill","color"))+
  labs(x="변수이름", y="결측치 개수", fill="결측치 개수", color="결측치 개수", main="변수별 결측치 개수" )+
  geom_text(aes(label = na_cnt, color = na_cnt), position = position_stack(0.5), size = 3)
plot1

## 50%이상 결측치인 변수
colna_20000 <- colna %>% filter(na_cnt>20000)
colna_20000
colna_20000 %>% dim() ##8개
# 시각화
plot2 <- ggplot(colna_20000, aes(x=reorder(index, -na_cnt), y=na_cnt, fill=na_cnt, color=na_cnt))+
  geom_bar(stat="identity", alpha=0.1)+
  theme(panel.background = element_rect(fill='white', color='black', linetype='solid'),
        axis.title.x=element_text(face="bold", size=10), 
        axis.title.y=element_text(face="bold", size=10),
        plot.title=element_text(face="bold", size=10),
        axis.text.x = element_text(size=7, face="bold", colour = "grey50"),
        axis.text.y = element_text(size=7, face="bold", colour = "grey50"),
        legend.title=element_text(face="bold", size=10),
        legend.text = element_text(face="bold", size=10))+
  scale_fill_gradient(
    high = "#E71D1D",
    low = "#47E71D",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = c("fill","color"))+
  labs(x="변수이름", y="결측치 개수", fill="결측치 개수", color="결측치 개수", main="50%이상 결측치인 변수시각화" )+
  geom_text(aes(label = na_cnt, color = na_cnt), position = position_stack(0.5), size = 3)
plot2

## 30%이상 결측치인 변수
colna_13000 <- colna %>% filter(na_cnt>13000)
colna_13000
colna_13000 %>% dim() ##10개
# 시각화
plot3 <- ggplot(colna_13000, aes(x=reorder(index, -na_cnt), y=na_cnt, fill=na_cnt, color=na_cnt))+
  geom_bar(stat="identity", alpha=0.1)+
  theme(panel.background = element_rect(fill='white', color='black', linetype='solid'),
        axis.title.x=element_text(face="bold", size=10), 
        axis.title.y=element_text(face="bold", size=10),
        plot.title=element_text(face="bold", size=10),
        axis.text.x = element_text(size=7, face="bold", colour = "grey50"),
        axis.text.y = element_text(size=7, face="bold", colour = "grey50"),
        legend.title=element_text(face="bold", size=10),
        legend.text = element_text(face="bold", size=10))+
  scale_fill_gradient(
    high = "#E71D1D",
    low = "#47E71D",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = c("fill","color"))+
  labs(x="변수이름", y="결측치 개수", fill="결측치 개수", color="결측치 개수", main="50%이상 결측치인 변수시각화" )+
  geom_text(aes(label = na_cnt, color = na_cnt), position = position_stack(0.5), size = 3)
plot3

# 변수의 NA개수 시각화
nas = sort(apply(is.na(train[,-1]),2,sum),decreasing=T)
nas = data.frame(nas)
colnames(nas)="num"
nas$cat = NULL
for (i in 1:nrow(nas)){
  if (nas$num[i] >= 10000){nas$cat[i]=5}
  else if (nas$num[i] >= 5000){nas$cat[i]=4}
  else if (nas$num[i] >= 1000){nas$cat[i]=3}
  else if (nas$num[i] > 0){nas$cat[i]=2}
  else nas$cat[i]=1
}

table(nas$cat) %>% data.frame() %>% 
  ggplot(aes(x = "", y = Freq, fill = Var1)) + 
  geom_bar(width = 1, stat = "identity", alpha=0.6, color = "white")+
  coord_polar("y")+
  geom_text(aes(label = paste0(Freq)), 
            position = position_stack(vjust = 0.5),color = "black") +
  theme_void()+
  theme(text =element_text(face = "bold"))+
  scale_fill_brewer(palette = "Oranges",
                    labels=c("0개","1개이상 1000개 미만","1000개이상 5000개 미만","5000개이상 10000개 미만","10000개이상"),
                    name = "NA 개수") 


# 결측치수가 20000개(50%) 이상인 컬럼 제거
colna_20000 <- colna %>% filter(na_cnt>20000)
index_20000 <- colna_20000$index
train1 <- train %>% select(-index_20000)
train1 %>% dim()


# --- 2. 행별 결측치수 확인
na <- apply(is.na(train1),1,sum) ## 행별 na 수 구하기
na %>% length()
train2 <- cbind(train1, na)
train2 %>% dim()

# 결측치수가 85개(총 X변수의 50%) 이상인 행 제거
train3 <- train2 %>% filter( na < 85 ) %>% select(-na) %>% as.data.frame()
train3 %>% dim()

train2$na <- train2$na %>% as.factor()
row_na <- train2 %>% group_by(na) %>% summarise(n= n())
row_na <- data.frame(row_na)
row_na %>% dim()

# 시각화
plot4 <- ggplot(row_na, aes(x=reorder(na, na), y=n, fill=n, color=n))+
  geom_bar(stat="identity", alpha=0.1)+
  theme(panel.background = element_rect(fill='white', color='black', linetype='solid'),
        axis.title.x=element_text(face="bold", size=10), 
        axis.title.y=element_text(face="bold", size=10),
        plot.title=element_text(face="bold", size=10),
        axis.text.x = element_text(size=7, face="bold", colour = "grey50"),
        axis.text.y = element_text(size=7, face="bold", colour = "grey50"),
        legend.title=element_text(face="bold", size=10),
        legend.text = element_text(face="bold", size=10))+
  scale_fill_gradient(
    high = "#E71D1D",
    low = "#47E71D",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = c("fill","color"))+
  labs(x="결측치 개수", y="행 개수", fill="행 개수", color="행 개수", main="행별 결측치 개수 시각화" )+
  geom_text(aes(label = n, color = n), position = position_stack(0.5), size = 3)
plot4


# --- 3. 변수의 unique 개수 확인
# dat의 unique 개수 리턴하는 함수
cnt_uniq <- function(dat){
  dat = dat[!is.na(dat)]
  cnt = length(unique(dat))
  return(cnt)
} 

uniq_cnt = apply(train3[,-1], 2, cnt_uniq) %>% as.vector()
var_uniq = data.frame(
  index = colnames(train3)[-1],
  uniq_cnt = uniq_cnt
)
var_uniq = var_uniq %>% arrange(uniq_cnt)

# 시각화
plot5 <- var_uniq %>% filter(uniq_cnt < 1000) %>%
  ggplot(aes(x=reorder(index, uniq_cnt), y=uniq_cnt, fill=uniq_cnt, color=uniq_cnt))+
  geom_bar(stat="identity", alpha=0.1)+
  theme(panel.background = element_rect(fill='white', color='black', linetype='solid'),
        axis.title.x=element_text(face="bold", size=10), 
        axis.title.y=element_text(face="bold", size=10),
        plot.title=element_text(face="bold", size=10),
        axis.text.x = element_text(size=7, face="bold", colour = "grey50"),
        axis.text.y = element_text(size=7, face="bold", colour = "grey50"),
        legend.title=element_text(face="bold", size=10),
        legend.text = element_text(face="bold", size=10))+
  scale_fill_gradient(
    high = "#E71D1D",
    low = "#47E71D",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = c("fill","color"))+
  labs(x="변수이름", y="uniq 개수", fill="uniq 개수", color="uniq 개수", main="변수의 uniq개수 시각화" )+
  geom_text(aes(label = uniq_cnt, color = uniq_cnt), position = position_stack(0.5), size = 3)
plot5

# NA를 제외한 unique 개수가 1개인 변수 제거
train3$cd_000 %>% unique()
train4 = train3 %>% select(-cd_000)

dat = train4 %>% as.data.frame()
dat %>% dim() # 39764 162

write_csv(dat, "train_delete.csv")


# --- 4. 클래스 불균형 확인
train$class = as.factor(train$class)
cls=as.data.frame(table(train$class))
colnames(cls)=c("class","Freq")
cls %>% 
  ggplot(aes(x = "", y = Freq, fill = class)) + 
  geom_bar(width = 1, stat = "identity",alpha=0.8, color = "white")+
  coord_polar("y")+
  geom_text(aes(label = paste0(round(Freq/400),"%")), 
            position = position_stack(vjust = 0.5),color = "black") +
  theme_void()   +
  theme(text =element_text(face = "bold"))+
  scale_fill_manual(values = c("#EF7C7D","#15496C"))


# --- 5. 컬럼별 0 데이터 개수 확인
zeros = sort(apply(train[,-1]==0,2,sum,na.rm=T),decreasing=T)
zeros = data.frame(zeros)
colnames(zeros)="num"
zeros$cat = NULL
for (i in 1:nrow(zeros)){
  if (zeros$num[i] >= 30000){zeros$cat[i]=4}
  else if (zeros$num[i] >= 20000){zeros$cat[i]=3}
  else if (zeros$num[i] >= 10000){zeros$cat[i]=2}
  else zeros$cat[i]=1
}

table(zeros$cat) %>% data.frame() %>% 
  ggplot(aes(x = "", y = Freq, fill = Var1)) + 
  geom_bar(width = 1, stat = "identity",color = "white")+
  coord_polar("y")+
  geom_text(aes(label = paste0(round(Freq/1.7),"%")), 
            position = position_stack(vjust = 0.5),color = "black") +
  theme_void()   +
  theme(text =element_text(face = "bold"))+
  scale_fill_brewer(labels=c("10000개미만","10000개이상 20000개 미만","20000개이상 30000개 미만","30000개이상"),
                    palette = "Blues",name = "0 개수")


#####################
######3. 전처리######
#####################

dat = fread('train_delete.csv',data.table = F)

# --- 1. target 변수 0(neg), 1(pos)로 재범주화
dat2 = dat %>% mutate(class = ifelse(class == 'pos',1,0) %>% as.factor)
dat2$class %>% str()

# --- 2. integer64 -> num으로 형변환 (mice 실행 과정에서의 에러 방지를 위해)
dat2 %>% str()
dat3 = dat2 %>% mutate_if(bit64::is.integer64,as.numeric)
dat3 %>% str()

# --- 3. 결측치 처리: MICE

# predictorMatrix 지정: class는 imputator로 사용하지 않음
pred = matrix(1,nrow = ncol(dat3),ncol = ncol(dat3))
rownames(pred) = names(dat3)
colnames(pred) = names(dat3)
diag(pred) = 0
pred[,'class'] = 0

dat3 %>% dim() # 39764 162
dat3 %>% is.na() %>% colSums() %>% data.frame() # class, aa_000 변수는 NA 없음

# mice
set.seed(123)
imp = mice(dat3, m = 1, method = c('','',rep('cart',160)),
           predictorMatrix = pred, maxit = 3, remove.collinear = FALSE)

train_imp = complete(imp)
train_imp %>% is.na() %>% sum()

write_csv(train_imp, "train_imp.csv")

# --- 4. 데이터 불균형 처리: SMOTE
train_imp$class = as.factor(train_imp$class)
set.seed(123)
train_imp_sm = SMOTE(class ~ ., data = train_imp, 
                 perc.over = 200, k = 5, perc.under = 300)

# target 변수 분포 확인
table(train_imp_sm$class)

###########################
###4. 전처리 데이터 저장###
###########################

write_csv(train_imp_sm,'train_imp_sm.csv')
rm(list = ls())
