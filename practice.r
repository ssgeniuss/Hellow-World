library(plyr)
head(baseball)
View(baseball)
dim(baseball)
str(baseball)

baseball$sf[baseball$year<1954] <- 0
any(is.na(baseball$sf)) #已無NA值

sum(is.na(baseball$hbp)) #看幾個true(幾個na) 377個
   #以此為索引抓出NA直
baseball$hbp[is.na(baseball$hbp)]  
baseball$hbp[is.na(baseball$hbp)] <- 0
sum(is.na(baseball$hbp)) #已無na直

#只保留一季中打數至少為50的選手
baseball <- baseball[baseball$ab >= 50,]

#新增上壘率的欄位
baseball$OBP <- with(baseball, (h+bb+hbp)/(ab+bb+hbp+sf))
head(baseball, 20)
   #這裡是計算該選手當年的上壘率

sum(baseball$h+baseball$bb)
sum(baseball$h)
sum(baseball$bb)


#計算某選手生涯OBP

nrow(as.data.frame((unique(baseball$id)))) #共1118名選手
nrow(baseball)   #14828筆歷年資料
  #資料內有每個選手每年的數據

any(is.na(baseball$ab))
any(is.na(baseball$bb))
any(is.na(baseball$hbp))
any(is.na(baseball$sf))
any(is.na(baseball$hbp))
any(is.na(baseball$bb))
any(is.na(baseball$ab))

obp <- function(data){
  c(OBP= with(data, sum(h+bb+hbp)/sum(ab+bb+hbp+sf)),nothing=with(data,sum(h+bb)/sum(ab+bb))) 
  
        #要用c()將結果刮起來,最後ddply出來的結果才會顯示OBP於欄位???????????????,
        #why???????????
  
        #生涯OBP,所以要用sum
        #資料內有每個選手每年的數據
        #with函數,指定一個dataframe,就能直接使用其欄位做運算
}
      #使用ddply針對每個選手('id'),計算整個棒球生涯的OBP
careerOBP <- ddply(baseball, .variables = 'id', .fun = obp)
View(careerOBP)
str(careerOBP)
############ 純粹練習
df <- as.data.frame(x =careerOBP$OBP, row.names = unique(careerOBP$id) ) 
colnames(df)[1] <- "careerOBP"
View(df)   ######################


w<- cbind(id=careerOBP$id,
          careerOBP =careerOBP[order(careerOBP$OBP, decreasing = TRUE),"OBP"],
          rank=1:nrow(careerOBP)) %>% as.data.frame()

View(w)
str(w)
dim(w)

View(careerOBP)
colnames(careerOBP)

a<- matrix(1:16,4)
a
class(a)
b <- 1:5
b
class(b)




