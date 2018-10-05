# setwd("F:/大學ㄉ作業&課業/大三/大三上/從數據分析到知識分析/Project")
setwd("D:/大學/BDK")

# install.packages("plyr")
# install.packages("stringr")
# install.packages("data.table")
# install.packages("arules")
# install.packages("igraph")

library(plyr) #-- ddply
library(stringr) #-- str_replace_all
library(data.table) #-- setDT
library(arules) #-- apriori
library(igraph) #-- graph.edgelist
library(jiebaR) #-- worker

#------------------------------------------------------------------------------------------------------------------------------

#--   作者模型

X = read.csv("TestData-1.csv", quote = "")
colnames(X) <- c("", "Title",	"Category",	"Href",	"ReplyNO",	"PopNO",	"Author", "CommentNum")
X$nch = sapply(X$Title, FUN=function(x)nchar(as.character(x))) #-- 算主題字數
dim(X)
names(X);head(X)
# 依作者
Cv = ddply(X, c("Author"), summarise, Times=unique(length(Title)), AC=max(as.integer(CommentNum)), POP=sum(as.integer(PopNO) ))
head(Cv)

#-- 分析作者發文篇數
head( Cv[order(as.integer(Cv$Times), decreasing = TRUE),] ) #-- 根據發文篇數由大到小排序
range(Cv$Times)
hist(Cv$Times,100);   table(Cv$Times)
table(cut(Cv$Times,breaks=c(0,1,3,10,50,200)))
Mbreaks = c(0,1,3,10,50,200);   Cv$Times0 <- cut(Cv$Times,breaks=Mbreaks)
table(Cv$Times0)

#-- 分析作者最受歡迎的文章
head( Cv[order(as.integer(Cv$POP), decreasing = TRUE),] )

#-- 分析得到回文最多的作者
head( Cv[order(as.integer(Cv$AC), decreasing = TRUE),] )

#-- 分析"發文篇數"和"總人氣數"
range(Cv$Times)
hist(Cv$Times,100);   table(Cv$Times)
table(cut(Cv$Times,breaks=c(0,1,3,10,50,200)))
Mbreaks = c(0,1,3,10,50,200);   Cv$Times0 <- cut(Cv$Times,breaks=Mbreaks)

range(Cv$POP)
hist(Cv$POP,100);   table(Cv$POP)
table(cut(Cv$POP,breaks=c(0,100,1000,10000,50000,400000)))
Mbreaks = c(0,100,1000,10000,50000,400000);   Cv$POP0 <- cut(Cv$POP,breaks=Mbreaks)

head(Cv)
table(Cv$Times0, Cv$POP0)


#------------------------------------------------------------------------------------------------------------------------------

#--   主題模型-1

Y = read.csv("TestData-2.csv", quote = "")
colnames(Y) <- c("", "Title", "URL",	"Author",	"AuthorID",	"PostDate",	"Career",	"Race",	"UserLV",	"UserGP", "PostContent", 
                      "PostGP", "PostBP", "isTitle")
dim(Y) # [1] 84793    14
Y$nTitle = sapply(Y$Title, FUN=function(x)nchar(as.character(x))) #-- Title字數
Y$nch = sapply(Y$PostContent, FUN=function(x)nchar(as.character(x))) #-- 回文字數

#-- 以下為處理資料
Y$PostGP = str_replace_all( Y$PostGP, "爆", "1000" )
Y$PostGP = str_replace_all( Y$PostGP, "\"", "" )
Y$PostBP = str_replace_all( Y$PostBP, "-", "0" )
Y$PostBP = str_replace_all( Y$PostBP, "\"", "" )
Y$Title0 = substr(Y$Title, start = 2, stop = regexpr("】", Y$Title)) #-- 擷取分類
Y$Title1 = substr(Y$Title, start = regexpr("】", Y$Title)+1, stop = Y$nTitle-1 )

Y$Year = substr(Y$PostDate, start = 2, stop = 5)
Y$Month = substr(Y$PostDate, start = 7, stop = 8)
Y$Date = substr(Y$PostDate, start = 10, stop = 12)
Y$Hour = substr(Y$PostDate, start = 13, stop = 14)
Y$Dall = substr(Y$PostDate, start = 2, stop = 20)
Y$isTitle1 = substr(Y$isTitle, start = 2, stop = 2)
#-- 以上為處理資料

#-- 廢物
# CCCC = ddply(Y, c("PostDate"), summarise, Title=unique(Title1) )
# head( CCCC[order(CCCC$PostDate, decreasing = TRUE),] )

#-- 分析發文年月
range(Y$Year)
table(Y$Year, Y$Month)
CQ = ddply(Y, c("Year", "Month", "Date"), summarise, Times=unique(length(Hour)) )
dim(CQ)

#-- 分析發文時間
range(Y$Hour)
table(Y$Month)

#--  主題數據框
setDT(Y, key = "Title") 
Pv = Y[, .(Times=length(Dall), D0=min(Dall), Df=max(Dall), GP=sum(as.integer(PostGP)), BP = sum(as.integer(PostBP))
           , nch=sum(as.integer(nch),na.rm=T)), by=Title]
Pv$nDay = as.Date(Pv$Df)-as.Date(Pv$D0);   range(Pv$nDay)
Pv$nTitle = sapply(Pv$Title, FUN=function(x)nchar(as.character(x))) #-- Title字數
Pv$Title0 = substr(Pv$Title, start = 2, stop = regexpr("】", Pv$Title)) #-- 擷取分類
Pv$Title1 = substr(Pv$Title, start = regexpr("】", Pv$Title)+1, stop = Pv$nTitle-1 )
head(Pv, 3)

#-- 首次貼文
head( Pv[order(as.Date(Pv$D0), decreasing = FALSE),], 3 )
#-- 最近貼文
head( Pv[order(as.Date(Pv$Df), decreasing = TRUE),], 3 )
#-- 主題討論天數
range(Pv$nDay)   #-- 0 2090
table(Pv$nDay)
Pv$nDay0 = cut(as.numeric(Pv$nDay), breaks=c(-1,0,7,30,100,365,1095,2555));   table(Pv$nDay0)

#-- 分析"回文數"和"總GP數"
range(Pv$Times)
Pv$Times0 <- cut(Pv$Times,breaks=c(0,1,10,100,1000,10000))
table(Pv$Times0)

range(Pv$GP)
Pv$GP0 <- cut(Pv$GP,breaks=c(0,10,100,1000,10000,50000))
table(Pv$GP0)

table(Pv$Times0, Pv$GP0)
#-- 分析"回文數"和"總BP數"
range(Pv$Times)
hist(Pv$Times,100);   table(Pv$Times)
table(cut(Pv$Times,breaks=c(0,1,10,100,1000,10000)))
Mbreaks = c(0,1,10,100,1000,10000);   Pv$Times0 <- cut(Pv$Times,breaks=Mbreaks)

range(Pv$BP)
hist(Pv$BP,100);   table(Pv$BP)
table(cut(Pv$BP,breaks=c(-1,0,50,100,500,1000)))
Mbreaks = c(-1,0,50,100,500,1000);   Pv$BP0 <- cut(Pv$BP,breaks=Mbreaks)

table(Pv$Times0, Pv$BP0)

#-- 分析各種長度的貼文數量
range(Y$nch)
hist(Y$nch,100);   table(Y$nch)
table(cut(Y$nch,breaks=c(0,100,1000,5000,10000,50000)))
Mbreaks = c(0,100,1000,5000,10000,50000);   Y$nch0 <- cut(Y$nch,breaks=Mbreaks)
table(Y$nch0)

#-- 分析分類
CB = ddply(Y, c("Title0"), summarise, Times=unique(length(PostDate)), GP=sum(as.integer(PostGP)) )
head( CB[order(as.integer(CB$GP), decreasing = TRUE),] )

#------------------------------------------------------------------------------------------------------------------------------

#-- 作者模型
setDT(Y, key = "AuthorID") 
Cv = Y[, .(D0=min(Dall), Df=max(Dall),Post=sum(as.integer(isTitle1)), Reply=unique(length(PostDate)),GP=sum(as.integer(PostGP)), BP = sum(as.integer(PostBP))
           , nch=sum(as.integer(nch))), by=AuthorID]

Cv$PAR = Cv$Reply
Cv$Reply = Cv$Reply - Cv$Post
Cv$nDay = as.Date(Cv$Df)-as.Date(Cv$D0);   range(Cv$nDay)
head(Cv, 3)

#-- 作者分類(看)
range(Cv$nch)
table(Cv$nch)
table(cut(Cv$nch,breaks=c(-1,9,99,999,9999,99999,199999)))

#--發文回文數切分
range(Cv$Post)
Cv$Post0 = cut(Cv$Post,breaks=c(-1,0,1,9,99,199)) ; table(Cv$Post0)
range(Cv$Reply)
Cv$Reply0 = cut(Cv$Reply,breaks=c(-1,0,1,9,99,999,1999)) ; table(Cv$Reply0)
range(Cv$PAR)
Cv$PAR0 = cut(Cv$PAR,breaks=c(0,1,9,99,999,1999)) ; table(Cv$PAR0)

#-- 字數切分
range(Cv$nch)
Cv$nch0 = cut(Cv$nch,breaks=c(-1,9,99,999,9999,99999,199999)) ; table(Cv$nch0)

#-- 日期切分
range(Cv$nDay)   #-- 0 2090
Cv$nDay0 = cut(as.numeric(Cv$nDay), breaks=c(-1,0,7,30,100,365,1095,2190));   table(Cv$nDay0)

dim(Cv);   head(Cv,3) # [1] 17818  12

#-- 首次貼文
head( Cv[order(as.Date(Cv$D0), decreasing = FALSE),], 3 )
#-- 最近貼文
head( Cv[order(as.Date(Cv$Df), decreasing = TRUE),], 3 )
#-- 發文篇數/字數
table(Cv$PAR0, Cv$nch0)
#-- 發文篇數/日期
table(Cv$PAR0, Cv$nDay0)

#-- 參與最多討論的會員
head( Cv[order(as.integer(Cv$Reply), decreasing = TRUE),] )
#-- 打最多字的會員
head( Cv[order(as.integer(Cv$nch), decreasing = TRUE),] )
#-- 得到最多GP的會員
head( Cv[order(as.integer(Cv$GP), decreasing = TRUE),] )
#-- 得到最多BP的會員
head( Cv[order(as.integer(Cv$BP), decreasing = TRUE),] )
#-- 會員回應活躍度
range(Cv$Times)
hist(Cv$Times,100);   table(Cv$Times)
table(cut(Cv$Times,breaks=c(0,1,5,100,1000,2000)))
Mbreaks = c(0,1,5,100,1000,2000);   Cv$Times0 <- cut(Cv$Times,breaks=Mbreaks)
table(Cv$Times0)

#------------------------------------------------------------------------------------------------------------------------------

#-- CV PV 互動模型

table(Cv$PAR, Pv$GP)


#-- 關聯圖

setDT(Y)

Tsp = Y[, .(Title0=paste(Title0,collapse="/")), by=AuthorID]
LTsp = lapply(1:length(Tsp$Title0), function (k) unlist(strsplit(Tsp$Title0[k],"/")))
rulesSP = apriori( LTsp, parameter=list(support=0.05,confidence=0.3)) #-- confidence條件機率

inspect(rulesSP)

rulesSP.df = data.frame( lhs=labels(lhs(rulesSP)), rhs=labels(rhs(rulesSP)), rulesSP@quality)[15:26,]
gR = graph.edgelist(cbind(as.character(rulesSP.df$lhs),as.character(rulesSP.df$rhs)))
E.gR = format(as.numeric(rulesSP.df$confidence), digits=2)
plot(gR, edge.curved=0.5, edge.label=E.gR, edge.label.color="#FF5555", edge.arrow.size=0.7)

#------------------------------------------------------------------------------------------------------------------------------

#-- 關鍵字分析
# jiebaR, hclust(dist(rules[."lift"])), cutree 分群做推薦
# TF-IDF N-gram

# setDT(Y) 
# Pv = Y[, .(Title1=unique(Title1)), by=Title];
# Pv2 = Y[, .(cmd=unique(PostContent)), by=AuthorID]

wkr = worker(type="tag")
wkrn = worker(user="D:/大學/BDK/LOL名詞.txt", type="tag") #-- 自訂詞庫
wkradj = worker(user="D:/大學/BDK/LOL形容詞.txt", type="tag") #-- 自訂詞庫

# segment(as.character(Pv$Title1[156]), wkrn) #-- Only for test

#-- 擷取名詞並抓下來自己加詞性
# term = segment(Pv$Title1, wkr)
# nterm1 = unique(term[grep("[n]+",names(term))]);   length(nterm1)   #-- 名詞共有  3024個
# nterm2 = unique(term[grep("[v]+",names(term))]);   length(nterm2)   #-- 動詞共有  1515個
# nterm3 = unique(term[grep("[a]+",names(term))]);   length(nterm3)   #-- 形容詞共有 226個
# write.table(nterm3,"D:/大學/BDK/LOL3.txt",quote=FALSE, row.names=FALSE, col.names=FALSE )

# term2[grep("nConcept",names(term2))]

#-- 開畫風向圖

sportList = c("nTeam","nGame","nGM","nLive","nItem","nChar","nConcept","nSkill")
sportDegree = c(0,25,50,75,100,  120,150,180,  210,240,270,  300,330)
adjList = c("ay","a0","an")
sportMat = matrix(0, nrow=dim(Pv)[1], ncol=length(sportList))
adjMat   = matrix(0, nrow=dim(Pv)[1], ncol=length(adjList))
for (i in 1:length(Pv$Title1)) { 
  sportKW = segment(Pv$Title1[i], wkrn);
  for (j in 1:length(sportList)) { IND = grep(sportList[j], names(sportKW)); sportMat[i,j] = paste(sportKW[IND],collapse="/") }
  adjKW = segment(Pv$Title1[i], wkradj);
  for (j in 1:length(adjList)) { IND = grep(adjList[j], names(adjKW)); adjMat[i,j] = paste(adjKW[IND],collapse="/") }
}
colnames(sportMat) = sportList;  colnames(adjMat) = adjList
## cbind(sportMat,adjMat)[31:40,]
Wkey = list("nGame","nTeam","nConcept","nSkill","nItem","nChar","nGM","nLive")
wkey2 = list("ay","a0","an")

content = segment(Pv$Title1, wkrn)
content = names(content)
plotWindCompass(content, Wkey, as.numeric(adjMat[,1]!=""),as.numeric(adjMat[,3]!=""),"巴哈LOL")

#-- 以下為 Function plotWindCompass()
plotWindCompass <- function (content,Wkey,pPower,nPower,topic) {
  # install.packages("plotrix")
  library(plotrix)
  par( family="STKaiti" )
  plot(x=NULL, y=NULL, xlim=c(-5,5), ylim=c(-5,5), 
       main=paste0(topic," 網路風向羅盤圖 ","(貼文數=",as.character(length(content)),")"), 
       xlab="遊戲內 ---遊戲--- 遊戲外",  ylab="非專業 ---專業度--- 專業")
  abline(v=0);         abline(h=0);                
  draw.circle(0,0,  1,lty=2,border="yellow");     
  draw.circle(0,0,  2,lty=2,border="yellow");     
  # draw.circle(0,0, 2.5,lty=1,border="brown")
  draw.circle(0,0,  3,lty=1,border="brown");     
  draw.circle(0,0,  4,lty=2,border="yellow")
  draw.circle(0,0,  5,lty=2,border="yellow")
  quadText <- c("比賽","戰隊","觀念","技能","道具","角色","實況","官方")
  for (j in 1:8) text( 5.5*cos(2*pi*(j-0.5)/8), 5.5*sin(2*pi*(j-0.5)/8), quadText[j], col="brown" )
  for (j in 1:8) {
    print(j)
    dt = 45/(length(Wkey[[j]])+1) 
    for (i in 1:length(Wkey[[j]])) {
      theta = ((j-1)*45+dt*i) * pi / 180;       
      kContent = grep(Wkey[[j]][i], content)
      if (length(kContent)!=0) {
        rPos = max(as.numeric(pPower[kContent]),na.rm=T)
        print(paste("Pos= ", rPos))
        if (rPos > 0.5) {
          boxed.labels( (3+2*rPos)*cos(theta), (3+2*rPos)*sin(theta), labels=Wkey[[j]][i], bg="#FFFFFF", col="blue", cex=0.9, xpad=0.2, ypad=0.4, border=FALSE)
        } else {
          boxed.labels( (3+2*rPos)*cos(theta), (3+2*rPos)*sin(theta), labels=Wkey[[j]][i], bg="#FFFFFF", col="green", cex=0.9, xpad=0.2, ypad=0.4, border=FALSE)
        }   
        rNeg = max(as.numeric(nPower[kContent]),na.rm=T)
        print(paste("Neg= ", rNeg))
        if (rNeg < 0.5) {
          boxed.labels( (3-2*rNeg)*cos(theta), (3-2*rNeg)*sin(theta), labels=Wkey[[j]][i], bg="#FFFFFF", col="orange", cex=0.9, xpad=0.2, ypad=0.4, border=FALSE)
        } else {
          boxed.labels( (3-2*rNeg)*cos(theta), (3-2*rNeg)*sin(theta), labels=Wkey[[j]][i], bg="#FFFFFF", col="red", cex=0.9, xpad=0.2, ypad=0.4, border=FALSE)
        }   
        lines( c( (2*(rPos+1)+1)*cos(theta), (2*(1-rNeg)+1)*cos(theta)), c( (2*(rPos+1)+1)*sin(theta), (2*(1-rNeg)+1)*sin(theta)))
      }
    }  
  }
}
#-- 以上為 Function plotWindCompass()
