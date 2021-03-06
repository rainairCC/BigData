# 0930336928  帥哥電話
# setwd("/Users/user/Desktop/BDK")
# setwd("F:/大學ㄉ作業&課業/大三/大三上/從數據分析到知識分析/Project")

# install.packages("curl")
# install.packages("RCurl")
# install.packages("XML")
# install.packages("httr")
# install.packages("xml")
# install.packages("xml2")
# install.packages("rvest")
# install.packages("xlsx")
# install.packages("stringr")
# install.packages("plyr")
# library(curl) ; library(RCurl) ; library(XML)
# library(httr) ; library(XML) ; library(xml2)
# library(rvest) ; library(rJava) ; library(xlsxjars)
# library(xlsx) ; library(stringr) ; library(plyr)

URL = "https://forum.gamer.com.tw/B.php?page=1&bsn=17532"
X = read_html( curl(URL, handle = new_handle("useragent" = "Mozilla/5.0") ))

Xpage = html_text(html_nodes(X, xpath = "//*[(@id = \"BH-pagebtn\")]//a[(((count(preceding-sibling::*) + 1) = 5) and parent::*)]"))
#-- print(Xpage[1])

for( page in 1: 200 ) {
  URL = paste("https://forum.gamer.com.tw/B.php?page=", paste( page, "&bsn=17532", sep=""), sep="")
  print(paste("Page = ", page, sep=""))
  # URL = "https://forum.gamer.com.tw/B.php?page=5&bsn=17532"
  X = read_html( curl(URL, handle = new_handle("useragent" = "Mozilla/5.0") ))

  Xtitle = html_text( html_nodes(X, xpath = "//*[contains(concat( \" \", @class, \" \" ), concat( \" \", \"b-list__main__title\", \" \" ))]") )
  Xntag1 = gsub("<.,，*?>", "", Xtitle)  #-- 正規表示法移除tag
  Xtitle = str_replace_all( Xntag1, "\n", "" )  #-- 合併為一字串，並移除換行碼
  Xtitle = str_replace_all( Xtitle, ",", "，" )  #-- 合併為一字串，並移除換行碼
  #-- With Reply & Popularity ------------
  Xno = html_text( html_nodes(X, xpath = "//p[@class='b-list__count__number']/span") )
  #-- Reply ------------
  Xreply = Xno[seq(from=1,by=2,to=length(Xno))]
  #-- Popularity ------------
  Xpop = Xno[seq(from=2,by=2,to=length(Xno))]

  Xhref = html_attr( html_nodes(X, ".b-list__main__title"), "href" )

  Xauth = html_text( html_nodes(X, xpath = "//p[@class='b-list__count__user']/a") )

  # XlastTime = html_text( html_nodes(X, xpath = "//p[@class='b-list__time__edittime']/a") )

  # XlastUser = html_text( html_nodes(X, xpath = "//p[@class='b-list__time__user']/a") )

  XCategory = html_attr( html_nodes(X, xpath = "//*[contains(concat( \" \", @class, \" \" ), concat( \" \", \"b-list__summary__sort\", \" \" ))]//a"), name = "data-subbsn" )
  
  temp1 = data.frame( Title=Xtitle, Category=XCategory, Href=Xhref, ReplyNO = Xreply, PopNO = Xpop, 
                     Author=Xauth )
  colnames(temp1) <- c("Title",	"Category",	"Href",	"ReplyNO",	"PopNO",	"Author")
  
  temp1<-na.omit(temp1)
  
  #-----------------------------------------------------------------------------------------
  
  countAll <- character(0)
  InURL = paste0("https://forum.gamer.com.tw/", Xhref)
  for( temp in 1:length(InURL) ) {
    print(paste("Num = ", temp, sep=""))
    
    if ( !is.na(Xhref[temp]) ) {
      count <- 0 #-- 算總共有多少個留言
      XC = read_html( curl(InURL[temp], handle = new_handle("useragent" = "Mozilla/5.0") ))
      XInPage = html_text(html_nodes(XC, xpath = "//p[@class='BH-pagebtnA']/a")) #-- 頁數 XInPage[1]
      left = substr( InURL[temp], start = 1, stop = 33 )
      right = substr( InURL[temp], start = 34, stop = nchar(InURL[temp]) )
      
      for( a in 1:XInPage[length(XInPage)] ) {
        URLLL = paste0( left, "page=", a, "&", right )
        XC = read_html( curl(URLLL, handle = new_handle("useragent" = "Mozilla/5.0") ))
        
        XPostAuthor = html_text( html_nodes(XC, ".username") )
        
        
        if ( identical(XPostAuthor, character(0)) ) #-- 
          break ;
        XPostAuthor = str_replace_all( XPostAuthor, ",", "" )
        XPostUserID = html_text( html_nodes(XC, ".userid") )
        XPostTitleAll <- NA
        #---以下一組的
        if ( a == 1 )
          XPostTitle = html_text( html_nodes(XC, ".c-post__header__title") ) #-- 只有一個
        if(identical(XPostTitle, character(0)))
          XPostTitle <- NA
        
        XPostTitle = str_replace_all( XPostTitle, ",", "，" )  #-- 合併為一字串，並移除換行碼
        XPostTitleAll = rep( XPostTitle, times = length(XPostAuthor) )
        count = count + length(XPostAuthor)
        
        #---以上一組的
        XPostDate = html_attr( html_nodes(XC, xpath = "//div[@class='c-post__header__info']/a"), "data-mtime" )
        XPostDate = XPostDate[!is.na(XPostDate)]
        if ( a == 1 )
          XisTitle = c("1", rep("0", times = length(XPostAuthor)-1))
        else
          XisTitle = rep("0", times = length(XPostAuthor))
        XURL = rep( URLLL, times = length(XPostAuthor) )
        
        XCareer = html_attr( html_nodes(XC, xpath = "//div[@class='usericon usercareer']"), "title" )
        XRace = html_attr( html_nodes(XC, xpath = "//div[@class='usericon userrace']"), "title" )
        
        #---以下一組的
        XUserLV = html_text( html_nodes(XC, ".userlevel"), trim = TRUE )
        XUserLV = gsub("LV.\n", "", XUserLV)
        #---以上一組的
        
        XUserGP = html_attr( html_nodes(XC, ".usergp"), "title" )
        
        #---以下一組的
        XPostContent = html_text( html_nodes(XC, ".c-article__content") )
        Xntag = gsub("<.,，*?>", "", XPostContent)  #-- 正規表示法移除tag
        XPostContent = str_replace_all( Xntag, "\n", "" )  #-- 合併為一字串，並移除換行碼
        XPostContent = str_replace_all( XPostContent, ",", "，" )  #-- 合併為一字串，並移除換行碼
        #---以上一組的
        
        if ( identical(XPostContent, character(0)) || str_detect( XPostContent, "首篇已刪") )
          break ;
        
        XPostGP = html_text( html_nodes(XC, xpath = "//span[@class='postgp']/span") )
        XPostBP = html_text( html_nodes(XC, xpath = "//span[@class='postbp']/span") )
        
        tempIn = data.frame( Title=XPostTitleAll, URL = XURL, Author=XPostAuthor, AuthorID=XPostUserID, PostDate = XPostDate, 
                           Career = XCareer, Race = XRace, UserLV = XUserLV, UserGP = XUserGP, PostContent = XPostContent, 
                           PostGP = XPostGP, PostBP = XPostBP, isTitle = XisTitle )
        
        colnames(tempIn) <- c("Title", "URL",	"Author",	"AuthorID",	"PostDate",	"Career",	"Race",	"UserLV",	"UserGP", "PostContent", 
                            "PostGP", "PostBP", "isTitle")
        if ( a == 1 )
          OneDataIn <- tempIn
        else
          OneDataIn <- rbind(OneDataIn, tempIn)
        
      } # for(a)
      
      print(count)
      if ( identical(countAll, character(0)) )
        countAll <- count
      else
        countAll <- c(countAll, count)
      
    } # if(NA)
    
    
    if ( temp == 1 )
      pageDataIn <- OneDataIn
    else
      pageDataIn <- rbind(pageDataIn, OneDataIn)
    
  } # for(temp)
  if ( page == 1 )
    AllDataIn <- pageDataIn
  else
    AllDataIn <- rbind(AllDataIn, pageDataIn)
  
  #-----------------------------------------------------------------------------------------
  
  # colnames(temp1) <- c("Title",	"Category",	"Href",	"ReplyNO",	"PopNO",	"Author")
  temp1 <- data.frame(temp1, CommentNum=countAll)
  temp1 <- subset(temp1, temp1$Title != "首篇已刪")
  
  if ( page == 1 )
    AllData <- temp1
  else
    AllData <- rbind(AllData, temp1) 
  
  # AllData<-na.omit(AllData)
}
AllData<-na.omit(AllData)
AllDataIn<-na.omit(AllDataIn)
write.table(AllData, file = "TestData-1.csv", sep = ",", col.names = NA, append = FALSE, na = "NA")
write.table(AllDataIn, file = "TestData-2.csv", sep = ",", col.names = NA, append = FALSE, na = "NA")
