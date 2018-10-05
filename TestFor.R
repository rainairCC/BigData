for( page in 2: 2 ) {
  URL = paste("https://forum.gamer.com.tw/B.php?page=", paste( page, "&bsn=17532", sep=""), sep="")
  print(paste("Page = ", page, sep=""))
  X = read_html( curl(URL, handle = new_handle("useragent" = "Mozilla/5.0") ))
  
  Xhref = html_attr( html_nodes(X, ".b-list__main__title"), "href" )
  InURL = paste0("https://forum.gamer.com.tw/", Xhref)
  for( temp in 1:length(InURL) ) {
    print(paste("Num = ", temp, sep=""))
    if ( !is.na(Xhref[temp]) ) {
      XC = read_html( curl(InURL[temp], handle = new_handle("useragent" = "Mozilla/5.0") ))
      XInPage = html_text(html_nodes(XC, xpath = "//p[@class='BH-pagebtnA']/a")) #-- 頁數 XInPage[1]
      # if(identical(XInPage[1], character(0)))
      #   XInPage[1] = 1
      # if(is.na(XInPage[1]))
      #   XInPage[1] = 1
      # print(XInPage[length(XInPage)])
      left = substr( InURL[temp], start = 1, stop = 33 )
      right = substr( InURL[temp], start = 34, stop = nchar(InURL[temp]) )
      for( a in 1:XInPage[length(XInPage)] ) {
        URLLL = paste0( left, "page=", a, "&", right )
        XC = read_html( curl(URLLL, handle = new_handle("useragent" = "Mozilla/5.0") ))
        
        XPostAuthor = html_text( html_nodes(XC, ".username") )
        XPostUserID = html_text( html_nodes(XC, ".userid") )
        XPostTitleAll <- NA
        #---以下一組的
        if ( a == 1 )
          XPostTitle = html_text( html_nodes(XC, ".c-post__header__title") ) #-- 只有一個
        if(identical(XPostTitle, character(0)))
          XPostTitle <- NA
        
        XPostTitleAll = rep( XPostTitle, times = length(XPostAuthor) )

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
        Xntag = gsub("LV.\n", "", XPostContent)  #-- 正規表示法移除tag
        XPostContent = str_replace_all( Xntag, "\n", "" )  #-- 合併為一字串，並移除換行碼
        #---以上一組的
        
        XPostGP = html_text( html_nodes(XC, xpath = "//span[@class='postgp']/span") )
        XPostBP = html_text( html_nodes(XC, xpath = "//span[@class='postbp']/span") )
        
        temp = data.frame( Title=XPostTitleAll, URL = XURL, Author=XPostAuthor, AuthorID=XPostUserID, PostDate = XPostDate, 
                           Career = XCareer, Race = XRace, UserLV = XUserLV, UserGP = XUserGP, PostContent = XPostContent, 
                           PostGP = XPostGP, PostBP = XPostBP, isTitle = XisTitle )
        
        colnames(temp) <- c("Title", "URL",	"Author",	"AuthorID",	"PostDate",	"Career",	"Race",	"UserLV",	"UserGP", "PostContent", 
                            "PostGP", "PostBP", "isTitle")
        if ( a == 1 )
          AllData <- temp
        else
          AllData <- rbind(AllData, temp)
        
      } # for(a)
    } # if(NA)
    write.table(AllData, file = "TestData-4.csv", sep = ",", col.names = FALSE, append = TRUE, na = "NA")
  } # for(temp)
} # for(page)
