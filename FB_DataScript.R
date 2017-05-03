##############################################################################################
##                                       DAP                                                ##
##                                FACEBOOK WEBSCRAPING                                      ##
##                               THIS IS A DRAFT SCRIPT                                     ##
##############################################################################################


library(Rfacebook)
library(Rook)
library(igraph)
library(gtools)
library(reshape)
library(lubridate)
library(ggplot2)
library(scales)

#Authenticate & Create an oAuth token.
#Authentication can be done using the app or the token. Note: the token expires after 2 hours of use.
#Conncet R session with our test app and authenticate it to our Facebook Profile for Data Mining.

#Get/Save Facebook Authorization 
#Steps to get Add_id & App_secrete
#(1): Login to Facebook 
#(2): Sign up as a developer?
#(3): Go to the Settings of the App https://developers.facebook.com/apps/327563220980323/settings/
#(4): From settings get the App ID: 327563220980323
#(5): From settings get the App Secret: 
#(6): Run fb_oauth 
#(7): As prompted, Copy and paste into Site URL on Facebook App Settings
#(7a): Get to FB App Settings by Settings -> Add Platform -> Website -> Then Paste Site URL: http://localhost:1410/
#(8): Next Save & Load 'fb_oauth'

fb_oauth <- fbOAuth(app_id = 327563220980323, app_secret = "Put App Secret Here")
save(fb_oauth, file="fb_oauth")
load("fb_oauth")

# Another Alternative: Generate a Token 2 replace every 2 hours-- https://developers.facebook.com/tools/explorer
token = "Put Generated Token Here"

#************************************
# FUNCTIONS
#************************************
#Evaluate My Personal Information 
me <- getUsers("me", token=token, private_info = TRUE)
me$name 
me$gender 
me$location 
#Evaluate My Personal Likes
my_likes <- getLikes(user="me", token=token)
#Evaluate Information from other FB Users 
presidents <- getUsers(c("barackobama", "donaldtrump"), token)
serena <- getUsers("serenawilliams", token)
#Get Information from my Friends
my_friends_info <- getUsers(my_friends$id, token = token, private_info = TRUE)
#Get Information About Likes from Other People
#serena_likes <- getLikes(user="14185406833", token=token)

#************************************
# Download Data From FB Pages 
#************************************
#Grab All Data From FB Page
page.post1 <- getPage(page = "DAVAuxiliary" , token = fb_oauth, n = 1000, feed = TRUE )
page.post2 <- getPage(page = "AVDLM", token = fb_oauth, n = 1000, feed = TRUE )
page.post3 <- getPage(page = "VDH2015", token = fb_oauth, n = 1000, feed = TRUE )
page.post4 <- getPage(page = "IAVA.org", token = fb_oauth, n = 1000, feed = TRUE )
page.post5 <- getPage(page = "the.american.legion" , token = fb_oauth, n = 1000, feed = TRUE )
page.post6 <- getPage(page = "VFWFans", token = fb_oauth, n = 1000, feed = TRUE)
page.post7 <- getPage(page = "TheVeteransSite", token = fb_oauth, n = 1000, feed = TRUE)
page.post8 <- getPage(page = "SupportingOurVeterans", token = fb_oauth, n = 1000, feed = TRUE)
page.post9 <- getPage(page = "VeteransAffairs", token = fb_oauth, n = 1000, feed = TRUE)
page.post10 <- getPage(page = "VeteransHealth", token = fb_oauth, n = 1000, feed = TRUE)

#Identify Page Information Pertaining to the FB Page Account
pp1_userinfo <- getUsers("DAVAuxiliary", token)
pp2_userinfo <- getUsers("AVDLM", token)
pp3_userinfo <- getUsers("VDH2015", token)
pp4_userinfo <- getUsers("IAVA.org", token)
pp5_userinfo <- getUsers("the.american.legion", token)
pp6_userinfo <- getUsers("VFWFans", token)
pp7_userinfo <- getUsers("TheVeteransSite", token)
pp8_userinfo <- getUsers("SupportingOurVeterans", token)
pp9_userinfo <- getUsers("VeteransAffairs", token)
pp10_userinfo <- getUsers("VeteransHealth", token)

#Identify The Likes From FB Page Account 
pp1_likes <- getLikes(user=pp1_userinfo[1], token=fb_oauth)
pp2_likes <- getLikes(user=pp2_userinfo[1], token=fb_oauth)
pp3_likes <- getLikes(user=pp3_userinfo[1], token=fb_oauth)
pp4_likes <- getLikes(user=pp4_userinfo[1], token=fb_oauth)
pp5_likes <- getLikes(user=pp5_userinfo[1], token=fb_oauth)
pp6_likes <- getLikes(user=pp6_userinfo[1], token=fb_oauth)
pp7_likes <- getLikes(user=pp7_userinfo[1], token=fb_oauth)
pp8_likes <- getLikes(user=pp8_userinfo[1], token=fb_oauth)
pp9_likes <- getLikes(user=pp9_userinfo[1], token=fb_oauth)
pp10_likes <- getLikes(user=pp10_userinfo[1], token=fb_oauth)

#Identify the Most Popular Posts
page.post1[which.max(page.post1$likes_count), ] 
page.post2[which.max(page.post2$likes_count), ] 
page.post3[which.max(page.post3$likes_count), ] 
page.post4[which.max(page.post4$likes_count), ] 
page.post5[which.max(page.post5$likes_count), ] 
page.post6[which.max(page.post6$likes_count), ] 
page.post7[which.max(page.post7$likes_count), ] 
page.post8[which.max(page.post8$likes_count), ] 
page.post9[which.max(page.post9$likes_count), ] 
page.post10[which.max(page.post10$likes_count), ] 

#************************************
# Plot Count Data From FB Pages
#************************************
#Convert Facebook Date format to R Date Format
format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}
#Aggregate metric counts over month
aggreg <- function(page){
    aggregate.metric <- function(metric) {
        m <- aggregate(page[[paste0(metric, "_count")]], list(month = page$month), mean)
        m$month <- as.Date(paste0(m$month, "-15"))
        m$metric <- metric
    return(m)
    }
}
#Evaluate the Plot Function & Insert Page
page.plot <- function(page){
    #Create data frame with average metric counts per month
    page$datetime <- format.facebook.date(page$created_time)
    page$month <- format(page$datetime, "%Y-%m")
    df.list <- lapply(c("likes", "comments", "shares"), aggreg(page))
    df <- do.call(rbind, df.list)
    
    #Visualize evolution in metric
    gg <- ggplot(fortify(df, melt=TRUE), aes(x=month, y=x))
    gg <- gg + geom_line(size=1, colour="blue")
    return(gg)
}

#Print the Page Posts 
plot(page.post1)
plot(page.post2)
plot(page.post3)
plot(page.post4)
plot(page.post5)
plot(page.post6)
plot(page.post7)
plot(page.post8)
plot(page.post9)
plot(page.post10)


#************************************
# Get Comment Replies from FB Post
#************************************

getCommentReplies <- function(comment_id, token, n=500, replies=TRUE, likes=FALSE, n.likes=n,
                              n.replies=n, api=NULL){
  
  url <- paste0("https://graph.facebook.com/", comment_id,
                "?fields=from,message,created_time,like_count,comment_count") #return initial comments
  
  if (replies==TRUE){
    url <- paste0(url, ",comments.summary(true).",
                  "fields(from,id,message,created_time,like_count)") #return reply
    if (n.replies>=500){
      url <- paste0(url, ".limit(500)")
    }
    if (n.replies<500){
      url <- paste0(url, ".limit(", n.replies, ")")
    }
  }
  if (replies==FALSE){
    url <- paste0(url, ",comments.summary(true)")
  }
  if (likes==TRUE){
    url <- paste0(url, ",likes.summary(true).",
                  "fields(id,name)")
    if (n.likes>=2000){
      url <- paste0(url, ".limit(2000)")
    }
    if (n.likes<2000){
      url <- paste0(url, ".limit(", n.likes, ")")
    }
  }
  if (likes==FALSE){
    url <- paste0(url, ",likes.summary(true)")
  }
  
  # making query
  content <- callAPI(url=url, token=token)
  
  # error traps: retry 3 times if error
  error <- 0
  while (length(content$error_code)>0){
    cat("Error!\n")
    Sys.sleep(0.5)
    error <- error + 1
    content <- callAPI(url=url, token=token, api=api)
    if (error==3){ stop(content$error_msg) }
  }
  if (length(content)==0){ 
    message("Comment could not be found")
    return(data.frame())
  }
  
  # putting it together
  out <- list()
  out[["comment"]] <- replyDataToDF(content)
  if (likes && n.likes > 0) out[["likes"]] <- likesDataToDF(content$likes$data)
  if (likes && n.likes > 0) n.l <- ifelse(!is.null(out$likes), dim(out$likes)[1], 0)
  if (n.likes == 0) n.l <- 0
  if (!likes) n.l <- Inf
  if (replies && n.likes > 0) out[["replies"]] <- repliesDataToDF(content$comments$data)
  if (replies && n.likes > 0) n.c <- ifelse(!is.null(out$replies), dim(out$replies)[1], 0)
  if (n.replies == 0) n.c <- 0
  if (!replies) n.c <- Inf
  
  # paging if we n.comments OR n.likes haven't been downloaded
  if (n.likes > n.l || n.replies > n.c){
    # saving URLs for next batch of likes and comments
    if (likes) url.likes <- content$likes$paging$`next`
    if (!likes) url.likes <- NULL
    if (replies) url.comments <- content$comments$paging$`next`
    if (!replies) url.comments <- NULL
    
    if (!is.null(url.likes) && likes && n.likes > n.l){
      # retrieving next batch of likes
      url <- content$likes$paging$`next`
      content <- callAPI(url=url.likes, token=token, api=api)
      out[["likes"]] <- rbind(out[["likes"]],
                              likesDataToDF(content$data))
      n.l <- dim(out$likes)[1]
      # next likes, in batches of 500
      while (n.l < n.likes & length(content$data)>0 &
             !is.null(url <- content$paging$`next`)){
        url <- content$paging$`next`
        content <- callAPI(url=url, token=token, api=api)
        out[["likes"]] <- rbind(out[["likes"]],
                                likesDataToDF(content$data))
        n.l <- dim(out$likes)[1]
      }
    }
    if (!is.null(url.comments) && replies && n.replies > n.c){
      # retriving next batch of comments
      content <- callAPI(url=url.comments, token=token, api=api)
      out[["replies"]] <- rbind(out[["replies"]],
                                repliesDataToDF(content$data))
      n.c <- dim(out$replies)[1]
      # next comments, in batches of 500
      while (n.c < n.replies & length(content$data)>0 &
             !is.null(content$paging$`next`)){
        url <- content$paging$`next`
        content <- callAPI(url=url, token=token, api=api)
        out[["replies"]] <- rbind(out[["replies"]],
                                  repliesDataToDF(content$data))
        n.c <- dim(out$replies)[1]
      }
    }
  }
  
  return(out)
}

#getCommentReplies(comment_id = "383960585156_10158956032000157", token = token)

#Other Functions 
#getFriends 
#getGroup
#getInsights
#getPost 
#getReactions 
#getShares
#searchFacebook
#searchGroup
#searchPages


#************************************
# Get Group Information 
#************************************
#getGroup("Veterans Against Republican Ignorance", token,  n = 25)
