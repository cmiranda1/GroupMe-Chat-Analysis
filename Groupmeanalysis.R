library(stringr)
library(ggplot2)
library(rockchalk)
library(scatterplot3d)


group <- as.data.frame(readLines("YourChatHistory.txt"))

d <- c(1:nrow(group))
d <- as.data.frame(d)


for(i in 1:nrow(group)){
  if(length(grep(">User1<", as.character(group[i,])))>0){
    d[i,1]<- "User1"
  } else if (length(grep(">User2<",as.character(group[i,])))>0){
    d[i,1]<- "User2"
      } else if (length(grep(">User3<",as.character(group[i,])))>0){
        d[i,1]<- "User3"
      }else if (length(grep(">User4<",as.character(group[i,])))>0){
        d[i,1]<- "User4"
      }else if (length(grep(">User5<", as.character(group[i,])))>0) {
        d[i,1]<- "User5"
      }else if (length(grep(">User6<", as.character(group[i,])))>0) {
        d[i,1]<- "User6"
      }else if (length(grep(">User7<", as.character(group[i,])))>0) {
        d[i,1]<- "User7"
      }else if (length(grep(">User8<", as.character(group[i,])))>0) {
        d[i,1]<- "User8"
      }else if (length(grep("colspan", as.character(group[i,])))>0) {
        d[i,1]<- "colspan"
      }
      else {
        d[i,1]<- "colspan"
      }
    }
group$name <- as.factor(d[,1])
group$original <- as.character(group[,1])

y<- NULL
h<- c(1:nrow(group))
h<- as.data.frame(h)


for(i in 1:nrow(group)){
  if(length(grep("'s", group$original[i]))>0){
     group$original[i] <- gsub("'s", "s", group$original[i])
  }else{
    group$original[i] <- group$original[i]
  }
}

for(i in 1:nrow(group)){
  if(length(grep("@", group$original[i]))>0){
    group$original[i] <- gsub("@", "", group$original[i])
  }else{
    group$original[i] <- group$original[i]
    }
}

for(i in 1:nrow(group)){
  if(length(grep("'v", group$original[i]))>0){
    group$original[i] <- gsub("'v", "v", group$original[i])
  }else{
    group$original[i] <- group$original[i]
  }
}

for(i in 1:nrow(group)){
  if(length(grep("'m", group$original[i]))>0){
    group$original[i] <- gsub("'m", "m", group$original[i])
  }else{
    group$original[i] <- group$original[i]
  }
}


for(i in 1:nrow(group)){
  if(length(grep("*", group$original[i]))>0){
    group$original[i] <- gsub("*", "", group$original[i])
  }else{
    group$original[i] <- group$original[i]
    }
}

group$length <- (nchar(group$original) - nchar(as.character(group$name))) - 88

multi.fun <- function(x) {cbind(freq = table(x), percentage = prop.table(table(x))*100)}
multi.fun(group$name)

group$split <- strsplit(group$original, 'hour">')

g <- as.data.frame(group$split[1:nrow(group)])
f <- t(g[2,])

group$untrimtimes <- f

group$times <- str_split_fixed(group$untrimtimes, ':<', 8)
group$times <- sub(":", ".", group$times)

remove <- which(group$name == "colspan")
group <- group[-remove,]

group$times <- substr(group$times, 2, 6)

t <- c(1:nrow(group))
t <- as.data.frame(t)

t$mess.name <- group$name
t$times <- group$times
t$chara <- group$length

l <- as.numeric(t$times[1:31952])
l <- as.data.frame(l)
t$times <- l

ggplot(t[1:31952,], aes(x  = mess.name, fill = mess.name))+
 geom_bar(width = .75)+
 ggtitle('Messenger Frequency')+
 xlab('Name of Messenger')+
 ylab('# of Messages (Lifespan)')+
 labs(fill = "Messenger Name")

ggplot(t[1:31952,], aes(x = times, y = chara))+
  geom_point(shape = 0)+
  scale_color_hue(c =100)+
  xlab("Time of Day (Army)")+
  ylab("Activity")+
  ylim(0,500)
  

 
 
ggplot(t[1:31952,], aes(x = mess.name, y = (chara), fill = mess.name))+
  geom_bar(stat = "identity")+
  ggtitle('Total Characters Sent')+
  xlab('Name of Messenger')+
  ylab("# of Characters")+
  labs(fill = "Messenger Name")
    
h.bindi <- which(t$mess.name == "User1")
mean.bindi <- mean(t$chara[h.user1])

h.john <- which(t$mess.name == "User2")
mean.john <- mean(t$chara[h.user2])

h.colin <- which(t$mess.name == "User3")
mean.colin <- mean(t$chara[h.user3])

h.lev <- which(t$mess.name == "User4")
mean.lev <- mean(t$chara[h.user4])

h.matt <- which(t$mess.name == "User5")
mean.matt <- mean(t$chara[h.user5])

h.max <- which(t$mess.name == "User6")
mean.max <- mean(t$chara[h.user6])

h.sam <- which(t$mess.name == "User7")
mean.sam <- mean(t$chara[h.user7])

h.adam <- which(t$mess.name == "User8")
mean.adam <- mean(t$chara[h.user9])

avgs <- c(mean.user1,mean.user2,mean.user3,mean.user4,mean.user5,mean.user6,mean.user7,mean.user8)
avgs.one <- data.frame(avgs)
t$mess.name <- droplevels(t$mess.name)
avgs.names <- levels(t$mess.name)
avgs.one$mean.names <- avgs.names
avgs.one$mean.names <- as.factor(avgs.one$mean.names)


ggplot(avgs.one[1:8,], aes(x = mean.names, y = avgs,  fill = mean.names))+
 geom_bar(stat = "identity")+
  ggtitle("Average Text Length")+
  xlab("Messenger Names")+
  ylab("Length")+
  labs(fill = "Messengers")

ggplot(t[1:31952,], aes(x = times, y = chara, fill = mess.name))+
  geom_bar(stat = "identity", width = .1)+
  ggtitle('Peak Chat Hours')+
  xlab('Time')+
  ylab("Activity")+
  labs(fill = "Messenger")

with(t, {scatterplot3d(mess.name, times, chara, main = "3D Scatter")})
