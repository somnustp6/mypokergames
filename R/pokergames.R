
ninenine = function(n_people = 4,n_computer = 0,block = 10){
  
  n = n_people + n_computer
  
  pointsum = 0
  
  poker = paste(c(rep(c("spade","heart","diamond","club"),13)),
                c(as.character(rep(1:10,each = 4)),rep(c("J","Q","K"),each = 4)))
  card = sample(1:52,size = 52,replace = F)
  u_card = c()
  
  if (!(n<=4&&n>=2)){
    cat("game limit 2~4 people", "\n")
  }
  
  loser = c()
  
  if (n<=4&&n>=2){
    cat("number of player:",n,"game start", "\n")
    playerlist = vector("list")
    for(i in 1:n){
      playerlist[[i]] = sort(card[(1+5*(i-1)):(5+5*(i-1))])
    }
    
    if (n_computer==0){
      np = as.character(c(1:n_people))
    }
    else if (n_people==0){
      np = paste0("c",as.character(c(1:n_computer)))
    }
    else{
      np = c(as.character(c(1:n_people)),paste0("c",as.character(c(1:n_computer))))
    }
    names(playerlist) = sample(np,size = length(np),replace = F)
    card = card[-c(1:(5*n))]
    
    i = 1
    
    cat("The current order of cards is:",names(playerlist), "\n")
    cat("player",names(playerlist)[i],"start", "\n")
    st = readline("game start")
    
    while (n==4){
      cat("The current sum of the deck = ", pointsum, "\n")
      cat("player", names(playerlist)[i] , "\n")
      cat(poker[playerlist[[i]]], "\n")
      pl = names(playerlist)[i]
      
      if (grepl("c",names(playerlist)[i])){
        if (pointsum>90){
          pc = playerlist[[i]][1:5]
          pc = pc %in% as.character(c(1,13:20,37:52))
          if (sum(pc)==0){
            a = sample(as.character(c(1:5)),size = 1,replace = F)
          }
          else{
            pc = which(pc)
            a = sample(as.character(pc),size = 1,replace = F)
          }
        }
        else{
          a = sample(as.character(c(1:5)),size = 1,replace = F)
        }
      }
      else{
        a = readline("Which card to play?")
      }
      
      if (!(a %in% c("Q","1","2","3","4","5"))){
        cat("Input error, leave the game", "\n")
        break
      }
      
      ########################################
      if (a=="Q"){
        cat("leave the game", "\n")
        break
      }
      ########################################
      
      ##b1
      if (playerlist[[i]][as.integer(a)] %in% c(1)){
        pointsum = 0
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        playerlist = playerlist[c(2,3,4,1)]
      }
      
      ##4
      else if (playerlist[[i]][as.integer(a)] %in% c(13:16)){
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        playerlist = playerlist[c(4,3,2,1)]
      }
      
      ##5
      else if (playerlist[[i]][as.integer(a)] %in% c(17:20)){
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        if (grepl("c",names(playerlist)[i])){
          a5 = sample(names(playerlist),size = 1,replace = F)
        }
        else{
          a5 = readline("Specify which player?")
        }
        pa5 = which(names(playerlist) == a5)
        playerlist = playerlist[c(pa5,
                                  ifelse((pa5+1)<=4,pa5+1,pa5-3),
                                  ifelse((pa5+2)<=4,pa5+2,pa5-2),
                                  ifelse((pa5+3)<=4,pa5+3,pa5-1))]
      }
      
      ##J
      else if (playerlist[[i]][as.integer(a)] %in% c(41:44)){
        pointsum = pointsum
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        playerlist = playerlist[c(2,3,4,1)]
      }
      
      ##10
      else if (playerlist[[i]][as.integer(a)] %in% c(37:40)){
        if (grepl("c",names(playerlist)[i])){
          if (pointsum>89){
            a10 = "B"
          }
          else if (pointsum<10){
            a10 = "A"
          }
          else{
            a10 = sample(c("A","B"),size = 1,replace = F)
          }
        }
        else{
          a10 = readline("A:+10  B:-10 ?")
        }
        if (a10 =="A") a10 = 10
        if (a10 =="B") a10 = -10
        if ((pointsum + as.integer(a10))>99|(pointsum + as.integer(a10))<0){
          break
        }
        pointsum = pointsum + as.integer(a10)
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        playerlist = playerlist[c(2,3,4,1)]
      }
      
      ##Q
      else if (playerlist[[i]][as.integer(a)] %in% c(45:48)){
        if (grepl("c",names(playerlist)[i])){
          if (pointsum>79){
            a20 = "B"
          }
          else if (pointsum<20){
            a20 = "A"
          }
          else{
            a20 = sample(c("A","B"),size = 1,replace = F)
          }
        }
        else{
          a20 = readline("A:+20  B:-20 ?")
        }
        if (a20 =="A") a20 = 20
        if (a20 =="B") a20 = -20
        if ((pointsum + as.integer(a20))>99|(pointsum + as.integer(a20))<0){
          break
        }
        pointsum = pointsum + as.integer(a20)
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        playerlist = playerlist[c(2,3,4,1)]
      }
      
      ##K
      else if (playerlist[[i]][as.integer(a)] %in% c(49:52)){
        pointsum = 99
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        playerlist = playerlist[c(2,3,4,1)]
      }
      
      else{
        ae = playerlist[[i]][as.integer(a)]
        ae = as.integer((ae+3)/4)
        if ((pointsum + ae)>99|(pointsum + ae)<0){
          break
        }
        pointsum = pointsum + ae
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        playerlist = playerlist[c(2,3,4,1)]
      }
      
      if (length(u_card)>15){
        card = c(card,sample(u_card,size = length(u_card),replace = F))
        u_card = c()
      }
      cat("player",pl,"End of cards", "\n")
      cat("The current sum of the deck = ", pointsum, "\n")
      cat("Change the next one,player",names(playerlist)[i], "\n")
      if (grepl("c",pl)){
        ch = "Y"
      }
      else{
        ch = readline("Hand it over to the next player")
      }
      for (e in c(1:block)){
        cat("=====================================================", "\n")
      }
      cat("player",names(playerlist)[i], "\n")
      if (grepl("c",names(playerlist)[i])){
        ch = "Y"
      }
      else{
        ch = readline("Handed over")
      }
    }
    
    if (n == 4){
      n = n-1
      cat("player", names(playerlist)[i] ,"Make the sum of the deck not between 0~99", "\n")
      cat("player", names(playerlist)[i] ,"Weed out,",n,"players continue to play", "\n")
      loser = c(names(playerlist)[i],loser)
      playerlist[names(playerlist)[i]] = NULL
    }
    
    while (n==3){
      cat("The current sum of the deck = ", pointsum, "\n")
      cat("player", names(playerlist)[i] , "\n")
      cat(poker[playerlist[[i]]], "\n")
      pl = names(playerlist)[i]
      
      if (grepl("c",names(playerlist)[i])){
        if (pointsum>90){
          pc = playerlist[[i]][1:5]
          pc = pc %in% as.character(c(1,13:20,37:52))
          if (sum(pc)==0){
            a = sample(as.character(c(1:5)),size = 1,replace = F)
          }
          else{
            pc = which(pc)
            a = sample(as.character(pc),size = 1,replace = F)
          }
        }
        else{
          a = sample(as.character(c(1:5)),size = 1,replace = F)
        }
      }
      else{
        a = readline("Which card to play?")
      }
      
      if (!(a %in% c("Q","1","2","3","4","5"))){
        cat("Input error, leave the game", "\n")
        break
      }
      
      ########################################
      if (a=="Q"){
        cat("leave the game", "\n")
        break
      }
      ########################################
      
      ##b1
      if (playerlist[[i]][as.integer(a)] %in% c(1)){
        pointsum = 0
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        playerlist = playerlist[c(2,3,1)]
      }
      
      ##4
      else if (playerlist[[i]][as.integer(a)] %in% c(13:16)){
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        playerlist = playerlist[c(3,2,1)]
      }
      
      ##5
      else if (playerlist[[i]][as.integer(a)] %in% c(17:20)){
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        if (grepl("c",names(playerlist)[i])){
          a5 = sample(names(playerlist),size = 1,replace = F)
        }
        else{
          a5 = readline("Specify which player?")
        }
        pa5 = which(names(playerlist) == a5)
        playerlist = playerlist[c(pa5,
                                  ifelse((pa5+1)<=3,pa5+1,pa5-2),
                                  ifelse((pa5+2)<=3,pa5+2,pa5-1))]
      }
      
      ##J
      else if (playerlist[[i]][as.integer(a)] %in% c(41:44)){
        pointsum = pointsum
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        playerlist = playerlist[c(2,3,1)]
      }
      
      ##10
      else if (playerlist[[i]][as.integer(a)] %in% c(37:40)){
        if (grepl("c",names(playerlist)[i])){
          if (pointsum>89){
            a10 = "B"
          }
          else if (pointsum<10){
            a10 = "A"
          }
          else{
            a10 = sample(c("A","B"),size = 1,replace = F)
          }
        }
        else{
          a10 = readline("A:+10  B:-10 ?")
        }
        if (a10 =="A") a10 = 10
        if (a10 =="B") a10 = -10
        if ((pointsum + as.integer(a10))>99|(pointsum + as.integer(a10))<0){
          break
        }
        pointsum = pointsum + as.integer(a10)
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        playerlist = playerlist[c(2,3,1)]
      }
      
      ##Q
      else if (playerlist[[i]][as.integer(a)] %in% c(45:48)){
        if (grepl("c",names(playerlist)[i])){
          if (pointsum>79){
            a20 = "B"
          }
          else if (pointsum<20){
            a20 = "A"
          }
          else{
            a20 = sample(c("A","B"),size = 1,replace = F)
          }
        }
        else{
          a20 = readline("A:+20  B:-20 ?")
        }
        if (a20 =="A") a20 = 20
        if (a20 =="B") a20 = -20
        if ((pointsum + as.integer(a20))>99|(pointsum + as.integer(a20))<0){
          break
        }
        pointsum = pointsum + as.integer(a20)
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        playerlist = playerlist[c(2,3,1)]
      }
      
      ##K
      else if (playerlist[[i]][as.integer(a)] %in% c(49:52)){
        pointsum = 99
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        playerlist = playerlist[c(2,3,1)]
      }
      
      else{
        ae = playerlist[[i]][as.integer(a)]
        ae = as.integer((ae+3)/4)
        if ((pointsum + ae)>99){
          break
        }
        pointsum = pointsum + ae
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        playerlist = playerlist[c(2,3,1)]
      }
      
      if (length(u_card)>15){
        card = c(card,sample(u_card,size = length(u_card),replace = F))
        u_card = c()
      }
      cat("player",pl,"End of cards", "\n")
      cat("player = ", pointsum, "\n")
      cat("Change the next one,player",names(playerlist)[i], "\n")
      if (grepl("c",pl)){
        ch = "Y"
      }
      else{
        ch = readline("Hand it over to the next player")
      }
      for (e in c(1:block)){
        cat("=====================================================", "\n")
      }
      cat("player",names(playerlist)[i], "\n")
      if (grepl("c",names(playerlist)[i])){
        ch = "Y"
      }
      else{
        ch = readline("Handed over")
      }
    }
    
    if (n == 3){
      n = n-1
      cat("player", names(playerlist)[i] ,"Make the sum of the deck not between 0~99", "\n")
      cat("player", names(playerlist)[i] ,"Weed out",n,"players continue to play", "\n")
      loser = c(names(playerlist)[i],loser)
      playerlist[names(playerlist)[i]] = NULL
    }
    
    while (n==2){
      cat("The current sum of the deck = ", pointsum, "\n")
      cat("player", names(playerlist)[i] , "\n")
      cat(poker[playerlist[[i]]], "\n")
      pl = names(playerlist)[i]
      
      if (grepl("c",names(playerlist)[i])){
        if (pointsum>90){
          pc = playerlist[[i]][1:5]
          pc = pc %in% as.character(c(1,13:20,37:52))
          if (sum(pc)==0){
            a = sample(as.character(c(1:5)),size = 1,replace = F)
          }
          else{
            pc = which(pc)
            a = sample(as.character(pc),size = 1,replace = F)
          }
        }
        else{
          a = sample(as.character(c(1:5)),size = 1,replace = F)
        }
      }
      else{
        a = readline("Which card to play?")
      }
      
      if (!(a %in% c("Q","1","2","3","4","5"))){
        cat("Input error, leave the game", "\n")
        break
      }
      
      ########################################
      if (a=="Q"){
        cat("leave the game", "\n")
        break
      }
      ########################################
      
      ##b1
      if (playerlist[[i]][as.integer(a)] %in% c(1)){
        pointsum = 0
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        playerlist = playerlist[c(2,1)]
      }
      
      ##4
      else if (playerlist[[i]][as.integer(a)] %in% c(13:16)){
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        playerlist = playerlist[c(2,1)]
      }
      ##5
      else if (playerlist[[i]][as.integer(a)] %in% c(17:20)){
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        if (grepl("c",names(playerlist)[i])){
          a5 = sample(names(playerlist),size = 1,replace = F)
        }
        else{
          a5 = readline("Specify which player??")
        }
        pa5 = which(names(playerlist) == a5)
        playerlist = playerlist[c(pa5,
                                  ifelse((pa5+1)<=2,pa5+1,pa5-1))]
      }
      
      ##J
      else if (playerlist[[i]][as.integer(a)] %in% c(41:44)){
        pointsum = pointsum
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        playerlist = playerlist[c(2,1)]
      }
      
      ##10
      else if (playerlist[[i]][as.integer(a)] %in% c(37:40)){
        if (grepl("c",names(playerlist)[i])){
          if (pointsum>89){
            a10 = "B"
          }
          else if (pointsum<10){
            a10 = "A"
          }
          else{
            
            a10 = sample(c("A","B"),size = 1,replace = F)
          }
        }
        else{
          a10 = readline("A:+10  B:-10 ?")
        }
        if (a10 =="A") a10 = 10
        if (a10 =="B") a10 = -10
        if ((pointsum + as.integer(a10))>99|(pointsum + as.integer(a10))<0){
          break
        }
        pointsum = pointsum + as.integer(a10)
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        playerlist = playerlist[c(2,1)]
      }
      
      ##Q
      else if (playerlist[[i]][as.integer(a)] %in% c(45:48)){
        if (grepl("c",names(playerlist)[i])){
          if (pointsum>79){
            a20 = "B"
          }
          else if (pointsum<20){
            a20 = "A"
          }
          else{
            a20 = sample(c("A","B"),size = 1,replace = F)
          }
        }
        else{
          a20 = readline("A:+20  B:-20 ?")
        }
        if (a20 =="A") a20 = 20
        if (a20 =="B") a20 = -20
        if ((pointsum + as.integer(a20))>99|(pointsum + as.integer(a20))<0){
          break
        }
        pointsum = pointsum + as.integer(a20)
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        playerlist = playerlist[c(2,1)]
      }
      
      ##K
      else if (playerlist[[i]][as.integer(a)] %in% c(49:52)){
        pointsum = 99
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        playerlist = playerlist[c(2,1)]
      }
      
      else{
        ae = playerlist[[i]][as.integer(a)]
        ae = as.integer((ae+3)/4)
        if ((pointsum + ae)>99|(pointsum + ae)<0){
          break
        }
        pointsum = pointsum + ae
        u_card = c(u_card,playerlist[[i]][c(as.integer(a))])
        playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
        playerlist[[i]] = sort(c(playerlist[[i]],card[1]))
        card = card[-c(1)]
        playerlist = playerlist[c(2,1)]
      }
      
      if (length(u_card)>15){
        card = c(card,sample(u_card,size = length(u_card),replace = F))
        u_card = c()
      }
      cat("player",pl,"End of cards", "\n")
      cat("The current sum of the deck = ", pointsum, "\n")
      cat("Change the next one,player",names(playerlist)[i], "\n")
      if (grepl("c",pl)){
        ch = "Y"
      }
      else{
        ch = readline("Hand it over to the next player")
      }
      for (e in c(1:block)){
        cat("=====================================================", "\n")
      }
      cat("player",names(playerlist)[i], "\n")
      if (grepl("c",names(playerlist)[i])){
        ch = "Y"
      }
      else{
        ch = readline("Handed over")
      }
    }
    
    if (n==2){
      n = n-1
      cat("player", names(playerlist)[i] ,"Make the sum of the deck not between 0~99", "\n")
      cat("player", names(playerlist)[i] ,"Weed out", "\n")
      loser = c(names(playerlist)[i],loser)
      playerlist[names(playerlist)[i]] = NULL
      loser = c(names(playerlist)[i],loser)
    }
    
    cat("=======game over=======", "\n")
    cat("Last place:", "\n")
    for (w in c(1:length(loser))){
      cat(w,"place : player", loser[w],"\n")
    }
  }
}


twentyone = function(n_people = 2,counter = 100,block = 10){
  
  n = n_people
  
  if (!(n<=4&&n>=2)){
    cat("game limit 2~4 people", "\n")
  }
  
  if (n<=4&&n>=2){
    counter = rep(counter,n)
    
    poker = paste0(c(rep(c("spade","heart","diamond","club"),13)),
                   c(as.character(rep(1:10,each = 4)),rep(c("J","Q","K"),each = 4)))
    point = c(rep(1:10,each = 4),rep(10,each = 12))
    
    while (sum(counter<=0)<1){
      co = c()
      card = sample(1:52,size = 52,replace = F)
      
      player = c(paste0("player",as.character(c(1:n_people))),"Bank")
      card_1 = card[1:length(player)]
      card = card[-c(1:length(player))]
      card_2 = card[1:length(player)]
      card = card[-c(1:length(player))]
      df = data.frame("player" = player,
                      "counter" = c(counter,Inf),
                      "Hidden" = poker[card_1],
                      "card1" = poker[card_2],
                      "card2" = "NA",
                      "card3" = "NA",
                      "card4" = "NA",
                      "card5" = "NA",
                      "card6" = "NA",
                      "card7" = "NA",
                      "card8" = "NA",
                      "card9" = "NA",
                      "card10" = "NA")
      
      cat("Current situation on the table", "\n")
      w = 9
      sign_pad = stringr::str_pad(colnames(df), width = w, side = c("both"), pad = c("_"))
      cat(sign_pad, "\n")
      w = 9
      for (i in c(1:(n+1))){
        d = c(c(df[i,])[1:2],"*",c(df[i,])[4:13])
        sign_pad = stringr::str_pad(d, width = w, side = c("both"), pad = c("_"))
        cat(sign_pad, "\n")
      }
      
      cat("Now every player will check their hidden cards", "\n")
      for (i in c(1:length(player))){
        ch = readline(player[i])
        cat(player[i],"Hidden is",df[i,3], "\n")
        cat(player[i],"card1 is",df[i,4], "\n")
        if (i==length(player)){
          cat("After watching, start the game")
        }
        else{
          co = c(co,readline("How much to bet"))
          ch = readline("After watching, change to the next one")
        }
        for (e in c(1:block)){
          cat("=====================================================", "\n")
        }
      }
      
      cat("Now the game starts", "\n")
      for (p in c(1:length(player))){
        if (p==length(player)){
          cat(player[p],"Start to get cards", "\n")
          cat("Your have",c("*",df[p,4]), "\n")
          ta = 0
          tac = c()
          po = which(poker %in% c(df[p,3],df[p,4]))
          s = sum(point[po])
          while (s<17){
            y = "Y"
            if (y == "Y"){
              cat(player[p],"You got",poker[card[1]], "\n")
              s = s + point[card[1]]
              tac = c(tac,poker[card[1]])
              card = card[-c(1)]
              ta = ta + 1
            }
            else{
              break
            }
          }
          if (ta!=0){
            df[p,5:(5-1+ta)] = tac
          }
          
          sign_pad = stringr::str_pad(colnames(df), width = w, side = c("both"), pad = c("_"))
          cat(sign_pad, "\n")
          for (i in c(1:(n+1))){
            d = c(c(df[i,])[1:2],"*",c(df[i,])[4:13])
            sign_pad = stringr::str_pad(d, width = w, side = c("both"), pad = c("_"))
            cat(sign_pad, "\n")
          }
        }
        else{
          cat(player[p],"Start to get cards", "\n")
          cat("Your have",c("*",df[p,4]), "\n")
          ta = 0
          tac = c()
          while (ta<10){
            y = readline("Do you want to get another card?? Y:Yes N:No")
            if (y == "Y"){
              cat(player[p],"You got",poker[card[1]], "\n")
              tac = c(tac,poker[card[1]])
              card = card[-c(1)]
              ta = ta + 1
            }
            else{
              break
            }
          }
          if (ta!=0){
            df[p,5:(5-1+ta)] = tac
          }
          
          sign_pad = stringr::str_pad(colnames(df), width = w, side = c("both"), pad = c("_"))
          cat(sign_pad, "\n")
          for (i in c(1:(n+1))){
            d = c(c(df[i,])[1:2],"*",c(df[i,])[4:13])
            sign_pad = stringr::str_pad(d, width = w, side = c("both"), pad = c("_"))
            cat(sign_pad, "\n")
          }
        }
      }
      cat("Show hidden cards and count points", "\n")
      cat("Current situation on the table", "\n")
      sign_pad = stringr::str_pad(colnames(df), width = w, side = c("both"), pad = c("_"))
      cat(sign_pad, "\n")
      for (i in c(1:(n+1))){
        d = c(c(df[i,])[1:13])
        sign_pad = stringr::str_pad(d, width = w, side = c("both"), pad = c("_"))
        cat(sign_pad, "\n")
      }
      cat("The sum of the Bank's points is",s, "\n")
      sc = c()
      for (p in c(1:(length(player)-1))){
        cat(player[p],"has:",c(df[p,3],df[p,4],df[p,5],df[p,6],df[p,7],df[p,8],
                              df[p,9],df[p,10],df[p,11],df[p,12],df[p,13]), "\n")
        ss = readline("The sum of points is")
        sc = c(sc, ss)
      }
      
      if (sum(sc %in% c("21"))>0){
        for(p in c(1:length(sc))){
          if (sc[p]=="21" && df[p,5]=="NA"){
            cat(player[p],"achieve BlackJack", "\n")
            cat("gave",player[p],"double the bet", "\n")
            df[p,2] = df[p,2] + 2*as.numeric(co[p])
            sc[p] = "50"
          }
        }
      }
      
      if (s<=21){
        cat("The Bank has not Bust", "\n")
        cat("Confiscate the bets of the Bust players and point smaller than Bank", "\n")
        cat("Pay one bet to players who have not Bust and points bigger than Bank", "\n")
        cat("If the points are the same as Bank, Tie, not confiscate the bets", "\n")
        for (p in c(1:length(sc))){
          ss = as.numeric(sc[p])
          if (ss==50){
            next
          }
          if (ss<=21&&ss>s){
            df[p,2] = df[p,2] + as.numeric(co[p])
          }
          else if (ss>21|ss<s){
            df[p,2] = df[p,2] - as.numeric(co[p])
          }
          else{
            df[p,2] = df[p,2]
          }
        }
      }
      else if (s>21){
        cat("The Bank Bust", "\n")
        cat("Confiscate the bets of the Bust players", "\n")
        cat("Pay one bet to players who have not Bust", "\n")
        for (p in c(1:length(sc))){
          ss = as.numeric(sc[p])
          if (ss<=21){
            df[p,2] = df[p,2] + as.numeric(co[p])
          }
          else{
            df[p,2] = df[p,2] - as.numeric(co[p])
          }
        }
      }
      
      cat("Current situation on the table", "\n")
      sign_pad = stringr::str_pad(colnames(df), width = w, side = c("both"), pad = c("_"))
      cat(sign_pad, "\n")
      for (i in c(1:(n+1))){
        d = c(c(df[i,])[1:13])
        sign_pad = stringr::str_pad(d, width = w, side = c("both"), pad = c("_"))
        cat(sign_pad, "\n")
      }
      
      cat("This round of the game is over", "\n")
      y = readline("Whether to start the next round? Y:Yes N:No")
      if (y=="Y"){
        for (p in c(1:length(sc))){
          counter[p] = df[p,2]
          if (counter[p]<=0){
            cat(player[p],"bankrupted", "\n")
          }
        }
        if (sum(counter<=0)>=1){
          cat("End Game", "\n")
          break
        }
      }
      else{
        cat("End Game", "\n")
        break
      }
    }
  }
}