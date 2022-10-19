##------Names-----------
## Jialong He (s2281875)
## Linshen Shu (s2317223)
## Heyu Nie (s2404675)

##---Address of github repo-----
# https://github.com/Yancy0517/practical-2.git
 
##----Team member contribution----
## Linshen was responsible for first 2 parts(35%); 
## Jialong for part 3 to part 5(35%)
## Heyu for last part and debugging and overall comments(30%)






find_one <- function(card, n, k, strategy){#单独一个函数为后面铺垫，主要用来算单个犯人是否能成功摸到自己的号码
  
  
  
  success <- 0 #初始成功次数为0，成功摸到号码就为1
  if(strategy==1){ #第一个策略
    box <- k #第一个盒子选k
    j <- 1 #初始化循环次数
    while (j <= n) { #循环次数不能超过n
      if(card[box]==k){ #第k或者第box个箱子的号码，抽到的话成功次数加1 然后退出while循环
        success <- 1
        break
      }
      else{
        box <- card[box] # 第box个盒子里卡片的号码等于下一个要找的盒子的编号
        j = j+1 #循环次数+1（不能超过n次）
      }  
    }
  }else if(strategy == 2){ #第二个策略 
    box <- sample(1:(2*n), 1) # 1到2n中随机选一个盒子，后面部分和策略1一样
    j <- 1
    while (j <= n) {
      if(card[box] == k){
        success <- 1
        break
      }
      else{
        box <- card[box]
        j = j+1
      }  
    }
  }else{ #第三个策略
    box <- sample(1:(2*n), n) #从1到2n随机抽n个盒子，囚犯看n个盒子中有没有自己编号的卡片
    for(i in 1:n){
      one_box <- box[i] #取n个里的第一个box，一个一个比对是否相等
      if(card[one_box] == k){
        success <- 1
        break
      }
    }
  }
  return(success)
}
##-----(1)-----###
Pone <- function(n, k, strategy, nreps){#第k个犯人摸到自己号码的概率function
  success <- 0 #成功次数
  #dn <- 2*n #2n
  #box <- 1:dn #盒子
  card <- 1:(2*n) #纸条
  for(i in 1:nreps){
    ncard <- sample(card,length(card))#打乱卡片
    #bwc <- cbind(box,ncard) #把卡片放到盒子里，一个盒子的编号对应一个卡片的号码
    success =success + find_one(ncard, n, k, strategy)
  }
  return(success/nreps)
}
Pone(5, 1, 1, 10000);Pone(50, 1, 1, 10000)
Pone(5, 1, 2, 10000);Pone(50, 1, 2, 10000)
Pone(5, 1, 3, 10000);Pone(50, 1, 3, 10000)

##-----(2)-----###
Pall <- function(n, strategy, nreps){ #所有犯人都摸到自身号码的概率function
  success <- 0
  #box <- 1:(2*n) #盒子
  card <- 1:(2*n) #纸条
  for(i in 1:nreps){
    ncard <- sample(card,length(card)) #随机洗牌
    #bwc <- cbind(box,ncard) #盒子序号和里面的卡片号码两列合并，一一对应
    for(j in 1:(2*n)){
      if(find_one(ncard, n, j, strategy)!=1){
        break
      }else if( j == n && find_one(ncard, n, j, strategy) == 1){
        success= 1 + success
      }
    }
  }
  return(success/nreps)
}
Pall(5, 1, 10000);Pall(50, 1, 10000);Pall(100, 1, 10000)
Pall(5, 2, 10000);Pall(50, 2, 10000)
Pall(5, 3, 10000);Pall(50, 3, 10000)
#令人感到惊奇的是，策略1有高达百分之30多的概率，而其他两种策略概率为0，
#而且不论犯人个数如何变化，概率变化不大，我们猜概率趋近于0.3


##----(5)----###
dloop <- function(n, nreps){
  prob_matrix <- matrix(0, nrow = nreps, ncol = 2*n) #概率矩阵（维数是nreps X 2n），每一行是一种模拟情况
  for (i in 1:nreps) {
    ncard <- sample(1:(2*n), 2*n) #随机洗牌
    for (j in 1:(2*n)) {
      box <- j #盒子序号（用于更新），从j=1，1号犯人开1号盒子开始
      depth <- 1
      while (depth <= 2*n) { #深度最多为2n
        if(ncard[box] == j){
          prob_matrix[i, depth] = 1 #第i行的对应的深度那列计数1
          break
        }else{
          box = ncard[box] #去找下一个盒子，编号为上一个盒子里的卡片号码
          depth = depth + 1 #深度加1
        }
      }
    }
  }
  prob <- colSums(prob_matrix)/nreps #将概率矩阵按列加总，比如第一列的nreps行加总，第二列...，最后除于总模拟次数
  return(prob)
}
y <- dloop(50, 10000) #纵轴的值
x <- 1:100 #横轴的值
# 每一个x对应一个y，每一个循环长度，对应它的概率，生成一个plot
plot(x, y, xlab = "Length of loop", ylab = "Probabilities", pch = 20,
     main = "Probabilities of the length of each loop")

##----(6)-----##
dloop1 <- function(n, nreps){#与第五题大致相同，主要用于统计循环长度小于等于50的情况，任何大于50的情况都不考虑
  prob_matrix <- matrix(0, nrow = nreps, ncol = 2*n) #概率矩阵
  box <- 1:(2*n) #盒子
  b <- 0 #循环长度小于等于50的计数器
  for (i in 1:nreps) {
    ncard <- sample(1:(2*n), 2*n) #随机洗牌
    for (j in 1:(2*n)) {
      box <- j
      depth <- 1
      while (depth <= 2*n) {
        if(ncard[box] == j){
          prob_matrix[i, depth] = 1
          break
        }else{
          box = ncard[box]
          depth = depth + 1
        }
      }
    }
    if(sum(prob_matrix[i, (n+1):(2*n)]) == 0){ #51-100全为0时
      b <- b + 1 #计数器+1
    }
  }
  return(b/nreps)
}
dloop1(50, 10000)
