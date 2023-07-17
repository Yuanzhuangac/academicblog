---
title: Import RMarkdown in Blogs
date: 2023-07-18
authors:
  - admin
tags:
  - Wowchemy
  - RMarkdown
  - Markdown
categories:
  - Wowchemy
  - Markdown
  - RMarkdown 
math: true
image:
  caption: ''
  focal_point: ''
---

死亡率的预测一直是保险精算行业非常关注的事情。在前面的课中我们已经学习了如何使用Lee-Carter、CBD和二维P样条进行死亡率的拟合，但如何使用这些模型对未来的死亡率进行预测呢？下面我将分别使用之前学过的模型进行死亡率的预测。

    library(gnm) # 做Lee-Carter的相关包
    library(astsa) # 时间序列分析的相关包
    library(svcm)
    library(MortalitySmooth) # 死亡率修匀的关键包

# 1. 数据准备

这一次需要用到的数据是1961-2013年，英国50-90岁男性的数据。数据来自HMD（Human
Mortality
Database）。数据已经下载到本地了，是个txt文件。首先，我们定义一下`Read.HMD`这个函数，它可以自动读取我们下载来的txt文件。代码在下一面，我参考了*Modelling
Mortality with Actuarial Applications*这本书。

    check.integer <- function(x) {
        x == round(x)
    }
    Read.HMD <- function(file){
      x <- scan(file, skip = 3, what = "character") 
      x <- matrix(x, ncol = 5, byrow = T)
      #   删除110岁以上的数据
      Select <- rep(1, nrow(x))
      Select[check.integer((1:nrow(x))/111)] <- 0
      x <- x[Select == 1, ]
      x <- matrix(as.numeric(c(x)), ncol = 5)
      #   把年份找出来，分成男女
      Age <- 0:109
      Year <- min(x[ ,1]) : max(x[ ,1])
      Female <- x[ ,3]
      Female.Matrix <- matrix(Female, nrow = 110)
      dimnames(Female.Matrix) <- list(Age, Year)
      Male <- x[ ,4]
      Male.Matrix <- matrix(Male, nrow = 110)
      dimnames(Male.Matrix) <- list(Age, Year)
      return(list(Age = Age, Year = Year, Female.Matrix = Female.Matrix,
                  Male.Matrix = Male.Matrix))
    }

我们利用刚才编写的函数，把数据读进来，找到想要的年份和年龄后，计算一些常用的变量：

    Death.Data <-  Read.HMD("C:\\Users\\lenovo\\Desktop\\lt\\leecarter\\Lee-Carter Application\\HMD_UK_Deaths_2013.txt")
    Exposure.Data = Read.HMD("C:\\Users\\lenovo\\Desktop\\lt\\leecarter\\Lee-Carter Application\\HMD_UK_Exposures_2013.txt")
    # 年龄和年份，一个是所有年龄，一个是目标年龄
    Age <-  Death.Data$Age
    Year <-  Death.Data$Year
    AGE <-  50:90
    YEAR <-  1961:2013
    # 1961-2013年，英国50-90岁男性
    Dth <-  Death.Data$Male.Matrix[(49 < Age) & (Age < 91),
    (1960 < Year) & (Year < 2014)]
    Exp <-  Exposure.Data$Male.Matrix[(49 < Age) & (Age < 91),
    (1960 < Year) & (Year < 2014)]
    # Obs是中心死亡率取对数
    Obs <-  log(Dth/Exp)
    # 取得死亡率矩阵的维度
    n.x <-  nrow(Dth)
    n.y <-  ncol(Dth)

# 2. 使用时间序列方法做Lee-Carter模型的预测

## 2.1 参数估计

Lee-Carter模型同时对年龄和时间这两个影响死亡率的因素进行建模，相关假设如下：