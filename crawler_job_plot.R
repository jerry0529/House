library(ggplot2)
library(wordcloud2)
table <-read.csv("C:\\Users\\BIG DATA\\Desktop\\爬蟲結果.csv",header = T)

#建立畫布，x軸為lan重新以count大到小排序，y軸為count
gp <- ggplot(table,aes(x = reorder(table$prog, -table$count), y = table$count))
#建立長條圖，因資料關係，一筆資料只有一個數字，所以用identity
gp+geom_bar(stat="identity")
#建立文字雲，size最大的字，minSize最小的字，gridSize字的間距，color字的顏色，backC背景色
wordcloud2(table, size = 1.5, minSize = 18, gridSize = 20, color = 'random-light', backgroundColor = '#888' )


