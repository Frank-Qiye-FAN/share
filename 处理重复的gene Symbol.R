#R脚本由蕃薯叶同学编写, email: 635614385@qq.com

#在使用GEO数据库进行差异表达分析时，可能会遇到基因符号（Gene Symbol）重复的情况。
#这通常是因为同一个基因可能对应多个探针或条目。
#处理这些重复项的常见方法包括选取表达最强的探针、取平均值或合并表达值。

# 假设你的数据框名为 df，包含 GeneSymbol 和 expression_value 列
#(后面是expression_value 不只一列的情况，需在Excel里手动更改样本名为sample1 to samplen
#或手动输入样本列名)

rm(list = ls())#清空
# 安装dplyr包（如果尚未安装）
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
# 加载dplyr包
library(dplyr)

# 示例数据框
# df <- data.frame(
#   GeneSymbol = c("GeneA", "GeneA", "GeneB", "GeneC", "GeneC", "GeneC"),
#   expression_value = c(1.2, 2.3, 3.4, 4.5, 5.6, 6.7)
# )
#读取表格
all<-read.table("data肾小球内皮细胞高糖模型GSE220228.txt",sep="\t",header=TRUE)

# 方法1：选取表达最强的探针
df_max <- df %>%
  group_by(GeneSymbol) %>%
  slice(which.max(expression_value))

# 方法2：取平均值
df_mean <- df %>%
  group_by(GeneSymbol) %>%
  summarise(mean_expression = mean(expression_value, na.rm = TRUE))

# 方法3：合并表达值（例如，取中位数）
df_median <- df %>%
  group_by(GeneSymbol) %>%
  summarise(median_expression = median(expression_value, na.rm = TRUE))

# 查看结果
print("Max Expression:")
print(df_max)
print("Mean Expression:")
print(df_mean)
print("Median Expression:")
print(df_median)
###########################################################################

#(expression_value 不只一列的情况，需在Excel里手动更改样本名为sample1 to samplen
#或手动输入样本列名)

# 安装dplyr包（如果尚未安装）
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
rm(list = ls())
# 加载dplyr包
library(dplyr)

# # 示例数据框
# df <- data.frame(
#   GeneSymbol = c("GeneA", "GeneA", "GeneB", "GeneC", "GeneC", "GeneC"),
#   Sample1 = c(1.2, 2.3, 3.4, 4.5, 5.6, 6.7),
#   Sample2 = c(2.2, 3.3, 4.4, 5.5, 6.6, 7.7),
#   Sample3 = c(3.2, 4.3, 5.4, 6.5, 7.6, 8.7)
# )

#读取表格

df<-read.table("data肾小球内皮细胞高糖模型GSE220228.txt",sep="\t",header=TRUE)


# 方法1：选取表达最强的探针
df_max <- df %>%
  group_by(GeneSymbol) %>%
  slice(which.max(rowSums(across(starts_with("Sample")))))

# 方法2：取平均值
df_mean <- df %>%
  group_by(GeneSymbol) %>%
  summarise(across(starts_with("Sample"), mean, na.rm = TRUE))

# 方法3：合并表达值（例如，取中位数）
df_median <- df %>%
  group_by(GeneSymbol) %>%
  summarise(across(starts_with("Sample"), median, na.rm = TRUE))

# 查看结果
print("Max Expression:")
print(df_max)
print("Mean Expression:")
print(df_mean)
print("Median Expression:")
print(df_median)

#输出处理结果（Max/Mean/Median）
write.table(df_mean,file="取平均值法去重.txt",sep="\t",row.names=F,quote=F)



#方法1：选取表达最强的探针
# #优点:
#   
#   简单直接。
# 保留了最高表达值的探针，可能代表最活跃的基因表达。
# 缺点:
#   
#   可能忽略了其他探针的信息。
# 如果最高表达值的探针是噪音或异常值，可能导致误导结果。
# 适用情景:
#   
#   你对单一最强表达的探针最感兴趣。
# 数据中探针的表达值分布较为均匀，且没有明显的异常值。
# 方法2：取平均值
# 优点:
#   
#   综合了所有探针的信息，减少单个探针异常值的影响。
# 能够更好地代表基因的整体表达水平。
# 缺点:
#   
#   如果探针表达值差异较大，平均值可能不代表实际情况。
# 可能会稀释某些重要的表达信息。
# 适用情景:
#   
#   你希望综合所有探针的信息，得到一个代表性的表达值。
# 数据中探针的表达值差异不是特别大。
# 方法3：合并表达值（例如，取中位数）
# 优点:
#   
#   中位数能够减少极端值（异常值）的影响。
# 能够更稳健地代表基因的表达水平。
# 缺点:
#   
#   可能会忽略一些高表达值的信息。
# 对于表达值分布较为均匀的数据，中位数和平均值可能没有明显差异。
# 适用情景:
#   
#   数据中存在一些极端值或噪音，你希望降低这些值的影响。
# 你希望得到一个稳健的代表性表达值。
#
# 总结和建议
# 如果你的数据中探针表达值差异较大，且你对最高表达值最感兴趣，可以选择方法1（选取表达最强的探针）。
# 如果你希望综合所有探针的信息，得到一个代表性的表达值，可以选择方法2（取平均值）。
# 如果你担心数据中存在极端值或噪音，希望得到一个稳健的代表性表达值，可以选择方法3（取中位数）。