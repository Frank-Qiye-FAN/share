# 代码由蕃薯叶同学编写。635614385@qq.com
# counts的表达矩阵进行log2(CPM+1)和DESeq2 VST转换


# # 安装必要包（如果未安装）
# if (!requireNamespace("readxl", quietly = TRUE)) {
#   install.packages("readxl")
# }
# if (!requireNamespace("DESeq2", quietly = TRUE)) {
#   if (!requireNamespace("BiocManager", quietly = TRUE)) {
#     install.packages("BiocManager")
#   }
#   BiocManager::install("DESeq2")
# }

library(readxl)
library(DESeq2)

# 1. 读取Excel文件
count_data <- read_excel("no_IR.xlsx", sheet = 1)

# 将第一列作为行名，转换为矩阵
rownames(count_data) <- count_data[[1]]
count_matrix <- as.matrix(count_data[, -1])

# 确保数据为整数（DESeq2要求）
count_matrix <- apply(count_matrix, 2, function(x) as.integer(round(x)))

# 2. log2(CPM+1) 转换
log2cpm_transform <- function(count_matrix) {
  # 计算CPM
  cpm <- apply(count_matrix, 2, function(x) (x/sum(x)) * 1e6)
  # log2转换并加1防止取log(0)
  log2cpm <- log2(cpm + 1)
  return(log2cpm)
}

log2cpm_matrix <- log2cpm_transform(count_matrix)

# 3. DESeq2 VST 转换
deseq2_vst_transform <- function(count_matrix) {
  # 创建DESeqDataSet对象
  dds <- DESeqDataSetFromMatrix(
    countData = count_matrix,
    colData = data.frame(sample = colnames(count_matrix)),
    design = ~ 1  # 使用最简单的设计公式
  )
  
  # 运行VST转换（自动估算离散度和大小因子）
  vsd <- vst(dds, blind = TRUE)
  
  # 提取转换后的矩阵
  return(assay(vsd))
}

vst_matrix <- deseq2_vst_transform(count_matrix)

# 查看结果
head(log2cpm_matrix[, 1:5])  # 查看前5个样本的log2CPM结果
head(vst_matrix[, 1:5])      # 查看前5个样本的VST结果
