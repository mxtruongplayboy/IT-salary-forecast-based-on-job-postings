# Tải các thư viện cần thiết
library(poLCA)
library(cluster)
library(ggplot2)
library(gridExtra)

# Đọc dữ liệu từ file CSV
seg.raw <- read.csv("~/clean_data_for_lca.csv")

# Sao chép dữ liệu gốc vào một dataframe mới
seg.df <- seg.raw

# Hiển thị 6 dòng đầu tiên của dataframe để kiểm tra
head(seg.df)

# Hàm tóm tắt dữ liệu theo nhóm
seg.summ <- function(data, groups) {
  # Tạo dataframe kết quả với nhóm
  result <- aggregate(. ~ groups, data, function(x) {
    if (is.factor(x) || is.character(x)) {
      # Đối với biến danh mục, trả về giá trị mode (phổ biến nhất)
      return(names(sort(table(x), decreasing = TRUE))[1])
    } else {
      # Đối với biến số, trả về giá trị trung bình
      return(mean(as.numeric(x), na.rm = TRUE))
    }
  })
  return(result)
}

# Hiển thị tóm tắt dữ liệu ban đầu
summary(seg.df)

# Tạo một bản sao của dataframe để chỉnh sửa
seg.df.cut <- seg.df

# Chuyển đổi cột 'Level' thành factor với giá trị 1 nếu nhỏ hơn median, ngược lại là 2
seg.df.cut$Level <- factor(ifelse(seg.df$Level < median(seg.df$Level), 1, 2))

# Chuyển đổi cột 'Year.of.Experience' thành factor với giá trị 1 nếu nhỏ hơn median, ngược lại là 2
seg.df.cut$Year.of.Experience <- factor(ifelse(seg.df$Year.of.Experience < median(seg.df$Year.of.Experience), 1, 2))

# Chuyển đổi các cột khác thành factor
seg.df.cut$Requirement.Language <- factor(seg.df.cut$Requirement.Language)
seg.df.cut$Description.Language <- factor(seg.df.cut$Description.Language)
seg.df.cut$Location <- factor(seg.df.cut$Location)
seg.df.cut$Programming.Language <- factor(seg.df.cut$Programming.Language)

# Hiển thị tóm tắt dữ liệu sau khi chỉnh sửa
summary(seg.df.cut)

# Xác định công thức cho mô hình LCA
seg.f <- with(seg.df.cut, cbind(Level, Year.of.Experience, Programming.Language, Requirement.Language, Description.Language, Location) ~ 1)

# Đặt seed để đảm bảo tính tái lập của kết quả
set.seed(09999)

# Áp dụng mô hình Phân tích lớp tiềm ẩn (LCA) với số lớp là 3
seg.LCA3 <- poLCA(seg.f, data=seg.df.cut, nclass=3)

# Tóm tắt dữ liệu theo các lớp dự đoán
seg.summ(seg.df, seg.LCA3$predclass)

clusplot(seg.df, seg.LCA3$predclass, color=TRUE, shade=TRUE, labels=4, lines=0, main="LCA plot (K=3)")
