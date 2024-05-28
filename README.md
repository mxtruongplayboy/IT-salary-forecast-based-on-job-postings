# IT-salary-forecast-based-on-job-postings

# Bước 1: Crawl dữ liệu
- Vào file src/crawl.ipynb, chạy lần lượt các cell từ trên xuống theo hướng dẫn của các markdown đó
- Có 2 lựa chọn
+ Chỉ crawl 1 trang duy nhất, cần nhập tham số page. Ví dụ: https://www.topcv.vn/viec-lam-it?page=1
+ Crawl toàn bộ các trang của trang TopCV có url: https://www.topcv.vn/viec-lam-it
- Cuối cùng file dữ liệu thô được lưu vào file data/raw data.csv
# Bước 2: Tiến hành làm sạch và chuẩn hóa dữ liệu
- Mở file src/Data-Cleaning-Processing.ipynb, chạy toàn bộ các cell từ trên xuống dưới
- Sau đó dữ liệu đã được làm sạch lưu vào file data/clean_data.csv
# Bước 3: Trực quan hóa đa biến và đánh giá, phân tích bài toán
- Mở file src/presentation.ipynb, chạy tất cả các cell từ trên xuống dưới
# Bước 4: 
- Đối với bài toán Regression
+ Mở file src/Data-Science-Analysis-Regression.ipynb, chạy tất cả các cell từ trên xuống dưới

- Đối với bài toán Clustering
+ Mở file src/Data-Science-Analysis-K means.ipynb, chạy tất cả các cell từ trên xuống dưới

- Đối với bài toán Clustering sử dụng poLCA, python không có thư viện chuyên dụng để sử dụng thư viện poLCA nên phải sử dụng ngôn ngữ R
+ Cài đặt R enviroment và R studio
+ Install các package
+ Cấu hình lại root path
+ Run chương trình