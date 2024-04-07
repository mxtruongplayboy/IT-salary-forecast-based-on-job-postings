# IT-salary-forecast-based-on-job-postings

# Bước 1: Crawl dữ liệu
- Vào file src/crawl.ipynb, chạy lần lượt các cell từ trên xuống theo hướng dẫn của các markdown đó
- Có 2 lựa chọn
+ Chỉ crawl 1 trang duy nhất, cần nhập tham số page. Ví dụ: https://www.topcv.vn/viec-lam-it?page=1
+ Crawl toàn bộ các trang của trang TopCV có url: https://www.topcv.vn/viec-lam-it
- Cuối cùng file dữ liệu thô được lưu vào file data/raw data.csv
# Bước 2: Tiến hành làm sạch và chuẩn hóa dữ liệu
- Mở file src/data_cleaning_processing.ipynb, chạy toàn bộ các cell từ trên xuống dưới
- Sau đó dữ liệu đã được làm sạch lưu vào file data/clean data.csv
# Bước 3: Trực quan hóa đa biến và đánh giá, phân tích bài toán
- Mở file src/presentation.ipynb, chạy tất cả các cell từ trên xuống dưới