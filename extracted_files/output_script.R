## ----setup, include=FALSE----------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----------------------------------------------------------------------------------------------------
# Loading necessary libraries - Gerekli kütüphaneleri yüklüyoruz
library(dplyr)
library(ggplot2)


## ----------------------------------------------------------------------------------------------------
# Reading the datasets - Veri setlerini okuyoruz
order_payments <- read.csv("olist_order_payments_dataset.csv")
orders <- read.csv("olist_orders_dataset.csv")
products <- read.csv("olist_products_dataset.csv")
product_categories <- read.csv("product_category_name_translation.csv")
customers <- read.csv("olist_customers_dataset.csv")
geolocation <- read.csv("olist_geolocation_dataset.csv")
order_items <- read.csv("olist_order_items_dataset.csv")
order_reviews <- read.csv("olist_order_reviews_dataset.csv")
sellers <- read.csv("olist_sellers_dataset.csv")



## ----------------------------------------------------------------------------------------------------
# Displaying first few rows of each dataset - Her veri setinin ilk birkaç satırını gösteriyoruz
head(order_payments)
head(orders)
head(products)
head(product_categories)
head(customers)
head(geolocation)
head(order_items)
head(order_reviews)
head(sellers)


## ----------------------------------------------------------------------------------------------------
# Checking the structure of the 'orders' dataset - 'orders' veri setinin yapısını kontrol ediyoruz
str(orders)

# Summary statistics of 'orders' - 'orders' veri seti için özet istatistikler
summary(orders)



## ----------------------------------------------------------------------------------------------------
## Checking for missing values in all datasets

# Checking for missing values in the 'order_payments' dataset
sapply(order_payments, function(x) sum(is.na(x)))

# Checking for missing values in the 'orders' dataset
sapply(orders, function(x) sum(is.na(x)))

# Checking for missing values in the 'products' dataset
sapply(products, function(x) sum(is.na(x)))

# Checking for missing values in the 'product_categories' dataset
sapply(product_categories, function(x) sum(is.na(x)))

# Checking for missing values in the 'customers' dataset
sapply(customers, function(x) sum(is.na(x)))

# Checking for missing values in the 'geolocation' dataset
sapply(geolocation, function(x) sum(is.na(x)))

# Checking for missing values in the 'order_items' dataset
sapply(order_items, function(x) sum(is.na(x)))

# Checking for missing values in the 'order_reviews' dataset
sapply(order_reviews, function(x) sum(is.na(x)))

# Checking for missing values in the 'sellers' dataset
sapply(sellers, function(x) sum(is.na(x)))




## ----------------------------------------------------------------------------------------------------
# Filling missing values with the mean for numeric columns in 'products'
# Sayısal sütunlar için eksik değerleri ortalama ile dolduruyoruz
products$product_weight_g[is.na(products$product_weight_g)] <- mean(products$product_weight_g, na.rm = TRUE)
products$product_length_cm[is.na(products$product_length_cm)] <- mean(products$product_length_cm, na.rm = TRUE)
products$product_height_cm[is.na(products$product_height_cm)] <- mean(products$product_height_cm, na.rm = TRUE)
products$product_width_cm[is.na(products$product_width_cm)] <- mean(products$product_width_cm, na.rm = TRUE)

# Dropping rows with missing values for 'product_name_lenght', 'product_description_lenght', and 'product_photos_qty'
# 'product_name_lenght', 'product_description_lenght', ve 'product_photos_qty' sütunlarındaki eksik satırları siliyoruz
products <- products[!is.na(products$product_name_lenght), ]
products <- products[!is.na(products$product_description_lenght), ]
products <- products[!is.na(products$product_photos_qty), ]



## ----------------------------------------------------------------------------------------------------
# Rechecking for missing values in all datasets after cleaning

# Checking for missing values in the 'order_payments' dataset
sapply(order_payments, function(x) sum(is.na(x)))

# Checking for missing values in the 'orders' dataset
sapply(orders, function(x) sum(is.na(x)))

# Checking for missing values in the 'products' dataset
sapply(products, function(x) sum(is.na(x)))

# Checking for missing values in the 'product_categories' dataset
sapply(product_categories, function(x) sum(is.na(x)))

# Checking for missing values in the 'customers' dataset
sapply(customers, function(x) sum(is.na(x)))

# Checking for missing values in the 'geolocation' dataset
sapply(geolocation, function(x) sum(is.na(x)))

# Checking for missing values in the 'order_items' dataset
sapply(order_items, function(x) sum(is.na(x)))

# Checking for missing values in the 'order_reviews' dataset
sapply(order_reviews, function(x) sum(is.na(x)))

# Checking for missing values in the 'sellers' dataset
sapply(sellers, function(x) sum(is.na(x)))



## ----------------------------------------------------------------------------------------------------
# Structure and summary statistics of the orders dataset
# 'orders' veri setinin yapısı ve özet istatistikleri
str(orders)
summary(orders)

# Structure and summary statistics of the products dataset
# 'products' veri setinin yapısı ve özet istatistikleri
str(products)
summary(products)



## ----------------------------------------------------------------------------------------------------
# Payment method distribution
# Ödeme yöntemi dağılımı
payment_method_counts <- table(order_payments$payment_type)
barplot(payment_method_counts, main='Payment Method Distribution', xlab='Payment Type', ylab='Number of Orders')



## ----------------------------------------------------------------------------------------------------
# Convert 'order_purchase_timestamp' to Date format
# 'order_purchase_timestamp' tarih formatına çeviriyoruz
orders$order_purchase_timestamp <- as.Date(orders$order_purchase_timestamp)

# Group orders by month and count them
# Siparişleri ay bazında gruplayıp sayıyoruz
monthly_orders <- table(format(orders$order_purchase_timestamp, "%Y-%m"))

# Plotting the monthly order trend
# Aylık sipariş trendini çiziyoruz
plot(monthly_orders, type='o', main='Monthly Order Trend', xlab='Month', ylab='Number of Orders')



## ----------------------------------------------------------------------------------------------------
# Average payment value by payment type
# Ödeme türüne göre ortalama ödeme değerini hesaplıyoruz
average_payment_value <- aggregate(payment_value ~ payment_type, data=order_payments, FUN=mean)

# Barplot for average payment value
# Ortalama ödeme değerini gösteren çubuk grafiği
barplot(average_payment_value$payment_value, names.arg=average_payment_value$payment_type, main='Average Payment Value by Payment Type', xlab='Payment Type', ylab='Average Payment Value')



## ----------------------------------------------------------------------------------------------------
# Structure and summary statistics of the orders dataset
# 'orders' veri setinin yapısı ve özet istatistikleri
str(orders)
summary(orders)



## ----------------------------------------------------------------------------------------------------
# Structure and summary statistics of the products dataset
# 'products' veri setinin yapısı ve özet istatistikleri
str(products)
summary(products)



## ----------------------------------------------------------------------------------------------------
# Payment method distribution
# Ödeme yöntemi dağılımı
payment_method_counts <- table(order_payments$payment_type)
barplot(payment_method_counts, main='Payment Method Distribution', xlab='Payment Type', ylab='Number of Orders')



## ----------------------------------------------------------------------------------------------------
# Convert 'order_purchase_timestamp' to Date format
# 'order_purchase_timestamp' tarih formatına çeviriyoruz
orders$order_purchase_timestamp <- as.Date(orders$order_purchase_timestamp)

# Group orders by month and count them
# Siparişleri ay bazında gruplayıp sayıyoruz
monthly_orders <- table(format(orders$order_purchase_timestamp, "%Y-%m"))

# Plotting the monthly order trend
# Aylık sipariş trendini çiziyoruz
plot(monthly_orders, type='o', main='Monthly Order Trend', xlab='Month', ylab='Number of Orders')



## ----------------------------------------------------------------------------------------------------
# Average payment value by payment type
# Ödeme türüne göre ortalama ödeme değerini hesaplıyoruz
average_payment_value <- aggregate(payment_value ~ payment_type, data=order_payments, FUN=mean)

# Barplot for average payment value
# Ortalama ödeme değerini gösteren çubuk grafiği
barplot(average_payment_value$payment_value, names.arg=average_payment_value$payment_type, main='Average Payment Value by Payment Type', xlab='Payment Type', ylab='Average Payment Value')



## ----------------------------------------------------------------------------------------------------
# Subsetting the payment data for two payment types
# İki ödeme yöntemi için ödeme verilerini alt kümelere ayırıyoruz
payment_type_1 <- subset(order_payments, payment_type == "credit_card")$payment_value
payment_type_2 <- subset(order_payments, payment_type == "boleto")$payment_value

# Performing the t-test
# T-test uyguluyoruz
t_test_result <- t.test(payment_type_1, payment_type_2)

# Displaying the t-test result
# T-test sonucunu görüntülüyoruz
print(t_test_result)



## ----------------------------------------------------------------------------------------------------
# Check for missing or NA values in 'order_purchase_timestamp'
# 'order_purchase_timestamp' sütununda eksik veya NA değerlerini kontrol ediyoruz
sum(is.na(orders$order_purchase_timestamp))



## ----------------------------------------------------------------------------------------------------
# Ensure the 'order_purchase_timestamp' column is in Date format
# 'order_purchase_timestamp' sütununu doğru tarih formatına çeviriyoruz
orders$order_purchase_timestamp <- as.Date(orders$order_purchase_timestamp, format="%Y-%m-%d")



## ----------------------------------------------------------------------------------------------------
# Checking the structure and summary of the 'order_purchase_timestamp' column
# 'order_purchase_timestamp' sütununun yapısını ve özetini kontrol edelim
str(orders$order_purchase_timestamp)
summary(orders$order_purchase_timestamp)



## ----------------------------------------------------------------------------------------------------
# Removing rows with NA or empty 'order_purchase_timestamp' values
# 'order_purchase_timestamp' sütunundaki NA veya boş değerleri temizliyoruz
orders <- orders[!is.na(orders$order_purchase_timestamp) & orders$order_purchase_timestamp != "", ]



## ----------------------------------------------------------------------------------------------------
# Check the unique values in the 'order_purchase_timestamp' column
# 'order_purchase_timestamp' sütunundaki benzersiz değerleri kontrol edelim
unique(orders$order_purchase_timestamp)



## ----------------------------------------------------------------------------------------------------
# Remove rows with NA or empty 'order_purchase_timestamp' values
# NA veya boş 'order_purchase_timestamp' değerlerine sahip satırları kaldırıyoruz
orders <- orders[!is.na(orders$order_purchase_timestamp) & orders$order_purchase_timestamp != "", ]



## ----------------------------------------------------------------------------------------------------
# Kontrol etmek için birkaç satırı görüntüleyelim
head(orders$order_purchase_timestamp)



## ----------------------------------------------------------------------------------------------------
# Checking for NA values or any unexpected formats in the timestamp column
# Zaman damgası sütununda NA değerleri veya beklenmeyen formatları kontrol ediyoruz
orders_with_na <- orders[is.na(orders$order_purchase_timestamp), ]
head(orders_with_na)



## ----------------------------------------------------------------------------------------------------
# Checking the number of rows left in the 'orders' dataset
# 'orders' veri setinde kalan satır sayısını kontrol ediyoruz
nrow(orders)



## ----------------------------------------------------------------------------------------------------
# Reload the 'orders' dataset
# 'orders' veri setini yeniden yüklüyoruz
orders <- read.csv("olist_orders_dataset.csv")

# Display the first few rows to ensure it's loaded correctly
# Veri setinin doğru yüklendiğinden emin olmak için ilk birkaç satırı görüntüleyin
head(orders)



## ----------------------------------------------------------------------------------------------------
# Convert 'order_purchase_timestamp' to Date format
# 'order_purchase_timestamp' tarih formatına çeviriyoruz
orders$order_purchase_timestamp <- as.Date(orders$order_purchase_timestamp, format="%Y-%m-%d %H:%M:%S")

# Convert other date columns to Date format as well
# Diğer tarih sütunlarını da tarih formatına çeviriyoruz
orders$order_delivered_customer_date <- as.Date(orders$order_delivered_customer_date, format="%Y-%m-%d %H:%M:%S")
orders$order_estimated_delivery_date <- as.Date(orders$order_estimated_delivery_date, format="%Y-%m-%d %H:%M:%S")

# Check for missing values in date columns
# Tarih sütunlarında eksik değer olup olmadığını kontrol ediyoruz
sum(is.na(orders$order_purchase_timestamp))
sum(is.na(orders$order_delivered_customer_date))
sum(is.na(orders$order_estimated_delivery_date))



## ----------------------------------------------------------------------------------------------------
# Removing rows with missing 'order_delivered_customer_date' values
# Eksik 'order_delivered_customer_date' değerlerine sahip satırları kaldırıyoruz
orders <- orders[!is.na(orders$order_delivered_customer_date), ]



## ----------------------------------------------------------------------------------------------------
# Removing rows with missing 'order_delivered_customer_date' values
# Eksik 'order_delivered_customer_date' değerlerine sahip satırları kaldırıyoruz
orders <- orders[!is.na(orders$order_delivered_customer_date), ]

# Kaldıktan sonra veri setinin durumunu kontrol et
# Veri setinin durumunu kontrol edelim
nrow(orders)  # Kalan satır sayısını kontrol ediyoruz
sum(is.na(orders$order_delivered_customer_date))  # Eksik değer kontrolü



## ----------------------------------------------------------------------------------------------------
# Grouping orders by month and counting them
# Siparişleri ay bazında gruplayıp sayıyoruz
monthly_orders <- aggregate(order_id ~ format(order_purchase_timestamp, "%Y-%m"), data=orders, FUN=length)

# Rename columns for clarity
# Kolonları daha anlaşılır hale getiriyoruz
colnames(monthly_orders) <- c("Month", "Number_of_Orders")

# Plotting the monthly order trend
# Aylık sipariş trendini çiziyoruz
plot(as.Date(paste0(monthly_orders$Month, "-01")), monthly_orders$Number_of_Orders, type='o', main='Monthly Order Trend', xlab='Month', ylab='Number of Orders', las=2)



## ----------------------------------------------------------------------------------------------------
# Displaying the monthly orders in a table format
# Aylık siparişleri tablo formatında görüntülüyoruz
print(monthly_orders)



## ----------------------------------------------------------------------------------------------------
# Load the knitr package for better table formatting
# Daha iyi tablo formatlaması için knitr paketini yüklüyoruz
library(knitr)

# Displaying the monthly orders using kable for better formatting
# Kable kullanarak aylık siparişleri daha iyi formatlayarak görüntülüyoruz
kable(monthly_orders, caption = "Monthly Orders Summary", col.names = c("Month", "Number of Orders"))



## ----------------------------------------------------------------------------------------------------
# Add a new column for day of the week
# Haftanın gününü belirlemek için yeni bir sütun ekliyoruz
orders$day_of_week <- weekdays(orders$order_purchase_timestamp)

# Check the first few rows to ensure the new column is added
# Yeni sütunun eklendiğinden emin olmak için ilk birkaç satırı kontrol edelim
head(orders)



## ----------------------------------------------------------------------------------------------------
# Add a new column for day of the week
# Haftanın gününü belirlemek için yeni bir sütun ekliyoruz
orders$day_of_week <- weekdays(orders$order_purchase_timestamp)

# Check the first few rows to ensure the new column is added
# Yeni sütunun eklendiğinden emin olmak için ilk birkaç satırı kontrol edelim
head(orders)



## ----------------------------------------------------------------------------------------------------
# Create a new column for weekend/weekday
# Hafta sonu/hafta içi için yeni bir sütun oluşturuyoruz
orders$week_part <- ifelse(orders$day_of_week %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

# Check the first few rows to ensure the new column is added
# Yeni sütunun eklendiğinden emin olmak için ilk birkaç satırı kontrol edelim
head(orders)



## ----------------------------------------------------------------------------------------------------
# Join the 'orders' and 'order_payments' datasets to combine information
# Bilgileri birleştirmek için 'orders' ve 'order_payments' veri setlerini birleştiriyoruz
combined_data <- merge(orders, order_payments, by = "order_id")

# Grouping orders by week part and summing payment values
# Hafta içi ve hafta sonu sipariş tutarlarını toplamak için grupluyoruz
weekly_orders <- aggregate(payment_value ~ week_part, data=combined_data, FUN=sum)

# Displaying the result
# Sonucu görüntülüyoruz
print(weekly_orders)




## ----------------------------------------------------------------------------------------------------
# Barplot for weekday vs weekend purchases
# Hafta içi ve hafta sonu alışveriş tutarlarını gösteren çubuk grafiği
barplot(weekly_orders$payment_value, names.arg=weekly_orders$week_part, main='Weekday vs Weekend Purchases', xlab='Part of Week', ylab='Total Payment Value')



## ----------------------------------------------------------------------------------------------------
# Load ggplot2 package
# ggplot2 paketini yüklüyoruz
library(ggplot2)



## ----------------------------------------------------------------------------------------------------
# Create a bar plot for weekday vs weekend purchases using ggplot2
# Hafta içi ve hafta sonu alışveriş tutarlarını gösteren çubuk grafiği (ggplot2 kullanarak)
ggplot(weekly_orders, aes(x=week_part, y=payment_value, fill=week_part)) +
  geom_bar(stat="identity") +
  labs(title='Weekday vs Weekend Purchases', x='Part of Week', y='Total Payment Value') +
  scale_y_continuous(labels = scales::comma) +  # Y eksenindeki değerleri daha anlaşılır hale getiriyoruz
  theme_minimal()



## ----------------------------------------------------------------------------------------------------
# Müşteri veri setinin ilk birkaç satırını görüntüleyelim
# Viewing the first few rows of the customer dataset
head(customers)



## ----------------------------------------------------------------------------------------------------
# Grouping customers by city and counting the number of customers
# Müşterileri şehir bazında gruplandırıp müşteri sayısını hesaplıyoruz
city_segments <- table(customers$customer_city)

# Creating a data frame for better visualization
# Daha iyi görselleştirme için bir veri çerçevesi oluşturuyoruz
city_segments_df <- as.data.frame(city_segments)
colnames(city_segments_df) <- c("City", "Number_of_Customers")

# Displaying the top cities by number of customers
# Müşteri sayısına göre en çok müşteriye sahip şehirleri görüntülüyoruz
top_cities <- head(city_segments_df[order(-city_segments_df$Number_of_Customers), ], 10)
print(top_cities)



## ----------------------------------------------------------------------------------------------------
# Grouping customers by state and counting the number of customers
# Müşterileri eyalet bazında gruplandırıp müşteri sayısını hesaplıyoruz
state_segments <- table(customers$customer_state)

# Creating a data frame for better visualization
# Daha iyi görselleştirme için bir veri çerçevesi oluşturuyoruz
state_segments_df <- as.data.frame(state_segments)
colnames(state_segments_df) <- c("State", "Number_of_Customers")

# Displaying the top states by number of customers
# Müşteri sayısına göre en çok müşteriye sahip eyaletleri görüntülüyoruz
top_states <- head(state_segments_df[order(-state_segments_df$Number_of_Customers), ], 10)
print(top_states)



## ----------------------------------------------------------------------------------------------------
# Grouping customers by payment type and counting the number of customers
# Müşterileri ödeme türlerine göre gruplandırıp müşteri sayısını hesaplıyoruz
payment_segments <- table(order_payments$payment_type)

# Creating a data frame for better visualization
# Daha iyi görselleştirme için bir veri çerçevesi oluşturuyoruz
payment_segments_df <- as.data.frame(payment_segments)
colnames(payment_segments_df) <- c("Payment_Type", "Number_of_Customers")

# Displaying the payment method distribution
# Ödeme yöntemi dağılımını görüntülüyoruz
print(payment_segments_df)



## ----------------------------------------------------------------------------------------------------
# Display the first few rows of the product dataset
# Ürün veri setinin ilk birkaç satırını görüntülüyoruz
head(products)



## ----------------------------------------------------------------------------------------------------
# Merge the order_items and order_payments datasets by 'order_id'
# 'order_id' üzerinden order_items ve order_payments veri setlerini birleştiriyoruz
merged_items_payments <- merge(order_items, order_payments, by="order_id")

# Merge the result with the products dataset by 'product_id'
# Sonucu products veri seti ile 'product_id' üzerinden birleştiriyoruz
merged_data <- merge(merged_items_payments, products, by="product_id")

# Grouping by product category and summing up the payment values
# Ürün kategorilerine göre gruplandırıp toplam ödeme değerlerini hesaplıyoruz
category_sales <- aggregate(payment_value ~ product_category_name, data=merged_data, FUN=sum)

# Sorting categories by total sales value
# Kategorileri toplam satış değerine göre sıralıyoruz
category_sales <- category_sales[order(-category_sales$payment_value), ]

# Displaying the top categories by sales
# Satışlara göre en çok satan kategorileri görüntülüyoruz
head(category_sales, 10)



## ----------------------------------------------------------------------------------------------------
# Bar plot for the top product categories by sales
# En çok satış yapan ürün kategorilerini gösteren çubuk grafiği
library(ggplot2)

# En çok satış yapan ilk 10 kategoriye göre çubuk grafiği oluşturuyoruz
# Creating a bar plot for the top 10 selling categories
ggplot(category_sales[1:10, ], aes(x=reorder(product_category_name, -payment_value), y=payment_value)) +

  # Çubuk grafiği için 'geom_bar' fonksiyonunu kullanıyoruz
  # Using 'geom_bar' function to create the bar plot
  geom_bar(stat="identity", fill="steelblue") +

  # Başlık ve etiketler ekliyoruz
  # Adding title and labels
  labs(title="Top 10 Product Categories by Sales", 
       x="Product Category", 
       y="Total Payment Value (BRL)") +

  # X eksenindeki etiketleri 45 derece eğiyoruz
  # Rotating the labels on the x-axis by 45 degrees for readability
  theme(axis.text.x = element_text(angle=45, hjust=1))



## ----------------------------------------------------------------------------------------------------
# Check the column names of each dataset
# Her veri setinin sütun adlarını kontrol edelim
colnames(order_items)
colnames(order_payments)
colnames(products)
colnames(customers)



## ----------------------------------------------------------------------------------------------------
# Check for duplicates in customer_id in customers dataset
# customers veri setinde customer_id sütununda tekrar eden değerleri kontrol edelim
sum(duplicated(customers$customer_id))

# Check for duplicates in order_id in order_items dataset
# order_items veri setinde order_id sütununda tekrar eden değerleri kontrol edelim
sum(duplicated(order_items$order_id))

# Check for duplicates in order_id in order_payments dataset
# order_payments veri setinde order_id sütununda tekrar eden değerleri kontrol edelim
sum(duplicated(order_payments$order_id))



## ----------------------------------------------------------------------------------------------------
# Check the columns of merged_items_payments dataset
# merged_items_payments veri setinin sütunlarını kontrol edelim
colnames(merged_items_payments)



## ----------------------------------------------------------------------------------------------------
# Check the columns of merged_data
# merged_data veri setinin sütunlarını kontrol edelim
colnames(merged_data)

# Check the columns of customers dataset
# customers veri setinin sütunlarını kontrol edelim
colnames(customers)



## ----------------------------------------------------------------------------------------------------
# Check for duplicates in customer_id in customers dataset
# customers veri setinde customer_id sütununda tekrar eden değerleri kontrol edelim
sum(duplicated(customers$customer_id))



## ----------------------------------------------------------------------------------------------------
# Check the column names of merged_data to ensure customer_id exists
# merged_data veri setinin sütun adlarını kontrol ediyoruz
colnames(merged_data)



## ----------------------------------------------------------------------------------------------------
# Check the columns of order_items to verify if customer_id is present
# order_items veri setinin sütunlarını kontrol ediyoruz
colnames(order_items)

# Check the columns of order_payments to verify if customer_id is present
# order_payments veri setinin sütunlarını kontrol ediyoruz
colnames(order_payments)



## ----------------------------------------------------------------------------------------------------
# Check the columns of orders dataset
# orders veri setinin sütunlarını kontrol ediyoruz
colnames(orders)



## ----------------------------------------------------------------------------------------------------
# Merge order_items with orders to get customer_id
# order_items ve orders veri setlerini birleştirerek customer_id değerini alıyoruz
order_items_with_customers <- merge(order_items, orders, by="order_id")

# Merge order_items_with_customers with order_payments using 'order_id'
# order_items_with_customers veri setini order_payments ile 'order_id' kullanarak birleştiriyoruz
merged_items_payments <- merge(order_items_with_customers, order_payments, by="order_id")

# Merge the result with the products dataset using 'product_id'
# Sonucu products veri seti ile 'product_id' kullanarak birleştiriyoruz
final_merged_data <- merge(merged_items_payments, products, by="product_id")

# Check the first few rows of the final merged dataset
# Birleştirilen son veri setinin ilk birkaç satırını kontrol ediyoruz
head(final_merged_data)



## ----------------------------------------------------------------------------------------------------
# Check the columns of final_merged_data
# final_merged_data veri setinin sütunlarını kontrol edelim
colnames(final_merged_data)



## ----------------------------------------------------------------------------------------------------
# Merge order_items with orders to get customer_id and customer_state
# order_items ile orders veri setini birleştirerek customer_id ve customer_state alıyoruz
order_items_with_customers <- merge(order_items, orders, by="order_id")

# Merge order_items_with_customers with order_payments using 'order_id'
# order_items_with_customers veri setini order_payments ile 'order_id' kullanarak birleştiriyoruz
merged_items_payments <- merge(order_items_with_customers, order_payments, by="order_id")

# Merge the result with the products dataset using 'product_id'
# Sonucu products veri seti ile 'product_id' kullanarak birleştiriyoruz
final_merged_data <- merge(merged_items_payments, products, by="product_id")

# Check the columns of final_merged_data to ensure customer_state exists
# final_merged_data veri setinin sütun adlarını kontrol ediyoruz
colnames(final_merged_data)

# If customer_state exists, proceed to analyze sales by state and product category
# customer_state varsa, eyalet ve ürün kategorisine göre satış analizi yapmaya devam ediyoruz



## ----------------------------------------------------------------------------------------------------
# Merge orders with customers to obtain customer_state
# orders veri setini customers ile birleştirerek customer_state değerini alıyoruz
orders_with_customers <- merge(orders, customers, by="customer_id")

# Merge order_items with the merged orders_customers dataset using 'order_id'
# order_items veri setini birleştirilen orders_with_customers veri seti ile 'order_id' kullanarak birleştiriyoruz
order_items_with_customers <- merge(order_items, orders_with_customers, by="order_id")

# Merge with order_payments using 'order_id'
# order_items_with_customers veri setini order_payments ile 'order_id' kullanarak birleştiriyoruz
merged_items_payments <- merge(order_items_with_customers, order_payments, by="order_id")

# Merge the result with the products dataset using 'product_id'
# Sonucu products veri seti ile 'product_id' kullanarak birleştiriyoruz
final_merged_data <- merge(merged_items_payments, products, by="product_id")

# Check the columns of final_merged_data to ensure customer_state exists
# final_merged_data veri setinin sütun adlarını kontrol ediyoruz
colnames(final_merged_data)

# If customer_state exists, check the first few rows
# Eğer customer_state varsa, ilk birkaç satırı kontrol ediyoruz
if ("customer_state" %in% colnames(final_merged_data)) {
    head(final_merged_data)
} else {
    print("customer_state column not found in final_merged_data!")
}



## ----------------------------------------------------------------------------------------------------
colnames(final_merged_data)


## ----------------------------------------------------------------------------------------------------
# Grouping by product category and customer state, and summing up the payment values
# Ürün kategorilerine ve müşteri eyaletlerine göre gruplandırıp toplam ödeme değerlerini hesaplıyoruz
state_category_sales <- aggregate(payment_value ~ product_category_name + customer_state, data=final_merged_data, FUN=sum)

# Sort the results by payment_value
# Sonuçları payment_value'ya göre sıralıyoruz
state_category_sales <- state_category_sales[order(-state_category_sales$payment_value), ]

# Display the top results
# En çok satış yapan eyalet ve kategorileri görüntülüyoruz
head(state_category_sales, 10)



## ----------------------------------------------------------------------------------------------------
# Check the distribution of states in final_merged_data
# final_merged_data veri setinde eyaletlerin dağılımını kontrol edelim
table(final_merged_data$customer_state)



## ----------------------------------------------------------------------------------------------------
# Get the top 3 product categories based on total sales
# Toplam satışlara göre en çok satan 3 ürün kategorisini alıyoruz
top_product_categories <- head(state_category_sales[order(-state_category_sales$payment_value), ], 3)

# Display the top product categories
# En çok satan ürün kategorilerini görüntüleyelim
print(top_product_categories)



## ----------------------------------------------------------------------------------------------------
# Initialize an empty data frame to store results
# Sonuçları saklamak için boş bir veri çerçevesi oluşturalım
top_states_data <- data.frame()

# Loop through each top product category
# Her bir en çok satan ürün kategorisi için döngü
for (category in top_product_categories$product_category_name) {
    # Filter data for the current product category
    # Mevcut ürün kategorisi için verileri filtreleyelim
    category_data <- state_category_sales[state_category_sales$product_category_name == category, ]
    
    # Get the top 3 states for this category
    # Bu kategori için en fazla satış yapan 3 eyaleti alalım
    top_states <- head(category_data[order(-category_data$payment_value), ], 3)
    
    # Add the product category to the top states data
    # Ürün kategorisini en fazla satış yapan eyalet verisine ekleyelim
    top_states$product_category_name <- category
    
    # Append to the results data frame
    # Sonuç veri çerçevesine ekleyelim
    top_states_data <- rbind(top_states_data, top_states)
}

# Display the top states data
# En fazla satış yapan eyalet verisini görüntüleyelim
print(top_states_data)



## ----------------------------------------------------------------------------------------------------
library(ggplot2)  # ggplot2 kütüphanesini yükle (Load the ggplot2 library)
library(scales)   # label_number fonksiyonunu kullanmak için scales kütüphanesini yükle (Load the scales library to use the label_number function)

# En çok satan ürün kategorileri ve bunların en fazla satıldığı eyaletler için bar grafiği
# Bar plot for top product categories and their top selling states
ggplot(top_states_data, aes(x=reorder(customer_state, -payment_value), y=payment_value, fill=product_category_name)) +
  geom_bar(stat="identity", position="dodge") +  # Bar grafiği için verileri ekle (Add data to create a bar graph)
  labs(title="Top Product Categories and Their Top Selling States",  # Başlık ve eksen etiketlerini ayarla (Set the title and axis labels)
       x="State", 
       y="Total Sales Value") +  
  theme_minimal() +  # Minimal tema uygula (Apply a minimal theme)
  theme(axis.text.x = element_text(angle=45, hjust=1)) +  # X eksenindeki etiketleri döndür (Rotate the x-axis labels)
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +  # Y eksenindeki etiketleri formatla (Format the y-axis labels to show in thousands (K))
  scale_fill_brewer(palette="Set1")  # Renk paletini değiştir (Change the color palette)



## ----------------------------------------------------------------------------------------------------
# Veriyi hazırlama ve zaman serisine uygun hale getirme (Prepare data for time series analysis)
# Sipariş tarihlerinin ay bazında gruplanması (Group orders by month)

library(dplyr)

# Tarihi 'Date' formatına çevir (Convert the date to 'Date' format)
final_merged_data$order_purchase_timestamp <- as.Date(final_merged_data$order_purchase_timestamp)

# Aylık sipariş sayısını hesapla (Calculate the number of orders per month)
monthly_orders <- final_merged_data %>%
  mutate(month = format(order_purchase_timestamp, "%Y-%m")) %>%  # Ay bilgisi ekle (Add month info)
  group_by(month) %>%
  summarise(total_orders = n())  # Toplam sipariş sayısını hesapla (Calculate total number of orders)

# Aylık siparişleri görüntüle (View monthly orders)
print(monthly_orders)



## ----------------------------------------------------------------------------------------------------
# Zaman serisi oluşturma (Create time series data)
monthly_orders_ts <- ts(monthly_orders$total_orders, start = c(2016, 1), frequency = 12)  # Aylık frekanslı zaman serisi (Monthly frequency time series)

# Zaman serisini görselleştirme (Visualize time series)
plot(monthly_orders_ts, main="Monthly Orders Time Series", ylab="Total Orders", xlab="Year")



## ----------------------------------------------------------------------------------------------------
library(forecast)  # Forecasting için gerekli kütüphane (Load library for forecasting)

# ARIMA modelini oluştur (Fit ARIMA model)
arima_model <- auto.arima(monthly_orders_ts)

# Tahmin yap (Make a forecast)
forecasted_orders <- forecast(arima_model, h=12)  # Önümüzdeki 12 ay için tahmin (Forecast for the next 12 months)

# Tahmini görselleştir (Visualize the forecast)
plot(forecasted_orders, main="ARIMA Forecast of Monthly Orders")



## ----------------------------------------------------------------------------------------------------
# Modelin hata oranlarını görüntüle (View model's error metrics)
accuracy(arima_model)



## ----------------------------------------------------------------------------------------------------
library(forecast)  # Holt-Winters modeli için forecast kütüphanesini yükle (Load forecast library for Holt-Winters model)

# Holt-Winters Exponential Smoothing modelini oluştur (Fit Holt-Winters Exponential Smoothing model)
hw_model <- hw(monthly_orders_ts, seasonal="multiplicative")  # Mevsimsellik çarpımsal olarak ayarlandı (Seasonality set as multiplicative)

# Tahmin yap (Make a forecast)
hw_forecast <- forecast(hw_model, h=12)  # Önümüzdeki 12 ay için tahmin (Forecast for the next 12 months)

# Tahmini görselleştir (Visualize the forecast)
plot(hw_forecast, main="Holt-Winters Exponential Smoothing Forecast")



## ----------------------------------------------------------------------------------------------------
# Modelin hata oranlarını görüntüle (View model's error metrics)
accuracy(hw_model)



## ----------------------------------------------------------------------------------------------------
# ARIMA modelinin hata oranları (ARIMA model accuracy)
arima_accuracy <- accuracy(arima_model)
print("ARIMA Model Accuracy")
print(arima_accuracy)

# Holt-Winters modelinin hata oranları (Holt-Winters model accuracy)
hw_accuracy <- accuracy(hw_model)
print("Holt-Winters Model Accuracy")
print(hw_accuracy)

# Modelleri karşılaştırma (Compare the models)
comparison <- data.frame(
  Model = c("ARIMA", "Holt-Winters"),
  RMSE = c(arima_accuracy[2], hw_accuracy[2]),
  MAE = c(arima_accuracy[3], hw_accuracy[3]),
  MASE = c(arima_accuracy[6], hw_accuracy[6])
)

print("Comparison of ARIMA and Holt-Winters Models")
print(comparison)



## ----------------------------------------------------------------------------------------------------
# Gerekli kütüphaneleri yükleyelim (Load required libraries)
library(arules)         # Birliktelik kuralları için (For association rules)
library(arulesViz)      # Birliktelik kuralları görselleştirme için (For visualizing association rules)

# Sepet analizi için gerekli veriyi hazırlama (Prepare data for Market Basket Analysis)
# order_items veri setini kullanarak her siparişteki ürünleri listeleyelim (Use the order_items dataset to list products per order)
orders_products <- split(final_merged_data$product_category_name, final_merged_data$order_id)

# Veriyi arules paketi için transactions formatına dönüştürelim (Convert data to transactions format for arules package)
transactions <- as(orders_products, "transactions")

# İlk 10 transaction'ı inceleyelim (Inspect the first 10 transactions)
inspect(transactions[1:10])



## ----------------------------------------------------------------------------------------------------
# Transaction nesnesini kontrol etme (Check the transactions object)
summary(transactions)



## ----------------------------------------------------------------------------------------------------
# Apriori algoritmasını daha düşük parametrelerle çalıştırma (Run Apriori with even lower support and confidence)
rules <- apriori(transactions, parameter = list(supp = 0.00001, conf = 0.2, minlen = 2))

# Oluşturulan kural sayısını kontrol et (Check how many rules are generated)
length(rules)

# İlk birkaç kuralı görüntüle (View the first few rules)
inspect(rules[1:10])



## ----------------------------------------------------------------------------------------------------
# Birliktelik kurallarını görselleştirme (Visualize the association rules)
plot(rules, method = "graph", control = list(type = "items"))


