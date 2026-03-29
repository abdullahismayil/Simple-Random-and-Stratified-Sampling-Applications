#1
#Veri girişi
library(dplyr)
data <- read.csv("C:/Users/HP/Downloads/student_lifestyle_dataset.csv", header = TRUE, sep = ",")
names(data)
head(data)

#2
# Nh, Sh ve Wh bulunması
tabaka_ozet <- data %>%
  group_by(Stress_Level) %>%
  summarise(
    N_h = n(),  # Tabaka büyüklüğü
    S_h = sd(GPA, na.rm = TRUE)  # Standart sapma
  ) %>%
  mutate(
    N = sum(N_h),        # Tüm popülasyon büyüklüğü
    W_h = N_h / N        # Tabaka ağırlığı
  )

tabaka_ozet <- tabaka_ozet %>%
  mutate(
    kh = (W_h * S_h) / sum(W_h * S_h)  # Kh formülüne göre hesaplama
  )

# Sonuçları görüntüleme
print(tabaka_ozet)


# Veriyi oluşturuyoruz
data1 <- tibble::tibble(
  Stress_Level = c("High", "Low", "Moderate"),
  N_h = c(1029, 297, 674),
  S_h = c(0.275, 0.215, 0.221),
  W_h = c(0.514, 0.148, 0.337),
  kh = c(0.571, 0.129, 0.300)

)
N=2000
# Değerleri tek tek ayıralım
for(i in 1:nrow(data1)) {
  assign(paste0("N_", i), data1$N_h[i]) 
  assign(paste0("S_", i), data1$S_h[i])
  assign(paste0("W_", i), data1$W_h[i])
  assign(paste0("k_", i), data1$kh[i])
}

# Değerlerin doğru şekilde ayrıldığını kontrol edelim
print(N_1)
print(S_1)
print(W_1)
print(k_1)

print(N_2)
print(S_2)
print(W_2)
print(k_2)

print(N_3)
print(S_3)
print(W_3)
print(k_3)

# Formülün uygulanması
n <- (((W_1^2 * S_1^2) / k_1) / (0.0009 + ((W_1 * S_1^2) / N))) + 
  (((W_2^2 * S_2^2) / k_2) / (0.0009 + ((W_2 * S_2^2) / N))) + 
  (((W_3^2 * S_3^2) / k_3) / (0.0009 + ((W_3 * S_3^2) / N)))

# n sonuç
print(n)

#Örnelem Büyüklükleri
n_1=n*k_1
n_2=n*k_2
n_3=n*k_3
n_1
n_2
n_3

#3
# Tabakalı Rastgele Örneklemleri
set.seed(42)  
sample_1 <- sample(data$GPA, n_1, replace = TRUE)
sample_2 <- sample(data$GPA, n_2+1, replace = TRUE)
sample_3 <- sample(data$GPA, n_3, replace = TRUE)
sample_1
sample_2
sample_3



#4
#Ortalama(E(y))
# Ortalama hesaplama
y_1 <- mean(sample_1)

# Sonucu yazdır
print(y_1)

# Ortalama hesaplama
y_2 <- mean(sample_2)

# Sonucu yazdır
print(y_2)

# Ortalama hesaplama
y_3 <- mean(sample_3)

# Sonucu yazdır
print(y_3)


#TRÖ ortalama tahmini: (E(ytb))
y_tb= (W_1 * y_1) + (W_2 * y_2) + (W_3 * y_3)
y_tb



#TRÖ ortalama tahmininin varyansı: (V(y_tb))
# Örnek varyansını hesaplama
s2_sample_1 <- var(sample_1)

# Sonucu yazdır
print(s2_sample_1)

# Örnek varyansını hesaplama
s2_sample_2 <- var(sample_2)

# Sonucu yazdır
print(s2_sample_2)

# Örnek varyansını hesaplama
s2_sample_3 <- var(sample_3)

# Sonucu yazdır
print(s2_sample_3)

Vy_tb = (W_1^2 * s2_sample_1 * (1-(n_1/N_1) / n_1) +
        (W_2^2 * s2_sample_2 * (1-(n_2/N_2) / n_2)) +  
        (W_3^2 * s2_sample_3 * (1-(n_3/N_3) / n_3)))

Vy_tb


#E(ytb)'nin güven aralığı:
# Güven aralığı hesaplama(alpha = 0.05)
t_critical = 2
y_tb_alt_sınır <- y_tb - (t_critical * sqrt(Vy_tb))
y_tb_üst_sınır <- y_tb + (t_critical * sqrt(Vy_tb))

# Sonuçları yazdır
cat("Güven Aralığı: [", y_tb_alt_sınır, ",", y_tb_üst_sınır, "]\n")



#TRÖ toplam:(E(Y_tb))
Y_tb = N * y_tb
Y_tb

VY_tb = N^2 * Vy_tb 
VY_tb  

#E(Ytb)'nin güven aralığı:
# Güven aralığı hesaplama(alpha = 0.05)
t_critical = 2
Y_tb_alt_sınır <- Y_tb - (t_critical * sqrt(VY_tb))
Y_tb_üst_sınır <- Y_tb + (t_critical * sqrt(VY_tb))

# Sonuçları yazdır
cat("Güven Aralığı: [", Y_tb_alt_sınır, ",", Y_tb_üst_sınır, "]\n")


  
  
#TRÖ oran: (E(p_tb))
#Ortalaması 3'ün üzerinde olanlarla ilgilenilecektir.
# 3'ten büyük olanların sayısını bulma ve örneklem büyüklüğüne bölme
p_1 <- sum(sample_1 > 3) / length(sample_1)
print(p_1)



p_2 <- sum(sample_2 > 3) / length(sample_2)
print(p_2)



p_3 <- sum(sample_3 > 3) / length(sample_3)
print(p_3)



#TRÖ oran: (E(p_tb))
p_tb = (W_1 * p_1) + (W_2 * p_2) + (W_3 * p_3)
p_tb


#TRÖ oran tahmininin varyansı: V(p_tb)
q_1 = 1 - p_1
print(q_1)
q_2 = 1 - p_2
print(q_2)
q_3 = 1 - p_3
print(q_3)

Vp_tb = (W_1^2 * (p_1 * q_1) * (1-(n_1/N_1) / n_1) +
           (W_2^2 * (p_2 * q_2) * (1-(n_2/N_2) / n_2)) +  
           (W_3^2 * (p_3 * q_3) * (1-(n_3/N_3) / n_3)))
Vp_tb


#E(p_tb)'nin güven aralığı:
# Güven aralığı hesaplama(alpha = 0.05)
t_critical = 2
p_tb_alt_sınır <- p_tb - (t_critical * sqrt(Vp_tb))
p_tb_üst_sınır <- p_tb + (t_critical * sqrt(Vp_tb))

# Sonuçları yazdır
cat("Güven Aralığı: [", p_tb_alt_sınır, ",", p_tb_üst_sınır, "]\n")

# Alt ve üst sınırları 0 ile 1 arasında bağlamak
p_tb_alt_sınır <- max(0, p_tb - (t_critical * sqrt(Vp_tb)))
p_tb_üst_sınır <- min(1, p_tb + (t_critical * sqrt(Vp_tb)))

# Sonuçları yazdır
cat("Düzeltilmiş Güven Aralığı: [", p_tb_alt_sınır, ",", p_tb_üst_sınır, "]\n")




#TRÖ belli özelliğe sahip birim sayısı: (E(A_tb))
A_tb = N * p_tb
A_tb


#TRÖ belli özelliğe sahip birim sayısın;n tahminin varyansı: (V(A_tb))
VA_tb = N^2 * Vp_tb
VA_tb

#E(p_tb)'nin güven aralığı:
# Güven aralığı hesaplama(alpha = 0.05)
t_critical = 2
A_tb_alt_sınır <- A_tb - (t_critical * sqrt(VA_tb))
A_tb_üst_sınır <- A_tb + (t_critical * sqrt(VA_tb))

# Sonuçları yazdır
cat("Güven Aralığı: [", A_tb_alt_sınır, ",", A_tb_üst_sınır, "]\n")


# Alt ve üst sınırları 0 ile 2000 arasında bağlamak
A_tb_alt_sınır <- max(0, A_tb - (t_critical * sqrt(VA_tb)))
A_tb_üst_sınır <- min(2000, A_tb + (t_critical * sqrt(VA_tb)))

# Sonuçları yazdır
cat("Düzeltilmiş Güven Aralığı: [", A_tb_alt_sınır, ",", A_tb_üst_sınır, "]\n")


#5

#BRÖ için 2.tabaka seçilmiştir.

#BRÖ ortalama tahmini: E(y)
y = mean(sample_2)
y

#BRÖ ortalama tahmininin varyansı: (V(y))
V_y = s2_sample_2 * (1-(n_2/N_2)) / N_2
V_y

# Güven aralığı hesaplama(alpha = 0.05)
t_critical = 2
y_alt_sınır <- y - (t_critical * sqrt(V_y))
y_üst_sınır <- y + (t_critical * sqrt(V_y))

# Sonuçları yazdır
cat("Güven Aralığı: [", y_alt_sınır, ",", y_üst_sınır, "]\n")



#BRÖ toplam:(E(Y))
Y = N_2 * y
Y

V_Y = N_2^2 * V_y 
V_Y 

#E(Ytb)'nin güven aralığı:
# Güven aralığı hesaplama(alpha = 0.05)
t_critical = 2
Y_alt_sınır <- Y - (t_critical * sqrt(V_Y))
Y_üst_sınır <- Y + (t_critical * sqrt(V_Y))

# Sonuçları yazdır
cat("Güven Aralığı: [", Y_alt_sınır, ",", Y_üst_sınır, "]\n")




#BRÖ oran: (E(p))
#Ortalaması 3'ün üzerinde olanlarla ilgilenilecektir.
# 3'ten büyük olanların sayısını bulma ve örneklem büyüklüğüne bölme
p_2 <- sum(sample_2 > 3) / length(sample_2)
print(p_2)



#BRÖ oran tahmininin varyansı: V(p)
q_2 = 1 - p_2
print(q_2)
Vp_2 = p_2 * q_2 * (1-(n_2/N_2)) / n_2
Vp_2


#E(p_tb)'nin güven aralığı:
# Güven aralığı hesaplama(alpha = 0.05)
t_critical = 2
p_2_alt_sınır <- p_2 - (t_critical * sqrt(Vp_2))
p_2_üst_sınır <- p_2 + (t_critical * sqrt(Vp_2))

# Sonuçları yazdır
cat("Güven Aralığı: [", p_2_alt_sınır, ",", p_2_üst_sınır, "]\n")




#BRÖ belli özelliğe sahip birim sayısı: (E(A))
A = N_2 * p_2
A


#TRÖ belli özelliğe sahip birim sayısın;n tahminin varyansı: (V(A_tb))
VA = N_2^2 * Vp_2
VA

#E(p_tb)'nin güven aralığı:
# Güven aralığı hesaplama(alpha = 0.05)
t_critical = 2
A_alt_sınır <- A - (t_critical * sqrt(VA))
A_üst_sınır <- A + (t_critical * sqrt(VA))

# Sonuçları yazdır
cat("Güven Aralığı: [", A_alt_sınır, ",", A_üst_sınır, "]\n")











