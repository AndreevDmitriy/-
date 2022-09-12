library(googlesheets4)
library(data.table)
# Загрузка датасета с указанием типов столбцов
# Убираем ненужный столбец

dt <-  data.table(read_sheet("https://docs.google.com/spreadsheets/d/165sp-lWd1L4qWxggw25DJo_njOCvzdUjAd414NSE8co/edit?usp=sharing",
                               col_types = "cicciidd"))[,!"good (1)"]

# Убирем строки, в которых существуют неформатные (NA) значения
dt <- dt[complete.cases(dt)]

# Всего из 230 строк осталось 225
summary(dt)

# Выбираем строки внутри областей с одинаковыми названиями

dt[, .(N_keyword= .N), by=.(area,keyword)][N_keyword>1]
# Запишем дублирующиеся названия в переменную
dubles <- dt[, .(N_keyword= .N), by=.(area,keyword)][N_keyword>1][, .(area, keyword)]

# Внутри пары область-кластер одинаковых названий не наблюдается
dt[, .(N_keyword= .N), by=.(area, cluster, keyword)][N_keyword>1]

# Посмотрим пристальнее на дубли

dt[order(keyword), .SD, by=area][paste0(area, keyword) %in% paste0(dubles$area, dubles$keyword)]

# Существуют два вида дублей: "полные", которые различаются только номером кластера, а в остальном идентичные
#                           и "неполные", в которых cluster, count, x и y различны и совпадает только поле keyword

# Отсюда возможны 3 варианта действий
#   1. Как сказано в условии задачи "Не должно быть дубликатов слов в одной и той же области (area),
#                                     но словосочетание может повторяться из area в area",
#       но тогда по какому принципу избавляться от дублей? Оставлять строку с наименьшим(наибольшим) номером кластера или
#       с наибольшим значением count?
#   2. Несколько отойти от условия задачии избавиться только от полных дублей. Хотя опять возникает вопрос,
#       по какому принципу оставлять записи (наименьший или наибольший номер кластера)
#   3. Предположить, что в условии задачи имелось ввиду именно пара area-cluster, а не просто area. Но тогда возникает вопрос,
#       что делать с полными дублями, т.к. на диаграмме рассеяния они будут полностью друг друга закрывать


# Возьмем за основу вариант 1 с условием удаления минимум по полю count и максимум по полю cluster если count1==count2

# Добавим к данным столбец кандидатов на удаление
# dt[, to_remove:=rep(FALSE, nrow(dt))]

# Создадим дополнительный датафрейм, в который будем добавлять строки, подлежащие удалению
to_remove <- setNames(data.table(matrix(nrow = 0, ncol = length(dt))), colnames(dt))

# Для каждой пары area-keyword из списка дублей
# находим в наших данных соответствующие им пары строк, а затем выбираем по условию наименьшего count
# или при равенстве count с наибольшим значением cluster 
for (dd in paste0(dubles$area, dubles$keyword)){
  to_remove <- rbind(to_remove, dt[paste0(area, keyword)==dd][count==min(count) & cluster==max(cluster)])
}
# Устанавливаем флаг удалениея на TRUE
to_remove[, to_remove:=TRUE]

cols <- colnames(dt)

# Создаем новый data.table с очищеными данными 
dt_mod <- merge.data.table(dt[,cols, with=F], to_remove, by=cols, all.x = T)[is.na(to_remove)]

# Убираем вспомогательный столбец
dt_mod$to_remove <- NULL

# Максимальное количество различных кластеров 
range(dt_mod$cluster)

# Создадим таблицу цветов (источник Tableau Classic 10)
color_map <- c("#17becf", "#bcbd22", "#7f7f7f", "#e377c2" )

# Добавим столбец color для каждой пары area-cluster
dt_mod[, color:=color_map[cluster+1], by=.(area)]

# Запишем получившийся датасет как google spreadshhet
ss <- gs4_create(name = "ТЗ ВШЭ готовый", sheets = dt_mod[order(area, cluster, -count)])
write_sheet(ss)

# Сохраним итоговый датасет для передачи в скрипт визуализации
write.csv(dt_mod, file = "ТЗ ВШЭ готовый.csv")
