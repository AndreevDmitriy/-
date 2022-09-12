library(data.table)
library(ggplot2)

# Загрузка подготовленного датасета

dt_mod <- fread("ТЗ ВШЭ готовый.csv")
# Создадим диаграмму рассеяния для каждой area
areas <- unique(dt_mod$area)

dt_mod[nchar(keyword)>=15, keyword:=gsub(" ", "\n", keyword)]

for (ar in areas){
  p <- ggplot(dt_mod[area==ar], aes(x=x, y=y, size=count, show.legend=F))+
    geom_point(aes(color=color))+
    xlim(min(dt_mod[area==ar, x])-1, max(dt_mod[area==ar, x])+1)+
    ylim(min(dt_mod[area==ar, y])-1, max(dt_mod[area==ar, y])*1.05)+
    labs(title = paste0("Диаграмма рассеяния для области ", ar),
         caption = "Источник: система интеллектуального анализа больших данных",
         color="Кластеры")+
    scale_color_discrete(labels=unique(dt_mod$cluster_name))+
    geom_text(aes(label=keyword, size=count), check_overlap = F)+
    guides(size="none")+
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          plot.caption = element_text(size = 10),
          legend.background = element_rect(color = "black"))
  ggsave(paste0(ar,".png"), p, device = "png", width = 1500, height = 1500, units = "px", dpi = 150)
}

