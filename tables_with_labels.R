
library(flextable)
library(ggplot2)


table_J <- table_J |> mutate(ACTIVE = if_else(FISHING_TECH != "INACTIVE", TOTVES, NA))

Fleet_summary <- table_J |> group_by(YEAR, VESSEL_LENGTH) |> summarise(TOTVES=sum(TOTVES),ACTIVE_VES=sum(ACTIVE, na.rm = TRUE),TOTTRIPS = sum(TOTTRIPS))


Fleet_age <- table_J |> group_by(YEAR) |> summarise(AVGAGE=mean(AVGAGE), TOTVES = sum(TOTVES), ACTIVE_VES = sum(ACTIVE, na.rm = TRUE))

# SUMMARY PPT####
# Luo flextable-taulukko
ft1 <- flextable(Fleet_summary) |> autofit()|> 
  colformat_double(j=2, digits=0)|> 
  colformat_num(j=1, big.mark = "")|> 
  colformat_num(j=3, big.mark = "")|> 
  colformat_num(j=4, big.mark = "") |> 
  set_caption(as_paragraph(as_chunk("Finnish fishing Fleet trips by segment", props = fp_text_default(font.family = "Cambria"))), word_stylename = "Table Caption")|>
  line_spacing(space = 1)|>
  hline(i=c(6,12,18,24,30,36,42,48,54,60,66))

save_as_pptx(ft1, path ="results/2025/tables/mydoc1.pptx")

ggplot(data=Fleet_summary, aes(y=TOTTRIPS, x=VESSEL_LENGTH,fill = VESSEL_LENGTH)) + 
  geom_bar(stat="identity") +
  ggtitle("Total trips by Vessels length")+
  facet_grid(~YEAR) +
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.position="bottom") + 
  labs(fill='')
ggsave("trips_by_length.png", width = 656, height = 434, units = "px")


# AGE PPT####
# Luo flextable-taulukko
ft2 <- flextable(Fleet_age) |> autofit() |> 
  colformat_double(j=2, digits=0)|> 
  colformat_num(j=1, big.mark = "")|> 
  colformat_num(j=3, big.mark = "")|> 
  colformat_num(j=4, big.mark = "") |> 
  set_caption(as_paragraph(as_chunk("Finnish fishing Fleet vessel numbers and average age", props = fp_text_default(font.family = "Cambria"))), word_stylename = "Table Caption")|>
  line_spacing(space = 1)



save_as_pptx(ft2, path ="results/2025/tables/mydoc2.pptx")


