library(tidyverse)

sequenced = readLines("data/samples-from-bgi.txt")
acc = read_csv("data/Emelliodora_Accessions.csv")
samp = read_csv("data/Emelliodora_PlantsSamples.csv") %>%
  select(AccessionID, AccessionName, SampleID, SampleName, PlateName, PlateCoordinate) %>%
  filter(grepl("Emelliodora_LB_Block[1234]$", PlateName)) %>%
  mutate(PlateName = sub("Emelliodora_LB_Block", "", PlateName),
         PlateCoordinate=sub("([A-H])(\\d)$", "\\10\\2", PlateCoordinate),
         AnonName = sprintf("LBM0%s%s", PlateName, PlateCoordinate)) %>%
  select(-PlateName, -PlateCoordinate)
readstat = read_tsv("data/readstats.tsv") %>%
  mutate(ID = basename(dirname(r1_file))) %>%
  select(ID, BGIReadPairs=read_pairs)

acc = acc %>%
  select(AccessionID, PopulationName, Latitude, Longitude, Altitude)

meta = left_join(samp, acc, by="AccessionID") %>%
  mutate(HaveBGISeq = AnonName %in% sequenced) %>%
  left_join(readstat, by=c("AnonName"="ID")) %>%
  mutate_if(is.factor, fct_drop)

write_csv(meta, "BGIseq-metadata.csv")    

pop.summ = meta %>%
  group_by(PopulationName) %>%
  summarise(N=n(), NSeq=sum(HaveBGISeq))


summary(meta$BGIReadPairs)
ggplot(meta, aes(BGIReadPairs)) +
  geom_histogram()
