m_to_n <- filter(all_ee_recent_class,
               ! FinalEcls == "SE",
               FTE < 1)
m_to_n <- mton[str_detect(mton$`Position Number`, "^4M"),]
m_to_n <- filter(mton, !duplicated(Key))
View(m_to_n)
