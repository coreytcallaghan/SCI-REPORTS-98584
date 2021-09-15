library(ggplot2)


a <- c(seq(0, 1000, by=2)) %>%
  as.data.frame() %>%
  rename(a=1) %>%
  sample_n(100) %>%
  arrange(desc(a)) 
b <- c(seq(0, 1000, by=3)) %>%
  as.data.frame() %>%
  rename(b=1) %>%
  sample_n(100) %>%
  arrange(desc(b))

a <- rnorm(1000) %>%
  as.data.frame() %>%
  rename(a=1) %>%
  arrange(desc(a))
b <- rnorm(1000) %>%
  as.data.frame() %>%
  rename(b=1) %>%
  arrange(desc(b))

dat <- data.frame(a=a, b=b)

ggplot(dat, aes(x=a, y=b))+
  xlab("Number of eBird observations")+
  ylab("Number of iNaturalist observations")+
  theme_classic()+
  theme(axis.text=element_blank())+
  theme(axis.title=element_text(size=18))+
  theme(axis.ticks=element_blank())+
  geom_smooth(method="lm", color="blue", se=FALSE)+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())


ggplot(dat, aes(x=a, y=b))+
  xlab("Body size")+
  ylab("Relative iNaturalist observations")+
  theme_bw()+
  theme(axis.text=element_blank())+
  theme(axis.ticks=element_blank())+
  geom_smooth(method="lm", color="gray30", linetype="dashed", se=FALSE)+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  theme(axis.title=element_text(size=8))

ggplot(dat, aes(x=a, y=b))+
  xlab("Color (distance from brown)")+
  ylab("Relative iNaturalist observations")+
  theme_bw()+
  theme(axis.text=element_blank())+
  theme(axis.ticks=element_blank())+
  geom_smooth(method="lm", color="gray30", linetype="dashed", se=FALSE)+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  theme(axis.title=element_text(size=8))

ggplot(dat, aes(x=a, y=b))+
  xlab("Gregariousness (flock size)")+
  ylab("Relative iNaturalist observations")+
  theme_bw()+
  theme(axis.text=element_blank())+
  theme(axis.ticks=element_blank())+
  geom_smooth(method="lm", color="gray30", linetype="dashed", se=FALSE)+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  theme(axis.title=element_text(size=8))

a <- c(seq(0, 1000, by=2)) %>%
  as.data.frame() %>%
  rename(a=1) %>%
  sample_n(100) %>%
  arrange(a)
b <- c(seq(0, 1000, by=3)) %>%
  as.data.frame() %>%
  rename(b=1) %>%
  sample_n(100) %>%
  arrange(b)

dat <- data.frame(a=a, b=b)

ggplot(dat, aes(x=a, y=b))+
  xlab("Commonness")+
  ylab("Relative iNaturalist observations")+
  theme_bw()+
  theme(axis.text=element_blank())+
  theme(axis.ticks=element_blank())+
  geom_smooth(method="lm", color="gray30", linetype="dashed", se=FALSE)+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  theme(axis.title=element_text(size=8))
