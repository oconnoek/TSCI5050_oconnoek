library(ggplot2)
library(GGally)

head(mpg)

ggplot(mpg, aes(x=cty, y=hwy, colour = class)) + geom_point()

#adding several different "aesthetic mappings" : color, size, shape
ggplot(mpg, aes(x=cty, y=hwy, colour = class, size=displ, shape=drv, fill=class)) + geom_point(alpha=0.2)

#using "facet" to include even more dimensions
ggplot(mpg, aes(x=cty, y=hwy, colour = class, size=displ, shape = drv, fill=class)) + 
  geom_point(alpha=0.2) + facet_grid(rows=vars(trans),cols=vars(fl))

library(ggplot2);
library(GGally);

head(mpg)
# basic plot
ggplot(mpg, aes(x=cty, y=hwy, colour = class)) + geom_point()

# adding as many different "aesthetic mappings" as possible: color, size, shape, fill, and alpha (transparency)
ggplot(mpg, aes(x=cty, y=hwy, colour = class, size=displ,shape=drv,fill=class,alpha=cyl)) + geom_point()

# using 'facet' to include even more dimensions
ggplot(mpg, aes(x=cty, y=hwy, colour = class, size=displ,shape=drv,fill=class,alpha=cyl)) + 
  geom_point() + 
  facet_grid(rows=vars(trans),cols=vars(fl))

#adding layers
ggplot(mpg, aes(x=cty, y=hwy, colour = class)) + geom_point() + geom_smooth(formula = y~x)

#override color
ggplot(mpg, aes(x=cty, y=hwy, colour = class)) + geom_point() + geom_smooth(aes(colour = NA),formula = y~x)

ggplot(mpg, aes(x=cty, y=hwy, colour = class)) + geom_point() + geom_smooth(aes(y = displ),formula = y~x)

#assign specific color in geom smooth
ggplot(mpg, aes(x=cty, y=hwy, colour = class)) + geom_point() + 
  geom_smooth(aes(colour = NA),formula = y~x, colour="blue", fill="blue")

#add linear model to line fit
ggplot(mpg, aes(x=cty, y=hwy, colour = class)) + geom_point() + 
  geom_smooth(aes(colour = NA),formula = y~x, colour="blue", fill="blue", method = "lm") 

#remove CI with se = FALSE
ggplot(mpg, aes(x=cty, y=hwy, colour = class)) + geom_point() + 
  geom_smooth(aes(colour = NA),formula = y~x, colour="blue", fill="blue", method = "lm", se = FALSE) 

#boxplots
ggplot(mpg, aes(x=class, y=hwy, colour = class)) + geom_boxplot()

ggplot(mpg, aes(x=class, y=hwy, colour = class, fill = model)) + geom_boxplot()

#make discrete variable with factor
ggplot(mpg, aes(x=class, y=hwy, colour = class, fill = factor(cyl))) + geom_boxplot()

#x is more specific, color is general
ggplot(mpg, aes(x=model, y=hwy, colour = manufacturer)) + geom_boxplot()

ggplot(mpg, aes(x=model, y=hwy, colour = drv)) + geom_boxplot()
ggplot(mpg, aes(x=model, y=hwy, colour = drv)) + geom_violin()
ggplot(mpg, aes(x=manufacturer, y=hwy, colour = drv)) + geom_violin()
ggplot(mpg, aes(x=drv, y=hwy)) + geom_violin()


#add median to each box; geom are layers
ggplot(mpg, aes(x=class, y=hwy, colour = class)) + geom_boxplot() + geom_line()

#line plot
ggplot(mpg, aes(x=displ, y=hwy, colour = manufacturer)) + geom_line()

ggplot(mpg, aes(x=displ, y=hwy, colour = class)) + geom_line()


##histrograms; specify only x or y variable
ggplot(mpg, aes(x=displ)) + geom_histogram(binwidth = 0.5)

#add color for bars
ggplot(mpg, aes(x=displ)) + geom_histogram(binwidth = 0.5, fill = "blue", colour = "black")

ggplot(mpg, aes(x=class)) + geom_histogram(binwidth = 0.5,stat= "count", fill = "blue", colour = "black")

#add color by data
ggplot(mpg, aes(x=class, fill = class)) + geom_histogram(binwidth = 0.5,stat= "count")

#fill breaks up bars; stacked histogram***
ggplot(mpg, aes(x=class, fill = manufacturer)) + geom_histogram(binwidth = 0.5,stat= "count")


#break out the stacked histo
ggplot(mpg, aes(x=class, fill = manufacturer)) +
  geom_histogram(binwidth = 0.5,stat = "count", position = "dodge")

#time series comparison 
ggplot(economics_long, aes(x=date, y = value)) + geom_line(aes(colour = variable))
head(economics_long)

ggplot(economics_long, aes(x=date, y = value)) +
  geom_line(aes(colour = variable)) + facet_wrap(~variable)

##Scales data***
ggplot(economics_long, aes(x=date, y = value)) +
  geom_line(aes(colour = variable)) + facet_wrap(~variable, scales = "free_y")

#by columns 
ggplot(economics_long, aes(x=date, y = value)) +
  geom_line(aes(colour = variable)) +
  facet_wrap(~variable, scales = "free_y", ncol = 1)

#by rows
ggplot(economics_long, aes(x=date, y = value)) +
  geom_line(aes(colour = variable)) +
  facet_wrap(~variable, scales = "free_y", nrow = 1)

#date label in quotes is a literal representation "-"
#%Y is year, %m is month
ggplot(economics_long, aes(x=date, y = value)) +
  geom_line(aes(colour = variable)) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  scale_x_date(date_labels = "%Y-%m")

##logrithmic scaling 
ggplot(economics_long, aes(x=date, y = value)) +
  geom_line(aes(colour = variable)) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  scale_x_date(date_labels = "%Y-%m")+
  scale_y_log10(labels = scales::dollar_format())

# test -- control, shift, c will comment out mult rows
# test

#using themes, adding labels to plot; 
ggplot(economics_long, aes(x=date, y = value)) +
  geom_line(aes(colour = variable)) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  scale_x_date(date_labels = "%Y-%m")+
  scale_y_log10(labels = scales::dollar_format())+
  labs(title = "Title", subtitle = "subtitle", caption = "caption", tag = "tag")+
  xlab("xlabel") + ylab("ylabel")

 
#using themes, adding legend title as colour in labs()
ggplot(economics_long, aes(x=date, y = value)) +
  geom_line(aes(colour = variable)) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  scale_x_date(date_labels = "%Y-%m")+
  scale_y_log10(labels = scales::dollar_format())+
  labs(title = "Title", subtitle = "subtitle", caption = "caption", tag = "tag", colour = "color legend")+
  xlab("xlabel") + ylab("ylabel") 

