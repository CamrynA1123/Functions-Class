library(dplyr)
library(kableExtra)
library(knitr)
iris |>
  group_by(Species) |>
  summarise(
    Avg = mean(Petal.Length)
  ) |>
  kable()

iris

groupsum = function(df, group, var) {
 require(dplyr); require(knitr) #semicolons are hard stops to fxn
   df |>
    group_by({{group}})|> #the double curlies are "the embrace"
  summarise(
    Avg = mean({{var}}), na.rm = TRUE
  ) |>
    kable()
}

groupsum(iris, Species, Petal.Length)

mtcars
groupsum(mtcars, cyl, mpg)


#make a fxn that will group_by Species for all variables----
require(dplyr); require(knitr)
iris |>
  group_by(Species) |>
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE)#is.numeric identifies the columns that will be impacted by fxn
  ) |>
  kable()

groupall = function(df, group, ...) {
  require(dplyr); require(knitr)
  df |>
    select({{group}}, ...) |> #takes dif columns out of df pulls out specifc varaiables 
    group_by({{group}}) |>
    summarise(
      across(everything(), mean, na.rm = TRUE) #more efficient than writing everything out
    ) |>
    kable()
}

#testing out the more adaptable fxn---------
groupall(iris, Species, is.numeric)

groupall(iris, Species, starts_with('Petal'))

groupall(iris, Species, Petal.Length, Petal.Width)

#--------------------
library(ggplot2)
mtcars

  mtcars |>
  ggplot(aes(x = mpg, y = disp)) +
  geom_point(
    shape = 21,
    color = 'pink'
  ) + 
  geom_smooth(
    color = 'dodgerblue'
  ) + 
  theme_bw() 

  
Wave = function(df, x, y) {
  require(ggplot2);
  df |>
    ggplot(aes({{x}}, {{y}})) +
    geom_point(
      shape = 21,
      color = 'pink'
    ) + 
    geom_smooth(
      method = 'lm', 
      se = FALSE,
      color = 'dodgerblue'
    ) + 
    theme_bw() + 
    theme(
      text = element_text(family = 'serif')
    )
}  

Wave(mtcars, mpg, disp) + 
  labs(
    y = 'Miles per Gallon',
    x = 'Displacement'
  )







