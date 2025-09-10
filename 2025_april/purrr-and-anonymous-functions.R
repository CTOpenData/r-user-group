# For folks who are struggling with the ~ notation in `purrr`
# I see you. The new anonymous functions in base R miiiiigghhtttt
# be able to help you out. 


# note: one isn't necessarily better than the other HOWEVER
# if your audience is using an older version of R they may
# not have access to the native pipe |> or the new anonymous 
# functions \(x) foo(x)
# So keep your audience and teammates in mind :)


# assuming you have the tidyverse installed
library(tidyverse) |> suppressPackageStartupMessages()

# make up some data

my_list <- 
  dplyr::lst(
    mtcars,
    diamonds,
    diamonds |> dplyr::slice_head(n = 25000),
    diamonds |> dplyr::slice_tail(n = 25000)
  ) |> 
  purrr::set_names(
    c("some_cars", "all_diamonds", "some_diamonds", "the_last_diamonds")
  )

class(my_list$some_cars) #notice some_cars is not a tibble


# let's ensure ALL dataframes are also tibbles
# purrr ~ notation

my_list |> 
  purrr::map(
    ~ tibble::as_tibble(.x)
  )

# ~ is usually for something a little more complex
# because otherwise you would just write the following

my_list |> 
  purrr::map(tibble::as_tibble)

# or

purrr::map(my_list, tibble::as_tibble)

# say you wanted take a different row from each data frame
# you might use purrr::map2() which takes a .x and .y
tilde <- 
  purrr::map2(
    .x = my_list,
    .y = c(1,2,3,4),
    ~ tibble::as_tibble(.x[.y,]) |> dplyr::mutate(row_id = .y)
  )
tilde

# and that ^ is totally fine. But what if you're scanning
# quickly and you can't remember what is .x or .y
# or you're teaching someone completely new to iterations
# in programming?  The new anonymous function
# might be a viable alternative for you.

longer_anon <- 
  purrr::map2(
    my_list,
    c(1,2,3,4),
    # new-ish anonymous function uses \() notation
    # user curly brackets for multi-line functions
    # or remove them if it's a one-liner
    \(dataframe, row_number_to_grab) {
      dataframe[row_number_to_grab,] |> 
        tibble::as_tibble() |> 
        dplyr::mutate(row_id = row_number_to_grab)
    }
  )
longer_anon

#these can be short and sweet too
short_anon <- 
  purrr::map2(
    my_list,
    c(1,2,3,4),
    #look familiar?
    \(ex, why) tibble::as_tibble(ex[why,]) |> dplyr::mutate(row_id = why)
  )
short_anon

# are they the same? survey says...

identical(tilde, longer_anon)
identical(tilde, short_anon)
