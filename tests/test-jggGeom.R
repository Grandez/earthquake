library(earthquake)
library(testthat)
library(dplyr)
library(lubridate)
library(ggplot2)
library(leaflet)

# Previous task

df = eq_load_data("signif.txt")

## geom_timeline test
test_geom_timeline <- geom_timeline()
test_that("geom_timeline has correct class", {
       expect_is(test_geom_timeline ,"ggproto")
})

## geom_timeline_label test
test_geom_timeline_label <- geom_timeline_label()
test_that("geom_timeline_label has correct class", {
   expect_is(test_geom_timeline_label ,"ggproto")
})

## eq_geom_timeline test
test_plot <- eq_geom_timeline(df)
test_that("result of eq_geom_timeline is a plot", {
    expect_is(test_plot,"ggplot")
})

## eq_geom_timeline test
test_plot_label <- eq_geom_timeline_label(df)
test_that("result of eq_geom_timeline_label is a plot", {
    expect_is(test_plot_label,"ggplot")
})

## eq_create_label tests
test_annote <- df %>% filter(COUNTRY == "MEXICO" & year(DATE) >= 2000)
# %>%   mutate(popup_text = eq_create_label(.))

test_that("eq_create_label has correct classes", {
    expect_is(test_annote,"data.frame")
})

## eq_map tests

test_map <- test_annote %>% eq_map(annot_col="DATE")

test_that("eq_map has correct class", {
    expect_is(test_map ,"leaflet")
})
