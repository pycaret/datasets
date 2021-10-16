Generate White Noise Data
================
Nikhil Gupta
2021-10-16 06:53:09

``` r
if (!require("pacman")) install.packages("pacman")
```

    ## Loading required package: pacman

``` r
pacman::p_load(dplyr, tswge)
```

``` r
data_dir = "../../data/time_series/white_noise/"
info_dir = "../../info/time_series/white_noise/"
```

``` r
# Remove current files
do.call(file.remove, list(list.files(data_dir, full.names = TRUE)))
```

    ## [1] TRUE TRUE TRUE TRUE TRUE

``` r
seed=42
set.seed(seed)

n_total = 0
n_pass = 0
n_error = 0
nreps = 500  # Number of time series to generate


results = tribble(~index, ~s, ~d, ~p, ~q, ~n, ~rep, ~mean, ~vara, ~seed, ~phi, ~theta)

for (rep in 1:nreps){
  s = 0
  d = 0
  p = 0
  q = 0
      
  # At least as many points as needed to compute the statistics
  n_min = 20
  len_multiplier = sample(2:100, 1)
  n = sample(n_min:(n_min*len_multiplier), size=1)
  
  mean = sample(-1000:1000, 1)
  vara = abs(rnorm(1, mean=0, sd=abs(mean/100)))  # Noise Variance
  sn = rep
  
  print(paste0("s=", s, " d=", d, " p=", p, " q=", q, " n=", n, " mean=", mean, " vara=", vara, " rep=",  rep, " sn=",  sn))
  
  pass = FALSE
  while (pass == FALSE){
  set.seed(n_error)
  
    if (p == 0){
      phi = 0
    }else {
      phi = rnorm(n=p, mean=0, sd=0.5)  
    }
    if (q == 0){
      theta = 0
    }else {
      theta = rnorm(n=q, mean=0, sd=0.5)
    }
    
    tryCatch(
      {
        y = mean + gen.aruma.wge(n=n, phi=phi, theta=theta, d=d, s=s, vara=vara, sn=sn, plot=FALSE)
        # plotts.sample.wge(y, lag.max=max(s*2, 25))
        pass = TRUE
        cat(paste0("\tPASS: ", pass, "\n"))
        n_pass = n_pass + 1
      }, error=function(e){
        n_error <<- n_error + 1
        cat(paste0("\tPASS: ", pass, " --> ERROR: ",conditionMessage(e), "\n"))
      }
    )
  }
  
  n_total = n_total + 1
  
  results = results %>% add_row(
    index=n_total,
    s=s, d=d, p=p, q=q,
    n=n,
    rep=rep,
    mean=mean,
    vara=vara,
    seed=sn,
    phi=paste(phi, collapse=" "),
    theta=paste(theta, collapse=" ")
  ) 
  
  ts_file_name = paste0(data_dir, n_total, ".csv")
  write.csv(y, ts_file_name, row.names = FALSE)

}
```

    ## [1] "s=0 d=0 p=0 q=0 n=340 mean=176 vara=0.639106003953717 rep=1 sn=1"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=241 mean=596 vara=7.61357524577275 rep=2 sn=2"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=452 mean=-675 vara=5.38865304478389 rep=3 sn=3"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=80 mean=-737 vara=4.11903386597799 rep=4 sn=4"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=441 mean=856 vara=1.82523319308466 rep=5 sn=5"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=912 mean=36 vara=0.312693796251237 rep=6 sn=6"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1492 mean=-287 vara=2.94251981509904 rep=7 sn=7"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=215 mean=-441 vara=6.72483756199434 rep=8 sn=8"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=728 mean=-290 vara=8.06845762962519 rep=9 sn=9"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=274 mean=501 vara=13.4983010091819 rep=10 sn=10"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1201 mean=-542 vara=1.70334235889429 rep=11 sn=11"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=418 mean=835 vara=5.04599783902856 rep=12 sn=12"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=112 mean=-237 vara=1.76084188234303 rep=13 sn=13"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1524 mean=435 vara=5.86456051593504 rep=14 sn=14"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1227 mean=-399 vara=1.11360600031963 rep=15 sn=15"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=527 mean=917 vara=10.2632591692743 rep=16 sn=16"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=128 mean=903 vara=7.83994915201602 rep=17 sn=17"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=69 mean=-578 vara=1.75777377853146 rep=18 sn=18"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=28 mean=335 vara=2.79476070564281 rep=19 sn=19"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=167 mean=379 vara=3.55768557286027 rep=20 sn=20"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1051 mean=-73 vara=0.438857901776653 rep=21 sn=21"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=47 mean=-858 vara=17.6000615781803 rep=22 sn=22"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1778 mean=213 vara=1.74819858140969 rep=23 sn=23"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=55 mean=-356 vara=3.79043394254037 rep=24 sn=24"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=481 mean=-621 vara=7.86606622594296 rep=25 sn=25"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1003 mean=-525 vara=1.41424412443791 rep=26 sn=26"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=62 mean=-414 vara=6.02800009040616 rep=27 sn=27"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=408 mean=230 vara=4.05960402012413 rep=28 sn=28"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=21 mean=-309 vara=0.56113167133375 rep=29 sn=29"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=165 mean=-677 vara=6.40355480186604 rep=30 sn=30"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=583 mean=-76 vara=1.5010481530326 rep=31 sn=31"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1333 mean=954 vara=1.34926379009635 rep=32 sn=32"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=816 mean=-115 vara=0.449575576669613 rep=33 sn=33"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=243 mean=589 vara=1.59561468158991 rep=34 sn=34"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=25 mean=998 vara=6.17659632437135 rep=35 sn=35"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=955 mean=307 vara=4.65801716626458 rep=36 sn=36"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=550 mean=793 vara=1.67012053687176 rep=37 sn=37"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1449 mean=421 vara=5.9453225753231 rep=38 sn=38"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=70 mean=876 vara=2.1327557859378 rep=39 sn=39"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=574 mean=-299 vara=3.10709464130806 rep=40 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=729 mean=625 vara=1.89755291299549 rep=41 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=27 mean=271 vara=1.04988022048666 rep=42 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=580 mean=-428 vara=1.29567728051755 rep=43 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=240 mean=-957 vara=3.40719935128103 rep=44 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=29 mean=120 vara=0.317218036610581 rep=45 sn=45"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=661 mean=-867 vara=4.11138818693897 rep=46 sn=46"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=196 mean=-40 vara=0.259969006588165 rep=47 sn=47"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=515 mean=-724 vara=4.62423918907629 rep=48 sn=48"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=252 mean=864 vara=11.9675363369356 rep=49 sn=49"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=499 mean=-886 vara=1.97242766704853 rep=50 sn=50"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=45 mean=50 vara=0.54693987605665 rep=51 sn=51"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1438 mean=-118 vara=0.00305083884098331 rep=52 sn=52"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=168 mean=-324 vara=1.35527654485339 rep=53 sn=53"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=634 mean=-989 vara=7.40405948089666 rep=54 sn=54"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=242 mean=-621 vara=6.84973610803805 rep=55 sn=55"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=94 mean=-465 vara=2.76563989520927 rep=56 sn=56"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1821 mean=828 vara=0.342706831076188 rep=57 sn=57"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=357 mean=-119 vara=0.518793774057463 rep=58 sn=58"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=51 mean=801 vara=4.40036633973358 rep=59 sn=59"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=885 mean=856 vara=21.8638275304367 rep=60 sn=60"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=56 mean=760 vara=3.45472062919843 rep=61 sn=61"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=995 mean=341 vara=1.87040780616902 rep=62 sn=62"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=500 mean=-344 vara=7.64995394272687 rep=63 sn=63"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=135 mean=-203 vara=0.681668456597344 rep=64 sn=64"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=667 mean=138 vara=1.67130967423241 rep=65 sn=65"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1471 mean=-334 vara=1.8857442064283 rep=66 sn=66"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=489 mean=505 vara=0.010967979000879 rep=67 sn=67"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1859 mean=-459 vara=8.54130222360239 rep=68 sn=68"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=48 mean=-998 vara=8.32440379274286 rep=69 sn=69"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=117 mean=975 vara=1.75348048623795 rep=70 sn=70"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1374 mean=-925 vara=0.658084096888681 rep=71 sn=71"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=909 mean=523 vara=1.32201891535001 rep=72 sn=72"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1210 mean=-769 vara=2.52293437405256 rep=73 sn=73"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=58 mean=446 vara=3.41017887788831 rep=74 sn=74"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=291 mean=478 vara=4.24720232840073 rep=75 sn=75"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=664 mean=416 vara=1.92600423794512 rep=76 sn=76"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=232 mean=671 vara=5.35546222071044 rep=77 sn=77"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=800 mean=992 vara=1.31664537744095 rep=78 sn=78"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1307 mean=358 vara=3.58204459892256 rep=79 sn=79"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=499 mean=-131 vara=1.62661436719131 rep=80 sn=80"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=165 mean=599 vara=4.63601299455905 rep=81 sn=81"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=119 mean=989 vara=3.55359313771139 rep=82 sn=82"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=112 mean=728 vara=2.75300051769736 rep=83 sn=83"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=522 mean=596 vara=3.33869632012995 rep=84 sn=84"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=369 mean=662 vara=0.904757644002467 rep=85 sn=85"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=535 mean=-204 vara=1.0997814420993 rep=86 sn=86"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=126 mean=259 vara=1.22559756097536 rep=87 sn=87"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=246 mean=179 vara=3.40248336792364 rep=88 sn=88"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=356 mean=-931 vara=9.90271484748967 rep=89 sn=89"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=259 mean=-304 vara=1.95804942421026 rep=90 sn=90"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1204 mean=449 vara=2.73542320081104 rep=91 sn=91"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=904 mean=-49 vara=0.0230786783853915 rep=92 sn=92"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=658 mean=-860 vara=10.6611886756295 rep=93 sn=93"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=411 mean=620 vara=3.839837786239 rep=94 sn=94"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=702 mean=-862 vara=15.5783868771 rep=95 sn=95"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=383 mean=-454 vara=8.18880775180699 rep=96 sn=96"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=140 mean=-212 vara=1.25818422813367 rep=97 sn=97"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=567 mean=-847 vara=9.47339330043757 rep=98 sn=98"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1001 mean=-640 vara=6.98005419543445 rep=99 sn=99"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=38 mean=553 vara=1.39137436579016 rep=100 sn=100"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1304 mean=-149 vara=0.608553293936776 rep=101 sn=101"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=255 mean=-364 vara=6.66647115014063 rep=102 sn=102"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=752 mean=-253 vara=3.19660799071113 rep=103 sn=103"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=418 mean=315 vara=1.87841651236142 rep=104 sn=104"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1189 mean=926 vara=0.0704224761948157 rep=105 sn=105"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1103 mean=-680 vara=0.555084510114864 rep=106 sn=106"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=261 mean=-9 vara=0.011240626547136 rep=107 sn=107"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=166 mean=-534 vara=5.43726910663578 rep=108 sn=108"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1462 mean=-860 vara=25.1419589357356 rep=109 sn=109"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=167 mean=838 vara=1.27380787720894 rep=110 sn=110"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=466 mean=754 vara=8.8763969581371 rep=111 sn=111"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1590 mean=-882 vara=1.48043483788589 rep=112 sn=112"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1774 mean=931 vara=3.09931696947039 rep=113 sn=113"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=132 mean=552 vara=3.39698005621191 rep=114 sn=114"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1212 mean=-283 vara=0.460227527446723 rep=115 sn=115"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=68 mean=166 vara=0.094933090310056 rep=116 sn=116"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=66 mean=525 vara=14.4785856110544 rep=117 sn=117"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=768 mean=990 vara=12.4626797934088 rep=118 sn=118"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=527 mean=921 vara=3.16822462155925 rep=119 sn=119"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=312 mean=250 vara=3.85737463825043 rep=120 sn=120"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=794 mean=-354 vara=4.67394304871172 rep=121 sn=121"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=501 mean=796 vara=14.6751942665327 rep=122 sn=122"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1207 mean=-755 vara=11.498278988798 rep=123 sn=123"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=55 mean=973 vara=13.0637307416592 rep=124 sn=124"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=285 mean=-67 vara=0.330802916013317 rep=125 sn=125"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=253 mean=341 vara=2.0456855525428 rep=126 sn=126"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=445 mean=724 vara=5.99996880012321 rep=127 sn=127"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=873 mean=-20 vara=0.196574056362372 rep=128 sn=128"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1007 mean=-40 vara=0.124050647476807 rep=129 sn=129"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1105 mean=837 vara=1.91461984683015 rep=130 sn=130"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=279 mean=388 vara=2.68524033266789 rep=131 sn=131"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1307 mean=683 vara=4.88415669235168 rep=132 sn=132"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=579 mean=211 vara=2.30216252635865 rep=133 sn=133"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1126 mean=-286 vara=0.593911981730565 rep=134 sn=134"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=702 mean=889 vara=16.4780001740129 rep=135 sn=135"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=103 mean=-820 vara=10.9381256544346 rep=136 sn=136"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=445 mean=167 vara=0.221205204217406 rep=137 sn=137"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1313 mean=-500 vara=2.05395447808058 rep=138 sn=138"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1361 mean=-320 vara=0.985152704417132 rep=139 sn=139"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=265 mean=-481 vara=4.96341083857749 rep=140 sn=140"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1520 mean=-52 vara=0.320948932292924 rep=141 sn=141"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=233 mean=481 vara=2.4149333485198 rep=142 sn=142"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=51 mean=291 vara=6.5918867503811 rep=143 sn=143"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=868 mean=69 vara=1.07134004216799 rep=144 sn=144"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=371 mean=-422 vara=11.7838253393658 rep=145 sn=145"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=328 mean=-389 vara=0.5195417434776 rep=146 sn=146"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=927 mean=-829 vara=20.1150096180739 rep=147 sn=147"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=688 mean=-20 vara=0.281076871354848 rep=148 sn=148"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=816 mean=-372 vara=5.1713967628611 rep=149 sn=149"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=288 mean=944 vara=1.9062668767238 rep=150 sn=150"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=35 mean=-519 vara=1.44168974223639 rep=151 sn=151"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=56 mean=-627 vara=1.34563538727532 rep=152 sn=152"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=72 mean=652 vara=1.40023572061461 rep=153 sn=153"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=92 mean=-258 vara=4.00606655671417 rep=154 sn=154"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=605 mean=-162 vara=3.67900854740018 rep=155 sn=155"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=219 mean=935 vara=9.56393741797297 rep=156 sn=156"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=80 mean=761 vara=9.5933097685259 rep=157 sn=157"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=218 mean=182 vara=4.12970503513915 rep=158 sn=158"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1324 mean=276 vara=2.10132988509715 rep=159 sn=159"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=285 mean=-531 vara=1.14467729433075 rep=160 sn=160"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=254 mean=-668 vara=2.60616966075219 rep=161 sn=161"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=31 mean=178 vara=1.40737609445647 rep=162 sn=162"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=214 mean=656 vara=0.854424144046597 rep=163 sn=163"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=165 mean=-868 vara=3.41696228539596 rep=164 sn=164"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=69 mean=-517 vara=2.22721204842242 rep=165 sn=165"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=710 mean=784 vara=7.87756542619026 rep=166 sn=166"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=484 mean=-228 vara=3.9898858841355 rep=167 sn=167"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=143 mean=-288 vara=2.09779573036633 rep=168 sn=168"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=589 mean=-148 vara=1.54798437296478 rep=169 sn=169"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=131 mean=-49 vara=0.393494046581405 rep=170 sn=170"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=381 mean=-629 vara=2.95321573683547 rep=171 sn=171"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=781 mean=-320 vara=2.31129593568642 rep=172 sn=172"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=834 mean=-192 vara=0.207177045511473 rep=173 sn=173"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=43 mean=169 vara=0.270181793094392 rep=174 sn=174"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1167 mean=715 vara=6.29012731332815 rep=175 sn=175"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1541 mean=-460 vara=2.209207614857 rep=176 sn=176"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=155 mean=0 vara=0 rep=177 sn=177"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=967 mean=-81 vara=0.310875220443957 rep=178 sn=178"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1062 mean=860 vara=4.94314786639772 rep=179 sn=179"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=49 mean=-907 vara=6.41087412197399 rep=180 sn=180"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=856 mean=54 vara=0.630098709358163 rep=181 sn=181"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=54 mean=-873 vara=0.71769573455827 rep=182 sn=182"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1665 mean=187 vara=0.290167320193687 rep=183 sn=183"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1745 mean=5 vara=0.00862094904815409 rep=184 sn=184"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=205 mean=385 vara=1.39928301367431 rep=185 sn=185"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=456 mean=-327 vara=2.7244797405011 rep=186 sn=186"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=70 mean=-274 vara=0.399456766185467 rep=187 sn=187"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=952 mean=-598 vara=5.40802503639016 rep=188 sn=188"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=128 mean=912 vara=0.967942389418671 rep=189 sn=189"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=31 mean=635 vara=15.8899572970329 rep=190 sn=190"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1232 mean=873 vara=2.22388362876273 rep=191 sn=191"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=71 mean=717 vara=1.21710276532023 rep=192 sn=192"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1741 mean=-987 vara=0.494307404802173 rep=193 sn=193"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=656 mean=701 vara=12.8726067799777 rep=194 sn=194"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=985 mean=-710 vara=5.35832814148414 rep=195 sn=195"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=158 mean=707 vara=1.03948288144667 rep=196 sn=196"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=232 mean=-274 vara=2.80334541119981 rep=197 sn=197"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=238 mean=540 vara=5.60570230595487 rep=198 sn=198"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=54 mean=-184 vara=4.0797600477849 rep=199 sn=199"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=134 mean=-188 vara=1.49670808454856 rep=200 sn=200"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=436 mean=447 vara=1.7112997529314 rep=201 sn=201"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=486 mean=-665 vara=2.68074759318012 rep=202 sn=202"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=647 mean=-115 vara=1.1513947713661 rep=203 sn=203"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1315 mean=-365 vara=0.126760364504186 rep=204 sn=204"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=33 mean=-189 vara=2.36629009285703 rep=205 sn=205"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1855 mean=-170 vara=0.76404058103631 rep=206 sn=206"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=416 mean=-239 vara=0.266214030768591 rep=207 sn=207"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=272 mean=418 vara=6.06010404859373 rep=208 sn=208"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=922 mean=847 vara=1.47205840819265 rep=209 sn=209"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=171 mean=-512 vara=9.30215210851539 rep=210 sn=210"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=58 mean=-305 vara=2.13129159900317 rep=211 sn=211"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=112 mean=-854 vara=12.610916961909 rep=212 sn=212"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=598 mean=-25 vara=0.16784935567495 rep=213 sn=213"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=322 mean=443 vara=2.79863839291731 rep=214 sn=214"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=524 mean=-956 vara=22.71122618866 rep=215 sn=215"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1061 mean=-458 vara=8.64916724139149 rep=216 sn=216"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1804 mean=-96 vara=0.30215322906485 rep=217 sn=217"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=663 mean=-736 vara=1.72089427530234 rep=218 sn=218"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=22 mean=-981 vara=3.36496823203387 rep=219 sn=219"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1096 mean=89 vara=0.398078895310106 rep=220 sn=220"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=218 mean=-103 vara=0.449931293477835 rep=221 sn=221"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=335 mean=718 vara=6.90488594792318 rep=222 sn=222"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=233 mean=648 vara=1.20841066756322 rep=223 sn=223"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=817 mean=-230 vara=1.10384925906783 rep=224 sn=224"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1110 mean=-553 vara=6.13195304936303 rep=225 sn=225"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=199 mean=-18 vara=0.258721632589005 rep=226 sn=226"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1045 mean=583 vara=0.608122736358755 rep=227 sn=227"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1633 mean=-404 vara=2.60308988733106 rep=228 sn=228"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1078 mean=750 vara=9.1823866224249 rep=229 sn=229"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=139 mean=608 vara=1.98987366065973 rep=230 sn=230"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=111 mean=370 vara=2.70521544839694 rep=231 sn=231"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1354 mean=-385 vara=5.08843250297302 rep=232 sn=232"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=37 mean=368 vara=4.14210734311868 rep=233 sn=233"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=563 mean=-776 vara=19.3431611006468 rep=234 sn=234"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=979 mean=-889 vara=7.2896117824738 rep=235 sn=235"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=610 mean=627 vara=1.39490307796163 rep=236 sn=236"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=85 mean=322 vara=2.07227178139805 rep=237 sn=237"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=831 mean=572 vara=6.68967068097604 rep=238 sn=238"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=67 mean=706 vara=17.8275249038376 rep=239 sn=239"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=582 mean=-116 vara=0.235518442996742 rep=240 sn=240"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=49 mean=-649 vara=3.02174579840809 rep=241 sn=241"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=506 mean=282 vara=0.94857878909474 rep=242 sn=242"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=395 mean=532 vara=5.10368112505427 rep=243 sn=243"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=70 mean=394 vara=0.431850495899149 rep=244 sn=244"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=990 mean=-767 vara=7.83972078505299 rep=245 sn=245"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=750 mean=302 vara=3.82727380484255 rep=246 sn=246"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=367 mean=-1 vara=0.0194481131631589 rep=247 sn=247"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=451 mean=716 vara=10.8819471625135 rep=248 sn=248"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=488 mean=889 vara=15.501526695451 rep=249 sn=249"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=168 mean=852 vara=1.76261992878962 rep=250 sn=250"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=431 mean=228 vara=4.75837429052967 rep=251 sn=251"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1523 mean=-905 vara=5.79591555918972 rep=252 sn=252"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=935 mean=-515 vara=1.57098029223227 rep=253 sn=253"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=449 mean=-477 vara=7.21951999099466 rep=254 sn=254"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=144 mean=-639 vara=7.64131602453758 rep=255 sn=255"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=60 mean=5 vara=0.0379496143217677 rep=256 sn=256"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=934 mean=-539 vara=4.94373740646994 rep=257 sn=257"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1287 mean=-615 vara=2.22605404916436 rep=258 sn=258"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=529 mean=-600 vara=3.18292729453546 rep=259 sn=259"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=33 mean=-805 vara=0.479869931138083 rep=260 sn=260"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=20 mean=-746 vara=3.21404171483722 rep=261 sn=261"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=68 mean=385 vara=1.28344695461874 rep=262 sn=262"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=801 mean=111 vara=0.0629587428410588 rep=263 sn=263"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=103 mean=577 vara=2.54071807015243 rep=264 sn=264"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=383 mean=81 vara=0.0447233316381394 rep=265 sn=265"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=100 mean=-618 vara=1.77662794863471 rep=266 sn=266"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=712 mean=520 vara=4.75044002539669 rep=267 sn=267"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=578 mean=-759 vara=6.5247234801408 rep=268 sn=268"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=240 mean=-358 vara=1.70006910711254 rep=269 sn=269"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1262 mean=-955 vara=5.19645005276595 rep=270 sn=270"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=772 mean=314 vara=4.11299648997645 rep=271 sn=271"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=666 mean=947 vara=10.1462797512696 rep=272 sn=272"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=737 mean=-649 vara=1.8127968570397 rep=273 sn=273"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=309 mean=457 vara=0.268792051952858 rep=274 sn=274"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=54 mean=-988 vara=5.95172906825805 rep=275 sn=275"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=68 mean=-66 vara=0.299926678955819 rep=276 sn=276"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=664 mean=784 vara=5.92533026534527 rep=277 sn=277"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=919 mean=-424 vara=3.90775545640244 rep=278 sn=278"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=318 mean=301 vara=6.64915972753722 rep=279 sn=279"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=648 mean=-436 vara=2.38027444526603 rep=280 sn=280"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1167 mean=-920 vara=1.41264208708176 rep=281 sn=281"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1374 mean=34 vara=0.42040235542958 rep=282 sn=282"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=738 mean=352 vara=3.60539497991145 rep=283 sn=283"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=707 mean=583 vara=3.74297516145522 rep=284 sn=284"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1475 mean=-16 vara=0.062242765365103 rep=285 sn=285"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1331 mean=926 vara=7.2628396887499 rep=286 sn=286"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=244 mean=887 vara=1.75251055319604 rep=287 sn=287"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=420 mean=-186 vara=2.02373333954074 rep=288 sn=288"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=900 mean=-652 vara=0.461549261347367 rep=289 sn=289"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1569 mean=-504 vara=2.42838918498828 rep=290 sn=290"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=377 mean=838 vara=9.92200480614043 rep=291 sn=291"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=49 mean=328 vara=0.465588391701414 rep=292 sn=292"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=25 mean=29 vara=0.243120799299482 rep=293 sn=293"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=174 mean=-222 vara=0.486025404570925 rep=294 sn=294"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=257 mean=-381 vara=1.9659173668571 rep=295 sn=295"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=360 mean=400 vara=1.38763798375793 rep=296 sn=296"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1141 mean=363 vara=6.65326188825458 rep=297 sn=297"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=664 mean=163 vara=2.32875346874294 rep=298 sn=298"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=636 mean=157 vara=1.31091430272943 rep=299 sn=299"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=79 mean=832 vara=13.6103470450859 rep=300 sn=300"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=202 mean=857 vara=12.9133270211616 rep=301 sn=301"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=275 mean=245 vara=1.40028905163953 rep=302 sn=302"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=637 mean=959 vara=0.94192047462602 rep=303 sn=303"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=53 mean=-512 vara=2.83522486242848 rep=304 sn=304"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=126 mean=506 vara=0.84446346083371 rep=305 sn=305"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1017 mean=-254 vara=0.149217426125466 rep=306 sn=306"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=39 mean=-304 vara=2.93329486688841 rep=307 sn=307"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1072 mean=907 vara=12.5467095362028 rep=308 sn=308"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=565 mean=-558 vara=5.85746076583349 rep=309 sn=309"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=23 mean=66 vara=0.637632916988333 rep=310 sn=310"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=432 mean=-661 vara=4.80831876055188 rep=311 sn=311"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=159 mean=-738 vara=0.755388540399889 rep=312 sn=312"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=102 mean=-184 vara=0.816432766565734 rep=313 sn=313"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=74 mean=853 vara=6.78091637483178 rep=314 sn=314"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=203 mean=-391 vara=14.5686536064413 rep=315 sn=315"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=436 mean=95 vara=0.642983440407009 rep=316 sn=316"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=318 mean=373 vara=4.10646973456494 rep=317 sn=317"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1041 mean=-832 vara=0.208795163974899 rep=318 sn=318"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=72 mean=301 vara=0.538192050748416 rep=319 sn=319"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=878 mean=901 vara=11.8816137270694 rep=320 sn=320"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=25 mean=-63 vara=0.0842496429833961 rep=321 sn=321"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1106 mean=-595 vara=6.82100548455022 rep=322 sn=322"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1182 mean=-963 vara=5.01335837253588 rep=323 sn=323"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=828 mean=619 vara=14.3229052728407 rep=324 sn=324"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=445 mean=-483 vara=0.340929094891186 rep=325 sn=325"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1013 mean=788 vara=4.89280835314257 rep=326 sn=326"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=120 mean=163 vara=0.929738545343316 rep=327 sn=327"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=195 mean=-833 vara=12.5999103384362 rep=328 sn=328"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=633 mean=-493 vara=3.37546914045655 rep=329 sn=329"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=331 mean=362 vara=6.44321253471092 rep=330 sn=330"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=165 mean=982 vara=25.3999611263241 rep=331 sn=331"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1256 mean=-127 vara=1.24283978345024 rep=332 sn=332"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=377 mean=876 vara=12.5081652076963 rep=333 sn=333"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=591 mean=-672 vara=5.04217248711285 rep=334 sn=334"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=343 mean=599 vara=2.66354783445937 rep=335 sn=335"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=313 mean=983 vara=1.0444555904083 rep=336 sn=336"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1476 mean=128 vara=0.499988513610547 rep=337 sn=337"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=188 mean=260 vara=3.87099719307667 rep=338 sn=338"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=353 mean=490 vara=0.694555344521811 rep=339 sn=339"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=289 mean=-444 vara=2.8045605732653 rep=340 sn=340"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=627 mean=165 vara=3.25439812178827 rep=341 sn=341"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=320 mean=-26 vara=0.13018255496963 rep=342 sn=342"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=107 mean=625 vara=5.66467200237707 rep=343 sn=343"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=245 mean=-456 vara=2.22910327293581 rep=344 sn=344"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=821 mean=-10 vara=0.0220450341900236 rep=345 sn=345"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=193 mean=6 vara=0.0309967042774709 rep=346 sn=346"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=161 mean=-846 vara=3.39836798684151 rep=347 sn=347"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=26 mean=713 vara=0.841084594781783 rep=348 sn=348"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1400 mean=-169 vara=0.99773299509469 rep=349 sn=349"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1313 mean=-77 vara=0.291778695626286 rep=350 sn=350"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=766 mean=656 vara=2.64882055513476 rep=351 sn=351"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1346 mean=-531 vara=5.91402517296881 rep=352 sn=352"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1743 mean=16 vara=0.0455369092481284 rep=353 sn=353"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=130 mean=644 vara=12.367142407804 rep=354 sn=354"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=775 mean=143 vara=1.99315507989262 rep=355 sn=355"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=119 mean=906 vara=4.77417434471959 rep=356 sn=356"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=326 mean=617 vara=2.3387360256027 rep=357 sn=357"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=318 mean=-606 vara=3.00932903784634 rep=358 sn=358"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=640 mean=663 vara=5.08911199741655 rep=359 sn=359"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1380 mean=-390 vara=9.08709539852999 rep=360 sn=360"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=412 mean=-986 vara=10.9648634799119 rep=361 sn=361"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=359 mean=766 vara=22.0518626285296 rep=362 sn=362"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=614 mean=-994 vara=8.80696091025719 rep=363 sn=363"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=330 mean=971 vara=0.267689204124425 rep=364 sn=364"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=661 mean=137 vara=0.0896473863635927 rep=365 sn=365"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=179 mean=-548 vara=1.08609925682444 rep=366 sn=366"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=133 mean=-48 vara=0.416122069767877 rep=367 sn=367"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=101 mean=-937 vara=4.42103445566224 rep=368 sn=368"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=177 mean=-982 vara=7.90008159075371 rep=369 sn=369"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=644 mean=808 vara=3.3406777386081 rep=370 sn=370"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=151 mean=-344 vara=5.86234506245165 rep=371 sn=371"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=198 mean=48 vara=0.0317298883137997 rep=372 sn=372"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=140 mean=690 vara=3.15642705013428 rep=373 sn=373"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=51 mean=74 vara=0.576294422567538 rep=374 sn=374"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=264 mean=921 vara=0.0567484991416089 rep=375 sn=375"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=245 mean=-447 vara=2.65291711147011 rep=376 sn=376"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=20 mean=102 vara=0.303411342220673 rep=377 sn=377"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=567 mean=3 vara=0.0170065490601796 rep=378 sn=378"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=338 mean=-281 vara=1.82626024001081 rep=379 sn=379"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=35 mean=89 vara=0.0384509711353905 rep=380 sn=380"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=127 mean=-769 vara=9.15601669716289 rep=381 sn=381"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=171 mean=147 vara=0.275878653053343 rep=382 sn=382"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=684 mean=92 vara=0.895351768304822 rep=383 sn=383"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=859 mean=775 vara=2.87606011199663 rep=384 sn=384"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=289 mean=-81 vara=0.461557567757379 rep=385 sn=385"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=575 mean=109 vara=0.952285960217945 rep=386 sn=386"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=767 mean=-299 vara=3.7170458776836 rep=387 sn=387"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1345 mean=-535 vara=6.42159019011413 rep=388 sn=388"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1421 mean=-302 vara=3.78705975997827 rep=389 sn=389"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=876 mean=-539 vara=2.39969787990447 rep=390 sn=390"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1386 mean=-991 vara=4.33418222459611 rep=391 sn=391"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=513 mean=557 vara=2.33687103591465 rep=392 sn=392"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1484 mean=-104 vara=1.92105045332101 rep=393 sn=393"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1869 mean=310 vara=1.43331745927431 rep=394 sn=394"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=29 mean=508 vara=0.899138113799617 rep=395 sn=395"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=302 mean=-297 vara=0.326077778233082 rep=396 sn=396"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=246 mean=562 vara=1.85792400463633 rep=397 sn=397"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=153 mean=-874 vara=1.95997488148874 rep=398 sn=398"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=76 mean=353 vara=0.847404641755676 rep=399 sn=399"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=298 mean=-588 vara=0.50580926276033 rep=400 sn=400"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=749 mean=-58 vara=0.0218555831091069 rep=401 sn=401"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=307 mean=298 vara=0.321811055917709 rep=402 sn=402"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=672 mean=966 vara=3.84882079266111 rep=403 sn=403"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=430 mean=928 vara=4.52674349087534 rep=404 sn=404"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=770 mean=858 vara=13.9359710699532 rep=405 sn=405"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=131 mean=-372 vara=0.431766262589798 rep=406 sn=406"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=795 mean=-511 vara=6.95789148646741 rep=407 sn=407"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=185 mean=-568 vara=0.552200345739529 rep=408 sn=408"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=282 mean=524 vara=4.10700045459792 rep=409 sn=409"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=34 mean=-819 vara=2.53689821210733 rep=410 sn=410"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=411 mean=-790 vara=8.57801555398505 rep=411 sn=411"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1329 mean=629 vara=5.04686428614219 rep=412 sn=412"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=196 mean=43 vara=0.558595116444748 rep=413 sn=413"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1124 mean=-30 vara=0.484668362257355 rep=414 sn=414"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=621 mean=-861 vara=8.57739723963193 rep=415 sn=415"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=67 mean=-416 vara=0.506953909287629 rep=416 sn=416"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=238 mean=-623 vara=12.04208758537 rep=417 sn=417"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=517 mean=-124 vara=0.853351685060107 rep=418 sn=418"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=385 mean=447 vara=0.0709450400512449 rep=419 sn=419"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=139 mean=847 vara=12.1391530215976 rep=420 sn=420"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=601 mean=175 vara=1.81744820629033 rep=421 sn=421"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=97 mean=-955 vara=12.2145756266997 rep=422 sn=422"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=734 mean=-334 vara=0.13556573869856 rep=423 sn=423"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=951 mean=-347 vara=3.079693103312 rep=424 sn=424"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=103 mean=204 vara=0.694464256883797 rep=425 sn=425"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=141 mean=826 vara=3.98647351332027 rep=426 sn=426"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=123 mean=-989 vara=11.8619108781627 rep=427 sn=427"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1718 mean=654 vara=1.47302422565481 rep=428 sn=428"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=37 mean=994 vara=5.36442932645002 rep=429 sn=429"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=87 mean=-770 vara=16.1268178179971 rep=430 sn=430"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=61 mean=418 vara=6.92260271462962 rep=431 sn=431"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=850 mean=-214 vara=0.729887143372619 rep=432 sn=432"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=129 mean=344 vara=0.53406555376233 rep=433 sn=433"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=550 mean=-184 vara=0.507471579085785 rep=434 sn=434"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=641 mean=568 vara=7.53199922200041 rep=435 sn=435"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=288 mean=-590 vara=1.87323856336369 rep=436 sn=436"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=175 mean=925 vara=14.8383255155952 rep=437 sn=437"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=81 mean=476 vara=8.67916440390998 rep=438 sn=438"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1235 mean=-165 vara=1.06280806311226 rep=439 sn=439"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=677 mean=501 vara=0.202617038836277 rep=440 sn=440"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=962 mean=-341 vara=0.96294725447947 rep=441 sn=441"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=540 mean=25 vara=0.0174787585682674 rep=442 sn=442"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=519 mean=-984 vara=9.49219787331992 rep=443 sn=443"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=645 mean=-425 vara=0.813109353549301 rep=444 sn=444"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=265 mean=356 vara=6.50445383772302 rep=445 sn=445"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1364 mean=-141 vara=0.113704737823804 rep=446 sn=446"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=800 mean=-457 vara=2.20440698376048 rep=447 sn=447"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1305 mean=-51 vara=0.0539682305120632 rep=448 sn=448"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=36 mean=29 vara=0.288367132603863 rep=449 sn=449"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=663 mean=141 vara=0.446555854215521 rep=450 sn=450"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=462 mean=886 vara=9.37743337986335 rep=451 sn=451"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=769 mean=986 vara=1.93229132051306 rep=452 sn=452"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=807 mean=248 vara=3.32219644564245 rep=453 sn=453"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1361 mean=182 vara=0.580874350553119 rep=454 sn=454"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1318 mean=179 vara=2.14275647376026 rep=455 sn=455"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1015 mean=-187 vara=0.874921716470001 rep=456 sn=456"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=773 mean=229 vara=0.624448830645592 rep=457 sn=457"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=752 mean=-248 vara=0.0768506925430694 rep=458 sn=458"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=564 mean=-692 vara=2.03119049118496 rep=459 sn=459"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=140 mean=-318 vara=5.4424136141993 rep=460 sn=460"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=806 mean=-71 vara=0.252989453283017 rep=461 sn=461"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=25 mean=856 vara=5.11266322955003 rep=462 sn=462"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1837 mean=-46 vara=0.83059482563931 rep=463 sn=463"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=275 mean=376 vara=1.3761619249695 rep=464 sn=464"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=869 mean=366 vara=1.78246304040498 rep=465 sn=465"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=868 mean=-489 vara=7.46316908900038 rep=466 sn=466"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=824 mean=951 vara=11.797675923379 rep=467 sn=467"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1425 mean=-61 vara=0.127817036761841 rep=468 sn=468"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=162 mean=469 vara=6.97964564099132 rep=469 sn=469"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=476 mean=-88 vara=0.256855384934599 rep=470 sn=470"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1486 mean=334 vara=0.821945568562682 rep=471 sn=471"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=54 mean=-195 vara=0.74374647040783 rep=472 sn=472"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=716 mean=-977 vara=9.67058415771383 rep=473 sn=473"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=293 mean=-600 vara=1.3729061620822 rep=474 sn=474"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1080 mean=-301 vara=1.27400492216953 rep=475 sn=475"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=427 mean=598 vara=3.54787388069287 rep=476 sn=476"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=35 mean=85 vara=0.170636811209312 rep=477 sn=477"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=482 mean=-186 vara=0.894704785139097 rep=478 sn=478"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1291 mean=327 vara=0.522179846326869 rep=479 sn=479"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=229 mean=-56 vara=0.192757403105973 rep=480 sn=480"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=507 mean=-108 vara=0.610474761522731 rep=481 sn=481"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1551 mean=-642 vara=2.1500846962461 rep=482 sn=482"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=719 mean=-560 vara=9.73086099986375 rep=483 sn=483"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=142 mean=354 vara=2.1474388220349 rep=484 sn=484"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=288 mean=395 vara=3.82809668050107 rep=485 sn=485"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=389 mean=801 vara=4.80667002270064 rep=486 sn=486"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=837 mean=745 vara=11.1700008236419 rep=487 sn=487"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=899 mean=-339 vara=2.46019962081357 rep=488 sn=488"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=1587 mean=901 vara=1.18326167920539 rep=489 sn=489"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=529 mean=647 vara=1.11269111479612 rep=490 sn=490"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=639 mean=-616 vara=1.57257825118566 rep=491 sn=491"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=514 mean=-989 vara=8.73438356532715 rep=492 sn=492"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=530 mean=50 vara=0.839415688034915 rep=493 sn=493"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=657 mean=-698 vara=5.13960676234856 rep=494 sn=494"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=338 mean=-96 vara=1.68164215959165 rep=495 sn=495"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=416 mean=-434 vara=8.22274731566587 rep=496 sn=496"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=606 mean=740 vara=9.08813820478849 rep=497 sn=497"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=509 mean=-472 vara=0.732093630151438 rep=498 sn=498"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=341 mean=-956 vara=0.728764542782154 rep=499 sn=499"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=0 n=350 mean=597 vara=8.87324992773246 rep=500 sn=500"
    ##  PASS: TRUE

``` r
info_file_name = paste0(info_dir, "info.csv")
write.csv(results, info_file_name, row.names = FALSE)
```

``` r
print(results)
```

    ## # A tibble: 500 x 12
    ##    index     s     d     p     q     n   rep  mean   vara  seed phi   theta
    ##    <dbl> <dbl> <dbl> <dbl> <dbl> <int> <int> <int>  <dbl> <int> <chr> <chr>
    ##  1     1     0     0     0     0   340     1   176  0.639     1 0     0    
    ##  2     2     0     0     0     0   241     2   596  7.61      2 0     0    
    ##  3     3     0     0     0     0   452     3  -675  5.39      3 0     0    
    ##  4     4     0     0     0     0    80     4  -737  4.12      4 0     0    
    ##  5     5     0     0     0     0   441     5   856  1.83      5 0     0    
    ##  6     6     0     0     0     0   912     6    36  0.313     6 0     0    
    ##  7     7     0     0     0     0  1492     7  -287  2.94      7 0     0    
    ##  8     8     0     0     0     0   215     8  -441  6.72      8 0     0    
    ##  9     9     0     0     0     0   728     9  -290  8.07      9 0     0    
    ## 10    10     0     0     0     0   274    10   501 13.5      10 0     0    
    ## # ... with 490 more rows

``` r
print(paste0("Total: ", n_total, " Error: ", n_error, " Pass: ", n_pass))
```

    ## [1] "Total: 500 Error: 0 Pass: 500"

``` r
results %>% 
  group_by(s, d) %>% 
  mutate(n=n(), mean_p= mean(p), sd_p = sd(p), mean_q= mean(q), sd_q = sd(q))
```

    ## # A tibble: 500 x 16
    ## # Groups:   s, d [1]
    ##    index     s     d     p     q     n   rep  mean   vara  seed phi   theta
    ##    <dbl> <dbl> <dbl> <dbl> <dbl> <int> <int> <int>  <dbl> <int> <chr> <chr>
    ##  1     1     0     0     0     0   500     1   176  0.639     1 0     0    
    ##  2     2     0     0     0     0   500     2   596  7.61      2 0     0    
    ##  3     3     0     0     0     0   500     3  -675  5.39      3 0     0    
    ##  4     4     0     0     0     0   500     4  -737  4.12      4 0     0    
    ##  5     5     0     0     0     0   500     5   856  1.83      5 0     0    
    ##  6     6     0     0     0     0   500     6    36  0.313     6 0     0    
    ##  7     7     0     0     0     0   500     7  -287  2.94      7 0     0    
    ##  8     8     0     0     0     0   500     8  -441  6.72      8 0     0    
    ##  9     9     0     0     0     0   500     9  -290  8.07      9 0     0    
    ## 10    10     0     0     0     0   500    10   501 13.5      10 0     0    
    ## # ... with 490 more rows, and 4 more variables: mean_p <dbl>, sd_p <dbl>,
    ## #   mean_q <dbl>, sd_q <dbl>
