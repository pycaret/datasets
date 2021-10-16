Generate Seasonal Data
================
Nikhil Gupta
2021-10-16 06:35:27

``` r
if (!require("pacman")) install.packages("pacman")
```

    ## Loading required package: pacman

``` r
pacman::p_load(dplyr, tswge)
```

``` r
data_dir = "../../data/time_series/seasonal/"
info_dir = "../../info/time_series/seasonal/"
```

``` r
# Remove current files
do.call(file.remove, list(list.files(data_dir, full.names = TRUE)))
```

    ##   [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [16] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [31] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [46] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [61] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [76] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [91] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [106] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [121] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [136] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [151] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [166] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [181] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [196] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [211] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [226] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [241] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [256] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [271] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [286] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [301] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [316] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [331] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [346] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [361] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [376] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [391] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [406] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [421] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [436] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [451] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [466] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [481] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [496] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [511] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

``` r
seed=42
set.seed(seed)

n_total = 0
n_pass = 0
n_error = 0
nreps = 5


results = tribble(~index, ~s, ~d, ~p, ~q, ~n, ~rep, ~mean, ~vara, ~seed, ~phi, ~theta)

for (s in 1:52){   
  for (d in 0:1){
    for (rep in 1:nreps){
      p = sample(0:12, 1)
      q = sample(0:12, 1)
      
      # At least as many points as needed to compute the statistics
      n_min = s*(d+1)*2 + p + q + 2
      len_multiplier = sample(2:10, 1)
      n = sample(n_min:(n_min*len_multiplier), size=1)
      
      mean = sample(-1000:1000, 1)
      vara = abs(rnorm(1, mean=0, sd=abs(mean/100)))  # Noise Variance
      sn = 42 - round(nreps/2) + rep - 1 
      
      print(paste0("s=", s, " d=", d, " p=", p, " q=", q, " n=", n, " mean=", mean, " vara=", vara, " rep=",  rep, " sn=",  sn))
      
      pass = FALSE
      while (pass == FALSE){
      set.seed(n_error)
      
        if (p == 0){
          phi = 0
        }else {
          phi = rnorm(n=p, mean=0, sd=0.5)  # runif(n=p, min=0, max=0.99)
        }
        if (q == 0){
          theta = 0
        }else {
          theta = rnorm(n=q, mean=0, sd=0.5) # runif(n=q, min=0, max=0.99)
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
  }
}
```

    ## [1] "s=1 d=0 p=0 q=4 n=16 mean=97 vara=0.0464481940114242 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=1 d=0 p=6 q=4 n=16 mean=991 vara=4.29435997401621 rep=2 sn=41"
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: TRUE
    ## [1] "s=1 d=0 p=9 q=10 n=39 mean=176 vara=0.480405825580741 rep=3 sn=42"
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: TRUE
    ## [1] "s=1 d=0 p=4 q=9 n=25 mean=929 vara=5.79538207977369 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=1 d=0 p=5 q=1 n=32 mean=-324 vara=0.342187545867958 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=1 d=1 p=7 q=0 n=112 mean=-587 vara=3.38006719605151 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=1 d=1 p=8 q=12 n=193 mean=963 vara=1.7529518114003 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=1 d=1 p=2 q=10 n=28 mean=-469 vara=0.251899643226532 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=1 d=1 p=5 q=3 n=67 mean=22 vara=0.0866852286317148 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=1 d=1 p=11 q=5 n=64 mean=849 vara=4.54781281720525 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=2 d=0 p=2 q=7 n=100 mean=559 vara=9.22838243157455 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=2 d=0 p=8 q=9 n=55 mean=-931 vara=11.2279145805175 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=2 d=0 p=2 q=6 n=74 mean=271 vara=0.91022095336331 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=2 d=0 p=10 q=3 n=71 mean=-721 vara=8.92096362021966 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=2 d=0 p=5 q=12 n=65 mean=-657 vara=0.125482083274823 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=2 d=1 p=0 q=6 n=18 mean=53 vara=0.0534251569970866 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=2 d=1 p=9 q=6 n=56 mean=-452 vara=3.39289912802349 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=2 d=1 p=4 q=8 n=71 mean=422 vara=6.03840093873758 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=2 d=1 p=1 q=6 n=34 mean=234 vara=2.54935294796636 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=2 d=1 p=9 q=11 n=66 mean=-888 vara=6.02067334887689 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=3 d=0 p=4 q=4 n=18 mean=370 vara=4.47434568185726 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=3 d=0 p=2 q=11 n=47 mean=-373 vara=6.60598559271579 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=3 d=0 p=9 q=11 n=82 mean=-496 vara=0.378822707315833 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=3 d=0 p=7 q=3 n=91 mean=19 vara=0.0105065032993691 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=3 d=0 p=1 q=4 n=109 mean=827 vara=2.8350666619333 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=3 d=1 p=7 q=11 n=113 mean=550 vara=1.00016051324773 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=3 d=1 p=7 q=9 n=143 mean=113 vara=0.403052469820752 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=3 d=1 p=3 q=0 n=57 mean=-589 vara=3.28042792466634 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=3 d=1 p=4 q=3 n=109 mean=-563 vara=7.52301236704846 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=3 d=1 p=5 q=12 n=73 mean=-657 vara=0.125482083274823 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=4 d=0 p=12 q=5 n=99 mean=-1000 vara=5.82888274119799 rep=1 sn=40"
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: TRUE
    ## [1] "s=4 d=0 p=10 q=11 n=153 mean=129 vara=1.27816833484625 rep=2 sn=41"
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: TRUE
    ## [1] "s=4 d=0 p=6 q=4 n=37 mean=-118 vara=0.476370888285962 rep=3 sn=42"
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: TRUE
    ## [1] "s=4 d=0 p=6 q=0 n=60 mean=-374 vara=1.71540356799152 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=4 d=0 p=10 q=10 n=81 mean=631 vara=12.8119087272814 rep=5 sn=44"
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: TRUE
    ## [1] "s=4 d=1 p=4 q=7 n=44 mean=936 vara=20.2941904115698 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=4 d=1 p=11 q=7 n=87 mean=644 vara=12.3061040769866 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=4 d=1 p=4 q=10 n=41 mean=611 vara=2.40543296536598 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=4 d=1 p=9 q=8 n=124 mean=-886 vara=1.34189921076108 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=4 d=1 p=9 q=2 n=128 mean=-892 vara=1.76973326407265 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=5 d=0 p=1 q=5 n=33 mean=-406 vara=3.58148971446802 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=5 d=0 p=1 q=8 n=126 mean=-445 vara=1.65451068548234 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=5 d=0 p=3 q=2 n=17 mean=713 vara=6.7919206617896 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=5 d=0 p=2 q=1 n=42 mean=787 vara=5.48430847746505 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=5 d=0 p=0 q=3 n=73 mean=-972 vara=12.0041643654575 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=5 d=1 p=7 q=9 n=68 mean=300 vara=1.14529409700997 rep=1 sn=40"
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: TRUE
    ## [1] "s=5 d=1 p=12 q=9 n=134 mean=514 vara=4.05703449233594 rep=2 sn=41"
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: FALSE --> ERROR: 'ar' part of model is not stationary
    ##  PASS: TRUE
    ## [1] "s=5 d=1 p=2 q=3 n=53 mean=-103 vara=0.754393607226308 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=5 d=1 p=12 q=6 n=66 mean=-395 vara=6.86595476374771 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=5 d=1 p=4 q=7 n=165 mean=-671 vara=4.48253279170906 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=6 d=0 p=1 q=3 n=45 mean=389 vara=4.22978466471374 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=6 d=0 p=9 q=9 n=100 mean=-960 vara=6.68933890628688 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=6 d=0 p=2 q=8 n=189 mean=472 vara=7.72498878345693 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=6 d=0 p=6 q=2 n=173 mean=884 vara=6.1853259456145 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=6 d=0 p=0 q=4 n=28 mean=645 vara=6.17140051769531 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=6 d=1 p=6 q=10 n=75 mean=806 vara=5.84361860437801 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=6 d=1 p=3 q=2 n=49 mean=394 vara=0.638390178101581 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=6 d=1 p=0 q=7 n=115 mean=-830 vara=11.500996072223 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=6 d=1 p=6 q=2 n=94 mean=43 vara=0.625472382848009 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=6 d=1 p=1 q=5 n=108 mean=-548 vara=6.91494746814052 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=7 d=0 p=7 q=3 n=93 mean=-490 vara=5.3957238693776 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=7 d=0 p=8 q=12 n=265 mean=-321 vara=4.20633480819009 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=7 d=0 p=3 q=11 n=43 mean=2 vara=0.00899843406825381 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=7 d=0 p=9 q=8 n=122 mean=-886 vara=1.34189921076108 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=7 d=0 p=6 q=7 n=105 mean=470 vara=5.73663848602248 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=7 d=1 p=4 q=1 n=127 mean=836 vara=2.27880126056289 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=7 d=1 p=1 q=11 n=46 mean=-758 vara=9.49621645430946 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=7 d=1 p=9 q=7 n=55 mean=-904 vara=8.99732179994604 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=7 d=1 p=9 q=0 n=293 mean=619 vara=6.75663012249327 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=7 d=1 p=2 q=9 n=248 mean=690 vara=5.74992717262358 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=8 d=0 p=4 q=8 n=44 mean=-731 vara=6.45897474372654 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=8 d=0 p=9 q=3 n=75 mean=517 vara=6.53578430801978 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=8 d=0 p=10 q=4 n=43 mean=-852 vara=22.4852465961328 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=8 d=0 p=9 q=10 n=197 mean=237 vara=2.06646668189339 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=8 d=0 p=5 q=11 n=113 mean=551 vara=3.27608157521975 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=8 d=1 p=6 q=6 n=175 mean=-518 vara=7.03047818869948 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=8 d=1 p=0 q=7 n=324 mean=798 vara=3.04518705486182 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=8 d=1 p=10 q=12 n=235 mean=939 vara=18.8240402255881 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=8 d=1 p=9 q=7 n=216 mean=-987 vara=3.57866660268251 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=8 d=1 p=12 q=12 n=250 mean=-13 vara=0.107102182711513 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=9 d=0 p=8 q=2 n=95 mean=-518 vara=3.2695343658442 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=9 d=0 p=1 q=4 n=149 mean=-911 vara=12.2790605321422 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=9 d=0 p=2 q=1 n=37 mean=148 vara=0.574404428622631 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=9 d=0 p=5 q=3 n=81 mean=22 vara=0.0866852286317148 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=9 d=0 p=7 q=8 n=201 mean=-77 vara=0.0697973576922525 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=9 d=1 p=11 q=8 n=172 mean=-964 vara=9.71182634146638 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=9 d=1 p=3 q=6 n=103 mean=-638 vara=3.1332114493578 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=9 d=1 p=12 q=0 n=88 mean=-30 vara=0.342876736552172 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=9 d=1 p=1 q=0 n=204 mean=-455 vara=3.97562632692492 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=9 d=1 p=7 q=3 n=387 mean=-914 vara=3.57377279457661 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=10 d=0 p=10 q=9 n=311 mean=134 vara=2.21141856476877 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=10 d=0 p=10 q=7 n=198 mean=-997 vara=10.0342377999617 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=10 d=0 p=9 q=3 n=43 mean=-165 vara=1.88325204824081 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=10 d=0 p=8 q=3 n=91 mean=-948 vara=13.351342457903 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=10 d=0 p=11 q=5 n=48 mean=740 vara=12.4390351043141 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=10 d=1 p=7 q=6 n=415 mean=-754 vara=1.53844067370896 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=10 d=1 p=4 q=7 n=176 mean=245 vara=5.08094982261761 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=10 d=1 p=1 q=7 n=380 mean=9 vara=0.0737109207302164 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=10 d=1 p=7 q=7 n=213 mean=749 vara=6.4441746411757 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=10 d=1 p=5 q=4 n=162 mean=864 vara=6.12224273940537 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=11 d=0 p=0 q=3 n=48 mean=28 vara=0.163339550485484 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=11 d=0 p=4 q=4 n=82 mean=485 vara=4.45313079503779 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=11 d=0 p=8 q=10 n=89 mean=733 vara=8.79616560751891 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=11 d=0 p=2 q=2 n=251 mean=172 vara=0.786104600303219 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=11 d=0 p=8 q=4 n=185 mean=-362 vara=4.24531488136361 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=11 d=1 p=11 q=3 n=112 mean=358 vara=0.507283023289174 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=11 d=1 p=11 q=11 n=185 mean=866 vara=4.78687965347651 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=11 d=1 p=3 q=1 n=96 mean=909 vara=18.9687903702223 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=11 d=1 p=2 q=11 n=83 mean=-381 vara=1.56441282283912 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=11 d=1 p=8 q=7 n=158 mean=-224 vara=0.762586934433642 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=12 d=0 p=4 q=8 n=52 mean=-731 vara=6.45897474372654 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=12 d=0 p=5 q=9 n=119 mean=236 vara=0.297378267810917 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=12 d=0 p=4 q=8 n=135 mean=391 vara=1.4898276369485 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=12 d=0 p=4 q=5 n=171 mean=191 vara=3.17562215817449 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=12 d=0 p=0 q=4 n=39 mean=-957 vara=5.57035919211062 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=12 d=1 p=2 q=4 n=223 mean=208 vara=0.778267955579623 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=12 d=1 p=2 q=2 n=72 mean=372 vara=0.21884932834322 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=12 d=1 p=4 q=3 n=120 mean=770 vara=6.16648096900191 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=12 d=1 p=0 q=8 n=69 mean=45 vara=0.392739935252129 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=12 d=1 p=7 q=4 n=61 mean=-454 vara=2.44338269928777 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=13 d=0 p=8 q=1 n=53 mean=991 vara=19.9757470437421 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=13 d=0 p=7 q=9 n=157 mean=113 vara=0.403052469820752 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=13 d=0 p=1 q=9 n=150 mean=664 vara=0.361293634601 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=13 d=0 p=3 q=10 n=208 mean=110 vara=1.94105444244365 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=13 d=0 p=6 q=6 n=67 mean=694 vara=1.90557380098933 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=13 d=1 p=5 q=3 n=124 mean=216 vara=1.1565247956 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=13 d=1 p=1 q=4 n=97 mean=-36 vara=0.265059553288455 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=13 d=1 p=5 q=11 n=408 mean=-628 vara=1.31535413488281 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=13 d=1 p=7 q=4 n=148 mean=-990 vara=7.60195239568048 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=13 d=1 p=11 q=3 n=290 mean=-118 vara=0.135660128300991 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=14 d=0 p=11 q=2 n=379 mean=609 vara=5.50229608101508 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=14 d=0 p=8 q=7 n=81 mean=909 vara=5.81880578168077 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=14 d=0 p=1 q=1 n=140 mean=-126 vara=0.779802188340468 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=14 d=0 p=2 q=10 n=207 mean=-364 vara=2.26906815937991 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=14 d=0 p=12 q=7 n=256 mean=447 vara=3.15147635118442 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=14 d=1 p=11 q=3 n=278 mean=-826 vara=9.63792567882689 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=14 d=1 p=10 q=7 n=284 mean=-526 vara=8.77054621022467 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=14 d=1 p=1 q=1 n=429 mean=-776 vara=0.229914978587195 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=14 d=1 p=8 q=6 n=145 mean=-84 vara=1.25396133847262 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=14 d=1 p=7 q=9 n=80 mean=872 vara=6.38685616499941 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=15 d=0 p=1 q=6 n=43 mean=-411 vara=1.35756843925537 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=15 d=0 p=10 q=3 n=156 mean=152 vara=1.11125578445126 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=15 d=0 p=1 q=6 n=64 mean=-779 vara=1.70278987168048 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=15 d=0 p=6 q=11 n=99 mean=545 vara=5.91747295255546 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=15 d=0 p=10 q=7 n=101 mean=866 vara=19.5590436091527 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=15 d=1 p=3 q=0 n=506 mean=876 vara=3.30051876933512 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=15 d=1 p=4 q=0 n=114 mean=935 vara=12.2045874685559 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=15 d=1 p=8 q=0 n=123 mean=-956 vara=11.3684759477416 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=15 d=1 p=1 q=5 n=223 mean=580 vara=3.05722712717186 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=15 d=1 p=3 q=3 n=196 mean=-970 vara=0.410819305961819 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=16 d=0 p=6 q=5 n=102 mean=781 vara=0.185028893780139 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=16 d=0 p=9 q=11 n=81 mean=-617 vara=3.52008228976828 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=16 d=0 p=0 q=0 n=179 mean=-721 vara=11.3778275748845 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=16 d=0 p=10 q=12 n=102 mean=927 vara=5.32699974728175 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=16 d=0 p=7 q=0 n=71 mean=-645 vara=8.45181672968597 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=16 d=1 p=3 q=4 n=525 mean=835 vara=5.25619209932189 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=16 d=1 p=0 q=0 n=281 mean=109 vara=0.612467062595452 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=16 d=1 p=2 q=4 n=95 mean=-591 vara=1.04555721370538 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=16 d=1 p=2 q=11 n=231 mean=-381 vara=1.56441282283912 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=16 d=1 p=6 q=6 n=146 mean=598 vara=0.836605617105581 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=17 d=0 p=3 q=3 n=66 mean=253 vara=0.562379809999487 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=17 d=0 p=0 q=4 n=178 mean=758 vara=11.8951639366534 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=17 d=0 p=1 q=1 n=146 mean=-126 vara=0.779802188340468 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=17 d=0 p=2 q=1 n=78 mean=-234 vara=1.10072055570503 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=17 d=0 p=11 q=8 n=164 mean=406 vara=2.1860441899181 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=17 d=1 p=4 q=2 n=159 mean=127 vara=1.02664574510215 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=17 d=1 p=6 q=5 n=418 mean=-303 vara=4.57221039588017 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=17 d=1 p=12 q=3 n=374 mean=576 vara=0.866947053154015 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=17 d=1 p=8 q=10 n=321 mean=-775 vara=22.4560156993075 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=17 d=1 p=3 q=9 n=283 mean=-577 vara=13.3031212642702 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=18 d=0 p=12 q=12 n=216 mean=-819 vara=5.66101409084951 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=18 d=0 p=5 q=11 n=258 mean=204 vara=1.20882011729668 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=18 d=0 p=0 q=3 n=185 mean=476 vara=2.15003084909704 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=18 d=0 p=11 q=2 n=106 mean=247 vara=0.0827141863686559 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=18 d=0 p=6 q=12 n=242 mean=-916 vara=12.8803614183437 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=18 d=1 p=12 q=12 n=184 mean=-533 vara=9.45498112861979 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=18 d=1 p=0 q=10 n=135 mean=-722 vara=10.5970143098865 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=18 d=1 p=0 q=0 n=287 mean=-278 vara=0.19330684599776 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=18 d=1 p=1 q=0 n=562 mean=565 vara=7.92429653232155 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=18 d=1 p=10 q=5 n=711 mean=656 vara=5.16822306568728 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=19 d=0 p=2 q=4 n=392 mean=232 vara=0.0153978514199586 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=19 d=0 p=2 q=11 n=297 mean=573 vara=5.94064719151994 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=19 d=0 p=4 q=9 n=254 mean=-262 vara=1.87673556137412 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=19 d=0 p=1 q=1 n=88 mean=984 vara=2.88582744105291 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=19 d=0 p=8 q=2 n=58 mean=-461 vara=3.65857265565527 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=19 d=1 p=2 q=9 n=183 mean=-56 vara=0.229009034011583 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=19 d=1 p=9 q=8 n=466 mean=364 vara=0.199579711680803 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=19 d=1 p=4 q=1 n=459 mean=-518 vara=2.18407179238932 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=19 d=1 p=1 q=8 n=379 mean=-280 vara=4.37939458121714 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=19 d=1 p=6 q=12 n=252 mean=44 vara=0.168931585940321 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=20 d=0 p=12 q=3 n=222 mean=-168 vara=0.00200142019668317 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=20 d=0 p=11 q=5 n=238 mean=752 vara=4.98446570475468 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=20 d=0 p=4 q=12 n=98 mean=16 vara=0.286633078659912 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=20 d=0 p=8 q=7 n=343 mean=-804 vara=8.3123447326366 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=20 d=0 p=5 q=7 n=285 mean=848 vara=0.720881982925567 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=20 d=1 p=2 q=4 n=206 mean=736 vara=7.62313676164849 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=20 d=1 p=10 q=11 n=257 mean=703 vara=5.90078825917294 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=20 d=1 p=12 q=3 n=386 mean=576 vara=0.866947053154015 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=20 d=1 p=1 q=5 n=303 mean=-285 vara=2.63100557846474 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=20 d=1 p=1 q=5 n=496 mean=-808 vara=6.72651328782995 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=21 d=0 p=4 q=12 n=157 mean=43 vara=0.324808189962168 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=21 d=0 p=5 q=11 n=252 mean=-339 vara=0.0827143215651251 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=21 d=0 p=6 q=10 n=95 mean=311 vara=1.75964902659639 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=21 d=0 p=8 q=3 n=81 mean=197 vara=1.84475100124601 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=21 d=0 p=12 q=10 n=76 mean=740 vara=12.4390351043141 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=21 d=1 p=4 q=9 n=523 mean=-664 vara=0.468727712819409 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=21 d=1 p=5 q=9 n=698 mean=865 vara=6.983052772079 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=21 d=1 p=0 q=12 n=425 mean=-113 vara=0.497353945723875 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=21 d=1 p=11 q=7 n=396 mean=-687 vara=6.6450875832454 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=21 d=1 p=1 q=12 n=254 mean=-421 vara=1.72063571990526 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=22 d=0 p=8 q=4 n=92 mean=-692 vara=2.01375231708061 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=22 d=0 p=3 q=11 n=74 mean=587 vara=3.45962078528949 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=22 d=0 p=2 q=6 n=301 mean=620 vara=4.61977830762215 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=22 d=0 p=1 q=6 n=93 mean=-574 vara=3.96788738368767 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=22 d=0 p=5 q=8 n=187 mean=230 vara=0.555537951075094 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=22 d=1 p=6 q=5 n=761 mean=-44 vara=0.218189755585008 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=22 d=1 p=11 q=10 n=535 mean=-230 vara=2.8839401736596 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=22 d=1 p=10 q=6 n=149 mean=-674 vara=15.2830668268709 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=22 d=1 p=7 q=5 n=598 mean=-734 vara=1.61348098469983 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=22 d=1 p=6 q=6 n=581 mean=90 vara=1.04861326075801 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=23 d=0 p=10 q=4 n=251 mean=-930 vara=7.23489190996051 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=23 d=0 p=7 q=9 n=393 mean=751 vara=0.811412261115859 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=23 d=0 p=7 q=7 n=438 mean=672 vara=6.31244786582089 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=23 d=0 p=6 q=7 n=120 mean=143 vara=0.0850461655835365 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=23 d=0 p=11 q=10 n=70 mean=519 vara=14.1454281479356 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=23 d=1 p=8 q=8 n=488 mean=623 vara=4.88854802053137 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=23 d=1 p=12 q=10 n=785 mean=-237 vara=0.102139918896037 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=23 d=1 p=8 q=2 n=346 mean=-870 vara=6.41356400086288 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=23 d=1 p=3 q=2 n=146 mean=-111 vara=1.03374862880986 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=23 d=1 p=7 q=8 n=868 mean=-706 vara=0.71987287093178 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=24 d=0 p=2 q=3 n=89 mean=-958 vara=10.3884970534681 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=24 d=0 p=2 q=12 n=297 mean=-419 vara=1.45965797547006 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=24 d=0 p=2 q=10 n=117 mean=-79 vara=1.31746665817017 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=24 d=0 p=0 q=10 n=191 mean=602 vara=2.99102119583067 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=24 d=0 p=11 q=0 n=194 mean=-152 vara=2.50352034753881 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=24 d=1 p=12 q=1 n=130 mean=637 vara=0.0451774837015476 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=24 d=1 p=10 q=4 n=553 mean=503 vara=0.185542743641318 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=24 d=1 p=11 q=7 n=319 mean=448 vara=0.118289623835929 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=24 d=1 p=11 q=1 n=152 mean=416 vara=3.71988243510774 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=24 d=1 p=4 q=5 n=346 mean=654 vara=1.60445954005514 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=25 d=0 p=2 q=1 n=120 mean=468 vara=1.69140875668398 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=25 d=0 p=0 q=1 n=177 mean=185 vara=0.936493163435923 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=25 d=0 p=7 q=1 n=140 mean=-44 vara=0.100298397641572 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=25 d=0 p=12 q=12 n=483 mean=884 vara=6.1853259456145 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=25 d=0 p=7 q=0 n=194 mean=-670 vara=0.401820831452465 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=25 d=1 p=12 q=2 n=152 mean=657 vara=0.159151789040431 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=25 d=1 p=5 q=5 n=227 mean=388 vara=5.73524632634073 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=25 d=1 p=3 q=5 n=477 mean=770 vara=11.5633884061646 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=25 d=1 p=6 q=11 n=427 mean=125 vara=0.85014996461964 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=25 d=1 p=8 q=12 n=177 mean=106 vara=0.192798614581618 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=26 d=0 p=6 q=7 n=130 mean=-552 vara=9.04182574157754 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=26 d=0 p=9 q=3 n=352 mean=-135 vara=0.386114567689842 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=26 d=0 p=2 q=2 n=322 mean=573 vara=3.53537832564223 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=26 d=0 p=9 q=0 n=157 mean=-129 vara=1.4811750405912 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=26 d=0 p=7 q=5 n=152 mean=-168 vara=2.04854745412569 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=26 d=1 p=6 q=5 n=174 mean=781 vara=0.185028893780139 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=26 d=1 p=2 q=2 n=128 mean=372 vara=0.21884932834322 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=26 d=1 p=5 q=2 n=121 mean=945 vara=10.4246069437149 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=26 d=1 p=4 q=5 n=507 mean=191 vara=3.17562215817449 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=26 d=1 p=6 q=11 n=334 mean=-124 vara=1.45068273371395 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=27 d=0 p=8 q=8 n=283 mean=-337 vara=4.56062230112003 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=27 d=0 p=10 q=7 n=154 mean=786 vara=4.59040175846828 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=27 d=0 p=10 q=2 n=136 mean=928 vara=3.6800567449565 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=27 d=0 p=3 q=7 n=538 mean=-943 vara=1.23647000662369 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=27 d=0 p=7 q=8 n=139 mean=861 vara=2.39839381811325 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=27 d=1 p=6 q=1 n=516 mean=573 vara=0.00637525275737878 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=27 d=1 p=3 q=12 n=185 mean=-531 vara=5.1852632184214 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=27 d=1 p=4 q=11 n=533 mean=484 vara=3.69492240468857 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=27 d=1 p=6 q=0 n=322 mean=607 vara=7.46775808972201 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=27 d=1 p=0 q=8 n=247 mean=-552 vara=1.40844676028544 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=28 d=0 p=6 q=6 n=174 mean=-285 vara=1.73564469687788 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=28 d=0 p=9 q=5 n=139 mean=-233 vara=0.655815561609541 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=28 d=0 p=4 q=0 n=95 mean=444 vara=1.7875414934219 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=28 d=0 p=5 q=2 n=134 mean=771 vara=5.42481597453459 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=28 d=0 p=5 q=11 n=75 mean=519 vara=14.1454281479356 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=28 d=1 p=1 q=5 n=138 mean=659 vara=3.37950715151746 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=28 d=1 p=0 q=0 n=483 mean=195 vara=2.33145351994078 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=28 d=1 p=11 q=12 n=316 mean=389 vara=2.78458588569949 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=28 d=1 p=7 q=9 n=332 mean=12 vara=0.185684667082221 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=28 d=1 p=4 q=4 n=551 mean=-898 vara=16.6194976221822 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=29 d=0 p=12 q=1 n=259 mean=-925 vara=9.45726526909612 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=29 d=0 p=6 q=6 n=659 mean=-291 vara=7.23519970753101 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=29 d=0 p=7 q=4 n=116 mean=-68 vara=0.593874293226719 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=29 d=0 p=9 q=9 n=85 mean=-901 vara=6.84978193063511 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=29 d=0 p=5 q=0 n=98 mean=-42 vara=0.198654360888946 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=29 d=1 p=7 q=8 n=168 mean=704 vara=9.41625247996269 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=29 d=1 p=5 q=10 n=200 mean=-233 vara=0.655815561609541 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=29 d=1 p=4 q=6 n=476 mean=-115 vara=0.589574237237984 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=29 d=1 p=3 q=12 n=394 mean=744 vara=5.78362689803411 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=29 d=1 p=7 q=6 n=335 mean=53 vara=0.26732213961603 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=30 d=0 p=3 q=7 n=231 mean=-778 vara=4.50802874349445 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=30 d=0 p=8 q=6 n=325 mean=484 vara=2.41762569919755 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=30 d=0 p=2 q=2 n=297 mean=-294 vara=0.386756123267812 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=30 d=0 p=1 q=6 n=178 mean=-462 vara=4.00700847105952 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=30 d=0 p=3 q=7 n=158 mean=776 vara=10.4006352542069 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=30 d=1 p=3 q=3 n=226 mean=812 vara=6.1660489630915 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=30 d=1 p=2 q=8 n=390 mean=-924 vara=3.59178257258573 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=30 d=1 p=10 q=1 n=286 mean=780 vara=2.74932177264299 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=30 d=1 p=6 q=4 n=282 mean=847 vara=5.5445914036284 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=30 d=1 p=10 q=0 n=219 mean=791 vara=6.16259462589898 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=31 d=0 p=7 q=8 n=662 mean=-521 vara=4.5426938613783 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=31 d=0 p=10 q=4 n=449 mean=-488 vara=5.55929759613038 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=31 d=0 p=0 q=9 n=91 mean=-51 vara=1.40310959626389 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=31 d=0 p=3 q=9 n=92 mean=-919 vara=2.47533932145772 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=31 d=0 p=2 q=11 n=122 mean=-12 vara=0.17240767845601 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=31 d=1 p=7 q=10 n=261 mean=1000 vara=6.69983471751227 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=31 d=1 p=1 q=3 n=599 mean=912 vara=10.430478667024 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=31 d=1 p=7 q=12 n=154 mean=-742 vara=0.691224437310188 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=31 d=1 p=5 q=5 n=230 mean=956 vara=16.0845170166121 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=31 d=1 p=12 q=4 n=291 mean=438 vara=5.71422041156463 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=32 d=0 p=3 q=1 n=117 mean=805 vara=4.32255439887529 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=32 d=0 p=9 q=11 n=113 mean=-617 vara=3.52008228976828 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=32 d=0 p=9 q=0 n=109 mean=-558 vara=3.35675346350638 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=32 d=0 p=4 q=6 n=597 mean=-622 vara=0.687747762239737 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=32 d=0 p=2 q=12 n=459 mean=-783 vara=0.740617256884602 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=32 d=1 p=2 q=6 n=436 mean=-44 vara=0.450053227540267 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=32 d=1 p=0 q=8 n=148 mean=-291 vara=0.129055386589378 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=32 d=1 p=3 q=10 n=1266 mean=-862 vara=6.30212551063122 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=32 d=1 p=12 q=3 n=420 mean=324 vara=2.24267555740492 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=32 d=1 p=8 q=4 n=318 mean=-257 vara=0.0700807541039021 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=33 d=0 p=0 q=1 n=315 mean=-940 vara=6.43293962181076 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=33 d=0 p=1 q=3 n=331 mean=-996 vara=14.1514321092809 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=33 d=0 p=9 q=8 n=188 mean=-569 vara=0.451685393712319 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=33 d=0 p=12 q=9 n=813 mean=255 vara=2.38071277742948 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=33 d=0 p=7 q=0 n=93 mean=-550 vara=4.44499973402495 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=33 d=1 p=7 q=10 n=269 mean=1000 vara=6.69983471751227 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=33 d=1 p=2 q=10 n=397 mean=-420 vara=6.56508530296658 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=33 d=1 p=9 q=11 n=209 mean=817 vara=8.56100341595227 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=33 d=1 p=5 q=9 n=242 mean=-129 vara=1.4811750405912 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=33 d=1 p=2 q=3 n=683 mean=144 vara=1.55117944428091 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=34 d=0 p=7 q=0 n=209 mean=225 vara=3.17882746593148 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=34 d=0 p=10 q=11 n=100 mean=636 vara=14.8675273341499 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=34 d=0 p=7 q=3 n=88 mean=636 vara=4.27279252098703 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=34 d=0 p=2 q=1 n=112 mean=-234 vara=1.10072055570503 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=34 d=0 p=12 q=11 n=367 mean=150 vara=2.31147510151783 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=34 d=1 p=7 q=6 n=955 mean=-927 vara=1.64325974871003 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=34 d=1 p=2 q=4 n=308 mean=-674 vara=4.75247560090547 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=34 d=1 p=9 q=8 n=239 mean=-462 vara=0.668370619984322 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=34 d=1 p=6 q=1 n=215 mean=677 vara=7.78298082524275 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=34 d=1 p=3 q=6 n=416 mean=-226 vara=3.42570031392081 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=35 d=0 p=1 q=0 n=463 mean=461 vara=7.43517091938426 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=35 d=0 p=4 q=9 n=151 mean=406 vara=1.68371944437814 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=35 d=0 p=6 q=6 n=93 mean=122 vara=0.67279010495407 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=35 d=0 p=4 q=5 n=248 mean=110 vara=1.94105444244365 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=35 d=0 p=9 q=10 n=94 mean=835 vara=8.48806390978838 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=35 d=1 p=9 q=11 n=822 mean=-44 vara=0.218189755585008 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=35 d=1 p=0 q=4 n=205 mean=524 vara=1.91097000159916 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=35 d=1 p=11 q=11 n=1074 mean=447 vara=1.43902810790322 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=35 d=1 p=10 q=3 n=419 mean=594 vara=1.75504137866291 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=35 d=1 p=5 q=1 n=963 mean=173 vara=3.09219222373125 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=36 d=0 p=6 q=8 n=336 mean=-830 vara=6.06076667983173 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=36 d=0 p=5 q=5 n=287 mean=-660 vara=2.86322880012168 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=36 d=0 p=8 q=10 n=176 mean=-462 vara=0.668370619984322 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=36 d=0 p=0 q=11 n=379 mean=-480 vara=5.23595549688906 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=36 d=0 p=1 q=2 n=367 mean=-813 vara=8.36617774220815 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=36 d=1 p=3 q=9 n=406 mean=-857 vara=17.463132909042 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=36 d=1 p=11 q=5 n=310 mean=-664 vara=0.499902054504818 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=36 d=1 p=8 q=7 n=789 mean=-498 vara=3.39217148429049 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=36 d=1 p=6 q=5 n=745 mean=-356 vara=3.56017772025845 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=36 d=1 p=10 q=10 n=170 mean=-761 vara=16.7452796381889 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=37 d=0 p=7 q=4 n=352 mean=995 vara=4.05199473771813 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=37 d=0 p=9 q=8 n=134 mean=-649 vara=0.76591307469913 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=37 d=0 p=3 q=3 n=472 mean=-633 vara=6.28229670301723 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=37 d=0 p=0 q=2 n=184 mean=505 vara=2.90661720111105 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=37 d=0 p=6 q=0 n=86 mean=-377 vara=3.54160972078657 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=37 d=1 p=10 q=2 n=1177 mean=-411 vara=4.07522878493838 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=37 d=1 p=6 q=1 n=1132 mean=-515 vara=5.87837735996185 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=37 d=1 p=7 q=0 n=859 mean=-189 vara=1.37479404403875 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=37 d=1 p=11 q=7 n=421 mean=707 vara=2.86462832259474 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=37 d=1 p=7 q=12 n=666 mean=-790 vara=5.31150091473191 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=38 d=0 p=9 q=5 n=572 mean=251 vara=0.267797118446894 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=38 d=0 p=9 q=11 n=267 mean=-205 vara=0.342787546236578 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=38 d=0 p=0 q=11 n=94 mean=-794 vara=1.29048944995797 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=38 d=0 p=6 q=0 n=297 mean=10 vara=0.101687282979954 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=38 d=0 p=12 q=0 n=256 mean=-961 vara=2.1910001642999 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=38 d=1 p=12 q=2 n=384 mean=628 vara=1.74762814294718 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=38 d=1 p=5 q=3 n=288 mean=-327 vara=1.4739422261955 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=38 d=1 p=3 q=4 n=239 mean=815 vara=1.51800531585301 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=38 d=1 p=0 q=7 n=920 mean=9 vara=0.120142132683793 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=38 d=1 p=11 q=1 n=426 mean=-165 vara=0.181572578187725 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=39 d=0 p=3 q=11 n=469 mean=246 vara=1.56549235139489 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=39 d=0 p=1 q=9 n=170 mean=922 vara=10.9988165815301 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=39 d=0 p=4 q=1 n=453 mean=664 vara=0.361293634601 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=39 d=0 p=6 q=7 n=152 mean=143 vara=0.0850461655835365 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=39 d=0 p=9 q=6 n=489 mean=1 vara=0.00310104697780112 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=39 d=1 p=11 q=0 n=806 mean=609 vara=6.45022946182311 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=39 d=1 p=0 q=3 n=405 mean=729 vara=6.06766630117543 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=39 d=1 p=1 q=9 n=433 mean=883 vara=7.24974534768687 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=39 d=1 p=10 q=7 n=497 mean=-574 vara=4.55017113891208 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=39 d=1 p=5 q=8 n=420 mean=-645 vara=14.3710297546238 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=40 d=0 p=3 q=10 n=390 mean=699 vara=5.32176260519984 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=40 d=0 p=6 q=1 n=327 mean=898 vara=11.4033568439232 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=40 d=0 p=3 q=2 n=208 mean=-390 vara=1.07225329952657 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=40 d=0 p=4 q=7 n=302 mean=126 vara=0.439842725414607 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=40 d=0 p=8 q=6 n=297 mean=-911 vara=7.64069994907026 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=40 d=1 p=10 q=1 n=196 mean=391 vara=4.03930819489019 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=40 d=1 p=5 q=3 n=171 mean=-495 vara=10.5328338212013 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=40 d=1 p=12 q=3 n=396 mean=-349 vara=4.25747756393845 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=40 d=1 p=8 q=4 n=1387 mean=381 vara=8.33817830421017 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=40 d=1 p=2 q=10 n=1055 mean=-661 vara=6.9755512860174 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=41 d=0 p=2 q=7 n=488 mean=-865 vara=5.13855224114357 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=41 d=0 p=5 q=3 n=99 mean=-298 vara=0.153651737399341 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=41 d=0 p=1 q=12 n=612 mean=588 vara=9.38967215341908 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=41 d=0 p=8 q=9 n=466 mean=138 vara=0.437986973297217 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=41 d=0 p=11 q=8 n=416 mean=907 vara=11.989056012638 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=41 d=1 p=3 q=11 n=670 mean=-113 vara=0.396681422149288 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=41 d=1 p=5 q=2 n=199 mean=-567 vara=4.39669522347068 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=41 d=1 p=5 q=7 n=328 mean=607 vara=6.71142230169133 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=41 d=1 p=7 q=7 n=337 mean=749 vara=6.4441746411757 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=41 d=1 p=6 q=6 n=748 mean=556 vara=9.29757275751565 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=42 d=0 p=10 q=11 n=132 mean=-403 vara=0.379032195108799 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=42 d=0 p=3 q=2 n=121 mean=-547 vara=0.792064736545376 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=42 d=0 p=2 q=3 n=166 mean=-433 vara=2.21284470135069 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=42 d=0 p=1 q=6 n=305 mean=332 vara=1.68407697572191 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=42 d=0 p=0 q=0 n=429 mean=791 vara=6.16259462589898 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=42 d=1 p=3 q=7 n=404 mean=462 vara=1.73769519312962 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=42 d=1 p=9 q=4 n=264 mean=-429 vara=3.24914290192929 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=42 d=1 p=2 q=0 n=1229 mean=-15 vara=0.0581990421280608 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=42 d=1 p=0 q=5 n=914 mean=547 vara=3.83266355967989 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=42 d=1 p=6 q=2 n=1121 mean=687 vara=5.37740118206449 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=43 d=0 p=4 q=5 n=734 mean=-106 vara=0.977253569738408 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=43 d=0 p=0 q=2 n=482 mean=994 vara=0.122981664921375 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=43 d=0 p=6 q=10 n=411 mean=-810 vara=2.05770594314805 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=43 d=0 p=1 q=2 n=107 mean=-657 vara=12.2583969854331 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=43 d=0 p=2 q=7 n=148 mean=-284 vara=0.0429246504125129 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=43 d=1 p=5 q=4 n=642 mean=47 vara=0.247010841291794 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=43 d=1 p=11 q=6 n=1638 mean=-836 vara=6.96421597971697 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=43 d=1 p=12 q=1 n=710 mean=184 vara=0.916983226032853 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=43 d=1 p=12 q=5 n=337 mean=80 vara=0.488201139917754 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=43 d=1 p=1 q=12 n=1180 mean=-849 vara=5.85812719808406 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=44 d=0 p=7 q=9 n=282 mean=-120 vara=0.461495860568516 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=44 d=0 p=5 q=12 n=539 mean=336 vara=3.46532156448452 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=44 d=0 p=6 q=9 n=230 mean=374 vara=2.3923251134126 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=44 d=0 p=11 q=0 n=216 mean=926 vara=13.8323886599679 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=44 d=0 p=8 q=5 n=346 mean=14 vara=0.0864150595136207 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=44 d=1 p=8 q=1 n=366 mean=-995 vara=0.206347581228512 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=44 d=1 p=4 q=12 n=384 mean=-418 vara=5.23042550021267 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=44 d=1 p=4 q=9 n=471 mean=530 vara=0.382089468578553 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=44 d=1 p=3 q=1 n=331 mean=453 vara=3.82690877120126 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=44 d=1 p=0 q=8 n=634 mean=-545 vara=15.0676714254231 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=45 d=0 p=0 q=12 n=145 mean=-339 vara=4.05835519507987 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=45 d=0 p=1 q=9 n=266 mean=725 vara=6.08836463751869 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=45 d=0 p=11 q=7 n=369 mean=752 vara=12.5312320329457 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=45 d=0 p=7 q=4 n=269 mean=453 vara=1.41687489305347 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=45 d=0 p=9 q=3 n=481 mean=-661 vara=4.00204127990937 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=45 d=1 p=3 q=2 n=292 mean=215 vara=0.408015685810525 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=45 d=1 p=5 q=11 n=975 mean=-318 vara=5.4439632339902 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=45 d=1 p=7 q=9 n=1112 mean=-419 vara=5.33397720231942 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=45 d=1 p=2 q=9 n=324 mean=82 vara=1.42549375405952 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=45 d=1 p=5 q=9 n=764 mean=-868 vara=0.27189986610707 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=46 d=0 p=0 q=6 n=597 mean=-151 vara=1.89648223213856 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=46 d=0 p=8 q=11 n=585 mean=705 vara=3.87917399265886 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=46 d=0 p=9 q=12 n=489 mean=-682 vara=2.24151225059525 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=46 d=0 p=9 q=1 n=426 mean=-689 vara=2.9096908029932 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=46 d=0 p=7 q=12 n=485 mean=-780 vara=8.94737978596539 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=46 d=1 p=0 q=6 n=222 mean=612 vara=0.817328617477296 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=46 d=1 p=9 q=2 n=1894 mean=857 vara=6.26692947745138 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=46 d=1 p=11 q=5 n=632 mean=463 vara=2.0837143068749 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=46 d=1 p=0 q=2 n=716 mean=-837 vara=11.5928991050478 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=46 d=1 p=4 q=2 n=781 mean=-753 vara=10.6758844008491 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=47 d=0 p=12 q=12 n=383 mean=-590 vara=2.57545090088663 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=47 d=0 p=2 q=8 n=127 mean=307 vara=0.107205799106872 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=47 d=0 p=10 q=4 n=377 mean=-852 vara=22.4852465961328 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=47 d=0 p=6 q=9 n=198 mean=80 vara=1.27940880378849 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=47 d=0 p=6 q=5 n=470 mean=770 vara=1.19090374455743 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=47 d=1 p=0 q=0 n=600 mean=-823 vara=1.54932183187522 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=47 d=1 p=4 q=9 n=915 mean=-411 vara=2.16413828826666 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=47 d=1 p=12 q=11 n=722 mean=-686 vara=5.94484564269847 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=47 d=1 p=2 q=3 n=949 mean=-503 vara=4.02726501997976 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=47 d=1 p=5 q=4 n=970 mean=890 vara=2.75286237639056 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=48 d=0 p=9 q=8 n=531 mean=-964 vara=10.1988844573301 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=48 d=0 p=2 q=7 n=206 mean=-314 vara=4.42731252407979 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=48 d=0 p=5 q=7 n=260 mean=607 vara=6.71142230169133 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=48 d=0 p=0 q=0 n=155 mean=780 vara=2.33528892998629 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=48 d=0 p=0 q=10 n=202 mean=-926 vara=2.57901690766705 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=48 d=1 p=11 q=7 n=425 mean=-54 vara=0.488985739872237 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=48 d=1 p=7 q=0 n=431 mean=-746 vara=10.6058995869106 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=48 d=1 p=12 q=5 n=1549 mean=-319 vara=0.311196496381526 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=48 d=1 p=12 q=0 n=402 mean=-446 vara=0.8762530815673 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=48 d=1 p=5 q=9 n=220 mean=864 vara=15.8106626571115 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=49 d=0 p=12 q=3 n=519 mean=-79 vara=0.127841453848647 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=49 d=0 p=2 q=12 n=652 mean=-375 vara=0.16162985880821 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=49 d=0 p=10 q=10 n=223 mean=6 vara=0.0124898436428034 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=49 d=0 p=4 q=6 n=378 mean=-712 vara=0.381425637811204 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=49 d=0 p=7 q=6 n=176 mean=628 vara=7.81189542686451 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=49 d=1 p=10 q=10 n=373 mean=372 vara=1.04202605555459 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=49 d=1 p=11 q=9 n=223 mean=21 vara=0.0680606734438507 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=49 d=1 p=8 q=2 n=454 mean=-667 vara=3.08295351498105 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=49 d=1 p=0 q=0 n=223 mean=885 vara=5.62356861599883 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=49 d=1 p=12 q=10 n=1984 mean=-970 vara=18.1396233018358 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=50 d=0 p=10 q=4 n=338 mean=-632 vara=6.39956141920136 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=50 d=0 p=3 q=12 n=177 mean=-531 vara=5.1852632184214 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=50 d=0 p=11 q=4 n=497 mean=-686 vara=2.64080148354756 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=50 d=0 p=0 q=7 n=175 mean=212 vara=0.830324768776763 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=50 d=0 p=4 q=4 n=139 mean=792 vara=0.0905051843138738 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=50 d=1 p=10 q=7 n=551 mean=137 vara=1.27859658371207 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=50 d=1 p=0 q=3 n=207 mean=845 vara=7.67227075206367 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=50 d=1 p=2 q=9 n=247 mean=-558 vara=3.35675346350638 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=50 d=1 p=7 q=7 n=493 mean=-101 vara=0.45871708112988 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=50 d=1 p=6 q=10 n=602 mean=458 vara=4.43388792254842 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=51 d=0 p=9 q=6 n=807 mean=-295 vara=3.65896630313769 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=51 d=0 p=5 q=2 n=623 mean=529 vara=4.99854366130592 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=51 d=0 p=8 q=7 n=176 mean=598 vara=6.04536223166561 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=51 d=0 p=11 q=0 n=299 mean=953 vara=8.64938086862071 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=51 d=0 p=12 q=0 n=358 mean=-60 vara=0.0751151905162791 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=51 d=1 p=5 q=0 n=242 mean=-34 vara=0.274677554081561 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=51 d=1 p=6 q=2 n=524 mean=-133 vara=0.597363097637311 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=51 d=1 p=7 q=9 n=695 mean=-39 vara=0.410194341779324 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=51 d=1 p=6 q=5 n=281 mean=577 vara=0.635495132641365 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=51 d=1 p=5 q=6 n=626 mean=-742 vara=4.26904738132887 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=52 d=0 p=8 q=9 n=483 mean=-328 vara=1.81036873292015 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=52 d=0 p=5 q=7 n=839 mean=-72 vara=0.24551693866 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=52 d=0 p=0 q=2 n=267 mean=-566 vara=2.71382473135798 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=52 d=0 p=2 q=6 n=944 mean=725 vara=2.75752382862171 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=52 d=0 p=6 q=8 n=179 mean=512 vara=3.32331919812617 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=52 d=1 p=6 q=6 n=582 mean=-285 vara=1.73564469687788 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=52 d=1 p=1 q=8 n=2080 mean=-368 vara=6.24284515269713 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=52 d=1 p=11 q=11 n=416 mean=-52 vara=0.226525100966772 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=52 d=1 p=9 q=9 n=266 mean=-579 vara=1.54515859911868 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=52 d=1 p=6 q=6 n=792 mean=556 vara=9.29757275751565 rep=5 sn=44"
    ##  PASS: TRUE

``` r
info_file_name = paste0(info_dir, "info.csv")
write.csv(results, info_file_name, row.names = FALSE)
```

``` r
print(results)
```

    ## # A tibble: 520 x 12
    ##    index     s     d     p     q     n   rep  mean   vara  seed phi      theta  
    ##    <dbl> <int> <int> <int> <int> <int> <int> <int>  <dbl> <dbl> <chr>    <chr>  
    ##  1     1     1     0     0     4    16     1    97 0.0464    40 0        0.6314~
    ##  2     2     1     0     6     4    16     2   991 4.29      41 -0.0422~ -0.085~
    ##  3     3     1     0     9    10    39     3   176 0.480     42 -0.0759~ 0.0358~
    ##  4     4     1     0     4     9    25     4   929 5.80      43 -0.0759~ 0.1167~
    ##  5     5     1     0     5     1    32     5  -324 0.342     44 -0.0759~ -0.181~
    ##  6     6     1     1     7     0   112     1  -587 3.38      40 -0.0759~ 0      
    ##  7     7     1     1     8    12   193     2   963 1.75      41 -0.0759~ -0.241~
    ##  8     8     1     1     2    10    28     3  -469 0.252     42 -0.0759~ 0.1325~
    ##  9     9     1     1     5     3    67     4    22 0.0867    43 -0.0759~ -0.181~
    ## 10    10     1     1    11     5    64     5   849 4.55      44 -0.0759~ -0.602~
    ## # ... with 510 more rows

``` r
print(paste0("Total: ", n_total, " Error: ", n_error, " Pass: ", n_pass))
```

    ## [1] "Total: 520 Error: 224 Pass: 520"

``` r
results %>% 
  group_by(s, d) %>% 
  mutate(n=n(), mean_p= mean(p), sd_p = sd(p), mean_q= mean(q), sd_q = sd(q))
```

    ## # A tibble: 520 x 16
    ## # Groups:   s, d [104]
    ##    index     s     d     p     q     n   rep  mean   vara  seed phi   theta
    ##    <dbl> <int> <int> <int> <int> <int> <int> <int>  <dbl> <dbl> <chr> <chr>
    ##  1     1     1     0     0     4     5     1    97 0.0464    40 0     0.63~
    ##  2     2     1     0     6     4     5     2   991 4.29      41 -0.0~ -0.0~
    ##  3     3     1     0     9    10     5     3   176 0.480     42 -0.0~ 0.03~
    ##  4     4     1     0     4     9     5     4   929 5.80      43 -0.0~ 0.11~
    ##  5     5     1     0     5     1     5     5  -324 0.342     44 -0.0~ -0.1~
    ##  6     6     1     1     7     0     5     1  -587 3.38      40 -0.0~ 0    
    ##  7     7     1     1     8    12     5     2   963 1.75      41 -0.0~ -0.2~
    ##  8     8     1     1     2    10     5     3  -469 0.252     42 -0.0~ 0.13~
    ##  9     9     1     1     5     3     5     4    22 0.0867    43 -0.0~ -0.1~
    ## 10    10     1     1    11     5     5     5   849 4.55      44 -0.0~ -0.6~
    ## # ... with 510 more rows, and 4 more variables: mean_p <dbl>, sd_p <dbl>,
    ## #   mean_q <dbl>, sd_q <dbl>
