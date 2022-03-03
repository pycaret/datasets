Generate AR(1) Data
================
Nikhil Gupta
2022-03-03 03:35:44

``` r
if (!require("pacman")) install.packages("pacman")
```

    ## Loading required package: pacman

``` r
pacman::p_load(dplyr, tswge)
```

``` r
data_dir = "../../data/time_series/ar1/"
meta_dir = "../../meta/time_series/ar1/"
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
    ## [511] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [526] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [541] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [556] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [571] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [586] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [601] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [616] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [631] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [646] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [661] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [676] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [691] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [706] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [721] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [736] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [751] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [766] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [781] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [796] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [811] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [826] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [841] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [856] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [871] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [886] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [901] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [916] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [931] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [946] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [961] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [976] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [991] TRUE TRUE TRUE TRUE TRUE

``` r
seed=42
set.seed(seed)

n_total = 0
n_pass = 0
n_error = 0
nreps = 5


results = tribble(~index, ~s, ~d, ~p, ~q, ~n, ~rep, ~mean, ~vara, ~seed, ~phi, ~theta)

for (phi in seq(0.99, -0.99, -0.01)){
  for (rep in 1:nreps){
    s = 0
    d = 0
    p = 1
    q = 0
    
    
    # At least as many points as needed to compute the statistics
    n_min = 20
    len_multiplier = sample(2:100, 1)
    n = sample(n_min:(n_min*len_multiplier), size=1)
    
    mean = sample(-1000:1000, 1)
    vara = abs(rnorm(1, mean=0, sd=abs(mean/100)))  # Noise Variance
    sn = 42 - round(nreps/2) + rep - 1 
    
    print(paste0("s=", s, " d=", d, " p=", p, " q=", q, " n=", n, " mean=", mean, " vara=", vara, " rep=",  rep, " sn=",  sn))
    
    pass = FALSE
    while (pass == FALSE){
      set.seed(n_error)
    
      theta = 0
      
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
```

    ## [1] "s=0 d=0 p=1 q=0 n=340 mean=176 vara=0.639106003953717 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=116 mean=308 vara=2.21007200409153 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1090 mean=140 vara=1.63697898925544 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=78 mean=48 vara=0.212783340268006 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=60 mean=32 vara=0.10251822040595 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=540 mean=612 vara=3.54281089354444 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=510 mean=-521 vara=5.2840147878922 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=48 mean=233 vara=2.95686001858934 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=499 mean=-461 vara=1.28408361398105 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=791 mean=946 vara=0.969366920600401 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=400 mean=449 vara=5.7738171630687 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1606 mean=894 vara=5.17050548898506 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=267 mean=924 vara=0.274151322530416 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=393 mean=-869 vara=2.11560216385247 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=88 mean=861 vara=2.39839381811325 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1352 mean=425 vara=4.95443288057414 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=63 mean=128 vara=0.379102333739115 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1382 mean=33 vara=0.536408403164727 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=27 mean=919 vara=2.33523775633862 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=47 mean=482 vara=1.54250577433906 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=66 mean=337 vara=7.42967022020168 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=295 mean=747 vara=7.36909364709083 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=974 mean=-109 vara=0.802624038401323 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=107 mean=78 vara=1.14531302470865 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=350 mean=645 vara=6.17140051769531 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=434 mean=-827 vara=1.40513743304271 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1224 mean=752 vara=4.98446570475468 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=37 mean=-255 vara=0.904011463760014 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=113 mean=-671 vara=11.1506708888664 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=637 mean=-957 vara=5.57035919211062 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1351 mean=897 vara=11.147796871547 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=243 mean=746 vara=1.34087044101696 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1355 mean=811 vara=4.98254801424183 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=40 mean=435 vara=1.5122133123368 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=710 mean=-284 vara=3.80106658235873 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=270 mean=864 vara=7.00514606141752 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=110 mean=-497 vara=6.06809397124571 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=95 mean=-433 vara=2.21284470135069 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=22 mean=-394 vara=1.70036756910296 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=846 mean=790 vara=2.89122806905921 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=836 mean=-93 vara=1.51094068271875 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=159 mean=735 vara=0.0870086179329371 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=52 mean=-347 vara=3.26118410762245 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=73 mean=-487 vara=3.64686265665966 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=853 mean=-757 vara=1.75967315723766 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=31 mean=367 vara=2.78428126578781 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=729 mean=853 vara=6.49798245996987 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=939 mean=-467 vara=3.7837988454217 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=340 mean=-513 vara=1.81695975058175 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=183 mean=-624 vara=0.817308443238337 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1140 mean=-313 vara=1.13268896318286 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=435 mean=645 vara=2.8628443943526 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=216 mean=152 vara=2.15523247373001 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=771 mean=-737 vara=11.5646950337621 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=600 mean=621 vara=2.01326435974245 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=90 mean=-971 vara=21.3070677984883 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=167 mean=-213 vara=1.13808632887059 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=299 mean=705 vara=10.0158278364821 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=559 mean=2 vara=0.0294721337880037 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=349 mean=-693 vara=1.67251081063692 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1666 mean=37 vara=0.416918885379047 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1092 mean=-692 vara=5.29585670828051 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=595 mean=-269 vara=0.519799285479748 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=25 mean=234 vara=0.101412303337213 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1114 mean=-185 vara=1.04954045267423 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=796 mean=505 vara=7.14348591655907 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=231 mean=-952 vara=12.9865161958029 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=91 mean=-382 vara=2.72650150624057 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1002 mean=-472 vara=4.16546487931479 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=540 mean=-373 vara=4.80846646332101 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=877 mean=377 vara=2.46506201025001 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=797 mean=321 vara=1.05186912894069 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1778 mean=-254 vara=3.6853882351694 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=875 mean=663 vara=6.73952897100483 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=76 mean=-960 vara=5.06827892523444 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=342 mean=214 vara=0.519893843276115 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=99 mean=404 vara=7.05687435767564 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1476 mean=-94 vara=0.31112097776346 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=298 mean=-440 vara=5.98804272980381 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=240 mean=-487 vara=0.127226045808004 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=126 mean=-428 vara=0.337276951588108 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=72 mean=-800 vara=4.14265201662632 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=163 mean=348 vara=1.3925495830211 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1166 mean=-508 vara=2.08090363589977 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=202 mean=-589 vara=0.722816103057461 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=267 mean=-680 vara=2.85556280224535 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=500 mean=-699 vara=0.00821857823827344 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=86 mean=343 vara=0.31930888101267 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=47 mean=-614 vara=0.262893739236624 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=524 mean=945 vara=0.872218543587421 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=502 mean=-86 vara=0.903584629845659 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1224 mean=752 vara=4.98446570475468 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=686 mean=435 vara=4.16391157264057 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=34 mean=753 vara=6.10955960774503 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=291 mean=-25 vara=0.52696344675583 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=538 mean=817 vara=1.25696882112692 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=456 mean=742 vara=0.568028187049651 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=309 mean=576 vara=0.866947053154015 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1056 mean=633 vara=0.374291924372173 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=430 mean=-290 vara=0.861454306491594 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=640 mean=823 vara=1.40009953926892 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=163 mean=2 vara=0.00280436574708284 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=152 mean=955 vara=4.68728819593832 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=924 mean=191 vara=3.17562215817449 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=289 mean=831 vara=10.3401311833052 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=874 mean=-533 vara=9.45498112861979 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=378 mean=-864 vara=8.6496356084276 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1369 mean=-379 vara=5.73361264736473 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=564 mean=468 vara=0.532334132350618 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=181 mean=-84 vara=0.939182174789121 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=54 mean=-52 vara=0.659059934269555 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=302 mean=-373 vara=6.60598559271579 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=607 mean=335 vara=0.763549500988381 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=426 mean=215 vara=0.697278685397479 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=187 mean=-657 vara=3.99951643799534 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=151 mean=923 vara=6.58503832626292 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=333 mean=865 vara=1.60501639111682 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=590 mean=-985 vara=20.4910545848354 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=640 mean=478 vara=5.40051080517293 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=64 mean=216 vara=1.60472566539842 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=202 mean=527 vara=11.5270904047253 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=635 mean=-241 vara=3.93388554728913 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=622 mean=802 vara=7.8733564835846 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=553 mean=-924 vara=4.92337346056957 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=888 mean=34 vara=0.249441042472782 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=965 mean=-361 vara=2.64010847692585 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=168 mean=-192 vara=4.56373809718519 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=119 mean=-862 vara=6.30212551063122 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=492 mean=369 vara=0.462409950059214 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1287 mean=123 vara=1.19399147313502 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=511 mean=972 vara=20.6339802183789 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1224 mean=752 vara=4.98446570475468 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1267 mean=181 vara=0.196412833644342 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=563 mean=-595 vara=1.90782780742503 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=178 mean=143 vara=1.06593392197218 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=693 mean=-397 vara=4.37817629579801 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=290 mean=348 vara=3.14952077258101 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=89 mean=-833 vara=3.60115707658999 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=883 mean=381 vara=0.164727309125289 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=125 mean=499 vara=0.197109894751022 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=38 mean=659 vara=3.37950715151746 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=300 mean=-556 vara=0.809062496202677 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=101 mean=-275 vara=2.37466320087484 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1255 mean=-254 vara=1.32536207712492 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=854 mean=889 vara=22.6338547630507 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=78 mean=776 vara=13.9796409995253 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=830 mean=-181 vara=1.79041606917027 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=325 mean=651 vara=0.838308321477283 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=275 mean=88 vara=0.759224556741707 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=442 mean=-452 vara=0.382367580414875 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=384 mean=-403 vara=5.78012881121345 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=200 mean=-871 vara=8.65731486699073 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1029 mean=237 vara=0.5391826192296 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=333 mean=188 vara=1.65257343909884 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=39 mean=-798 vara=0.345030455942968 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=700 mean=98 vara=0.0747814186961917 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=123 mean=447 vara=6.76794758413774 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1067 mean=406 vara=0.293979479720628 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=339 mean=924 vara=1.61298248358904 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=849 mean=181 vara=1.03051156564993 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=408 mean=-704 vara=4.49365865548878 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=360 mean=-244 vara=1.95489845975474 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=45 mean=-498 vara=1.38188947781299 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=172 mean=893 vara=3.23925405155759 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=314 mean=-387 vara=1.64084293974067 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1617 mean=-827 vara=1.18753194348648 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=124 mean=138 vara=2.20095149011886 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1067 mean=406 vara=0.293979479720628 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=880 mean=255 vara=3.21411683993808 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=81 mean=-913 vara=15.2251710605793 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1218 mean=375 vara=6.31838728677617 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1331 mean=-154 vara=0.902994166164755 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=593 mean=801 vara=13.0450561828076 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1501 mean=154 vara=1.5413265884163 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=31 mean=47 vara=0.410146758464954 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=140 mean=-224 vara=1.84798174243302 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=21 mean=142 vara=1.14990313237731 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=212 mean=477 vara=2.83438869832216 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=810 mean=-63 vara=0.806554836875444 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=690 mean=-96 vara=1.10260647366507 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1818 mean=805 vara=3.07498784149983 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=145 mean=-965 vara=7.73730792074143 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=897 mean=-825 vara=0.762999744590276 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=796 mean=-103 vara=0.647667332438931 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1635 mean=-509 vara=1.01659185744619 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=440 mean=233 vara=0.854913924083905 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=187 mean=51 vara=0.38329187586023 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=33 mean=-73 vara=0.0042340415610572 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=482 mean=-396 vara=4.1832189161568 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=340 mean=891 vara=10.893196348975 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=437 mean=-901 vara=2.95780779170431 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=799 mean=926 vara=13.7890151827373 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=188 mean=-158 vara=0.878655246782572 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1255 mean=-497 vara=0.727968956184383 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=804 mean=580 vara=4.68364397342914 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1401 mean=470 vara=0.293554790575562 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=164 mean=-448 vara=0.599887014849419 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=690 mean=-618 vara=4.65164017136333 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=191 mean=541 vara=0.66773753064719 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1011 mean=-120 vara=0.356092105547556 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=804 mean=-396 vara=1.58940986005134 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=279 mean=802 vara=7.544620103609 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=51 mean=467 vara=8.53815699190747 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=158 mean=-845 vara=8.38825171887521 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=463 mean=437 vara=1.42325599404111 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1658 mean=3 vara=0.00445640015593392 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=964 mean=-706 vara=2.90838707635584 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=598 mean=-944 vara=9.85774474740978 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=542 mean=36 vara=0.193342962459211 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=593 mean=-15 vara=0.171692690516027 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=95 mean=854 vara=3.5501148814975 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=503 mean=269 vara=5.39519965097264 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=23 mean=-921 vara=0.0315775690596199 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=31 mean=-296 vara=3.55643526672232 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=236 mean=-920 vara=17.2436760234189 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=489 mean=704 vara=0.528002740892845 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=588 mean=-638 vara=3.1332114493578 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=485 mean=494 vara=3.91175494709841 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=265 mean=-736 vara=0.984040563699652 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=88 mean=291 vara=0.859397745381751 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=515 mean=-493 vara=1.11419059076138 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=92 mean=591 vara=6.5081311504138 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=143 mean=967 vara=7.01224762265534 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=211 mean=-255 vara=1.29778110782281 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=260 mean=156 vara=0.408434951871033 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=396 mean=-149 vara=0.712821611271617 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=508 mean=-460 vara=0.0542440162299634 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1041 mean=-265 vara=2.50157198699957 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=32 mean=423 vara=11.1405914512714 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=236 mean=-920 vara=17.2436760234189 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=184 mean=-445 vara=0.443678793376071 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=22 mean=974 vara=3.69784296629649 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=684 mean=529 vara=4.5925940921451 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=235 mean=-285 vara=2.63100557846474 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=864 mean=470 vara=5.73663848602248 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=318 mean=374 vara=1.43088857875841 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=45 mean=-370 vara=4.34935444502065 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=478 mean=164 vara=0.555139338583301 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=446 mean=-687 vara=0.867079444239177 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=372 mean=-937 vara=0.150460870344576 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=131 mean=-406 vara=3.58148971446802 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=219 mean=908 vara=9.31822440144078 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=68 mean=-592 vara=2.41337940603651 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=365 mean=-886 vara=1.34189921076108 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1748 mean=369 vara=0.873297911653597 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1149 mean=-522 vara=0.857371725995747 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1077 mean=933 vara=16.8768072948344 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=253 mean=45 vara=0.233617453248765 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=160 mean=-898 vara=2.31757866612238 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=394 mean=512 vara=4.7987031477504 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=33 mean=246 vara=2.68522020237295 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=363 mean=968 vara=3.64424688535953 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=353 mean=-911 vara=11.650437308011 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=492 mean=-943 vara=1.23647000662369 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=446 mean=-545 vara=0.244379976197447 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=37 mean=-489 vara=6.46639730830274 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=132 mean=784 vara=5.22934449557759 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=571 mean=419 vara=4.35172260376048 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=128 mean=-611 vara=0.988644287203615 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=853 mean=-757 vara=1.75967315723766 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=40 mean=-660 vara=3.91425195488211 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=101 mean=-371 vara=4.72880165553091 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=303 mean=318 vara=6.41970987537237 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1680 mean=-495 vara=11.2710740341323 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=232 mean=674 vara=15.7181139796648 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1140 mean=-313 vara=1.13268896318286 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=359 mean=-63 vara=0.280389779342829 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=346 mean=283 vara=2.1194581559715 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=656 mean=-717 vara=5.12073227267338 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=229 mean=941 vara=11.5092422429666 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=662 mean=481 vara=6.32402591184425 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=158 mean=-183 vara=0.275232513090357 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=504 mean=464 vara=2.86669151495237 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=367 mean=781 vara=1.51864482188041 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1785 mean=582 vara=4.00988110725654 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=277 mean=-550 vara=0.16889814817408 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=268 mean=-160 vara=1.09482110687085 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=132 mean=-888 vara=6.19507450339523 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=492 mean=369 vara=0.462409950059214 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1324 mean=962 vara=3.68854794159397 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=823 mean=-201 vara=3.04652173579384 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=145 mean=363 vara=3.1338848410942 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=32 mean=563 vara=0.0553263011159952 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=29 mean=-34 vara=0.010011258890001 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=258 mean=-50 vara=0.271200779995634 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1927 mean=-964 vara=9.71182634146638 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=898 mean=18 vara=0.345935240613123 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=630 mean=-305 vara=3.31923744274435 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=160 mean=-648 vara=4.82337753066709 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1568 mean=-39 vara=0.133161371539835 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=780 mean=125 vara=0.475960543661085 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1086 mean=-947 vara=11.5265218071067 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=57 mean=-604 vara=1.54481363286198 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=175 mean=-727 vara=1.85551790596335 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=139 mean=-283 vara=3.45879727488274 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=529 mean=405 vara=10.620200889162 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=100 mean=922 vara=10.9988165815301 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=901 mean=299 vara=4.76101632527555 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=523 mean=728 vara=2.0646908241623 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=694 mean=-17 vara=0.0612437312031369 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=177 mean=708 vara=4.07979148398296 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1694 mean=-898 vara=10.8605481566513 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=812 mean=843 vara=4.93341740417649 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=810 mean=333 vara=6.43534345251833 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=220 mean=728 vara=10.4403949063331 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=203 mean=-930 vara=4.22021801643493 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=185 mean=-349 vara=0.510982212463973 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=168 mean=775 vara=11.283183803216 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=840 mean=-132 vara=1.74682282541427 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=119 mean=-94 vara=0.381587192857289 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=93 mean=732 vara=3.23725203591985 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=935 mean=-488 vara=7.61777708212684 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=101 mean=663 vara=1.37101993790914 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=883 mean=381 vara=0.164727309125289 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=642 mean=-20 vara=0.038618395371531 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=140 mean=914 vara=13.3576678637422 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=70 mean=228 vara=3.89756518264391 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=237 mean=-189 vara=1.06873895851261 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=114 mean=956 vara=16.0845170166121 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=347 mean=417 vara=5.76535233163771 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=437 mean=-901 vara=2.95780779170431 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1050 mean=-721 vara=4.3341055122919 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=257 mean=-892 vara=15.4530132097652 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=378 mean=364 vara=6.8876154314244 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=808 mean=-762 vara=6.52664538767055 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=879 mean=226 vara=0.160530379459872 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=35 mean=507 vara=6.1370494625808 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=143 mean=-556 vara=0.52776026833572 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1301 mean=957 vara=11.469605347258 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=258 mean=694 vara=10.2322764801127 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=99 mean=724 vara=2.46512881011682 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=64 mean=146 vara=0.588637693887179 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1195 mean=632 vara=0.622241021161509 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=336 mean=-539 vara=2.53498365496297 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=145 mean=-648 vara=8.87126223696435 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=97 mean=-711 vara=0.438145599685565 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=83 mean=922 vara=11.1170434099403 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1307 mean=-217 vara=1.45931740352913 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=763 mean=120 vara=0.220141830707735 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=431 mean=619 vara=11.0323642187982 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=193 mean=-246 vara=1.77399879536271 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=153 mean=934 vara=7.08488961706741 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=386 mean=-103 vara=0.124266457281904 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=82 mean=265 vara=3.14651709115675 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=683 mean=-772 vara=9.09687664489782 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=211 mean=-287 vara=0.231420721262774 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=71 mean=-775 vara=11.7061140814148 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=237 mean=-189 vara=1.06873895851261 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=114 mean=956 vara=16.0845170166121 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=282 mean=847 vara=9.90031948561526 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=371 mean=131 vara=2.96297052359444 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=821 mean=-248 vara=4.01981392095053 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=65 mean=825 vara=0.0814619915355593 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=105 mean=939 vara=8.12026521828945 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=439 mean=478 vara=1.58275813222279 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=156 mean=993 vara=4.66605979966647 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=351 mean=-53 vara=0.709247201477194 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=327 mean=-916 vara=10.9117762331895 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=686 mean=-12 vara=0.0500582535401125 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=183 mean=342 vara=0.952835963802318 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=171 mean=270 vara=4.20003193512027 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=116 mean=-345 vara=4.77821327705441 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=124 mean=362 vara=1.77335803366454 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=80 mean=43 vara=0.625472382848009 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=901 mean=395 vara=1.51936130467011 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=147 mean=-278 vara=0.40858113831679 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=406 mean=-952 vara=6.62860693200605 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=87 mean=-879 vara=1.87442212029083 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=200 mean=-182 vara=2.48479940612887 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1064 mean=634 vara=2.39755586981085 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=746 mean=147 vara=0.335387585333005 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=431 mean=919 vara=2.65897585991304 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=207 mean=-285 vara=3.66875509138162 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=320 mean=-831 vara=0.532119856551719 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=165 mean=-908 vara=1.19369214668406 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=190 mean=-679 vara=11.3096261970231 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=567 mean=777 vara=6.98791845484253 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=21 mean=604 vara=7.20690723507564 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=580 mean=-428 vara=1.29567728051755 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=126 mean=946 vara=3.90185352892965 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1236 mean=69 vara=0.416607043593083 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=721 mean=-574 vara=0.817032205590943 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=27 mean=271 vara=1.04988022048666 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=146 mean=231 vara=2.79381267962638 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=865 mean=476 vara=1.86036165193045 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=215 mean=-375 vara=0.0928167057286294 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=28 mean=599 vara=5.17886026417834 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=684 mean=529 vara=4.5925940921451 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=74 mean=-518 vara=0.836881167355602 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=524 mean=945 vara=0.872218543587421 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=581 mean=377 vara=0.745937600076006 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=511 mean=-197 vara=3.42167202693594 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=49 mean=907 vara=4.36378190256541 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=429 mean=-307 vara=0.0238294436963928 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=43 mean=407 vara=0.779649545710825 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=153 mean=-769 vara=2.69389171594587 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=24 mean=-758 vara=9.49621645430946 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=678 mean=510 vara=4.45177914547158 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=146 mean=850 vara=14.5653204929634 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=577 mean=-12 vara=0.17240767845601 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1397 mean=615 vara=6.68112670140687 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=805 mean=-344 vara=0.886084608298665 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=473 mean=475 vara=7.47068596260927 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=739 mean=292 vara=2.89603143054897 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=407 mean=-590 vara=3.22688206134392 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=797 mean=995 vara=4.05199473771813 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1131 mean=-296 vara=1.99926116245425 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=60 mean=701 vara=3.2915248627115 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=175 mean=-727 vara=1.85551790596335 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=343 mean=73 vara=1.47253799009397 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=97 mean=861 vara=4.5220569150319 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=40 mean=-772 vara=9.09795013598974 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1037 mean=790 vara=6.16819657894415 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=526 mean=99 vara=1.6028567150921 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=694 mean=-17 vara=0.0612437312031369 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=93 mean=-486 vara=10.1560362916441 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=107 mean=-550 vara=1.83657389926905 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1445 mean=-455 vara=1.71013483422187 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1198 mean=-725 vara=0.786546402007357 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=623 mean=-46 vara=0.254395580477212 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=69 mean=912 vara=3.53255500730021 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=56 mean=716 vara=14.1721756176334 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=395 mean=448 vara=0.144580717380731 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=36 mean=-519 vara=0.5128874380607 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=643 mean=498 vara=1.13898019888216 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1571 mean=287 vara=1.12287697529242 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=690 mean=305 vara=1.93674836188713 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=23 mean=499 vara=5.36208412293998 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=580 mean=-428 vara=1.29567728051755 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=139 mean=440 vara=1.60369997673159 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=459 mean=-58 vara=0.482260340312563 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=585 mean=-3 vara=0.0319960938557427 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=81 mean=-835 vara=0.657941899825405 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=644 mean=-459 vara=5.76871452225719 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=26 mean=308 vara=1.3538498099809 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=235 mean=-925 vara=1.85271435772517 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1479 mean=178 vara=1.54465814640073 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=53 mean=248 vara=1.10298296286562 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1042 mean=849 vara=10.9323628350132 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=32 mean=-963 vara=7.09538800811921 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=986 mean=-919 vara=2.90747533525057 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=654 mean=-726 vara=6.75656585549282 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=214 mean=187 vara=0.431944152706746 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=246 mean=-447 vara=2.53434565985207 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=98 mean=-757 vara=9.028298077695 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=796 mean=-147 vara=0.876391543946437 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=104 mean=-678 vara=8.74519715401011 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=901 mean=299 vara=4.76101632527555 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1133 mean=-889 vara=7.51130605257041 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1245 mean=-236 vara=3.09830560188055 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=310 mean=-226 vara=2.44679093333198 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=38 mean=-276 vara=4.21856671315652 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=143 mean=-556 vara=0.52776026833572 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=620 mean=565 vara=5.48832654470803 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=67 mean=-734 vara=8.74493590792282 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=475 mean=-521 vara=2.02425758220732 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=150 mean=-109 vara=0.0334698996395056 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=32 mean=563 vara=0.0553263011159952 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=212 mean=-331 vara=1.39499693793911 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=902 mean=-49 vara=0.00858453835818178 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=147 mean=-278 vara=0.40858113831679 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=106 mean=765 vara=0.79953927678632 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=303 mean=318 vara=6.41970987537237 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=167 mean=842 vara=0.878080884041296 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=463 mean=437 vara=1.42325599404111 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=797 mean=-889 vara=13.3905921277356 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=342 mean=733 vara=1.41975041406516 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1046 mean=-755 vara=7.77435044527276 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=166 mean=-191 vara=0.232355594167012 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=394 mean=512 vara=4.7987031477504 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=85 mean=468 vara=1.69140875668398 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=729 mean=853 vara=6.49798245996987 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=61 mean=-816 vara=0.0350925535002462 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=175 mean=-727 vara=1.85551790596335 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=35 mean=-546 vara=7.03844216962496 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=350 mean=-840 vara=0.709634021688227 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=119 mean=946 vara=6.97752563873481 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=124 mean=362 vara=1.77335803366454 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=354 mean=69 vara=0.862663256814315 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=324 mean=-865 vara=1.63494907270729 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=46 mean=-819 vara=5.66101409084951 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=101 mean=-371 vara=4.72880165553091 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=143 mean=967 vara=7.01224762265534 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=345 mean=771 vara=5.42481597453459 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=135 mean=631 vara=12.8119087272814 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=179 mean=-745 vara=2.10774025415062 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=713 mean=-740 vara=1.775633187283 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=722 mean=-861 vara=2.4763797227078 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=248 mean=221 vara=1.27419488601257 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=925 mean=-215 vara=1.50355975585478 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=545 mean=250 vara=2.33603270903827 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1256 mean=451 vara=6.51439971867015 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=38 mean=-245 vara=3.74503623870545 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=29 mean=-34 vara=0.010011258890001 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=44 mean=-156 vara=2.51284265395598 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=122 mean=-348 vara=4.17444058930789 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=119 mean=841 vara=2.7019396036166 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=917 mean=894 vara=4.64955914886806 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=169 mean=-845 vara=10.9114970389842 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=72 mean=866 vara=19.5590436091527 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1736 mean=908 vara=11.0509813628392 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=610 mean=-760 vara=7.17884807339463 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1401 mean=-662 vara=14.2300507264792 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=51 mean=-460 vara=3.26392455461521 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1546 mean=858 vara=0.681955846770356 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=351 mean=254 vara=4.44786424686491 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=175 mean=663 vara=15.5942121883091 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=50 mean=-618 vara=4.65164017136333 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=508 mean=685 vara=8.48202844670557 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=435 mean=-134 vara=0.85423187333912 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=193 mean=-246 vara=1.77399879536271 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=567 mean=777 vara=6.98791845484253 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=383 mean=-283 vara=4.03114667756195 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=933 mean=851 vara=7.70372239915098 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=220 mean=-615 vara=4.5097123992956 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=105 mean=423 vara=0.743279816459595 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=503 mean=269 vara=5.39519965097264 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1378 mean=-834 vara=9.57755063119755 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=409 mean=284 vara=0.478971631928224 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=205 mean=78 vara=0.178161280936643 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=593 mean=483 vara=3.75189830000766 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=41 mean=762 vara=4.25503777762512 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1037 mean=790 vara=6.16819657894415 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=621 mean=232 vara=4.60732722200396 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=197 mean=24 vara=0.114368404359707 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=915 mean=-16 vara=0.101251662903805 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=567 mean=-837 vara=17.8622666310499 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1107 mean=322 vara=0.806732719109716 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=254 mean=-149 vara=1.60635404047125 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1083 mean=-665 vara=3.11499642877987 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=566 mean=-143 vara=0.147468308559167 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=82 mean=-711 vara=9.79994053424306 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=104 mean=904 vara=30.0644467745654 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1847 mean=324 vara=5.0361522034163 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=130 mean=-671 vara=0.747868021189984 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=328 mean=206 vara=0.452077117025765 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=163 mean=926 vara=3.30493670809436 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=504 mean=464 vara=2.86669151495237 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=272 mean=-798 vara=3.89793294359127 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=146 mean=749 vara=5.78883192544976 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=282 mean=56 vara=0.359283931486121 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=268 mean=-160 vara=1.09482110687085 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=91 mean=-382 vara=2.72650150624057 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=56 mean=-295 vara=1.73013823332193 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=307 mean=-284 vara=2.00406086052015 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=117 mean=-169 vara=0.10348487640834 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=235 mean=-216 vara=2.23909790573236 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=360 mean=67 vara=0.00930321930336281 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=697 mean=122 vara=2.00912716564328 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=242 mean=-581 vara=2.60631716479768 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=680 mean=-44 vara=0.218189755585008 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=124 mean=861 vara=3.69030940050154 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=151 mean=233 vara=2.48163962608298 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=211 mean=-255 vara=1.29778110782281 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=242 mean=-118 vara=0.135660128300991 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=680 mean=-44 vara=0.218189755585008 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=124 mean=861 vara=3.69030940050154 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=151 mean=233 vara=2.48163962608298 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=211 mean=-255 vara=1.29778110782281 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=242 mean=-118 vara=0.135660128300991 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=680 mean=-44 vara=0.218189755585008 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=124 mean=861 vara=3.69030940050154 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=151 mean=233 vara=2.48163962608298 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=211 mean=-255 vara=1.29778110782281 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=242 mean=-118 vara=0.135660128300991 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=680 mean=-44 vara=0.218189755585008 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=652 mean=-106 vara=1.41793017246877 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=622 mean=802 vara=7.8733564835846 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=519 mean=584 vara=11.7350820743743 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=799 mean=146 vara=1.70570900099191 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1309 mean=183 vara=1.12552545353397 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=468 mean=-537 vara=1.84405230903791 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=396 mean=672 vara=6.31244786582089 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=551 mean=983 vara=19.7561221768688 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1375 mean=-390 vara=10.1776303274512 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1352 mean=541 vara=3.88011980285944 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=384 mean=-714 vara=16.6504235019264 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=84 mean=828 vara=8.13995559515309 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=124 mean=225 vara=0.168834735974914 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=540 mean=291 vara=5.91499376059333 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=330 mean=-615 vara=0.525968866288429 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1473 mean=-967 vara=3.93436034010824 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=282 mean=301 vara=3.99331369237604 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=292 mean=-55 vara=0.176103407863333 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=282 mean=-851 vara=3.93725944431394 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=225 mean=-726 vara=12.1975251555382 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=249 mean=191 vara=0.982613767630604 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=591 mean=359 vara=3.52594434595226 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=373 mean=-72 vara=0.0484343532328412 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=626 mean=-744 vara=17.3938776549627 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1171 mean=-260 vara=0.00926003995058852 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=77 mean=-903 vara=6.38061804597096 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=285 mean=919 vara=13.2061279597912 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=437 mean=-797 vara=3.97183902389686 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=107 mean=-838 vara=0.385814188821146 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=37 mean=-759 vara=6.24442911882736 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=858 mean=697 vara=5.91901972752752 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=127 mean=62 vara=1.11243476253428 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1002 mean=-472 vara=4.16546487931479 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=356 mean=-659 vara=9.49726994978382 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=656 mean=-564 vara=4.38713194411758 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1054 mean=880 vara=9.16975663907475 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=267 mean=-42 vara=0.331196301634603 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=80 mean=291 vara=0.908249714139213 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=743 mean=-931 vara=11.5589519944599 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=445 mean=15 vara=0.142039911347255 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1180 mean=507 vara=2.96187887068895 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=949 mean=-404 vara=0.107103196032869 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=328 mean=584 vara=11.5112050651252 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=921 mean=598 vara=5.95621223230217 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=851 mean=-266 vara=1.45923188474624 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=189 mean=-772 vara=4.64920229963977 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=431 mean=554 vara=3.50194089762664 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=98 mean=326 vara=1.94773489526448 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=876 mean=684 vara=8.13827750755805 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=756 mean=-763 vara=0.400738679119758 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=22 mean=-749 vara=0.53332335380196 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=678 mean=510 vara=4.45177914547158 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=146 mean=850 vara=14.5653204929634 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=865 mean=476 vara=1.86036165193045 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=215 mean=-375 vara=0.0928167057286294 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=83 mean=646 vara=10.1589332944174 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=863 mean=287 vara=2.4210952727383 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=217 mean=398 vara=6.42203834047218 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=260 mean=156 vara=0.408434951871033 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1526 mean=907 vara=1.41402754545551 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=163 mean=-65 vara=0.104524913932982 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=757 mean=391 vara=1.4898276369485 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=192 mean=280 vara=0.254422017016972 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=96 mean=-94 vara=0.5670508349399 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=796 mean=-147 vara=0.876391543946437 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1131 mean=-296 vara=1.99926116245425 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=595 mean=-269 vara=0.519799285479748 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=164 mean=597 vara=1.9445909053459 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=394 mean=512 vara=4.7987031477504 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1012 mean=66 vara=0.618371176033034 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=897 mean=-408 vara=1.80933521223257 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=694 mean=123 vara=1.81705655628853 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=353 mean=35 vara=0.37367311597686 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1284 mean=-184 vara=0.414339657626879 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=942 mean=-224 vara=2.47801166251625 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=878 mean=-11 vara=0.0229330567958511 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1252 mean=619 vara=0.367269094190091 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=43 mean=-647 vara=1.37985238779717 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=132 mean=-927 vara=13.9542064193119 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=179 mean=-745 vara=2.10774025415062 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1694 mean=-898 vara=10.8605481566513 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=264 mean=-269 vara=3.35959431851643 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=771 mean=-737 vara=11.5646950337621 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=64 mean=28 vara=0.0367102789459044 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=190 mean=479 vara=11.6936429275895 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=354 mean=-593 vara=1.47069670036708 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=101 mean=772 vara=0.137165420895718 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1847 mean=324 vara=5.0361522034163 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=123 mean=25 vara=0.154430128398011 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1232 mean=-922 vara=9.750740500208 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=708 mean=-183 vara=1.49651581594168 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=88 mean=-298 vara=3.03633560720642 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=56 mean=-295 vara=1.73013823332193 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=628 mean=-729 vara=0.844834464467134 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=125 mean=215 vara=0.408015685810525 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=416 mean=792 vara=1.4171407924665 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=244 mean=243 vara=2.59635171985692 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1204 mean=237 vara=2.06646668189339 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=150 mean=-821 vara=9.41682458207494 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=700 mean=-789 vara=9.97077484976419 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=568 mean=909 vara=5.81880578168077 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1036 mean=-649 vara=9.82728474731664 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=661 mean=621 vara=1.16061316641507 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=906 mean=-567 vara=6.29742699534872 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=586 mean=-215 vara=3.48840424498854 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1414 mean=-623 vara=1.40080738606464 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=527 mean=-25 vara=0.131446351985764 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=25 mean=744 vara=5.78362689803411 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=44 mean=-156 vara=2.51284265395598 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=521 mean=644 vara=5.04934991699396 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=701 mean=-397 vara=0.575788561975029 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=932 mean=708 vara=10.1000197596952 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=434 mean=820 vara=3.39466105437042 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1382 mean=-85 vara=0.575197267557481 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1080 mean=1000 vara=10.3909574024493 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=869 mean=-433 vara=2.43589291069377 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=200 mean=-686 vara=0.362914751537373 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=762 mean=-806 vara=1.75300564884958 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=431 mean=619 vara=11.0323642187982 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1310 mean=521 vara=9.82493665084733 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=233 mean=-828 vara=12.7884807482133 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=428 mean=484 vara=3.69492240468857 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=98 mean=326 vara=1.94773489526448 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=129 mean=406 vara=2.1860441899181 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=939 mean=763 vara=2.62785300836739 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=541 mean=371 vara=3.47587867277395 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=176 mean=795 vara=2.89896352820701 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1097 mean=592 vara=19.827998560154 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=340 mean=194 vara=0.141559418845679 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=97 mean=861 vara=4.5220569150319 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1166 mean=-200 vara=0.517001993080594 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=101 mean=60 vara=0.283316516398819 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=883 mean=381 vara=0.164727309125289 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=642 mean=-20 vara=0.038618395371531 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=140 mean=914 vara=13.3576678637422 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=834 mean=803 vara=10.1918963043861 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=141 mean=-727 vara=13.6661162794843 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1301 mean=957 vara=11.469605347258 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=540 mean=315 vara=1.12376877500186 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=369 mean=907 vara=15.6473104794413 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=821 mean=-248 vara=4.01981392095053 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=21 mean=572 vara=0.698890174938609 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=559 mean=-999 vara=11.008995294957 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=44 mean=-126 vara=1.07732039510563 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=52 mean=259 vara=1.27996718199806 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=488 mean=-932 vara=10.7493163001243 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=589 mean=-471 vara=0.127067393521659 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=120 mean=-746 vara=0.978424332622394 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=540 mean=291 vara=5.91499376059333 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1249 mean=885 vara=1.45047256505749 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=25 mean=934 vara=0.86608173807443 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=684 mean=529 vara=4.5925940921451 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=169 mean=273 vara=0.0335900854436322 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=603 mean=-985 vara=7.88148521482485 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=801 mean=104 vara=1.22219468633427 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=563 mean=-727 vara=2.53978774079555 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=358 mean=757 vara=3.04461069818524 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=47 mean=64 vara=0.275355263517205 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=331 mean=-573 vara=2.02224874156245 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=24 mean=-223 vara=1.58086457880983 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=277 mean=615 vara=0.977362439969435 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=131 mean=770 vara=11.5633884061646 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=492 mean=369 vara=0.462409950059214 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1324 mean=962 vara=3.68854794159397 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=823 mean=-201 vara=3.04652173579384 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=145 mean=363 vara=3.1338848410942 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=32 mean=563 vara=0.0553263011159952 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=29 mean=-34 vara=0.010011258890001 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=258 mean=-50 vara=0.271200779995634 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1927 mean=-964 vara=9.71182634146638 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=227 mean=-987 vara=5.59019816597083 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=658 mean=-673 vara=1.62453446432772 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=949 mean=-798 vara=4.99190381325289 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1179 mean=165 vara=0.795326015980528 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=201 mean=-802 vara=4.79693502507046 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=185 mean=-349 vara=0.510982212463973 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=431 mean=554 vara=3.50194089762664 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=423 mean=-6 vara=0.117788760625728 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1067 mean=407 vara=0.779649545710825 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=135 mean=-967 vara=7.47143764284963 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=419 mean=285 vara=1.03379081622567 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=41 mean=674 vara=16.9074275631032 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=244 mean=596 vara=1.63092838018952 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=102 mean=878 vara=0.659418230498122 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=37 mean=-759 vara=6.24442911882736 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=132 mean=784 vara=5.22934449557759 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=571 mean=419 vara=4.35172260376048 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=128 mean=-611 vara=0.988644287203615 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=853 mean=-757 vara=1.75967315723766 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=40 mean=-660 vara=3.91425195488211 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=758 mean=221 vara=0.0682777299699745 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=419 mean=555 vara=2.09135327259398 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=494 mean=-609 vara=9.93285197212636 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1294 mean=64 vara=0.510357719742283 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1394 mean=-1000 vara=15.4358808670329 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=709 mean=-105 vara=0.154526075687143 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1128 mean=-155 vara=0.321346439455323 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=596 mean=-150 vara=1.49648665201075 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=349 mean=-693 vara=1.67251081063692 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=925 mean=898 vara=11.2626799884988 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=427 mean=173 vara=1.28532237974045 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1709 mean=780 vara=2.74932177264299 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1289 mean=-859 vara=7.1262759376747 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=194 mean=-754 vara=0.82559634713208 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=765 mean=594 vara=0.250989844941072 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1697 mean=610 vara=4.69963212337328 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=770 mean=1000 vara=6.33814529256394 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1035 mean=-826 vara=9.19960550325603 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=57 mean=-222 vara=1.5067988881209 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=190 mean=479 vara=11.6936429275895 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=717 mean=-794 vara=0.413524235298034 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=27 mean=271 vara=1.04988022048666 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=578 mean=847 vara=13.940774910326 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=181 mean=-84 vara=0.939182174789121 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=36 mean=393 vara=0.74797485135736 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=101 mean=-371 vara=4.72880165553091 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=404 mean=-490 vara=1.04359709715711 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=99 mean=-137 vara=2.10059427426234 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=439 mean=478 vara=1.58275813222279 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=148 mean=-551 vara=2.61494085291745 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=220 mean=-331 vara=1.01065247723793 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=329 mean=239 vara=0.950632307250938 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=372 mean=-745 vara=7.08235535104055 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=808 mean=-762 vara=6.52664538767055 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=553 mean=-329 vara=4.94855722306332 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=456 mean=742 vara=0.568028187049651 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=124 mean=814 vara=0.715955580713271 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=709 mean=-822 vara=10.8242817394622 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=476 mean=-870 vara=9.07433148074167 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=247 mean=-32 vara=0.0166240854498353 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=314 mean=-945 vara=3.48539766400715 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1355 mean=811 vara=4.98254801424183 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=255 mean=-938 vara=4.19493335231714 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=885 mean=-292 vara=2.19986854148019 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1520 mean=-797 vara=9.10550291309714 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=455 mean=306 vara=1.08456469350831 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=124 mean=814 vara=0.715955580713271 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1030 mean=97 vara=0.768444343460056 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1452 mean=846 vara=9.41236384907046 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=334 mean=-944 vara=11.2473475321873 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=24 mean=-359 vara=2.55322102751931 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=145 mean=408 vara=4.24721296469922 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1065 mean=792 vara=11.8847424969342 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=66 mean=-682 vara=3.9917144406323 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=78 mean=415 vara=1.09866508531744 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=777 mean=769 vara=9.85459225514705 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=256 mean=109 vara=0.760060139837101 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=771 mean=-737 vara=11.5646950337621 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=399 mean=-218 vara=1.22261322862989 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=268 mean=-857 vara=17.463132909042 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=35 mean=238 vara=4.28749975649883 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=637 mean=718 vara=2.7156219206106 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=733 mean=-551 vara=3.14602610778686 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=292 mean=873 vara=13.7806713767489 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=429 mean=-884 vara=6.4034413247429 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1050 mean=-721 vara=4.3341055122919 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=949 mean=364 vara=0.802661645838519 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=922 mean=-859 vara=9.37203731677379 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=34 mean=-303 vara=0.0853474327067291 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=123 mean=265 vara=1.63481632287522 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=167 mean=-213 vara=1.13808632887059 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=95 mean=-433 vara=2.21284470135069 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=784 mean=-354 vara=0.297742675789755 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=157 mean=-691 vara=2.84894673677139 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=603 mean=962 vara=1.94904369340231 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=122 mean=-569 vara=4.11131199012949 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=120 mean=900 vara=9.12348953596134 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=952 mean=452 vara=0.412396066146862 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=50 mean=659 vara=4.73235640642084 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=171 mean=-780 vara=2.38910022163175 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=600 mean=808 vara=2.56025198622523 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1064 mean=354 vara=11.5364474734587 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=161 mean=40 vara=0.353840313621897 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=603 mean=-985 vara=7.88148521482485 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=306 mean=515 vara=3.32813138387761 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=337 mean=58 vara=0.253642058525142 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=29 mean=704 vara=7.80759416450107 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=482 mean=-396 vara=4.1832189161568 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=915 mean=15 vara=0.0289906593273899 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=71 mean=537 vara=1.90075787159966 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=729 mean=853 vara=6.49798245996987 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=390 mean=878 vara=4.30457595266254 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=125 mean=158 vara=1.76165544162274 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=664 mean=96 vara=0.493127193610101 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=139 mean=682 vara=0.184780981525509 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=24 mean=-758 vara=9.49621645430946 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=828 mean=-52 vara=0.0483171783930351 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=49 mean=908 vara=9.06992117026612 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=211 mean=318 vara=0.727805992121191 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=106 mean=413 vara=2.79475673713484 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=253 mean=-419 vara=1.45965797547006 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=303 mean=262 vara=2.02478075858912 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=550 mean=-980 vara=9.29129999441361 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=156 mean=551 vara=3.36130629802591 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=157 mean=-484 vara=0.77906523842924 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=675 mean=-298 vara=3.43151486596925 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1000 mean=-33 vara=0.636397361068689 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=743 mean=-624 vara=5.99560972505883 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=609 mean=-753 vara=10.6758844008491 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=210 mean=129 vara=2.43310990863602 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=612 mean=879 vara=0.837610402911391 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=247 mean=-852 vara=4.79767475102947 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1146 mean=-248 vara=2.67116265621513 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=148 mean=876 vara=11.7580678058731 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=367 mean=-440 vara=4.1138112818216 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=381 mean=-934 vara=13.0455193407125 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=742 mean=349 vara=0.0830152115577111 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=276 mean=723 vara=1.8012452859782 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=434 mean=-852 vara=19.7976619430298 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=37 mean=-489 vara=6.46639730830274 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=365 mean=801 vara=22.2576052817692 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=109 mean=-596 vara=6.86049506928453 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1002 mean=-472 vara=4.16546487931479 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=435 mean=-705 vara=7.51641969976658 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=640 mean=823 vara=1.40009953926892 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=108 mean=553 vara=7.02263113320172 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=73 mean=212 vara=0.936217729179479 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=437 mean=818 vara=10.8152649789816 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1595 mean=409 vara=4.5402144181013 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=27 mean=790 vara=5.38962359966428 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1748 mean=-510 vara=7.86614099051406 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=577 mean=332 vara=1.25953373127692 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1162 mean=-975 vara=2.37519471252015 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=278 mean=89 vara=0.168599227150639 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=194 mean=148 vara=0.999620703647367 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=83 mean=646 vara=10.1589332944174 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=303 mean=318 vara=6.41970987537237 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1135 mean=360 vara=1.69483310274255 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=701 mean=-94 vara=0.256219124227652 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=994 mean=495 vara=5.70582770249297 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=834 mean=13 vara=0.179986094512825 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=301 mean=639 vara=6.18447518582656 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=646 mean=344 vara=1.82108744893138 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=499 mean=90 vara=1.04861326075801 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1635 mean=-708 vara=4.31199742701272 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1566 mean=-915 vara=4.57706419581269 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=427 mean=728 vara=0.528335056093479 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=967 mean=-589 vara=4.81297822501271 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=901 mean=865 vara=4.94137269763695 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=366 mean=232 vara=0.0153978514199586 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1553 mean=-163 vara=0.473213472989142 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1715 mean=-189 vara=2.60989008392856 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=166 mean=612 vara=9.35422156907328 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=96 mean=-94 vara=0.5670508349399 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1111 mean=-490 vara=5.3957238693776 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=364 mean=-962 vara=4.93989715966768 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=226 mean=760 vara=0.80894624080108 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=27 mean=-137 vara=0.443483712120395 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=307 mean=-284 vara=2.00406086052015 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=29 mean=339 vara=1.02283419617535 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=72 mean=799 vara=3.01843931250076 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=901 mean=299 vara=4.76101632527555 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=864 mean=389 vara=3.28533902466117 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=585 mean=-895 vara=12.2593891958759 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1438 mean=662 vara=0.477562720391066 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=523 mean=161 vara=1.60217990452233 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=505 mean=223 vara=0.0481919283507041 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=422 mean=524 vara=0.358659195771134 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=165 mean=-266 vara=3.93145269146802 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=899 mean=504 vara=8.62545449995505 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=427 mean=-714 vara=19.98776936585 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=108 mean=-896 vara=0.782815534584842 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1301 mean=957 vara=11.469605347258 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=20 mean=-186 vara=1.43789937343015 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=694 mean=-641 vara=7.07358972446601 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=453 mean=-981 vara=14.4670778916958 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=589 mean=-471 vara=0.127067393521659 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=356 mean=-368 vara=0.36655286373037 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=296 mean=74 vara=0.662815938605642 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=29 mean=339 vara=1.02283419617535 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=891 mean=710 vara=2.27158180435415 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1529 mean=-895 vara=8.3481856551784 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=558 mean=722 vara=4.20822529436095 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=614 mean=204 vara=2.01794582212009 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=29 mean=565 vara=7.81897481374437 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=359 mean=564 vara=5.99767083144848 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=87 mean=-879 vara=1.87442212029083 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=492 mean=369 vara=0.462409950059214 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=546 mean=463 vara=1.83547267924888 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=960 mean=982 vara=4.96110154181254 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=897 mean=-408 vara=1.80933521223257 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=361 mean=197 vara=0.132625640751216 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=505 mean=-806 vara=2.68869827007766 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=73 mean=496 vara=3.74738938940071 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=748 mean=86 vara=0.705324813435476 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1230 mean=327 vara=2.73215962427757 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=470 mean=526 vara=6.1949298465275 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=100 mean=-616 vara=3.16339764607809 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=777 mean=197 vara=3.25251885297746 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=427 mean=687 vara=12.2632421785033 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=298 mean=-438 vara=1.11763559854192 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=494 mean=372 vara=2.22157271555947 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=190 mean=-816 vara=5.64466976362524 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=45 mean=520 vara=0.531432229208462 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=48 mean=689 vara=6.56800778790813 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=416 mean=792 vara=1.4171407924665 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=981 mean=766 vara=5.30330305567285 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=262 mean=-503 vara=4.02726501997976 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1429 mean=50 vara=0.239170379527288 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=623 mean=30 vara=0.114051662530169 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=965 mean=-63 vara=0.531051207977354 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=89 mean=539 vara=6.63364376786341 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=611 mean=133 vara=2.56836018780908 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=398 mean=800 vara=3.80378843166444 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=124 mean=-705 vara=2.14764561780242 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=36 mean=-757 vara=1.9975212455834 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=32 mean=563 vara=0.0553263011159952 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1301 mean=957 vara=11.469605347258 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=106 mean=268 vara=5.14912915216793 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=57 mean=-648 vara=3.08596606485193 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=656 mean=-911 vara=12.2790605321422 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=579 mean=-449 vara=2.29578392707748 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=419 mean=79 vara=0.00143765148220853 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=44 mean=-126 vara=1.07732039510563 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=798 mean=-589 vara=6.76841130726867 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=294 mean=168 vara=0.922713325076799 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1082 mean=799 vara=10.4857611694239 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1113 mean=-605 vara=0.821598601506214 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=295 mean=305 vara=1.58617978432558 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=676 mean=-309 vara=0.733579154166738 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=79 mean=524 vara=1.91097000159916 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=436 mean=933 vara=0.543959101836055 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=898 mean=-698 vara=6.39328698883192 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=577 mean=135 vara=2.46730188733432 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=318 mean=374 vara=1.43088857875841 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=244 mean=596 vara=6.31094996851034 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=141 mean=-727 vara=13.6661162794843 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1031 mean=-402 vara=2.68870761878126 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1711 mean=481 vara=2.3342067074589 rep=5 sn=44"
    ##  PASS: TRUE

``` r
meta_file_name = paste0(meta_dir, "index.csv")
write.csv(results, meta_file_name, row.names = FALSE)
```

``` r
print(results)
```

    ## # A tibble: 995 x 12
    ##    index     s     d     p     q     n   rep  mean  vara  seed phi   theta
    ##    <dbl> <dbl> <dbl> <dbl> <dbl> <int> <int> <int> <dbl> <dbl> <chr> <chr>
    ##  1     1     0     0     1     0   340     1   176 0.639    40 0.99  0    
    ##  2     2     0     0     1     0   116     2   308 2.21     41 0.99  0    
    ##  3     3     0     0     1     0  1090     3   140 1.64     42 0.99  0    
    ##  4     4     0     0     1     0    78     4    48 0.213    43 0.99  0    
    ##  5     5     0     0     1     0    60     5    32 0.103    44 0.99  0    
    ##  6     6     0     0     1     0   540     1   612 3.54     40 0.98  0    
    ##  7     7     0     0     1     0   510     2  -521 5.28     41 0.98  0    
    ##  8     8     0     0     1     0    48     3   233 2.96     42 0.98  0    
    ##  9     9     0     0     1     0   499     4  -461 1.28     43 0.98  0    
    ## 10    10     0     0     1     0   791     5   946 0.969    44 0.98  0    
    ## # ... with 985 more rows

``` r
print(paste0("Total: ", n_total, " Error: ", n_error, " Pass: ", n_pass))
```

    ## [1] "Total: 995 Error: 0 Pass: 995"

``` r
results %>% 
  group_by(s, d) %>% 
  mutate(n=n(), mean_p= mean(p), sd_p = sd(p), mean_q= mean(q), sd_q = sd(q))
```

    ## # A tibble: 995 x 16
    ## # Groups:   s, d [1]
    ##    index     s     d     p     q     n   rep  mean  vara  seed phi   theta
    ##    <dbl> <dbl> <dbl> <dbl> <dbl> <int> <int> <int> <dbl> <dbl> <chr> <chr>
    ##  1     1     0     0     1     0   995     1   176 0.639    40 0.99  0    
    ##  2     2     0     0     1     0   995     2   308 2.21     41 0.99  0    
    ##  3     3     0     0     1     0   995     3   140 1.64     42 0.99  0    
    ##  4     4     0     0     1     0   995     4    48 0.213    43 0.99  0    
    ##  5     5     0     0     1     0   995     5    32 0.103    44 0.99  0    
    ##  6     6     0     0     1     0   995     1   612 3.54     40 0.98  0    
    ##  7     7     0     0     1     0   995     2  -521 5.28     41 0.98  0    
    ##  8     8     0     0     1     0   995     3   233 2.96     42 0.98  0    
    ##  9     9     0     0     1     0   995     4  -461 1.28     43 0.98  0    
    ## 10    10     0     0     1     0   995     5   946 0.969    44 0.98  0    
    ## # ... with 985 more rows, and 4 more variables: mean_p <dbl>, sd_p <dbl>,
    ## #   mean_q <dbl>, sd_q <dbl>
