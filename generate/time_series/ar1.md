Generate AR(1) Data (positive phi)
================
Nikhil Gupta
2021-11-11 20:25:40

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
    ## [496] TRUE TRUE TRUE TRUE TRUE

``` r
seed=42
set.seed(seed)

n_total = 0
n_pass = 0
n_error = 0
nreps = 5


results = tribble(~index, ~s, ~d, ~p, ~q, ~n, ~rep, ~mean, ~vara, ~seed, ~phi, ~theta)

for (phi in seq(0.01, 0.99, 0.01)){
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
    ## [1] "s=0 d=0 p=1 q=0 n=110 mean=-497 vara=6.06809397124571 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=404 mean=-490 vara=1.04359709715711 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=477 mean=135 vara=0.835191309141361 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=42 mean=302 vara=0.96970888502653 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1563 mean=-7 vara=0.0646521229958962 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=947 mean=989 vara=5.19569608498021 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=403 mean=-457 vara=0.958998669567544 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=497 mean=-310 vara=3.02112165210755 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=915 mean=15 vara=0.0289906593273899 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=31 mean=367 vara=2.78428126578781 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=61 mean=200 vara=1.53646487654375 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=475 mean=-609 vara=1.96124354184564 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=739 mean=292 vara=2.89603143054897 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=763 mean=-288 vara=3.34705524198896 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=677 mean=757 vara=9.8310355517007 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=937 mean=-198 vara=1.75960094371445 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=163 mean=-935 vara=2.72293591735774 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1205 mean=-914 vara=24.6773584502722 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1533 mean=406 vara=2.63025565358954 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1152 mean=988 vara=12.8247497720382 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=359 mean=510 vara=1.38827727399911 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=474 mean=-41 vara=0.643193486514891 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=739 mean=292 vara=2.89603143054897 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=407 mean=-590 vara=3.22688206134392 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=120 mean=363 vara=5.05483169204452 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1189 mean=90 vara=0.647919150639522 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=844 mean=192 vara=0.783897528363212 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1497 mean=821 vara=10.7176487889782 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=657 mean=805 vara=3.65785030307804 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1013 mean=-104 vara=0.57942628445019 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=688 mean=-440 vara=0.197103777954924 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=177 mean=299 vara=2.38026801830727 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=515 mean=-783 vara=3.04223418805692 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=52 mean=-147 vara=1.19201741738136 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=700 mean=98 vara=0.0747814186961917 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=625 mean=-16 vara=0.164599298991823 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=116 mean=215 vara=0.796086499150259 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1255 mean=-254 vara=1.32536207712492 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=395 mean=-667 vara=4.48457789847216 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=597 mean=468 vara=1.69140875668398 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=387 mean=-835 vara=3.48953922363701 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=161 mean=942 vara=19.9925417989992 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=701 mean=-299 vara=1.12611691264969 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=33 mean=-850 vara=8.70785712904129 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=74 mean=-510 vara=0.862845363071849 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=302 mean=-373 vara=6.60598559271579 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1057 mean=-758 vara=8.23853284184164 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=371 mean=97 vara=1.21619779330366 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1748 mean=369 vara=0.873297911653597 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=152 mean=110 vara=0.963237227619593 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=641 mean=531 vara=0.471884810024339 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=486 mean=-446 vara=8.89738921414643 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=915 mean=909 vara=6.44099352995156 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=164 mean=-139 vara=1.18012059959024 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=312 mean=428 vara=0.446748100301131 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1092 mean=-766 vara=1.0422668831988 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1124 mean=-598 vara=8.87285348388903 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=303 mean=532 vara=8.0253138308209 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1017 mean=-245 vara=1.1976034115676 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=26 mean=-405 vara=1.72559821014291 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=101 mean=570 vara=0.968179983490419 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=32 mean=679 vara=7.46865837133447 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=212 mean=-331 vara=1.39499693793911 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=902 mean=-49 vara=0.00858453835818178 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=147 mean=-278 vara=0.40858113831679 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=406 mean=-952 vara=6.62860693200605 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=86 mean=-735 vara=12.5395071559806 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=410 mean=-603 vara=7.80283486687186 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=40 mean=604 vara=1.8519491458061 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1563 mean=-7 vara=0.0646521229958962 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=52 mean=-804 vara=8.67370534468603 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=48 mean=346 vara=2.80946386434856 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=508 mean=685 vara=8.48202844670557 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1287 mean=123 vara=1.19399147313502 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=954 mean=-39 vara=0.085462701605363 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=244 mean=783 vara=1.79709566329941 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=468 mean=-439 vara=4.46362808037194 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=62 mean=-552 vara=6.80714615543908 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=48 mean=-39 vara=0.121264612605024 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=255 mean=922 vara=2.24407395532036 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=314 mean=-945 vara=3.48539766400715 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=101 mean=-275 vara=2.37466320087484 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1391 mean=824 vara=5.32092827869413 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=754 mean=-312 vara=0.568046695697326 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=996 mean=732 vara=3.00987365080055 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=598 mean=-52 vara=0.243549936617132 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=52 mean=-618 vara=8.04078567730319 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1042 mean=849 vara=10.9323628350132 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=57 mean=-222 vara=1.5067988881209 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=694 mean=-641 vara=7.07358972446601 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1066 mean=-914 vara=0.756268726240419 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1323 mean=348 vara=9.36588410754953 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1013 mean=-112 vara=0.916935528706829 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=68 mean=65 vara=0.381739232354272 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=490 mean=495 vara=2.0937114883919 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1527 mean=-407 vara=9.42564117791792 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1471 mean=12 vara=0.150914341387542 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=82 mean=-370 vara=0.077342521124061 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=696 mean=-292 vara=0.612001749217049 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=128 mean=-83 vara=0.997195902719308 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=33 mean=219 vara=1.09596674232849 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=145 mean=408 vara=4.24721296469922 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1301 mean=957 vara=11.469605347258 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=550 mean=-444 vara=3.09038613271074 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1127 mean=-295 vara=0.643881162184529 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=26 mean=516 vara=0.292138355095675 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=45 mean=404 vara=5.03357444980766 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=483 mean=909 vara=4.06150536738462 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=226 mean=682 vara=5.01637420485377 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=102 mean=-943 vara=9.68574764488744 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=503 mean=269 vara=5.39519965097264 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=131 mean=582 vara=8.45150085932395 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=709 mean=-822 vara=10.8242817394622 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=352 mean=280 vara=6.39789042479039 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=26 mean=-902 vara=18.4046463661254 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=260 mean=587 vara=2.80420230370444 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=39 mean=-356 vara=1.8081799028381 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=37 mean=-820 vara=2.36630007579241 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=236 mean=-920 vara=17.2436760234189 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=62 mean=-397 vara=4.07766131866314 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=432 mean=-283 vara=0.0133405267001868 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1709 mean=780 vara=2.74932177264299 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=22 mean=-544 vara=4.25499739696388 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=110 mean=543 vara=6.64688098265708 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=281 mean=751 vara=10.2534283081122 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=222 mean=67 vara=0.419460157508848 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=38 mean=768 vara=8.4640350459809 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=22 mean=-151 vara=0.726299190897513 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=110 mean=543 vara=6.64688098265708 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=281 mean=751 vara=10.2534283081122 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=222 mean=67 vara=0.419460157508848 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=38 mean=768 vara=8.4640350459809 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=22 mean=-151 vara=0.726299190897513 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=110 mean=543 vara=6.64688098265708 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=281 mean=751 vara=10.2534283081122 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=222 mean=67 vara=0.419460157508848 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=38 mean=768 vara=8.4640350459809 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=22 mean=-151 vara=0.726299190897513 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=110 mean=543 vara=6.64688098265708 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=281 mean=751 vara=10.2534283081122 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=222 mean=67 vara=0.419460157508848 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=38 mean=768 vara=8.4640350459809 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=22 mean=-151 vara=0.726299190897513 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=110 mean=543 vara=6.64688098265708 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=281 mean=751 vara=10.2534283081122 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=222 mean=67 vara=0.419460157508848 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=38 mean=768 vara=8.4640350459809 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=22 mean=-151 vara=0.726299190897513 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=110 mean=543 vara=6.64688098265708 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=281 mean=751 vara=10.2534283081122 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1087 mean=23 vara=0.341326646310979 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=57 mean=-604 vara=1.54481363286198 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=158 mean=-845 vara=8.38825171887521 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=777 mean=197 vara=3.25251885297746 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=82 mean=434 vara=0.578068052510453 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=729 mean=853 vara=6.49798245996987 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=492 mean=-977 vara=11.7740760081059 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=118 mean=761 vara=1.29932944783655 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=240 mean=30 vara=0.133083641168899 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=184 mean=-445 vara=0.443678793376071 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=167 mean=-257 vara=0.943881127472068 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1877 mean=304 vara=1.01436657768628 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=105 mean=667 vara=1.31690635397462 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=524 mean=-438 vara=7.27470919763708 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=164 mean=-683 vara=9.22814402156126 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=808 mean=-680 vara=6.93818102443078 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=151 mean=-607 vara=3.8436183633059 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=68 mean=-541 vara=1.07091605686968 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=213 mean=-438 vara=0.593608698589425 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=958 mean=-393 vara=6.17706576390777 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=60 mean=-615 vara=1.85916961803778 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=630 mean=996 vara=4.22952377734339 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=495 mean=426 vara=1.87085706175328 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=446 mean=-545 vara=0.244379976197447 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=266 mean=-940 vara=6.43293962181076 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=989 mean=582 vara=3.484216259767 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=840 mean=228 vara=2.70857184322363 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=49 mean=908 vara=9.06992117026612 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=331 mean=-573 vara=2.02224874156245 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=274 mean=367 vara=5.25753853395004 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=21 mean=-688 vara=10.2957171064352 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=678 mean=510 vara=4.45177914547158 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=723 mean=-100 vara=0.44967731780321 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=241 mean=-440 vara=2.37967706782994 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=108 mean=-636 vara=13.2374716112709 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1205 mean=-855 vara=2.90097316894422 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=24 mean=924 vara=6.37556200125675 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=50 mean=671 vara=5.52715453719188 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=48 mean=-707 vara=12.654482744386 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=700 mean=98 vara=0.0747814186961917 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1332 mean=-362 vara=6.27447671532724 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=308 mean=-963 vara=21.9050190374578 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=176 mean=441 vara=1.18352319960158 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=949 mean=625 vara=7.35992133434524 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=280 mean=-147 vara=1.1409046068965 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1087 mean=23 vara=0.341326646310979 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=416 mean=-530 vara=4.99257356462121 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=769 mean=-422 vara=3.46757412335428 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=64 mean=28 vara=0.0367102789459044 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1736 mean=908 vara=11.0509813628392 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1084 mean=61 vara=0.515719315376115 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=735 mean=745 vara=1.2891188204862 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=291 mean=300 vara=1.98390326414624 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=383 mean=770 vara=1.19090374455743 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=109 mean=-448 vara=3.61897006939109 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=165 mean=536 vara=10.3982104108447 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=72 mean=-519 vara=0.778153812929326 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=166 mean=-269 vara=0.992090110775351 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=329 mean=30 vara=0.627740628103385 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=51 mean=922 vara=13.0765586351139 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=365 mean=801 vara=22.2576052817692 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=460 mean=-530 vara=11.210299795541 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=84 mean=-569 vara=0.895267172340772 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=295 mean=229 vara=1.34031867698028 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=194 mean=148 vara=0.999620703647367 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1381 mean=956 vara=1.38472317432384 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=833 mean=924 vara=15.3874642039322 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=111 mean=743 vara=9.19681377392848 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=282 mean=847 vara=9.90031948561526 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1197 mean=309 vara=4.42085234596691 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=121 mean=-401 vara=6.01364008935356 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=184 mean=-682 vara=1.13821739916744 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=81 mean=-131 vara=0.743564043448927 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=22 mean=-85 vara=0.0915291632200151 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=962 mean=301 vara=2.07918372317722 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=588 mean=-488 vara=2.06840450523464 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=722 mean=-564 vara=1.84051819671272 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=186 mean=453 vara=1.41687489305347 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1645 mean=-606 vara=5.67106845934008 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1727 mean=-721 vara=0.612247968604707 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=690 mean=272 vara=2.57043312251702 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=158 mean=710 vara=0.282739721486006 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=106 mean=-773 vara=0.603867009091744 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=117 mean=-224 vara=0.762586934433642 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=631 mean=-587 vara=3.38006719605151 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=75 mean=519 vara=0.92368629645611 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=922 mean=-253 vara=2.0266900806372 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=119 mean=811 vara=5.58262852226856 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=540 mean=291 vara=5.91499376059333 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1378 mean=-643 vara=5.32945398196913 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=671 mean=-871 vara=2.99730947265346 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=491 mean=-342 vara=2.15313902015598 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1731 mean=693 vara=9.61617801920374 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=99 mean=-816 vara=4.56690535840078 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=255 mean=691 vara=6.07098413343786 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=862 mean=-366 vara=3.05047682521727 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=56 mean=825 vara=3.62017298422107 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=74 mean=249 vara=0.122557617556256 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=726 mean=-175 vara=2.42744929327337 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=79 mean=-703 vara=0.727635284502238 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=729 mean=853 vara=6.49798245996987 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=799 mean=398 vara=5.95301317650234 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=835 mean=-219 vara=0.931085050499572 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=239 mean=517 vara=0.512378309490723 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=108 mean=-636 vara=13.2374716112709 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=280 mean=527 vara=7.35426401180349 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=630 mean=23 vara=0.0533368691014334 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1152 mean=810 vara=1.2856495122033 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=701 mean=-94 vara=0.256219124227652 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=625 mean=-837 vara=10.8333742256078 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=153 mean=724 vara=13.4844927558095 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=214 mean=749 vara=12.9194481775108 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1160 mean=981 vara=5.06960336836744 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=146 mean=407 vara=1.85354387307797 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=700 mean=-789 vara=9.97077484976419 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=971 mean=-397 vara=6.12645374368717 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=94 mean=890 vara=12.3434590043877 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1422 mean=-795 vara=8.37974346751367 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=532 mean=426 vara=3.04980340639805 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=39 mean=834 vara=5.11139802362321 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=758 mean=221 vara=0.0682777299699745 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=419 mean=555 vara=2.09135327259398 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=63 mean=323 vara=1.65197225208655 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=213 mean=-438 vara=0.593608698589425 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=372 mean=-709 vara=0.806988918633747 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=332 mean=-669 vara=4.11820540949367 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=941 mean=-30 vara=0.0389723619551006 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=72 mean=-692 vara=3.86352160169558 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=726 mean=-175 vara=2.42744929327337 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=390 mean=-808 vara=4.87049661391129 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=200 mean=-871 vara=8.65731486699073 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=101 mean=-56 vara=0.316173356041658 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=50 mean=-73 vara=0.0273130858661147 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1425 mean=264 vara=0.580388296020801 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=942 mean=388 vara=1.43069219135614 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1167 mean=-896 vara=4.33122541124231 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=123 mean=578 vara=3.7344078700006 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=952 mean=452 vara=0.412396066146862 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=149 mean=-119 vara=0.415908456246063 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=405 mean=-518 vara=7.03047818869948 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1647 mean=-875 vara=0.496477716645837 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=162 mean=-239 vara=2.05033236768116 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=786 mean=619 vara=6.75663012249327 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=157 mean=-691 vara=2.84894673677139 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=218 mean=292 vara=0.830711107926171 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=612 mean=879 vara=0.837610402911391 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=753 mean=-579 vara=2.66511164211662 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=386 mean=647 vara=3.94597646897033 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=396 mean=366 vara=0.163076661198472 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=406 mean=-625 vara=13.0716685530923 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=145 mean=482 vara=2.20999388576495 rep=2 sn=41"
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
    ## [1] "s=0 d=0 p=1 q=0 n=54 mean=-440 vara=10.5827840815648 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=557 mean=111 vara=1.4803717547008 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1144 mean=174 vara=0.831818061201464 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=628 mean=262 vara=6.73301625244243 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=105 mean=-962 vara=5.91020257506038 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=108 mean=-785 vara=3.41494916223575 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=392 mean=340 vara=3.60649221032861 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=477 mean=135 vara=0.835191309141361 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=823 mean=-275 vara=3.00476850627133 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=110 mean=3 vara=0.0208518720933511 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=253 mean=-419 vara=1.45965797547006 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=368 mean=-115 vara=0.589574237237984 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=37 mean=851 vara=7.70372239915098 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=210 mean=491 vara=0.673341629378514 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=122 mean=834 vara=0.317963508549561 rep=1 sn=40"
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
    ## [1] "s=0 d=0 p=1 q=0 n=59 mean=292 vara=4.39179851082107 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=237 mean=-189 vara=1.06873895851261 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=634 mean=-510 vara=5.91920237542952 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=683 mean=189 vara=2.27801487482647 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=68 mean=20 vara=0.0248295697171942 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=687 mean=-733 vara=1.82202006344133 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1211 mean=-87 vara=0.707914725555485 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=70 mean=250 vara=2.40891646820124 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=728 mean=343 vara=7.96300641211402 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=445 mean=15 vara=0.142039911347255 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=585 mean=-3 vara=0.0319960938557427 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=52 mean=-618 vara=8.04078567730319 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1156 mean=50 vara=0.872527930668347 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1259 mean=-260 vara=3.92605603685306 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=751 mean=-384 vara=2.87823981319658 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=494 mean=-659 vara=10.8248095034107 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=199 mean=389 vara=2.78458588569949 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=246 mean=-447 vara=2.53434565985207 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=616 mean=254 vara=0.550760522831908 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=125 mean=215 vara=0.408015685810525 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=419 mean=285 vara=1.03379081622567 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=735 mean=-657 vara=4.75905945541209 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=151 mean=924 vara=10.1759846432149 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=463 mean=437 vara=1.42325599404111 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=635 mean=350 vara=3.73522446414651 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=532 mean=919 vara=14.4330859132176 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=254 mean=560 vara=8.31870765225357 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=585 mean=-511 vara=0.684072206954499 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=614 mean=204 vara=2.01794582212009 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=125 mean=215 vara=0.408015685810525 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=83 mean=575 vara=12.274063796209 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=143 mean=967 vara=7.01224762265534 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=856 mean=-319 vara=2.59409216934302 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=339 mean=-958 vara=2.02056856800862 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=925 mean=898 vara=11.2626799884988 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=541 mean=371 vara=3.47587867277395 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=48 mean=951 vara=10.8267444208063 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1156 mean=50 vara=0.872527930668347 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=625 mean=236 vara=5.51179895364785 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=488 mean=551 vara=2.87531164229479 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1303 mean=328 vara=1.07026033361187 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=33 mean=-59 vara=0.26465921065565 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1042 mean=849 vara=10.9323628350132 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=385 mean=-916 vara=19.8329793557183 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1076 mean=410 vara=0.0588215823847695 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1207 mean=402 vara=1.7048102715428 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=140 mean=771 vara=7.91796896605689 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=856 mean=-319 vara=2.59409216934302 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=111 mean=-227 vara=2.07830519077955 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=472 mean=835 vara=5.25619209932189 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=350 mean=-862 vara=7.60579563686915 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=45 mean=-498 vara=1.38188947781299 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=105 mean=939 vara=8.12026521828945 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=34 mean=-890 vara=8.82636003921075 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=616 mean=268 vara=0.0295605539344555 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=741 mean=-72 vara=0.24551693866 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1075 mean=-880 vara=1.8215155633999 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1521 mean=-253 vara=2.21768793436227 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=36 mean=208 vara=1.30540597401596 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=41 mean=686 vara=3.40446551936723 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=356 mean=-638 vara=3.38168247921786 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=95 mean=841 vara=7.50273383739701 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=218 mean=-374 vara=5.75467054810526 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=864 mean=470 vara=5.73663848602248 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=806 mean=688 vara=1.00680158101305 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=287 mean=747 vara=13.9935974054784 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=208 mean=315 vara=4.84620297694909 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=576 mean=167 vara=0.53306687630602 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=614 mean=204 vara=2.01794582212009 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=790 mean=-245 vara=0.073576706259394 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=238 mean=-137 vara=1.38252553778233 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=51 mean=58 vara=0.0946226646531014 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=233 mean=-16 vara=0.0895155852723735 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=616 mean=254 vara=0.550760522831908 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1571 mean=287 vara=1.12287697529242 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1125 mean=-210 vara=0.426172156210701 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=215 mean=787 vara=10.1566886211135 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1037 mean=-670 vara=5.57810393262022 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=503 mean=-983 vara=1.46653044510794 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=89 mean=-634 vara=1.12768931884762 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1136 mean=-761 vara=15.3149169636308 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=152 mean=219 vara=1.11791326225955 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=943 mean=-474 vara=3.25920902749377 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=320 mean=127 vara=2.20070431582677 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=566 mean=-187 vara=1.40378765058079 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1005 mean=281 vara=5.01944604722505 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=215 mean=565 vara=6.10862041133789 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=87 mean=-173 vara=0.3133945876942 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=240 mean=30 vara=0.133083641168899 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=163 mean=930 vara=6.99185753774169 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=721 mean=-147 vara=0.506219298466831 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=757 mean=-679 vara=4.13271839631564 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=66 mean=-861 vara=8.10996751121926 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=47 mean=-146 vara=0.827465112772639 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=53 mean=-611 vara=1.50398573264003 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=107 mean=-550 vara=1.83657389926905 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=617 mean=589 vara=5.96561236379912 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=969 mean=-23 vara=0.144459374217844 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=94 mean=-906 vara=0.733027750649977 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=179 mean=-745 vara=2.10774025415062 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=576 mean=757 vara=7.4626314138691 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=753 mean=-579 vara=2.66511164211662 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=490 mean=-643 vara=2.79017595257018 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=418 mean=406 vara=4.99946263301667 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=537 mean=-440 vara=1.54031219466237 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=408 mean=-994 vara=9.57087518171652 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1288 mean=849 vara=11.8402490428197 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=414 mean=669 vara=0.225921764304781 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=603 mean=-466 vara=1.72222766834764 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=273 mean=-263 vara=1.03173094357845 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=322 mean=-225 vara=2.28870892183069 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=864 mean=-389 vara=2.03853057949113 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=116 mean=-618 vara=3.03399828431631 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=329 mean=30 vara=0.627740628103385 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=181 mean=733 vara=3.75114058286084 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=606 mean=-152 vara=0.0150990718459598 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=135 mean=-565 vara=4.07762473285225 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=119 mean=733 vara=3.76286585543932 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=949 mean=625 vara=7.35992133434524 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=210 mean=778 vara=1.19363145785374 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=35 mean=238 vara=4.28749975649883 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=901 mean=299 vara=4.76101632527555 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=120 mean=-566 vara=2.27853174180527 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=964 mean=-487 vara=2.49199850193982 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1157 mean=-381 vara=0.496792275939682 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=768 mean=-963 vara=0.685394316660038 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=333 mean=-43 vara=0.127180920488846 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=883 mean=-309 vara=0.118174258525632 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=843 mean=-615 vara=1.87233480612171 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=701 mean=703 vara=7.81924189450463 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=158 mean=-501 vara=0.229103688178988 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=113 mean=174 vara=1.60016817825121 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1021 mean=670 vara=3.46642348907189 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1098 mean=-79 vara=0.594165118380884 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=791 mean=249 vara=4.38356805884102 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=268 mean=-129 vara=0.448896031978618 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1112 mean=928 vara=3.6800567449565 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=70 mean=250 vara=2.40891646820124 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=314 mean=-387 vara=1.64084293974067 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=137 mean=-240 vara=0.0516326222043824 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1293 mean=484 vara=2.41762569919755 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=978 mean=-997 vara=8.05474377807829 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1444 mean=-848 vara=14.2467969661896 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=1773 mean=-5 vara=0.156418296448494 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=492 mean=905 vara=7.52738517934507 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=994 mean=124 vara=1.23127357575891 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=182 mean=-105 vara=0.685478092311576 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=478 mean=777 vara=6.99268125359577 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=60 mean=32 vara=0.10251822040595 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=549 mean=-421 vara=0.0916659734387554 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=38 mean=635 vara=6.20125824754087 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=61 mean=-319 vara=0.480509708640805 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=49 mean=607 vara=6.50830563578567 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=135 mean=631 vara=12.8119087272814 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=386 mean=110 vara=0.727517227750205 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=651 mean=-516 vara=1.55332703591612 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=204 mean=-129 vara=2.20560325463254 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=377 mean=289 vara=3.65607645690905 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=1 q=0 n=211 mean=-974 vara=3.30435916538979 rep=5 sn=44"
    ##  PASS: TRUE

``` r
meta_file_name = paste0(meta_dir, "index.csv")
write.csv(results, meta_file_name, row.names = FALSE)
```

``` r
print(results)
```

    ## # A tibble: 495 x 12
    ##    index     s     d     p     q     n   rep  mean   vara  seed phi   theta
    ##    <dbl> <dbl> <dbl> <dbl> <dbl> <int> <int> <int>  <dbl> <dbl> <chr> <chr>
    ##  1     1     0     0     1     0   340     1   176 0.639     40 0.01  0    
    ##  2     2     0     0     1     0   110     2  -497 6.07      41 0.01  0    
    ##  3     3     0     0     1     0   404     3  -490 1.04      42 0.01  0    
    ##  4     4     0     0     1     0   477     4   135 0.835     43 0.01  0    
    ##  5     5     0     0     1     0    42     5   302 0.970     44 0.01  0    
    ##  6     6     0     0     1     0  1563     1    -7 0.0647    40 0.02  0    
    ##  7     7     0     0     1     0   947     2   989 5.20      41 0.02  0    
    ##  8     8     0     0     1     0   403     3  -457 0.959     42 0.02  0    
    ##  9     9     0     0     1     0   497     4  -310 3.02      43 0.02  0    
    ## 10    10     0     0     1     0   915     5    15 0.0290    44 0.02  0    
    ## # ... with 485 more rows

``` r
print(paste0("Total: ", n_total, " Error: ", n_error, " Pass: ", n_pass))
```

    ## [1] "Total: 495 Error: 0 Pass: 495"

``` r
results %>% 
  group_by(s, d) %>% 
  mutate(n=n(), mean_p= mean(p), sd_p = sd(p), mean_q= mean(q), sd_q = sd(q))
```

    ## # A tibble: 495 x 16
    ## # Groups:   s, d [1]
    ##    index     s     d     p     q     n   rep  mean   vara  seed phi   theta
    ##    <dbl> <dbl> <dbl> <dbl> <dbl> <int> <int> <int>  <dbl> <dbl> <chr> <chr>
    ##  1     1     0     0     1     0   495     1   176 0.639     40 0.01  0    
    ##  2     2     0     0     1     0   495     2  -497 6.07      41 0.01  0    
    ##  3     3     0     0     1     0   495     3  -490 1.04      42 0.01  0    
    ##  4     4     0     0     1     0   495     4   135 0.835     43 0.01  0    
    ##  5     5     0     0     1     0   495     5   302 0.970     44 0.01  0    
    ##  6     6     0     0     1     0   495     1    -7 0.0647    40 0.02  0    
    ##  7     7     0     0     1     0   495     2   989 5.20      41 0.02  0    
    ##  8     8     0     0     1     0   495     3  -457 0.959     42 0.02  0    
    ##  9     9     0     0     1     0   495     4  -310 3.02      43 0.02  0    
    ## 10    10     0     0     1     0   495     5    15 0.0290    44 0.02  0    
    ## # ... with 485 more rows, and 4 more variables: mean_p <dbl>, sd_p <dbl>,
    ## #   mean_q <dbl>, sd_q <dbl>
