Generate MA(1) Data
================
Nikhil Gupta
2022-03-03 03:46:54

``` r
if (!require("pacman")) install.packages("pacman")
```

    ## Loading required package: pacman

``` r
pacman::p_load(dplyr, tswge)
```

``` r
data_dir = "../../data/time_series/ma1/"
meta_dir = "../../meta/time_series/ma1/"
```

``` r
# Remove current files
do.call(file.remove, list(list.files(data_dir, full.names = TRUE)))
```

    ##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [16] TRUE TRUE TRUE TRUE TRUE

``` r
seed=42
set.seed(seed)

n_total = 0
n_pass = 0
n_error = 0
nreps = 5


results = tribble(~index, ~s, ~d, ~p, ~q, ~n, ~rep, ~mean, ~vara, ~seed, ~phi, ~theta)

for (theta in seq(0.99, -0.99, -0.01)){
  for (rep in 1:nreps){
    s = 0
    d = 0
    p = 0
    q = 1
    
    
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
    
      phi = 0
      
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

    ## [1] "s=0 d=0 p=0 q=1 n=340 mean=176 vara=0.639106003953717 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1222 mean=-865 vara=0.776356740307132 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1647 mean=-355 vara=1.30032508402021 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=220 mean=-267 vara=1.51074772933624 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=183 mean=447 vara=6.95486761085075 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=788 mean=-14 vara=0.00715135304429341 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=886 mean=505 vara=4.27967575979677 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1469 mean=-8 vara=0.0331958760993753 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=20 mean=915 vara=8.11792367562298 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=747 mean=-297 vara=3.17984101277415 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=445 mean=15 vara=0.142039911347255 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1926 mean=101 vara=0.591909711734671 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1085 mean=491 vara=6.03370433636706 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=87 mean=255 vara=1.81559591828261 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=728 mean=343 vara=7.96300641211402 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=185 mean=-868 vara=6.37599760404004 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=325 mean=-703 vara=2.52986834259701 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=607 mean=335 vara=0.763549500988381 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1501 mean=154 vara=1.5413265884163 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=239 mean=-570 vara=4.05767769824911 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=29 mean=120 vara=0.317218036610581 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=101 mean=570 vara=0.968179983490419 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=231 mean=-216 vara=0.791295013919077 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1076 mean=930 vara=1.79546860944683 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=86 mean=-725 vara=2.16376283823421 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=202 mean=527 vara=11.5270904047253 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1381 mean=956 vara=1.38472317432384 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-869 vara=9.25167905383208 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=505 mean=-806 vara=2.68869827007766 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=640 mean=-792 vara=16.8405579785213 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1315 mean=-492 vara=5.19841795377399 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=996 mean=-294 vara=5.07799799584828 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=261 mean=-180 vara=0.0564363261853465 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=606 mean=-860 vara=1.31887785969014 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=349 mean=-693 vara=1.67251081063692 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1113 mean=-362 vara=3.21712407600704 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=156 vara=3.30830725217917 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=556 mean=418 vara=1.36629244588207 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=189 mean=-684 vara=1.40158433288586 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=33 mean=6 vara=0.102877969258539 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1074 mean=789 vara=7.37488883100931 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=752 mean=-365 vara=5.41665346656011 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=420 mean=689 vara=13.2936651284455 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=-317 vara=0.710844728423296 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=583 mean=-874 vara=7.35615788922531 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=109 mean=78 vara=1.03244775949764 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1115 mean=-241 vara=2.0420563820466 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=41 mean=588 vara=7.62557366168144 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=218 mean=278 vara=5.7869759402631 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=-26 vara=0.000813272743767773 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=255 mean=691 vara=6.07098413343786 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=544 mean=121 vara=0.389620763166173 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=755 mean=-563 vara=5.69096106955755 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=81 vara=1.05740329360251 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-146 vara=0.827465112772639 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=153 mean=-769 vara=2.69389171594587 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=470 mean=-872 vara=3.0839705056204 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=283 mean=-17 vara=0.0909842864601781 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-516 vara=6.68266237624935 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1045 mean=939 vara=0.19743254567756 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=766 mean=364 vara=1.33662069802591 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=327 mean=274 vara=0.408964135789027 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=279 mean=752 vara=12.5312320329457 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=30 mean=-412 vara=2.00207737486077 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=366 mean=420 vara=2.83778341108809 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=47 vara=0.156104501408812 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=158 mean=-321 vara=5.72773049449014 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=897 mean=-825 vara=0.762999744590276 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=302 mean=-63 vara=1.46478685104053 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=516 vara=9.00581573545185 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=533 mean=-626 vara=2.82123267687893 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=817 mean=809 vara=8.13478411305941 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=172 mean=-150 vara=1.72389687059901 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=421 mean=-863 vara=4.04030098102214 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=497 mean=-687 vara=11.8757063465601 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=637 mean=250 vara=1.50039746354386 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-921 vara=5.45686253626927 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=91 mean=-302 vara=3.04259103585876 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=31 mean=716 vara=6.8676603817634 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=44 mean=-156 vara=2.51284265395598 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1563 mean=-7 vara=0.0646521229958962 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=235 mean=603 vara=5.62538066728811 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=952 mean=-362 vara=1.407111962114 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=72 mean=-692 vara=3.86352160169558 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=132 mean=499 vara=10.390196543329 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=503 mean=-581 vara=6.13916687682135 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=616 mean=-164 vara=0.121958378307457 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1064 mean=354 vara=11.5364474734587 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=262 mean=-503 vara=4.02726501997976 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=45 mean=520 vara=0.531432229208462 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=140 mean=-224 vara=1.84798174243302 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=219 mean=908 vara=9.31822440144078 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=636 mean=-16 vara=0.248794729226958 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=495 mean=426 vara=1.87085706175328 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=340 mean=891 vara=10.893196348975 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=29 mean=339 vara=1.02283419617535 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=101 mean=570 vara=0.968179983490419 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=231 mean=-216 vara=0.791295013919077 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1076 mean=930 vara=1.79546860944683 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=86 mean=-725 vara=2.16376283823421 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=202 mean=527 vara=11.5270904047253 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1381 mean=956 vara=1.38472317432384 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-869 vara=9.25167905383208 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=505 mean=-806 vara=2.68869827007766 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=640 mean=-792 vara=16.8405579785213 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1315 mean=-492 vara=5.19841795377399 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=996 mean=-294 vara=5.07799799584828 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=261 mean=-180 vara=0.0564363261853465 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=606 mean=-860 vara=1.31887785969014 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=349 mean=-693 vara=1.67251081063692 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1113 mean=-362 vara=3.21712407600704 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=156 vara=3.30830725217917 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=556 mean=418 vara=1.36629244588207 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=189 mean=-684 vara=1.40158433288586 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=33 mean=6 vara=0.102877969258539 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1074 mean=789 vara=7.37488883100931 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=752 mean=-365 vara=5.41665346656011 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=420 mean=689 vara=13.2936651284455 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=-317 vara=0.710844728423296 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=583 mean=-874 vara=7.35615788922531 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=109 mean=78 vara=1.03244775949764 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1115 mean=-241 vara=2.0420563820466 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=41 mean=588 vara=7.62557366168144 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=218 mean=278 vara=5.7869759402631 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=-26 vara=0.000813272743767773 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=255 mean=691 vara=6.07098413343786 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=544 mean=121 vara=0.389620763166173 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=755 mean=-563 vara=5.69096106955755 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=81 vara=1.05740329360251 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-146 vara=0.827465112772639 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=153 mean=-769 vara=2.69389171594587 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=470 mean=-872 vara=3.0839705056204 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=283 mean=-17 vara=0.0909842864601781 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-516 vara=6.68266237624935 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1045 mean=939 vara=0.19743254567756 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=766 mean=364 vara=1.33662069802591 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=327 mean=274 vara=0.408964135789027 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=279 mean=752 vara=12.5312320329457 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=30 mean=-412 vara=2.00207737486077 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=366 mean=420 vara=2.83778341108809 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=47 vara=0.156104501408812 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=158 mean=-321 vara=5.72773049449014 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=897 mean=-825 vara=0.762999744590276 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=302 mean=-63 vara=1.46478685104053 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=516 vara=9.00581573545185 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=533 mean=-626 vara=2.82123267687893 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=817 mean=809 vara=8.13478411305941 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=172 mean=-150 vara=1.72389687059901 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=421 mean=-863 vara=4.04030098102214 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=497 mean=-687 vara=11.8757063465601 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=637 mean=250 vara=1.50039746354386 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-921 vara=5.45686253626927 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=91 mean=-302 vara=3.04259103585876 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=31 mean=716 vara=6.8676603817634 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=44 mean=-156 vara=2.51284265395598 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1563 mean=-7 vara=0.0646521229958962 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=235 mean=603 vara=5.62538066728811 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=952 mean=-362 vara=1.407111962114 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=72 mean=-692 vara=3.86352160169558 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=132 mean=499 vara=10.390196543329 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=503 mean=-581 vara=6.13916687682135 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=616 mean=-164 vara=0.121958378307457 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1064 mean=354 vara=11.5364474734587 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=262 mean=-503 vara=4.02726501997976 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=45 mean=520 vara=0.531432229208462 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=140 mean=-224 vara=1.84798174243302 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=219 mean=908 vara=9.31822440144078 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=636 mean=-16 vara=0.248794729226958 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=495 mean=426 vara=1.87085706175328 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=340 mean=891 vara=10.893196348975 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=29 mean=339 vara=1.02283419617535 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=101 mean=570 vara=0.968179983490419 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=231 mean=-216 vara=0.791295013919077 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1076 mean=930 vara=1.79546860944683 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=86 mean=-725 vara=2.16376283823421 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=202 mean=527 vara=11.5270904047253 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1381 mean=956 vara=1.38472317432384 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-869 vara=9.25167905383208 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=505 mean=-806 vara=2.68869827007766 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=640 mean=-792 vara=16.8405579785213 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1315 mean=-492 vara=5.19841795377399 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=996 mean=-294 vara=5.07799799584828 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=261 mean=-180 vara=0.0564363261853465 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=606 mean=-860 vara=1.31887785969014 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=349 mean=-693 vara=1.67251081063692 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1113 mean=-362 vara=3.21712407600704 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=156 vara=3.30830725217917 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=556 mean=418 vara=1.36629244588207 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=189 mean=-684 vara=1.40158433288586 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=33 mean=6 vara=0.102877969258539 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1074 mean=789 vara=7.37488883100931 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=752 mean=-365 vara=5.41665346656011 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=420 mean=689 vara=13.2936651284455 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=-317 vara=0.710844728423296 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=583 mean=-874 vara=7.35615788922531 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=109 mean=78 vara=1.03244775949764 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1115 mean=-241 vara=2.0420563820466 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=41 mean=588 vara=7.62557366168144 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=218 mean=278 vara=5.7869759402631 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=-26 vara=0.000813272743767773 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=255 mean=691 vara=6.07098413343786 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=544 mean=121 vara=0.389620763166173 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=755 mean=-563 vara=5.69096106955755 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=81 vara=1.05740329360251 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-146 vara=0.827465112772639 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=153 mean=-769 vara=2.69389171594587 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=470 mean=-872 vara=3.0839705056204 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=283 mean=-17 vara=0.0909842864601781 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-516 vara=6.68266237624935 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1045 mean=939 vara=0.19743254567756 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=766 mean=364 vara=1.33662069802591 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=327 mean=274 vara=0.408964135789027 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=279 mean=752 vara=12.5312320329457 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=30 mean=-412 vara=2.00207737486077 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=366 mean=420 vara=2.83778341108809 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=47 vara=0.156104501408812 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=158 mean=-321 vara=5.72773049449014 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=897 mean=-825 vara=0.762999744590276 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=302 mean=-63 vara=1.46478685104053 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=516 vara=9.00581573545185 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=533 mean=-626 vara=2.82123267687893 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=817 mean=809 vara=8.13478411305941 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=172 mean=-150 vara=1.72389687059901 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=421 mean=-863 vara=4.04030098102214 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=497 mean=-687 vara=11.8757063465601 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=637 mean=250 vara=1.50039746354386 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-921 vara=5.45686253626927 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=91 mean=-302 vara=3.04259103585876 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=31 mean=716 vara=6.8676603817634 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=44 mean=-156 vara=2.51284265395598 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1563 mean=-7 vara=0.0646521229958962 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=235 mean=603 vara=5.62538066728811 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=952 mean=-362 vara=1.407111962114 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=72 mean=-692 vara=3.86352160169558 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=132 mean=499 vara=10.390196543329 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=503 mean=-581 vara=6.13916687682135 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=616 mean=-164 vara=0.121958378307457 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1064 mean=354 vara=11.5364474734587 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=262 mean=-503 vara=4.02726501997976 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=45 mean=520 vara=0.531432229208462 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=140 mean=-224 vara=1.84798174243302 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=219 mean=908 vara=9.31822440144078 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=636 mean=-16 vara=0.248794729226958 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=495 mean=426 vara=1.87085706175328 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=340 mean=891 vara=10.893196348975 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=29 mean=339 vara=1.02283419617535 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=101 mean=570 vara=0.968179983490419 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=231 mean=-216 vara=0.791295013919077 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1076 mean=930 vara=1.79546860944683 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=86 mean=-725 vara=2.16376283823421 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=202 mean=527 vara=11.5270904047253 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1381 mean=956 vara=1.38472317432384 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-869 vara=9.25167905383208 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=505 mean=-806 vara=2.68869827007766 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=640 mean=-792 vara=16.8405579785213 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1315 mean=-492 vara=5.19841795377399 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=996 mean=-294 vara=5.07799799584828 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=261 mean=-180 vara=0.0564363261853465 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=606 mean=-860 vara=1.31887785969014 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=349 mean=-693 vara=1.67251081063692 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1113 mean=-362 vara=3.21712407600704 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=156 vara=3.30830725217917 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=556 mean=418 vara=1.36629244588207 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=189 mean=-684 vara=1.40158433288586 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=33 mean=6 vara=0.102877969258539 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1074 mean=789 vara=7.37488883100931 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=752 mean=-365 vara=5.41665346656011 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=420 mean=689 vara=13.2936651284455 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=-317 vara=0.710844728423296 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=583 mean=-874 vara=7.35615788922531 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=109 mean=78 vara=1.03244775949764 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1115 mean=-241 vara=2.0420563820466 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=41 mean=588 vara=7.62557366168144 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=218 mean=278 vara=5.7869759402631 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=-26 vara=0.000813272743767773 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=255 mean=691 vara=6.07098413343786 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=544 mean=121 vara=0.389620763166173 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=755 mean=-563 vara=5.69096106955755 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=81 vara=1.05740329360251 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-146 vara=0.827465112772639 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=153 mean=-769 vara=2.69389171594587 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=470 mean=-872 vara=3.0839705056204 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=283 mean=-17 vara=0.0909842864601781 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-516 vara=6.68266237624935 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1045 mean=939 vara=0.19743254567756 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=766 mean=364 vara=1.33662069802591 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=327 mean=274 vara=0.408964135789027 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=279 mean=752 vara=12.5312320329457 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=30 mean=-412 vara=2.00207737486077 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=366 mean=420 vara=2.83778341108809 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=47 vara=0.156104501408812 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=158 mean=-321 vara=5.72773049449014 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=897 mean=-825 vara=0.762999744590276 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=302 mean=-63 vara=1.46478685104053 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=516 vara=9.00581573545185 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=533 mean=-626 vara=2.82123267687893 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=817 mean=809 vara=8.13478411305941 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=172 mean=-150 vara=1.72389687059901 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=421 mean=-863 vara=4.04030098102214 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=497 mean=-687 vara=11.8757063465601 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=637 mean=250 vara=1.50039746354386 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-921 vara=5.45686253626927 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=91 mean=-302 vara=3.04259103585876 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=31 mean=716 vara=6.8676603817634 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=44 mean=-156 vara=2.51284265395598 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1563 mean=-7 vara=0.0646521229958962 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=235 mean=603 vara=5.62538066728811 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=952 mean=-362 vara=1.407111962114 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=72 mean=-692 vara=3.86352160169558 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=132 mean=499 vara=10.390196543329 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=503 mean=-581 vara=6.13916687682135 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=616 mean=-164 vara=0.121958378307457 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1064 mean=354 vara=11.5364474734587 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=262 mean=-503 vara=4.02726501997976 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=45 mean=520 vara=0.531432229208462 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=140 mean=-224 vara=1.84798174243302 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=219 mean=908 vara=9.31822440144078 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=636 mean=-16 vara=0.248794729226958 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=495 mean=426 vara=1.87085706175328 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=340 mean=891 vara=10.893196348975 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=29 mean=339 vara=1.02283419617535 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=101 mean=570 vara=0.968179983490419 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=231 mean=-216 vara=0.791295013919077 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1076 mean=930 vara=1.79546860944683 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=86 mean=-725 vara=2.16376283823421 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=202 mean=527 vara=11.5270904047253 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1381 mean=956 vara=1.38472317432384 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-869 vara=9.25167905383208 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=505 mean=-806 vara=2.68869827007766 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=640 mean=-792 vara=16.8405579785213 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1315 mean=-492 vara=5.19841795377399 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=996 mean=-294 vara=5.07799799584828 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=261 mean=-180 vara=0.0564363261853465 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=606 mean=-860 vara=1.31887785969014 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=349 mean=-693 vara=1.67251081063692 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1113 mean=-362 vara=3.21712407600704 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=156 vara=3.30830725217917 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=556 mean=418 vara=1.36629244588207 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=189 mean=-684 vara=1.40158433288586 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=33 mean=6 vara=0.102877969258539 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1074 mean=789 vara=7.37488883100931 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=752 mean=-365 vara=5.41665346656011 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=420 mean=689 vara=13.2936651284455 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=-317 vara=0.710844728423296 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=583 mean=-874 vara=7.35615788922531 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=109 mean=78 vara=1.03244775949764 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1115 mean=-241 vara=2.0420563820466 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=41 mean=588 vara=7.62557366168144 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=218 mean=278 vara=5.7869759402631 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=-26 vara=0.000813272743767773 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=255 mean=691 vara=6.07098413343786 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=544 mean=121 vara=0.389620763166173 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=755 mean=-563 vara=5.69096106955755 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=81 vara=1.05740329360251 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-146 vara=0.827465112772639 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=153 mean=-769 vara=2.69389171594587 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=470 mean=-872 vara=3.0839705056204 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=283 mean=-17 vara=0.0909842864601781 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-516 vara=6.68266237624935 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1045 mean=939 vara=0.19743254567756 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=766 mean=364 vara=1.33662069802591 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=327 mean=274 vara=0.408964135789027 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=279 mean=752 vara=12.5312320329457 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=30 mean=-412 vara=2.00207737486077 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=366 mean=420 vara=2.83778341108809 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=47 vara=0.156104501408812 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=158 mean=-321 vara=5.72773049449014 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=897 mean=-825 vara=0.762999744590276 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=302 mean=-63 vara=1.46478685104053 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=516 vara=9.00581573545185 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=533 mean=-626 vara=2.82123267687893 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=817 mean=809 vara=8.13478411305941 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=172 mean=-150 vara=1.72389687059901 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=421 mean=-863 vara=4.04030098102214 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=497 mean=-687 vara=11.8757063465601 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=637 mean=250 vara=1.50039746354386 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-921 vara=5.45686253626927 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=91 mean=-302 vara=3.04259103585876 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=31 mean=716 vara=6.8676603817634 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=44 mean=-156 vara=2.51284265395598 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1563 mean=-7 vara=0.0646521229958962 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=235 mean=603 vara=5.62538066728811 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=952 mean=-362 vara=1.407111962114 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=72 mean=-692 vara=3.86352160169558 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=132 mean=499 vara=10.390196543329 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=503 mean=-581 vara=6.13916687682135 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=616 mean=-164 vara=0.121958378307457 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1064 mean=354 vara=11.5364474734587 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=262 mean=-503 vara=4.02726501997976 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=45 mean=520 vara=0.531432229208462 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=140 mean=-224 vara=1.84798174243302 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=219 mean=908 vara=9.31822440144078 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=636 mean=-16 vara=0.248794729226958 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=495 mean=426 vara=1.87085706175328 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=340 mean=891 vara=10.893196348975 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=29 mean=339 vara=1.02283419617535 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=101 mean=570 vara=0.968179983490419 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=231 mean=-216 vara=0.791295013919077 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1076 mean=930 vara=1.79546860944683 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=86 mean=-725 vara=2.16376283823421 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=202 mean=527 vara=11.5270904047253 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1381 mean=956 vara=1.38472317432384 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-869 vara=9.25167905383208 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=505 mean=-806 vara=2.68869827007766 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=640 mean=-792 vara=16.8405579785213 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1315 mean=-492 vara=5.19841795377399 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=996 mean=-294 vara=5.07799799584828 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=261 mean=-180 vara=0.0564363261853465 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=606 mean=-860 vara=1.31887785969014 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=349 mean=-693 vara=1.67251081063692 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1113 mean=-362 vara=3.21712407600704 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=156 vara=3.30830725217917 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=556 mean=418 vara=1.36629244588207 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=189 mean=-684 vara=1.40158433288586 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=33 mean=6 vara=0.102877969258539 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1074 mean=789 vara=7.37488883100931 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=752 mean=-365 vara=5.41665346656011 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=420 mean=689 vara=13.2936651284455 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=-317 vara=0.710844728423296 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=583 mean=-874 vara=7.35615788922531 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=109 mean=78 vara=1.03244775949764 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1115 mean=-241 vara=2.0420563820466 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=41 mean=588 vara=7.62557366168144 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=218 mean=278 vara=5.7869759402631 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=-26 vara=0.000813272743767773 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=255 mean=691 vara=6.07098413343786 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=544 mean=121 vara=0.389620763166173 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=755 mean=-563 vara=5.69096106955755 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=81 vara=1.05740329360251 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-146 vara=0.827465112772639 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=153 mean=-769 vara=2.69389171594587 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=470 mean=-872 vara=3.0839705056204 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=283 mean=-17 vara=0.0909842864601781 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-516 vara=6.68266237624935 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1045 mean=939 vara=0.19743254567756 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=766 mean=364 vara=1.33662069802591 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=327 mean=274 vara=0.408964135789027 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=279 mean=752 vara=12.5312320329457 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=30 mean=-412 vara=2.00207737486077 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=366 mean=420 vara=2.83778341108809 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=47 vara=0.156104501408812 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=158 mean=-321 vara=5.72773049449014 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=897 mean=-825 vara=0.762999744590276 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=302 mean=-63 vara=1.46478685104053 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=516 vara=9.00581573545185 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=533 mean=-626 vara=2.82123267687893 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=817 mean=809 vara=8.13478411305941 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=172 mean=-150 vara=1.72389687059901 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=421 mean=-863 vara=4.04030098102214 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=497 mean=-687 vara=11.8757063465601 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=637 mean=250 vara=1.50039746354386 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-921 vara=5.45686253626927 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=91 mean=-302 vara=3.04259103585876 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=31 mean=716 vara=6.8676603817634 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=44 mean=-156 vara=2.51284265395598 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1563 mean=-7 vara=0.0646521229958962 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=235 mean=603 vara=5.62538066728811 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=952 mean=-362 vara=1.407111962114 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=72 mean=-692 vara=3.86352160169558 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=132 mean=499 vara=10.390196543329 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=503 mean=-581 vara=6.13916687682135 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=616 mean=-164 vara=0.121958378307457 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1064 mean=354 vara=11.5364474734587 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=262 mean=-503 vara=4.02726501997976 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=45 mean=520 vara=0.531432229208462 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=140 mean=-224 vara=1.84798174243302 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=219 mean=908 vara=9.31822440144078 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=636 mean=-16 vara=0.248794729226958 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=495 mean=426 vara=1.87085706175328 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=340 mean=891 vara=10.893196348975 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=29 mean=339 vara=1.02283419617535 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=101 mean=570 vara=0.968179983490419 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=231 mean=-216 vara=0.791295013919077 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1076 mean=930 vara=1.79546860944683 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=86 mean=-725 vara=2.16376283823421 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=202 mean=527 vara=11.5270904047253 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1381 mean=956 vara=1.38472317432384 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-869 vara=9.25167905383208 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=505 mean=-806 vara=2.68869827007766 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=640 mean=-792 vara=16.8405579785213 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1315 mean=-492 vara=5.19841795377399 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=996 mean=-294 vara=5.07799799584828 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=261 mean=-180 vara=0.0564363261853465 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=606 mean=-860 vara=1.31887785969014 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=349 mean=-693 vara=1.67251081063692 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1113 mean=-362 vara=3.21712407600704 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=156 vara=3.30830725217917 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=556 mean=418 vara=1.36629244588207 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=189 mean=-684 vara=1.40158433288586 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=33 mean=6 vara=0.102877969258539 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1074 mean=789 vara=7.37488883100931 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=752 mean=-365 vara=5.41665346656011 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=420 mean=689 vara=13.2936651284455 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=-317 vara=0.710844728423296 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=583 mean=-874 vara=7.35615788922531 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=109 mean=78 vara=1.03244775949764 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=260 mean=645 vara=9.82340657194069 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=550 mean=-991 vara=1.46664417092478 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=57 mean=-381 vara=4.76219497878763 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=867 mean=-847 vara=2.34227756065589 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=281 mean=580 vara=10.9653439098191 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=21 mean=-688 vara=10.2957171064352 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=515 mean=-475 vara=2.52625477944188 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=463 mean=143 vara=0.0850461655835365 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=489 mean=-417 vara=0.241570769881022 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=247 mean=-32 vara=0.0166240854498353 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=70 mean=703 vara=2.52645832015362 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1195 mean=632 vara=0.622241021161509 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=102 mean=-585 vara=4.05065487203177 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=665 mean=-821 vara=9.56738398622745 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1047 mean=367 vara=4.55974921794729 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=468 mean=992 vara=6.86555454508482 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=124 mean=814 vara=0.715955580713271 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1452 mean=-381 vara=1.56441282283912 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=46 mean=572 vara=7.51777709659652 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=122 mean=-348 vara=4.17444058930789 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=85 mean=-807 vara=2.1942792306841 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=104 mean=904 vara=30.0644467745654 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1422 mean=-795 vara=8.37974346751367 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=224 mean=-606 vara=0.986398402601022 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=372 mean=-709 vara=0.806988918633747 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=112 mean=165 vara=0.260070835666391 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=404 mean=-490 vara=1.04359709715711 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=497 mean=-310 vara=3.02112165210755 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=74 mean=-826 vara=0.57728415389487 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=304 mean=650 vara=4.1798114826505 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=341 mean=-470 vara=3.46453229702925 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=878 mean=979 vara=6.36824521767416 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=394 mean=742 vara=4.02997083225871 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=232 mean=213 vara=1.00667738436587 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=91 mean=949 vara=10.2767128157948 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=777 mean=769 vara=9.85459225514705 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1468 mean=-310 vara=1.81390713428074 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=220 mean=401 vara=0.199893506582031 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=183 mean=447 vara=6.95486761085075 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=788 mean=-14 vara=0.00715135304429341 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=886 mean=505 vara=4.27967575979677 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1469 mean=-8 vara=0.0331958760993753 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=20 mean=915 vara=8.11792367562298 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=747 mean=-297 vara=3.17984101277415 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=445 mean=15 vara=0.142039911347255 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1926 mean=101 vara=0.591909711734671 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1085 mean=491 vara=6.03370433636706 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=87 mean=255 vara=1.81559591828261 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=728 mean=343 vara=7.96300641211402 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=185 mean=-868 vara=6.37599760404004 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=325 mean=-703 vara=2.52986834259701 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=607 mean=335 vara=0.763549500988381 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1501 mean=154 vara=1.5413265884163 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=239 mean=-570 vara=4.05767769824911 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=29 mean=120 vara=0.317218036610581 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=101 mean=570 vara=0.968179983490419 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=231 mean=-216 vara=0.791295013919077 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1076 mean=930 vara=1.79546860944683 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=86 mean=-725 vara=2.16376283823421 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=202 mean=527 vara=11.5270904047253 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1381 mean=956 vara=1.38472317432384 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-869 vara=9.25167905383208 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=505 mean=-806 vara=2.68869827007766 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=640 mean=-792 vara=16.8405579785213 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1315 mean=-492 vara=5.19841795377399 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=996 mean=-294 vara=5.07799799584828 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=261 mean=-180 vara=0.0564363261853465 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=606 mean=-860 vara=1.31887785969014 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=349 mean=-693 vara=1.67251081063692 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1113 mean=-362 vara=3.21712407600704 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=156 vara=3.30830725217917 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=556 mean=418 vara=1.36629244588207 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=189 mean=-684 vara=1.40158433288586 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=33 mean=6 vara=0.102877969258539 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1074 mean=789 vara=7.37488883100931 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=752 mean=-365 vara=5.41665346656011 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=420 mean=689 vara=13.2936651284455 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=-317 vara=0.710844728423296 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=583 mean=-874 vara=7.35615788922531 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=109 mean=78 vara=1.03244775949764 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1115 mean=-241 vara=2.0420563820466 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=41 mean=588 vara=7.62557366168144 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=218 mean=278 vara=5.7869759402631 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=-26 vara=0.000813272743767773 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=255 mean=691 vara=6.07098413343786 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=544 mean=121 vara=0.389620763166173 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=755 mean=-563 vara=5.69096106955755 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=81 vara=1.05740329360251 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-146 vara=0.827465112772639 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=153 mean=-769 vara=2.69389171594587 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=470 mean=-872 vara=3.0839705056204 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=283 mean=-17 vara=0.0909842864601781 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-516 vara=6.68266237624935 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1045 mean=939 vara=0.19743254567756 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=766 mean=364 vara=1.33662069802591 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=327 mean=274 vara=0.408964135789027 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=279 mean=752 vara=12.5312320329457 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=30 mean=-412 vara=2.00207737486077 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=366 mean=420 vara=2.83778341108809 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=47 vara=0.156104501408812 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=158 mean=-321 vara=5.72773049449014 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=897 mean=-825 vara=0.762999744590276 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=302 mean=-63 vara=1.46478685104053 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=516 vara=9.00581573545185 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=533 mean=-626 vara=2.82123267687893 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=817 mean=809 vara=8.13478411305941 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=172 mean=-150 vara=1.72389687059901 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=421 mean=-863 vara=4.04030098102214 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=497 mean=-687 vara=11.8757063465601 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=637 mean=250 vara=1.50039746354386 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-921 vara=5.45686253626927 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=91 mean=-302 vara=3.04259103585876 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=31 mean=716 vara=6.8676603817634 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=44 mean=-156 vara=2.51284265395598 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1563 mean=-7 vara=0.0646521229958962 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=235 mean=603 vara=5.62538066728811 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=952 mean=-362 vara=1.407111962114 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=72 mean=-692 vara=3.86352160169558 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=132 mean=499 vara=10.390196543329 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=503 mean=-581 vara=6.13916687682135 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=616 mean=-164 vara=0.121958378307457 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1064 mean=354 vara=11.5364474734587 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=262 mean=-503 vara=4.02726501997976 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=45 mean=520 vara=0.531432229208462 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=140 mean=-224 vara=1.84798174243302 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=219 mean=908 vara=9.31822440144078 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=636 mean=-16 vara=0.248794729226958 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=495 mean=426 vara=1.87085706175328 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=340 mean=891 vara=10.893196348975 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=29 mean=339 vara=1.02283419617535 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=101 mean=570 vara=0.968179983490419 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=231 mean=-216 vara=0.791295013919077 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1076 mean=930 vara=1.79546860944683 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=86 mean=-725 vara=2.16376283823421 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=202 mean=527 vara=11.5270904047253 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1381 mean=956 vara=1.38472317432384 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-869 vara=9.25167905383208 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=505 mean=-806 vara=2.68869827007766 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=640 mean=-792 vara=16.8405579785213 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1315 mean=-492 vara=5.19841795377399 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=996 mean=-294 vara=5.07799799584828 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=261 mean=-180 vara=0.0564363261853465 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=606 mean=-860 vara=1.31887785969014 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=349 mean=-693 vara=1.67251081063692 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1113 mean=-362 vara=3.21712407600704 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=156 vara=3.30830725217917 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=556 mean=418 vara=1.36629244588207 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=189 mean=-684 vara=1.40158433288586 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=33 mean=6 vara=0.102877969258539 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1074 mean=789 vara=7.37488883100931 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=752 mean=-365 vara=5.41665346656011 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=420 mean=689 vara=13.2936651284455 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=-317 vara=0.710844728423296 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=583 mean=-874 vara=7.35615788922531 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=109 mean=78 vara=1.03244775949764 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1115 mean=-241 vara=2.0420563820466 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=41 mean=588 vara=7.62557366168144 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=218 mean=278 vara=5.7869759402631 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=-26 vara=0.000813272743767773 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=255 mean=691 vara=6.07098413343786 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=544 mean=121 vara=0.389620763166173 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=755 mean=-563 vara=5.69096106955755 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=81 vara=1.05740329360251 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-146 vara=0.827465112772639 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=153 mean=-769 vara=2.69389171594587 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=470 mean=-872 vara=3.0839705056204 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=283 mean=-17 vara=0.0909842864601781 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-516 vara=6.68266237624935 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1045 mean=939 vara=0.19743254567756 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=766 mean=364 vara=1.33662069802591 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=327 mean=274 vara=0.408964135789027 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=279 mean=752 vara=12.5312320329457 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=30 mean=-412 vara=2.00207737486077 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=366 mean=420 vara=2.83778341108809 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=47 vara=0.156104501408812 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=158 mean=-321 vara=5.72773049449014 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=897 mean=-825 vara=0.762999744590276 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=302 mean=-63 vara=1.46478685104053 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=516 vara=9.00581573545185 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=533 mean=-626 vara=2.82123267687893 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=817 mean=809 vara=8.13478411305941 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=172 mean=-150 vara=1.72389687059901 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=421 mean=-863 vara=4.04030098102214 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=497 mean=-687 vara=11.8757063465601 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=637 mean=250 vara=1.50039746354386 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-921 vara=5.45686253626927 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=91 mean=-302 vara=3.04259103585876 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=31 mean=716 vara=6.8676603817634 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=44 mean=-156 vara=2.51284265395598 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1563 mean=-7 vara=0.0646521229958962 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=235 mean=603 vara=5.62538066728811 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=952 mean=-362 vara=1.407111962114 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=72 mean=-692 vara=3.86352160169558 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=132 mean=499 vara=10.390196543329 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=503 mean=-581 vara=6.13916687682135 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=616 mean=-164 vara=0.121958378307457 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1064 mean=354 vara=11.5364474734587 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=262 mean=-503 vara=4.02726501997976 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=45 mean=520 vara=0.531432229208462 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=140 mean=-224 vara=1.84798174243302 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=219 mean=908 vara=9.31822440144078 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=636 mean=-16 vara=0.248794729226958 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=495 mean=426 vara=1.87085706175328 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=340 mean=891 vara=10.893196348975 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=29 mean=339 vara=1.02283419617535 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=101 mean=570 vara=0.968179983490419 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=231 mean=-216 vara=0.791295013919077 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1076 mean=930 vara=1.79546860944683 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=86 mean=-725 vara=2.16376283823421 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=202 mean=527 vara=11.5270904047253 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1381 mean=956 vara=1.38472317432384 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-869 vara=9.25167905383208 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=505 mean=-806 vara=2.68869827007766 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=640 mean=-792 vara=16.8405579785213 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1315 mean=-492 vara=5.19841795377399 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=996 mean=-294 vara=5.07799799584828 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=261 mean=-180 vara=0.0564363261853465 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=606 mean=-860 vara=1.31887785969014 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=349 mean=-693 vara=1.67251081063692 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1113 mean=-362 vara=3.21712407600704 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=156 vara=3.30830725217917 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=556 mean=418 vara=1.36629244588207 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=189 mean=-684 vara=1.40158433288586 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=33 mean=6 vara=0.102877969258539 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1074 mean=789 vara=7.37488883100931 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=752 mean=-365 vara=5.41665346656011 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=420 mean=689 vara=13.2936651284455 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=-317 vara=0.710844728423296 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=583 mean=-874 vara=7.35615788922531 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=109 mean=78 vara=1.03244775949764 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1115 mean=-241 vara=2.0420563820466 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=41 mean=588 vara=7.62557366168144 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=218 mean=278 vara=5.7869759402631 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=-26 vara=0.000813272743767773 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=255 mean=691 vara=6.07098413343786 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=544 mean=121 vara=0.389620763166173 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=755 mean=-563 vara=5.69096106955755 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=81 vara=1.05740329360251 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-146 vara=0.827465112772639 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=153 mean=-769 vara=2.69389171594587 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=470 mean=-872 vara=3.0839705056204 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=283 mean=-17 vara=0.0909842864601781 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-516 vara=6.68266237624935 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1045 mean=939 vara=0.19743254567756 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=766 mean=364 vara=1.33662069802591 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=327 mean=274 vara=0.408964135789027 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=279 mean=752 vara=12.5312320329457 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=30 mean=-412 vara=2.00207737486077 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=366 mean=420 vara=2.83778341108809 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=47 vara=0.156104501408812 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=158 mean=-321 vara=5.72773049449014 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=897 mean=-825 vara=0.762999744590276 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=302 mean=-63 vara=1.46478685104053 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=516 vara=9.00581573545185 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=533 mean=-626 vara=2.82123267687893 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=817 mean=809 vara=8.13478411305941 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=172 mean=-150 vara=1.72389687059901 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=421 mean=-863 vara=4.04030098102214 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=497 mean=-687 vara=11.8757063465601 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=637 mean=250 vara=1.50039746354386 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-921 vara=5.45686253626927 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=91 mean=-302 vara=3.04259103585876 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=31 mean=716 vara=6.8676603817634 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=44 mean=-156 vara=2.51284265395598 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1563 mean=-7 vara=0.0646521229958962 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=235 mean=603 vara=5.62538066728811 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=952 mean=-362 vara=1.407111962114 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=72 mean=-692 vara=3.86352160169558 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=132 mean=499 vara=10.390196543329 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=503 mean=-581 vara=6.13916687682135 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=616 mean=-164 vara=0.121958378307457 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1064 mean=354 vara=11.5364474734587 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=262 mean=-503 vara=4.02726501997976 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=45 mean=520 vara=0.531432229208462 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=140 mean=-224 vara=1.84798174243302 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=219 mean=908 vara=9.31822440144078 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=636 mean=-16 vara=0.248794729226958 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=495 mean=426 vara=1.87085706175328 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=340 mean=891 vara=10.893196348975 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=29 mean=339 vara=1.02283419617535 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=101 mean=570 vara=0.968179983490419 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=231 mean=-216 vara=0.791295013919077 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1076 mean=930 vara=1.79546860944683 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=86 mean=-725 vara=2.16376283823421 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=202 mean=527 vara=11.5270904047253 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1381 mean=956 vara=1.38472317432384 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-869 vara=9.25167905383208 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=505 mean=-806 vara=2.68869827007766 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=640 mean=-792 vara=16.8405579785213 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1315 mean=-492 vara=5.19841795377399 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=996 mean=-294 vara=5.07799799584828 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=261 mean=-180 vara=0.0564363261853465 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=606 mean=-860 vara=1.31887785969014 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=349 mean=-693 vara=1.67251081063692 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1113 mean=-362 vara=3.21712407600704 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=156 vara=3.30830725217917 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=556 mean=418 vara=1.36629244588207 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=189 mean=-684 vara=1.40158433288586 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=33 mean=6 vara=0.102877969258539 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1074 mean=789 vara=7.37488883100931 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=752 mean=-365 vara=5.41665346656011 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=420 mean=689 vara=13.2936651284455 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=-317 vara=0.710844728423296 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=583 mean=-874 vara=7.35615788922531 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=109 mean=78 vara=1.03244775949764 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1115 mean=-241 vara=2.0420563820466 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=41 mean=588 vara=7.62557366168144 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=218 mean=278 vara=5.7869759402631 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=-26 vara=0.000813272743767773 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=255 mean=691 vara=6.07098413343786 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=544 mean=121 vara=0.389620763166173 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=755 mean=-563 vara=5.69096106955755 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=81 vara=1.05740329360251 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-146 vara=0.827465112772639 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=153 mean=-769 vara=2.69389171594587 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=470 mean=-872 vara=3.0839705056204 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=283 mean=-17 vara=0.0909842864601781 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-516 vara=6.68266237624935 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1045 mean=939 vara=0.19743254567756 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=766 mean=364 vara=1.33662069802591 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=327 mean=274 vara=0.408964135789027 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=279 mean=752 vara=12.5312320329457 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=30 mean=-412 vara=2.00207737486077 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=366 mean=420 vara=2.83778341108809 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=47 vara=0.156104501408812 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=158 mean=-321 vara=5.72773049449014 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=897 mean=-825 vara=0.762999744590276 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=302 mean=-63 vara=1.46478685104053 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=516 vara=9.00581573545185 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=533 mean=-626 vara=2.82123267687893 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=817 mean=809 vara=8.13478411305941 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=172 mean=-150 vara=1.72389687059901 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=421 mean=-863 vara=4.04030098102214 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=497 mean=-687 vara=11.8757063465601 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=637 mean=250 vara=1.50039746354386 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-921 vara=5.45686253626927 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=91 mean=-302 vara=3.04259103585876 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=31 mean=716 vara=6.8676603817634 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=44 mean=-156 vara=2.51284265395598 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1563 mean=-7 vara=0.0646521229958962 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=235 mean=603 vara=5.62538066728811 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=952 mean=-362 vara=1.407111962114 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=72 mean=-692 vara=3.86352160169558 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=132 mean=499 vara=10.390196543329 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=503 mean=-581 vara=6.13916687682135 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=616 mean=-164 vara=0.121958378307457 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1064 mean=354 vara=11.5364474734587 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=262 mean=-503 vara=4.02726501997976 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=45 mean=520 vara=0.531432229208462 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=140 mean=-224 vara=1.84798174243302 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=219 mean=908 vara=9.31822440144078 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=636 mean=-16 vara=0.248794729226958 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=495 mean=426 vara=1.87085706175328 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=340 mean=891 vara=10.893196348975 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=29 mean=339 vara=1.02283419617535 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=101 mean=570 vara=0.968179983490419 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=231 mean=-216 vara=0.791295013919077 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1076 mean=930 vara=1.79546860944683 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=86 mean=-725 vara=2.16376283823421 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=202 mean=527 vara=11.5270904047253 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1381 mean=956 vara=1.38472317432384 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-869 vara=9.25167905383208 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=505 mean=-806 vara=2.68869827007766 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=640 mean=-792 vara=16.8405579785213 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1315 mean=-492 vara=5.19841795377399 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=996 mean=-294 vara=5.07799799584828 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=261 mean=-180 vara=0.0564363261853465 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=606 mean=-860 vara=1.31887785969014 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=349 mean=-693 vara=1.67251081063692 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1113 mean=-362 vara=3.21712407600704 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=156 vara=3.30830725217917 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=556 mean=418 vara=1.36629244588207 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=189 mean=-684 vara=1.40158433288586 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=33 mean=6 vara=0.102877969258539 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1074 mean=789 vara=7.37488883100931 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=752 mean=-365 vara=5.41665346656011 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=420 mean=689 vara=13.2936651284455 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=-317 vara=0.710844728423296 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=583 mean=-874 vara=7.35615788922531 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=109 mean=78 vara=1.03244775949764 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1115 mean=-241 vara=2.0420563820466 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=41 mean=588 vara=7.62557366168144 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=218 mean=278 vara=5.7869759402631 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=-26 vara=0.000813272743767773 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=255 mean=691 vara=6.07098413343786 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=544 mean=121 vara=0.389620763166173 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=755 mean=-563 vara=5.69096106955755 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=81 vara=1.05740329360251 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-146 vara=0.827465112772639 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=153 mean=-769 vara=2.69389171594587 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=470 mean=-872 vara=3.0839705056204 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=283 mean=-17 vara=0.0909842864601781 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-516 vara=6.68266237624935 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1045 mean=939 vara=0.19743254567756 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=766 mean=364 vara=1.33662069802591 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=327 mean=274 vara=0.408964135789027 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=279 mean=752 vara=12.5312320329457 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=30 mean=-412 vara=2.00207737486077 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=366 mean=420 vara=2.83778341108809 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=47 vara=0.156104501408812 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=158 mean=-321 vara=5.72773049449014 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=897 mean=-825 vara=0.762999744590276 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=302 mean=-63 vara=1.46478685104053 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=516 vara=9.00581573545185 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=533 mean=-626 vara=2.82123267687893 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=817 mean=809 vara=8.13478411305941 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=172 mean=-150 vara=1.72389687059901 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=421 mean=-863 vara=4.04030098102214 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=497 mean=-687 vara=11.8757063465601 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=637 mean=250 vara=1.50039746354386 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-921 vara=5.45686253626927 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=91 mean=-302 vara=3.04259103585876 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=31 mean=716 vara=6.8676603817634 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=44 mean=-156 vara=2.51284265395598 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1563 mean=-7 vara=0.0646521229958962 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=235 mean=603 vara=5.62538066728811 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=952 mean=-362 vara=1.407111962114 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=72 mean=-692 vara=3.86352160169558 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=132 mean=499 vara=10.390196543329 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=503 mean=-581 vara=6.13916687682135 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=616 mean=-164 vara=0.121958378307457 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1064 mean=354 vara=11.5364474734587 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=262 mean=-503 vara=4.02726501997976 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=45 mean=520 vara=0.531432229208462 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=140 mean=-224 vara=1.84798174243302 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=219 mean=908 vara=9.31822440144078 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=636 mean=-16 vara=0.248794729226958 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=495 mean=426 vara=1.87085706175328 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=340 mean=891 vara=10.893196348975 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=29 mean=339 vara=1.02283419617535 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=101 mean=570 vara=0.968179983490419 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=231 mean=-216 vara=0.791295013919077 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1076 mean=930 vara=1.79546860944683 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=86 mean=-725 vara=2.16376283823421 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=202 mean=527 vara=11.5270904047253 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1381 mean=956 vara=1.38472317432384 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-869 vara=9.25167905383208 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=505 mean=-806 vara=2.68869827007766 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=640 mean=-792 vara=16.8405579785213 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1315 mean=-492 vara=5.19841795377399 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=996 mean=-294 vara=5.07799799584828 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=261 mean=-180 vara=0.0564363261853465 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=606 mean=-860 vara=1.31887785969014 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=349 mean=-693 vara=1.67251081063692 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1113 mean=-362 vara=3.21712407600704 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=156 vara=3.30830725217917 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=556 mean=418 vara=1.36629244588207 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=189 mean=-684 vara=1.40158433288586 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=33 mean=6 vara=0.102877969258539 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1074 mean=789 vara=7.37488883100931 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=752 mean=-365 vara=5.41665346656011 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=420 mean=689 vara=13.2936651284455 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=-317 vara=0.710844728423296 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=583 mean=-874 vara=7.35615788922531 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=109 mean=78 vara=1.03244775949764 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1115 mean=-241 vara=2.0420563820466 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=41 mean=588 vara=7.62557366168144 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=218 mean=278 vara=5.7869759402631 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=-26 vara=0.000813272743767773 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=255 mean=691 vara=6.07098413343786 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=544 mean=121 vara=0.389620763166173 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=755 mean=-563 vara=5.69096106955755 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=107 mean=81 vara=1.05740329360251 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-146 vara=0.827465112772639 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=153 mean=-769 vara=2.69389171594587 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=470 mean=-872 vara=3.0839705056204 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=283 mean=-17 vara=0.0909842864601781 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=417 mean=-516 vara=6.68266237624935 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1045 mean=939 vara=0.19743254567756 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=766 mean=364 vara=1.33662069802591 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=327 mean=274 vara=0.408964135789027 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=279 mean=752 vara=12.5312320329457 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=30 mean=-412 vara=2.00207737486077 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=366 mean=420 vara=2.83778341108809 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=523 mean=47 vara=0.156104501408812 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=158 mean=-321 vara=5.72773049449014 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=897 mean=-825 vara=0.762999744590276 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=302 mean=-63 vara=1.46478685104053 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=75 mean=516 vara=9.00581573545185 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=533 mean=-626 vara=2.82123267687893 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=817 mean=809 vara=8.13478411305941 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=172 mean=-150 vara=1.72389687059901 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=421 mean=-863 vara=4.04030098102214 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=497 mean=-687 vara=11.8757063465601 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=637 mean=250 vara=1.50039746354386 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=47 mean=-921 vara=5.45686253626927 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=91 mean=-302 vara=3.04259103585876 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=31 mean=716 vara=6.8676603817634 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=44 mean=-156 vara=2.51284265395598 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1563 mean=-7 vara=0.0646521229958962 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=235 mean=603 vara=5.62538066728811 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=952 mean=-362 vara=1.407111962114 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=72 mean=-692 vara=3.86352160169558 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=132 mean=499 vara=10.390196543329 rep=5 sn=44"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=503 mean=-581 vara=6.13916687682135 rep=1 sn=40"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=616 mean=-164 vara=0.121958378307457 rep=2 sn=41"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=1064 mean=354 vara=11.5364474734587 rep=3 sn=42"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=262 mean=-503 vara=4.02726501997976 rep=4 sn=43"
    ##  PASS: TRUE
    ## [1] "s=0 d=0 p=0 q=1 n=45 mean=520 vara=0.531432229208462 rep=5 sn=44"
    ##  PASS: TRUE

``` r
meta_file_name = paste0(meta_dir, "index.csv")
write.csv(results, meta_file_name, row.names = FALSE)
```

``` r
print(results)
```

    ## # A tibble: 995 x 12
    ##    index     s     d     p     q     n   rep  mean    vara  seed phi   theta
    ##    <dbl> <dbl> <dbl> <dbl> <dbl> <int> <int> <int>   <dbl> <dbl> <chr> <chr>
    ##  1     1     0     0     0     1   340     1   176 0.639      40 0     0.99 
    ##  2     2     0     0     0     1  1222     2  -865 0.776      41 0     0.99 
    ##  3     3     0     0     0     1  1647     3  -355 1.30       42 0     0.99 
    ##  4     4     0     0     0     1   220     4  -267 1.51       43 0     0.99 
    ##  5     5     0     0     0     1   183     5   447 6.95       44 0     0.99 
    ##  6     6     0     0     0     1   788     1   -14 0.00715    40 0     0.98 
    ##  7     7     0     0     0     1   886     2   505 4.28       41 0     0.98 
    ##  8     8     0     0     0     1  1469     3    -8 0.0332     42 0     0.98 
    ##  9     9     0     0     0     1    20     4   915 8.12       43 0     0.98 
    ## 10    10     0     0     0     1   747     5  -297 3.18       44 0     0.98 
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
    ##    index     s     d     p     q     n   rep  mean    vara  seed phi   theta
    ##    <dbl> <dbl> <dbl> <dbl> <dbl> <int> <int> <int>   <dbl> <dbl> <chr> <chr>
    ##  1     1     0     0     0     1   995     1   176 0.639      40 0     0.99 
    ##  2     2     0     0     0     1   995     2  -865 0.776      41 0     0.99 
    ##  3     3     0     0     0     1   995     3  -355 1.30       42 0     0.99 
    ##  4     4     0     0     0     1   995     4  -267 1.51       43 0     0.99 
    ##  5     5     0     0     0     1   995     5   447 6.95       44 0     0.99 
    ##  6     6     0     0     0     1   995     1   -14 0.00715    40 0     0.98 
    ##  7     7     0     0     0     1   995     2   505 4.28       41 0     0.98 
    ##  8     8     0     0     0     1   995     3    -8 0.0332     42 0     0.98 
    ##  9     9     0     0     0     1   995     4   915 8.12       43 0     0.98 
    ## 10    10     0     0     0     1   995     5  -297 3.18       44 0     0.98 
    ## # ... with 985 more rows, and 4 more variables: mean_p <dbl>, sd_p <dbl>,
    ## #   mean_q <dbl>, sd_q <dbl>
