cs
[bool] >$results
: register_result if $results true append else $results false append endif >$results ;
: print_results 0 >$errs 0 >$oks $results for if oks 1 + >oks else errs 1 + >errs endif next $results len print " tests, ok: " print oks print ", failures: " print errs print ;
1 float string float int 1 == register_result
1 array 0 index 1 == register_result
3.14 array 6.28 append sum 9.42 == register_result
: isqrt dup dup 2 / dup2 != while dup2 dup >sqrt / + 2 / dup sqrt < loop drop drop drop sqrt ;
: isprime >n n 2 < if false return endif n 2 == if true return endif n 2 % 0 == if false return endif n isqrt >sqrt 3 >d d sqrt <= while n d % 0 == if false return endif d 2 + >d d sqrt <= loop true ;
: primes [int] >primes_list >end 0 >n n end <= while n isprime if primes_list n append >primes_list endif n 1 + >n n end <= loop primes_list ;
100 primes >pl pl len 25 == register_result
print_results
