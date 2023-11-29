: isprime (n -- bool) \ test, if n is prime and return true/false
>n
n 2 < if
    false return
endif
n 2 == if
    true return
endif
n 2 % 0 == if
    false return
endif
n isqrt >sqrt
3 >d d sqrt <= while
    n d % 0 == if
        false return
    endif
    d 2 + >d
    d sqrt <= loop
true ;

: isqrt (n -- sqrt n) \ return integer sqrt approximation
dup dup 2 /
dup2 != while
    dup2 dup >sqrt / + 2 /
    dup sqrt < loop
drop drop drop
sqrt ;

: primes (n -- ) \ list primes up to n
>end 0 >n
n end <= while
    n isprime if
        n print " " print
    endif
    n 1 + >n
    n end <= loop ;
