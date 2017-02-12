# Dataflow execution of reactive programs

The goal of this project is to investigate the potential of the dataflow execution model as an implementation platform for reactive programming languages.

More to come.

## Supported expressions

### Primitive values

```Scheme
1     ;; a number
"ABC" ;; a string
'()   ;; the empty list
```
### Primitive procedures

```Scheme
(cons x xs) ;; prepends x to a list of xs
(car xs)    ;; gets the head of a list
(cdr xs)    ;; gets the tail of a list
(null? xs)  ;; checks if a list is empty
```

### Constants

```Scheme
(define x expression) ;; evaluates the expression and assigns it to x
```

### Signals = values that change over time

```Scheme
(value $signal)                               -- gets the current value of a signal
(lift operator $signal1 $signal2 ...)         -- lifts existing signals into a new signal, where the operator will be called with the values of each signal every time a signal emits
```

*Note that the $-prefix is merely a convention and not enforced in any way*

Examples

```Scheme
(value $current-seconds)                      -- gets the current value of the current-seconds signal
(value $random-integer)                       -- gets the current value of the random-integer signal, which outputs random numbers between 1 and 100
(lift (lambda (x) (+ x 2)) $current-seconds)  -- creates a new signal by transforming another signal
(define $x (lift ...))                        -- assigns a new signal to x, created by transforming another signal
(value $x)                                    -- gets the current value of $x
```