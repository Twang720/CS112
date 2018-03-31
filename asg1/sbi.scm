#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; Student: Bobby Dhillon
;; CruzID: bosdhill
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

;;======================================================================
;;===================Hash Table definitions=============================
;;======================================================================

;; Hashtable for SBIR functions, which are the <keys> and scheme 
;; which are the functions <values>
(define *function-table* (make-hash))

;; Intialize *function-table*
;; for each element in the list, set the <key> to be the  car of the 
;; element and the <value> to be the cadr of the element (first and 
;; second values in element) stores operators and functions 
(for-each 
    (lambda (element) 
        (hash-set! *function-table* (car element) (cadr element)))
        `( 
           ;; operator section
           (+ , +) 
           (- , -) 
           (* , *)
           (/ , (lambda (x y) (/ x  y )))
           (% , (lambda (x y) (- (+ x 0.0) 
            (* (truncate (/ (+ x 0.0) (+ y 0.0))) (+ y 0.0)))))
           (^ , expt)
           ;; relop
           (= , equal?)
           (< , <)
           (> , >)
           (<> , (lambda (x y) (not (equal? x y))))
           (>= , >=)
           (<= , <=)
           ;; math functions 
           (exp , exp)
           (ceil , ceiling)
           (floor , floor)
           (sqrt , sqrt)
           (abs , abs)
           (acos , acos)
           (asin , asin)
           (atan , atan)
           (cos , cos)
           (round , round)
           (sin , sin)
           (tan , tan)
           (trunc , truncate)
           (log , (lambda (x) (log (+ x 0.0))))
           (log10 , (lambda (x) (/ (log (+ x 0.0)) (log 10.0))))
           (log2 , (lambda (x) (/ (log (+ x 0.0)) (log 2.0))))
         )
)

;; Hash table to store keys that are labels, 
;; and values that are the address of the 
;; line (program node) in reference 
;; (one level up from program list)
(define *label-table* (make-hash))

;; Hash table to store <keys> that are SBIR 
;; program variables, updated as program
;; is interpreted. Intialized with pi and e.
;; Arrays are created with make-vector and 
;; vector-set!
(define *variable-table* (make-hash))

;; Intializing *variable-table*
(for-each
    (lambda (element) 
        (hash-set! *variable-table* (car element) (cadr element)))
    `( 
       (e 2.718281828459045235360287471352662497757247093)
       (pi 3.141592653589793238462643383279502884197169399)
     )
)

;;======================================================================
;;==========================Trivial Functions===========================
;;======================================================================

;; gets first elt of line
(define (first-elt-of line) (car line))

;; gets second elt of line
(define (second-elt-of line) (cadr line))

;; gets third elt of line
(define (third-elt-of line) (caddr line))

;; next line in program
(define (rest-of line) (cdr line))

;; not a number
(define NaN (/ 0.0 0.0))

;;======================================================================
;;=====================Non-Trivial Functions============================
;;======================================================================

;; the interpret statement function transfers control to the 
;; approporiate statement (let, print, dim, etc.)
(define (interpret-statement statement)
              ;; pass to interpret-let
        (cond ((eqv? (first-elt-of statement) 'let)
              (interpret-let (second-elt-of statement) 
              (third-elt-of statement)))

              ;; pass to interpret-dim
              ((eqv? (first-elt-of statement) 'dim)
              (interpret-dim (first-elt-of (second-elt-of statement)) 
               (second-elt-of (second-elt-of statement))))
            
              ;; pass to interpret-goto
              ((eqv? (first-elt-of statement) 'goto)
                (interpret-goto (second-elt-of statement)))
              
              ;; pass to interpret-if
              ((eqv? (first-elt-of statement) 'if)
               (interpret-if (second-elt-of statement) 
                (third-elt-of statement)))
              
              ;; pass to interpret-input
              ((eqv? (first-elt-of statement) 'input)
               (interpret-input (rest-of statement)))
              
              ;; pass to interpret-print
              ((eqv? (first-elt-of statement) 'print)
               (interpret-print (rest-of statement)))
        )
)

;; eval-expr
;; an expression can be
;; a number
;; a symbol 
;; a pair
;;   where a pair can be an array subscript, i.e. (a max)
;;   or it can be a function, or else if its not found 
;;   in either the function or variable table, its an error
(define (eval-expr expr)
   ;; if number return the number
  (cond 
    ((number? expr) (+ expr 0.0))
    ;; if variable return the variable's value
    ((symbol? expr) (+ (hash-ref *variable-table* expr 0) 0.0))
    ;; if a pair, then either an array or function
    ((pair? expr)
    ;; if function return the function's val applied to arg(s)
    (if (hash-has-key? *function-table* (first-elt-of expr))
        (+ (apply (hash-ref *function-table* (first-elt-of expr)) 
               (map eval-expr (rest-of expr))) 0.0)
        ;; else if variable, return vector else return error
        (if (hash-has-key? *variable-table* (first-elt-of expr))
            ;; return vector subscript value
            (vector-ref (hash-ref *variable-table* (first-elt-of expr)) 
              (- (exact-round(eval-expr (second-elt-of expr))) 1))
                (die '(Invalid expression)))))))

;; (let variable expression)
;; let can update vector
;; so either var is a pair that is a vector
;; need to check if pair for let
;; var = (second-elt-of let)
;; expr = (third-elt-of let)
(define (interpret-let var expr)
  ;; either its an array or variable
  (cond ((symbol? var) 
        (hash-set! *variable-table* var (eval-expr expr)))
        ((pair? var) ;; check if its a vector
      ;; if vector is in hash table, update it
      ;; check if within bounds
      (if (and (hash-has-key? *variable-table* (first-elt-of var)) 
        (<= (- (eval-expr (second-elt-of var)) 1) 
        (vector-length (hash-ref *variable-table* (first-elt-of var)))))
          ;; set vector index to new value
        (vector-set! (hash-ref *variable-table* (first-elt-of var)) 
        (exact-round (- (eval-expr (second-elt-of var)) 1)) 
        (eval-expr expr))
          ;; else if its vector and not found, error!
        (printf "Error! Vector not found~n")))))

;; (dim variable expression)
;; generates a vector of size of resultant expression
(define (interpret-dim var expr) 
  (hash-set! *variable-table* var 
    (make-vector (abs (exact-round (eval-expr expr))))))

;; (goto Label)
;; either passes cdr of program referred to by label, or
;; halts interpreter
(define (interpret-goto label)
  (if (hash-has-key? *label-table* label)
      (interpret-prog (hash-ref *label-table* label))
      (die '("Error: jump to undeclared label."))))

;; (if (Relop expr expr) Label)
;; arglist = (Relop expr expr)
;; label = Label
(define (interpret-if arglist label)
  (when ((hash-ref *function-table* (first-elt-of arglist)) 
      (eval-expr (second-elt-of arglist)) 
      (eval-expr (third-elt-of arglist)))
      (interpret-goto label)))

;; (print {Printable})
;; where printable is either a String or an Expression
;; print newline if printable is null
;; printable is cadr of print
(define interpret-print
  (lambda (printable)
    (let print-next ((printable printable))
      (if (null? printable)
        (printf "~n")
        (begin
        (cond ((string? (first-elt-of printable))
              (display (first-elt-of printable)))
              (else (display (eval-expr (first-elt-of printable)))))
        (print-next (rest-of printable)))))))
                   
;; (input Memory {Memory})
;; where memory is an array or variable, 
;; followed by either 0 or more array or variables
;; arglist = (cdr input)
;; inputcount keeps track of values successfully read in, 
;; and when EOF becomes a sentinel value
(define interpret-input
  (lambda (arglist)
    (let interpret-in ((arglist arglist) (i 0))
      (if (not (null? arglist))
      (let* ((var (first-elt-of arglist)) (x (read)))
      (if (eof-object? x)
          (begin 
            (hash-set! *variable-table* 'inputcount -1))
          ;; assume all inputs are valid
          (if (number? x) 
          (cond  ((symbol? var) (hash-set! *variable-table* var x) 
          (interpret-in (rest-of arglist) (+ i 1)))
          ((pair? var) ;; check if its a vector
            ;; if vector is in hash table and within bounds, update it
          (if (and (hash-has-key? *variable-table* 
          (first-elt-of var)) (<= (- (eval-expr (second-elt-of var)) 1)
          (vector-length (first-elt-of (hash-ref *variable-table* 
          (first-elt-of var))))))
            ;; set vector index to new value
          (begin (vector-set! (hash-ref *variable-table* 
          (first-elt-of var)) (- (eval-expr (second-elt-of var)) 1) x)
          (interpret-in (rest-of arglist) (+ i 1)))
            ;; else if its vector and not found, error!
          (printf "Error! Vector not found~n"))))
          (begin (printf "Error! Input value NaN~n")
          (interpret-in arglist i)))))
      (hash-set! *variable-table* 'inputcount i)))))

;; intepret-prog (tail recursive function)
;; interpret-line 
;; extract statement from line (car program)
;; calls interpret_statement
(define interpret-prog 
  (lambda (program)
    (let interpret-next ((program program))
      ;; when its not the end of the program
      (when (not (null? program))
        ;; let the line be the car of the program, linenr = (car line)
        ;; linenr will be used for error reporting
        (let* ((line (first-elt-of program)) 
          (linenr (first-elt-of line)))
          ;; if second element is a label, 
          ;; skip it and pass statement to interpret statement
          (when (not (null? (rest-of line)))
          ;; interprets statement based on # of elements
          ;; in list
          (cond ((equal? (length line) 3)
                (interpret-statement (third-elt-of line)))
                ((pair? (second-elt-of line)) 
                  (interpret-statement (second-elt-of line)))))
          ;; exits with 0 if success
          (if (null? (cdr program)) (exit 0)
          (interpret-next (rest-of program))))))))
        

;; address we want label to reference is cdr of current program
;; something is a label if it is after line number, and not
;; a statement (let, print, etc.)
(define make-label-table 
  ;; define make-label-table as a lambda expression that accepts a list
  (lambda (program)
    ;; let find-labels be a recursive function (using named let)
    (let find-labels ((program program))
      ;; when its not yet the end of the program
      (when (not (null? program))
      ;; if length of the line is greater than 1
      (when (> (length (first-elt-of program)) 1)
        ;; when the line length is 2
        (if (equal? (length (first-elt-of program)) 2)
        ;; map second elt to label-table only if second elt isnt a list
        (unless (list? (second-elt-of (car program)))
        (hash-set! *label-table* 
          (second-elt-of (first-elt-of program)) program))
        ;; else its of length 3, thus second elt must be a label
        (hash-set! *label-table* 
          (second-elt-of (first-elt-of program)) program)))
        ;; make tail-recursive call to find-labels
        (find-labels (rest-of program))))))

;; name for standard error port
(define *stderr* (current-error-port))

; find existing path of the script and run file
(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath)))

;; for each first defines a function and then applies 
;; that to each element in a list
;; in this case, the list consists of strings to be displayed
;; and ends with a newline and exits
(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1))

;; note: `( a1 a1 ... , ai .... an) is a quasi quote, where
;; any occurence of , before an expression will evaluate it
;; instead of treating it as a literal, so in this case
;; its used to created a list and *run-file* is called
(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename")))

;; reads list from input file
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             ;; let program = lines from input file 
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

;; main where functions are called 
(define (main arglist)
  ;; if arglist is null or the second element is null, exit
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        ;; else let the sbprogfile file temporarily be the file name
        ;; where this will be pointing to the top level list
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
             ;; finds all labels
             (make-label-table program)
             ;; interprets the program
             (interpret-prog program))))

;; arguments read in as list
(main (vector->list (current-command-line-arguments)))
