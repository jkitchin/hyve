
(defn parenthesize [input]
  "Fully parenthize the input string."
  (let [s ""]
    (+= s "((((")
    (for [(, i char) (enumerate input)]
      (cond
       [(= char "(")
        (+= s "((((")]
       [(= char ")")
        (+= s "))))")]
       ;; rewrite ^ to **
       [(= char "^")
        (+= s ")**(")]
       [(= char "*")
        (+= s "))*((")]
       [(= char "/")
        (+= s "))/((")]
       [(= char "+")
        (if (or (= 0 i) (in (get input (- i 1)) ["(" "^" "*" "/" "+" "-"]))
          (+= s "+ ")
          (+= s ")))+((("))]
       [(= char "-")
        (if (or (= 0 i) (in (get input (- i 1)) ["(" "^" "*" "/" "+" "-"]))
          (+= s "- ")
          (+= s ")))-((("))]
       [true
        (+= s char)]))
    (+= s "))))")
    s))

(defmacro NFX [code]
  "Evaluate the infix CODE.
CODE is stringified, parenthesized, read back and infixed."
  `(do
    (import infix)
    (import serialize)
    (eval (infix.nfx
           (read-str
            (infix.parenthesize
             (serialize.stringify ~code)))))))

(defreader m [code]
 `(do
    (import infix)
    (import serialize)
    (eval (infix.nfx
           (read-str
            (infix.parenthesize
             (serialize.stringify ~code)))))))

(def py-eval (get __builtins__ "eval"))

(defreader p [code]
  `(do
    (import [serialize [stringify]])
    (import [infix [py-eval]])
    (py-eval (stringify '~code))))

(defmacro py [code]
  `(do
    (import [serialize [stringify]])
    (import [infix [py-eval]])
    (py-eval (stringify '~code))))

(import [serialize [*]])

(defn nfx [code &optional [indent 0] [debug False]]
  "Transform the CODE expression to prefix notation.
We assume that CODE is in infix notation."
  (when debug (print (* " " indent) "code: " code " type: " (type code)))
  (cond
   [(coll? code)
    (cond

     ;; treat lists in [] special
     [(and (instance?  hy.models.list.HyList code)
           (not (instance?  hy.models.expression.HyExpression code)))
      (when debug (print "list: " code " type: " (type code)))
      code]

     [(= 1 (len code))
      ;; element is an Expression
      (when debug (print (* " " indent) "1: " code))
      (if (isinstance (car code) hy.models.expression.HyExpression)
        (nfx (car code) (+ indent 1) debug)
        ;; single element
        (car code))]

     ;; {- 1} ->  (- 1)
     [(= 2 (len code))
      (when debug (print (* " " indent) "2: " code))
      `(~(nfx (get code 0) (+ indent 1) debug)
         ~(nfx (get code 1) (+ indent 1) debug))]

     ;; {1 + 2} -> (+ 1 2)
     [(= 3 (len code))
      (when debug (print (* " " indent) "3: " code))
      `(~(get code 1)
         ~(nfx (get code 0) (+ indent 1) debug)
         ~(nfx (get code 2) (+ indent 1) debug))]

     ;; longer expression, swap first two and take the rest.
     [true
      (when debug (print "expr: " code))
      `(~(nfx (get code 1) (+ indent 1) debug)
         ~(nfx (get code 0) (+ indent 1) debug)
         (~@(nfx (cut code 2) (+ indent 1) debug)))])]

   ;; non-iterable just gets returned
   [true
    (when debug (print (* " " indent) "true: " code))
    code]))

(defmacro $ [&rest code]
  "Eval CODE in infix notation."
  `(do
    (import infix)
    (eval (infix.nfx ~code))))

(defreader $ [code]
  (import infix)
  (infix.nfx code))

(defreader P [code]
  `(do (import infix)
       (eval (infix.nfx ~code))))
