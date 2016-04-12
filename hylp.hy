;; Copyright (c) 2016 John Kitchin <jkitchin@andrew.cmu.edu>

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;;;; A module of help-related functions.
;;;;

(import hy)
(import re)

;;* Lists of keywords. These might be useful for editors too, for fontification.
(defn hy-language-keywords []
  "Return list of functions in hy.core.language"
  (. hy core language *exports*))

(defn hy-shadow-keywords []
  "Return list of shadowed functions"
  (. hy core shadow *exports*))

(defn hy-macro-keywords []
  "Return list of macro keywords"
  (.keys (get hy.macros._hy_macros nil)))

(defn hy-compiler-keywords []
  "Return a list of keywords defined in compiler.py with @build."
  (list-comp (get x 0) [x (hy.compiler._compile_table.items)]
             (string? (get x 0))))


;; (defmacro hylp-info [sym]
;;   "Return Usage, docstring filename, lineno for the string SYM."
;;   `(cond
;;     [(in ~sym (hy-language-keywords))
;;      (,  (.format
;;           "({0} {1})"
;;           ~sym
;;           (if (hasattr (. hy core language ~(HySymbol sym)) "__code__")
;;             (get-args
;;              (get-code
;;               (. hy core language ~(HySymbol sym) __code__ co_filename)
;;               (. hy core language ~(HySymbol sym) __code__ co_firstlineno)))
;;             "unknown args"))
;;          (. hy core language ~(HySymbol sym) __doc__)
;;          (if (hasattr (. hy core language ~(HySymbol sym)) "__code__")
;;            (. hy core language ~(HySymbol sym) __code__ co_filename)
;;            "no code")
;;          (if (hasattr (. hy core language ~(HySymbol sym)) "__code__")
;;            (. hy core language ~(HySymbol sym) __code__ co_firstlineno)
;;            -1))]

;;     [(in ~sym (hy-shadow-keywords))
;;      (,  (.format "({0} {1})"
;;                   ~sym
;;                   (get-args
;;                    (get-code
;;                     (. hy core shadow ~(HySymbol sym) __code__ co_filename)
;;                     (. hy core shadow ~(HySymbol sym) __code__ co_firstlineno))))
;;          (. hy core shadow ~(HySymbol sym) __doc__)
;;          (. hy core shadow ~(HySymbol sym) __code__ co_filename)
;;          (. hy core shadow ~(HySymbol sym) __code__ co_firstlineno))]

;;     [(in ~sym (hy-macro-keywords))
;;      (, (.format "({0} {1})"
;;                  ~sym
;;                  (get-args
;;                   (get-code
;;                    (. (get hy.macros._hy_macros nil ~sym) func_code co_filename)
;;                    (. (get hy.macros._hy_macros nil ~sym) func_code co_firstlineno))))
;;         (. (get hy.macros._hy_macros nil ~sym)  __doc__)
;;         (. (get hy.macros._hy_macros nil ~sym) func_code co_filename)
;;         (. (get hy.macros._hy_macros nil ~sym) func_code co_firstlineno))]

;;     [(in ~sym (.keys (hy-compiler-keywords)))
;;      (, (.format "{0} defined in hy/compiler" ~sym)
;;         "No docstring available."
;;         (get (get (hy-compiler-keywords) ~sym) 0)
;;         (get (get (hy-compiler-keywords) ~sym) 1))]

;;     [(= (. (type ~(HySymbol (.replace (string sym) "-" "_"))) __name__)
;;         "builtin_function_or_method")
;;      (, ~sym
;;         (. ~(HySymbol sym) __doc__)
;;         nil
;;         nil)]

;;     ;; Not found. Maybe a regular symbol from hy? or a python func?
;;     [true
;;      (let [SYM ~(HySymbol (.replace (string sym) "-" "_"))]
;;        (, ~sym
;;           (. SYM __doc__)
;;           (. SYM func_code co_filename)
;;           (. SYM func_code co_firstlineno)))]))


(defn hylp-info-f [sym]
  "Return Usage, docstring filename, lineno for the string SYM."
  (import hy)
  (cond
   [(in sym (hy-language-keywords))
    (,  (.format
         "({0} {1})"
         sym
         (if (hasattr
              (eval `(. hy core language ~(HySymbol sym))) "__code__")
           (get-args
            (get-code
             (eval `(. hy core language ~(HySymbol sym) __code__ co_filename))
             (eval `(. hy core language ~(HySymbol sym) __code__ co_firstlineno))))
           "unknown args"))
        (eval `(. hy core language ~(HySymbol sym) __doc__))
        (if (hasattr (eval `(. hy core language ~(HySymbol sym))) "__code__")
          (eval `(. hy core language ~(HySymbol sym) __code__ co_filename))
          "no code")
        (if (hasattr (eval `(. hy core language ~(HySymbol sym))) "__code__")
          (eval `(. hy core language ~(HySymbol sym) __code__ co_firstlineno))
          -1))]

   [(in sym (hy-shadow-keywords))
    (,  (.format "({0} {1})"
                 sym
                 (get-args
                  (get-code
                   (eval `(. hy core shadow ~(HySymbol sym) __code__ co_filename))
                   (eval `(. hy core shadow ~(HySymbol sym) __code__ co_firstlineno)))))
        (eval `(. hy core shadow ~(HySymbol sym) __doc__))
        (eval `(. hy core shadow ~(HySymbol sym) __code__ co_filename))
        (eval `(. hy core shadow ~(HySymbol sym) __code__ co_firstlineno)))]

   [(in sym (hy-macro-keywords))
    (, (.format
        "({0} {1})"
        sym
        (get-args
         (get-code
          ;; (eval `(. (get hy.macros._hy_macros nil ~(string sym))
          ;; func_code co_filename)) ugg.. I want hy to avoid this kind
          ;; of stuff!!! I cannot figure out how to make this work
          ;; without a macro. Unfortunately macros are causing some
          ;; other issue with hyldoc when I use require. So I am trying
          ;; to get this function approach to work.
          (eval
           (read-str
            (string
             (.format
              "(. (get hy.macros._hy_macros nil \"{0}\") func_code co_filename)"
              sym))))

          ;; (eval `(. (get hy.macros._hy_macros nil ~(string sym)) func_code co_firstlineno))
          (eval
           (read-str
            (string (.format
                     "(. (get hy.macros._hy_macros nil \"{0}\") func_code co_firstlineno)"
                     sym)))))))

       (eval
        (read-str (string
                   (.format
                    "(. (get hy.macros._hy_macros nil \"{0}\") __doc__)"
                    sym))))
       (eval
        (read-str (string
                   (.format
                    "(. (get hy.macros._hy_macros nil \"{0}\") func_code co_filename)"
                    sym))))

       (eval (read-str (string (.format
                                "(. (get hy.macros._hy_macros nil \"{0}\") func_code co_firstlineno)"
                                sym)))))]

   ;; Things like cut
   [(in sym (.keys (hy-compiler-keywords)))
    (, (.format "{0} defined in hy/compiler" sym)
       "No docstring available."
       (get (get (hy-compiler-keywords) sym) 0)
       (get (get (hy-compiler-keywords) sym) 1))]

   ;; Something that has a name attribute
   [(= (. (type (HySymbol (.replace (string sym) "-" "_"))) __name__)
       "builtin_function_or_method")
    (, sym
       (. (HySymbol sym) __doc__)
       nil
       nil)]

   ;; Not found. Maybe a regular symbol from hy? or a python func?
   [(let [SYM (HySymbol (.replace (string sym) "-" "_"))]
      (and (hasattr SYM "__doc__")
           (hasattr SYM "co_filename")
           (hasattr SYM "co_firstlineno")))
    (let [SYM (HySymbol (.replace (string sym) "-" "_"))]
      (, (.format "{0} not found." sym)
         (. SYM __doc__)
         (. SYM func_code co_filename)
         (. SYM func_code co_firstlineno)))]

   [true (, (.format "{0} not found." sym) "Not Found" nil nil)]))


(defn get-code [fname lineno]
  "Extract the code for the sexp in FNAME after LINENO."
  (when (and fname lineno)
    ;; first we get the line right before the function.
    (with [f (open fname)]
          (for [i (range (- lineno 1))]
            (.readline f))
          (setv state 0
                in-string False
                in-comment False
                s "("
                j 0
                ch ""
                pch "")

          ;; get to function start by reading forward to a (
          (while True
            (setv pch ch
                  ch (.read f 1))
            (when (= ch "(")
              (setv state 1)
              (break)))

          ;; now we read to the end closing ).
          (while (and (not (= 0 state)))
            (setv ch (.read f 1))
            (+= s ch)
            (cond
             ;; check for in -string, but not escaped "
             ;; we do not consider comments. () in comments will break this.
             [(and (not (= pch "\\")) (= ch "\""))
              (setv in-string (not in-string))]
             ;; comment line
             [(and (not in-string) (not (= pch "\\")) (= ch ";"))
              (setv in-comment True)]
             ;; break out of comment
             [(and in-comment (= ch "\n"))
              (setv in-comment False)]
             [(and (not in-string) (not in-comment) (= ch ")"))
              (setv state (- state 1))]
             [(and (not in-string) (not in-comment) (= ch "("))
              (+= state 1)]))
          s)))


(defn get-args [code-string]
  "Parse the args out of the CODE-STRING."
  (when code-string
    (let [state 0
          in-string False
          i 0
          args "["]
      (while True
        (setv ch (get code-string i))
        (when (= "[" ch)
          (setv state 1)
          (break))
        (+= i 1))

      (while (not (= 0 state))
        (+= i 1)
        (setv ch (get code-string i))
        (+= args ch)
        (cond
         [(and (= ch "[") (not in-string))
          (+= state 1)]
         [(and (= ch "]") (not in-string))
          (-= state 1)]))
      (setv args (.replace args "\n" ""))
      (setv args (re.sub " +" " " args))
      ;; cut off leading and trailing []
      (cut args 1 -1))))


;; I found the macro is necessary for this to work with hylp-indo-f. I don't
;; fully understand why.

;; (defmacro ? [sym]
;;   "Return help for SYM which is a string."
;;   `(let [flds (hylp-info ~sym)]
;;      (.format "Usage: {0}

;; {1}

;; [[{2}::{3}]]

;; "
;;               (get flds 0) ;; Usage
;;               (get flds 1) ;; docstring
;;               (get flds 2) ;; file
;;               (get flds 3) ;; lineno
;;               )))

(defn ? [sym]
  "Return help for SYM which is a string."
  (let [flds (hylp-info-f sym)]
    (.format "Usage: {0}

{1}

[[{2}::{3}]]

"
             (get flds 0)  ;; Usage
             (get flds 1)  ;; docstring
             (get flds 2)  ;; file
             (get flds 3)  ;; lineno
             )))


;; This function version works because of a lot of eval stuff in hylp-info-f
(defn hyldoc [sym]
  "Return an eldoc style string for the string SYM."
  (try
   (get (hylp-info-f sym) 0)
   (except [e Exception]
     "")))


(defn hyspy-file-lineno [sym]
  "Return filename and line number where sym is defined as an org-mode link."
  (let [info (hylp-info-f sym)]
    (.format "[[{0}::{1}]]" (get info 2) (get info 3))))
