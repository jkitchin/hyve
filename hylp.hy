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

;;;; Provides macros to retrieve the docstrings, files and linenumbers where Hy
;;;; objects are defined. These are used to build help strings

(import hy)
(import pydoc)
(import inspect)

;; * Lists of keywords. These might be useful for editors too, for fontification.
(defn hy-language-keywords []
  "Return list of functions in hy.core.language"
  (. hy core language *exports*))


(defn hy-shadow-keywords []
  "Return list of shadowed functions"
  (. hy core shadow *exports*))


(defn hy-macro-keywords []
  "Return list of macro keywords"
  (+ (.keys (get hy.macros._hy_macros nil))
     (.keys (get hy.macros._hy_macros "__main__"))
     (.keys (get hy.macros._hy_macros "__console__"))))


(defn hy-compiler-keywords []
  "Return a list of keywords defined in compiler.py with @build."
  (list-comp (get x 0) [x (hy.compiler._compile_table.items)]
             (string? (get x 0))))


(defmacro hy? [sym]
  "Is SYM defined in hy?"
  `(in ~(name sym) (+ (hy-compiler-keywords)
                      (hy-macro-keywords)
                      (hy-shadow-keywords)
                      (hy-language-keywords))))


(defmacro do-import [sym]
  "Import the base module of SYM.
If SYM has a . in it, e.g. numpy.sum, import numpy.
This is so we can get docstrings on symbols in hy code in an
editor where the whole import sequence may not be executed in
the repl. There could be side effects from this macro."
  `(do
    (import hy)
    (cond
     ;; We catch these cases before we do anything.
     ;; the base is already in our namespace do nothing.
     [(in ~(get (.split (name sym) ".") 0) (.keys (globals)))
      nil]
     ;; A hy symbol we know about. we don't need to do anything.
     [(in ~(name sym) (+ (hy-compiler-keywords)
                         (hy-macro-keywords)
                         (hy-shadow-keywords)
                         (hy-language-keywords)))
      nil]
     ;; A dotted name where the base is not in the namespace. Try importing.
     [(and (in "." ~(name sym))
           (not (in ~(get (.split (name sym) ".") 0) (.keys (globals)))))
      (try
       (do
        (import ~(hy.models.symbol.HySymbol
                  (get (.split (name sym) ".") 0)))
        (print "imported " ~(get (.split (name sym) ".") 0)))
       (except [e ImportError] (print e)))]
     ;; Don't do anything at the end.
     [true
      nil])))


(defmacro get-python-object [sym]
  "Get the Python object for the symbol SYM.
SYM is a function or module.
If SYM has a . in it, import the base module."
  `(do
    (do-import ~sym)
    (try
     (or (->> false
              (.get hy.compiler._compile_table '~sym)
              (.get (get hy.macros._hy_macros nil) '~sym)
              (.get (get hy.macros._hy_macros "__main__") '~sym)
              (.get (get hy.macros._hy_macros "__console__") '~sym)
              (.get hy.core.shadow.__dict__ '~sym)
              (.get hy.core.language.__dict__ '~sym))
         ~sym)
     (except [e NameError] None))))


;; * Get the docstring
(defmacro getdoc [sym]
  "Get the docstring for the symbol SYM."
  `(pydoc.getdoc (get-python-object ~sym)))


;; * Source files where symbols are defined
(defmacro getsourcefile [sym]
  "Return the source file where symbol SYM is defined."
  `(do
    (import inspect)
    (inspect.getsourcefile (get-python-object ~sym))))


;; * Linenumbers of objects in files
(defmacro getlineno [sym]
  "Return the first line number where SYM is defined.
SYM should be a function or module."
  `(do
    (cond
     ;; special case for compiler functions because the function is checker, not
     ;; the one we want. we store __hylineno__ in the checkarg decorator.
     [(and (.get hy.compiler._compile_table '~sym)
           (hasattr (.get hy.compiler._compile_table '~sym) "__hylineno__"))
      (. (.get hy.compiler._compile_table '~sym) __hylineno__)]
     [(inspect.isfunction (get-python-object ~sym))
      (getattr (. (get-python-object ~sym) func_code) "co_firstlineno")]
     ;; For modules we return first line
     [(inspect.ismodule (get-python-object ~sym))
      1]
     ;; We don't know what this is
     [true
      nil])))


;; * Args of functions
(defmacro getargs [sym]
  "Return a string representing the args."
  ;; inspect.getargspec(func)

  ;; Get the names and default values of a Python function’s arguments. A tuple
  ;; of four things is returned: (args, varargs, keywords, defaults). args is a
  ;; list of the argument names (it may contain nested lists). varargs and
  ;; keywords are the names of the * and ** arguments or None. defaults is a
  ;; tuple of default argument values or None if there are no default arguments
  ;; if this tuple has n elements, they correspond to the last n elements listed
  ;; in args.

  `(do
    (import inspect)
    (let [argspec (inspect.getargspec (get-python-object ~sym))
          args (. argspec args)
          varargs (. argspec varargs)
          keywords (. argspec keywords)
          defaults (. argspec defaults)]

      ;; default values are in argspec.defaults and the correspond to the last n
      ;; variables in args. So if there are defaults, we should reverse args,
      ;; and replace them with [arg value]
      (when defaults
        (setv args (list (reversed args)))
        (for [ (, i value) (enumerate defaults)]
          (assoc args i (.format "[{0} ({1})]" (get args i) value)))
        (setv args (list (reversed args))))

      (.format "{0}{1}{2}"
               (.join " " (or args '("")))
               (if varargs (.format " [&rest {0}]" varargs) "")
               (if keywords (.format " [&kwargs {0}]" keywords) "") ))))


;; * hyldoc
(defmacro get-org-link [sym]
  "Return an org-mode link to the file location where SYM is defined."
  `(.format "[[{0}::{1}]]"
            (getsourcefile ~(get-python-object sym))
            (getlineno ~(get-python-object sym))))


(defmacro ?? [sym]
  "Return help string for the symbol SYM."
  `(do
    (require hy)
    (.format "{0} defined in {1}

  ({0} {2})

{3}" ~(name sym) (get-org-link ~sym) (getargs ~sym) (getdoc ~sym))))


(defmacro ? [sym]
  "Return an eldoc string for lispy C-1 for the symbol SYM."
  ;; `(try
  ;;   (.format "({0} {1})" ~(name sym) (getargs ~sym))
  ;;   (except [e Exception]
  ;;     (.format "{} not found." ~(name sym))))
  `(.format "({0} {1})" ~(name sym) (getargs ~sym)))
