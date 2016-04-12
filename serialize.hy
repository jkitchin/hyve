
(import hy)

(defn stringify [form &optional debug]
  "Convert a FORM to a string."
  (when debug (print (.format "{0}: {1}" form (type form))))
  (cond
   [(isinstance form hy.models.expression.HyExpression)
    (+ "(" (.join " " (list-comp (stringify x debug) [x form])) ")")]
   [(isinstance form hy.models.dict.HyDict)
    (+ "{" (.join " " (list-comp (stringify x debug) [x form])) "}")]
   [(isinstance form hy.models.list.HyList)
    (+ "[" (.join " " (list-comp (stringify x debug) [x form])) "]")]
   [(isinstance form hy.models.symbol.HySymbol)
    (.format "{}" form)]
   [(isinstance form hy.models.keyword.HyKeyword)
    ;; these have some unicode prefix I want to remove
    (.format "{}" (cut form 1))]
   [(or (isinstance form hy.models.string.HyString)
        (isinstance form unicode))
    (.format "\"{}\"" form)]
   [true
    (.format "{}" form)]))

(defn scriptify [form fname]
  (with [f (open fname "w")]
        (.write f "#!/usr/bin/env hy\n")
        (.write f (stringify form)))
  (import os)
  (os.chmod fname 0o755))

(import [hy.importer :as hi])
(import [hy._compat [PY3 PY33 MAGIC wr_long long_type]])
(import marshal)
(import os)

(defn hy2pyc [code fname]
  "Write CODE as Python compiled byte-code in FNAME."

  (setv program (stringify code))

  (setv _ast (hi.import_buffer_to_ast
              program
              "main"))

  (setv code (hi.ast_compile _ast "<string>" "exec"))

  ;; create file and close it so we get the size
  (with [f (open fname "wb")] nil)
  (with [f (open fname "wb")]
        (try
         (setv st (os.fstat (f.fileno)))
         (except [e AttributeError]
           (setv st (os.stat fname))))
        (setv timestamp (long_type (. st st_mtime))))
  (with [fc (open fname "wb")]
        (if PY3
          (.write fc b"\0\0\0\0") ; I amnot sure this is right in hy with b""
          (.write fc "\0\0\0\0"))
        (wr_long fc timestamp)
        (when PY33
          (wr_long fc st.st_size))
        (.dump marshal code fc)
        (.flush fc)
        (.seek fc 0 0)
        (.write fc MAGIC)))
