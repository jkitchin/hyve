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

;; Utility macros

(defmacro unwind-protect [do-form &rest body]
  "Execute DO-FORM, making sure that BODY is finally executed
even if DO-FORM raises an exception. Any exceptions are re-raised
after BODY is executed.

This macro is inspired from the similar emacs-lisp macro. It might not be
exactly the same, here we just make a nicer syntax to catch exceptions."
  `(try
    ~do-form
    (except [e Exception] (print "Caught " e))
    (finally (do ~@body
                 (raise)))))


(defmacro with-current-directory [directory &rest body]
  "Change to DIRECTORY, execute BODY and change back to original directory."
  (let [cwd (gensym)]
    `(do
      (import os)
      (let [~cwd (os.getcwd)]
        (os.chdir ~directory)
        (unwind-protect
         (do ~@body)
         (os.chdir ~cwd))))))
