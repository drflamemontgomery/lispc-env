(headers "stdio.h" "stdlib.h")

(comment "Hello World"
         "Have A Comment")

(DEFMACRO macroexpand-n (n expr)
  (IF (< n 1) expr
      `(macroexpand-n ,(- n 1) (MACROEXPAND-1 ,expr))))

(defun test-mul int ((a int) &optional (b int (- 2)))
  (return (* a b)))

(defun main int ((argc int) (*argv[] char))
  (let ((num-of-args int (- argc 1)))
    (printf "Number of args %d\\n" num-of-args)
    (comment "Do Something")
    (dotimes (i num-of-args)
      (printf "Arg %d: %s\\n" (/ i  1) (aref argv (+ i 1)))))

  (test-mul 4)
  (return argc))

