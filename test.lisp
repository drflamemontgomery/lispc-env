(headers "stdio.h" "stdlib.h")

(comment "Hello World"
         "Have A Comment")

(defun test-mul int ((a int) &optional (b int (- 2)))
  (return (* a b)))

(defun main int ((argc int) (*argv[] char))
  (let ((num-of-args int (- argc 1)))
    (printf "Number of args %d\\n" num-of-args)
    (comment "Do Something")
    (dotimes (i num-of-args)
      (printf "Arg %d: %s\\n" (/ i  1) (aref argv (+ i 1)))))

  (printf "test-mul 4 => %d\\n" (test-mul 4))
  (printf "test-mul 4 4 => %d\\n" (test-mul 4 4))
  (return argc))

