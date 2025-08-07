(load #P"/home/looking/quicklisp/setup.lisp")
(ql:quickload "uiop")

(load "c-env.lisp")

(defun change-file-extension (filepath &key ext)
  (let ((pn (pathname filepath)))
    (make-pathname :host (pathname-host pn)
                   :device (pathname-device pn)
                   :directory (pathname-directory pn)
                   :name (pathname-name pn)
                   :type ext
                   :version (pathname-version pn))))

(defun write-c (file)
  (with-output-to-string (s)
      (let ((*standard-output* s)
            (*package* (find-package :c-env)))
        (load file :verbose nil))))

    
(defun lisp-to-c (file &optional outfile)
  (let ((outfile (if outfile outfile
                     (change-file-extension file :ext "c"))))
    (with-open-file (out outfile :direction :output :if-does-not-exist :create :if-exists :supersede)
      (write-string (write-c file) out)
      (format t "~A: Compiled C to ~A~%" file outfile)
      )))

(defun cwf (file)
  (format t "~A" (write-c file))
  (values))

(defmacro def (a b) `(setf ,a (if ,a ,a ,b)))

(defun compile-lisp (file &key c-file outfile compiler flags)
  (let ((c-file (if c-file c-file
                     (change-file-extension file :ext "c"))))
    (lisp-to-c file c-file)
    (uiop:run-program (format nil "~:[gcc~;~a~] ~a ~@[-o ~a~] ~@[~a~]" compiler c-file outfile flags)
                      :error-output :string :ignore-error-status t)))

(defun compile-and-run-lisp (file &key c-file outfile compiler flags)
  (compile-lisp file :c-file c-file :outfile outfile :compiler compiler :flags flags)
  (uiop:run-program (format nil "./~:[a.out~;~a~]" outfile outfile)
                    :error-output :string :ignore-error-status t))


(defun build ()
  (compile-lisp "test.lisp" :c-file "test.c" :outfile "test")
  (values))

(defun run (&optional (program "./test") &rest args)
  (format t "~a" (uiop:run-program
                   (format nil "~a~{ ~a~}" program args)
                   :error-output :string :ignore-error-status t :output :string))
  (values))

(defun build-and-run ()
  (build)
  (run))

(defun clear-c-env ()
  (delete-package :c-env)
  (load "c-env.lisp"))
