(DEFPACKAGE :C-ENV (:CASE-SENSITIVE T) (:use :cl))
(IN-PACKAGE :C-ENV)

(DEFPARAMETER *block-indentation* 0)

(DEFUN REPLACE-CHAR (BEFORE AFTER STR)
  (LOOP FOR CHAR ACROSS STR
        IF (EQ CHAR BEFORE)
        COLLECT AFTER INTO STRING
        ELSE
        COLLECT CHAR INTO STRING
        FINALLY (RETURN (COERCE STRING 'STRING))))

(DEFMACRO defun (name type vars &BODY body)
  (LET* ((modifier NIL)
         (name (COND ((LISTP name)
                      (WHEN (> (LENGTH name) 2)
                        (ERROR "~S, function name list: ~s contains more than 2 items"
                               'defun name))
                      (LET ((nym (CAR name))
                            (MOD (CADR name)))
                        (UNLESS (OR (SYMBOLP nym)
                                    (STRINGP nym))
                          (ERROR "~S, FUNCTION name: ~S IS NOT A SYMBOL OR STRING" 'defun nym))
                        (UNLESS (SYMBOLP MOD)
                          (ERROR "~S ~S, FUNCTION modifier: ~S IS NOT A SYMBOL" 'defun nym))
                        (CASE MOD
                          ((:static :extern :auto :register)
                           (SETQ modifier (STRING-DOWNCASE MOD)))
                          (NIL NIL)
                          (ERROR "~S ~S, FUNCTION modifier: ~S IS NOT A VALID modifier"))
                        nym))
                     ((OR (SYMBOLP name)
                          (STRINGP name)) name)
                     (T (ERROR "~S: name ~S SHOULD BE SYMBOL OR STRING"
                               'defun)))))
    (FORMAT T "~@[~A ~]~A ~A(~{~A~^, ~})~:[;~;~]" 
            modifier (BUILD-C-SYMBOL type)
            (BUILD-C-SYMBOL name)
            (BUILD-PARAMS vars)
            body)
    (EVAL `(block NIL ,@body))
    (LET ((stripped-vars (REMOVE-IF
                           #'(LAMBDA (var) (CASE var
                                             ((&OPTIONAL &KEY &REST) T)
                                             (OTHERWISE NIL))) (MAPCAR #'LISPIFY-PARAM vars))))
      (IF (EQ (LENGTH stripped-vars) (LENGTH vars))
          NIL
          `(DEFMACRO ,name ,(MAPCAR #'LISPIFY-PARAM vars)
             (LIST
               'RESOLVE-EXPR
               ',(INTERN (STRING (BUILD-C-SYMBOL name)))
               ,@(MAPCAR #'(LAMBDA (var) (IF (LISTP var) (CAR var) var)) stripped-vars)))
        ))))


(DEFUN LISPIFY-PARAM (param)
  (LET* ((PARAM (IF (LISTP param) (CAR param) param))
         (PARAM (INTERN (STRING-UPCASE PARAM))))
    (IF (AND (LISTP param) (> (LENGTH param) 2)) (CONS PARAM (CDDR param)) PARAM)))

(DEFUN CAMELCASE (str &OPTIONAL (upper-case T))
  (LET ((UPCASE upper-case))
    (LOOP FOR CHAR ACROSS str
          WHEN (EQ CHAR #\-)
          DO (SETQ UPCASE T)
          UNLESS (EQ CHAR #\-)
          COLLECT (IF UPCASE
                      (PROGN
                        (SETQ UPCASE NIL)
                        (CHAR-UPCASE CHAR))
                      (CHAR-DOWNCASE CHAR)) INTO STRING
          FINALLY (RETURN (COERCE STRING 'STRING)))))

(DEFUN BUILD-C-SYMBOL (sym &KEY (case :snake-case))
  (LET ((sym (IF (SYMBOLP sym) (STRING (SYMBOL-NAME sym)) (STRING sym))))
    (CASE case
      (:snake-case (REPLACE-CHAR #\- #\_ sym))
      (:camelcase (CAMELCASE sym))
      (:lcamelcase (CAMELCASE sym NIL)))))

(DEFUN BUILD-PARAMS (vars)
  (UNLESS (LISTP vars) (ERROR "~S: expected ~S to be a list" 'BUILD-PARAMS vars))
  (LET* ((vars vars)
         (normal-args
           (LOOP FOR var = (CAR vars)
                 UNTIL (OR (NULL var)
                           (EQ '&optional var)
                           (EQ '&rest var)
                           (EQ '&key var))
                 COLLECT (POP vars) INTO args
                 FINALLY (RETURN args)))
         (optional-args
           (IF (EQ '&optional (CAR vars))
               (PROGN (POP vars)
                      (LOOP FOR var = (CAR vars)
                            UNTIL (OR (NULL var)
                                      (EQ '&rest var)
                                      (EQ '&key var))
                            COLLECT (POP vars) INTO args
                            FINALLY (RETURN args)))
               NIL))
         (rest-arg
           (IF (EQ '&rest (CAR vars))
               (PROGN (POP vars)
                      (POP vars))
               NIL))
         (key-args
           (IF (EQ '&key (CAR vars))
               (PROGN (POP vars)
                      (LOOP FOR var = (POP vars)
                            WHILE var
                            COLLECT var INTO args
                            FINALLY (RETURN args)))
               NIL)))
    (MAPCAR #'BUILD-VAR (CONCATENATE 'LIST normal-args optional-args key-args))))

(DEFUN BUILD-VAR (var)
  (WHEN (AND (CONSP var) (< (LENGTH var) 2))
    (ERROR "~S, variable list ~S: list length should be at least 2" 'BUILD-VAR var))
  (IF (CONSP var) (FORMAT NIL "~A ~A" (BUILD-C-SYMBOL (CADR var)) (BUILD-C-SYMBOL (CAR var)))))

(DEFMACRO call (name &REST args)
  `(PROGN
     (FORMAT T "~a(" ',name)
     ,@(IF args
           (LOOP FOR arg = (POP args)
                 WHILE arg
                 COLLECT (IF (LISTP arg)
                             `(RESOLVE-EXPR ,@arg)
                             `(RESOLVE-EXPR ,arg)) INTO BODY
                 WHEN (CAR args)
                 COLLECT `(FORMAT T ", ") INTO BODY
                 FINALLY (RETURN BODY)))
     (FORMAT T ")")
     T))


(DEFMACRO RESOLVE-EXPR (sym &REST args)
  (LET* ((args (IF (LISTP sym) (CDR sym) args))
         (sym (IF (LISTP sym) (CAR sym) sym)))
    (IF args
        (CASE sym
          (+ `(add ,@args))
          (- `(sub ,@args))
          (* `(mul ,@args))
          (/ `(div ,@args))
          ((< lt) `(logical "<" ,@args))
          ((<= le) `(logical "<=" ,@args))
          ((> gt) `(logical ">" ,@args))
          ((>= ge) `(logical ">=" ,@args))
          ((= eq) `(logical "==" ,@args))
          (OTHERWISE (IF (FBOUNDP sym)
                         `(,sym ,@args)
                         `(call ,sym ,@args))))
        (COND
          ((SYMBOLP sym)
           `(PROGN
              ,(CASE sym
                 ((t T) '(WRITE-STRING "true"))
                 ((nil NIL) '(WRITE-STRING "false"))
                 ((null NULL) '(WRITE-STRING "NULL"))
                 (OTHERWISE `(FORMAT T "~a"
                                     ,(IF (BOUNDP sym)
                                          (SYMBOL-VALUE sym)
                                          (BUILD-C-SYMBOL sym)))))
              T))
          ((STRINGP sym)
           `(PROGN
              (FORMAT T "\"~A\"" ,sym)
              T))
          (T `(PROGN
                (FORMAT T "~S" ,sym )
                T))))))

(DEFUN INDENT (level)
  (FORMAT T "~v@{~C~:*~}" level #\Space))

(DEFMACRO block (name &BODY body)
  `(PROGN
     (FORMAT T "~&~v@{~C~:*~}" *block-indentation* #\Space)
     (FORMAT T "~@[lisp_block_~A:~]{~%" ,name)
     (LET ((*block-indentation* (+ *block-indentation* 2)))
       ,@(MAPCAR #'(LAMBDA (expr) `(PROGN 
                                     (INDENT *block-indentation*)
                                     (WHEN 
                                         ,(IF (LISTP expr)
                                              `(RESOLVE-EXPR ,@expr)
                                              `(RESOLVE-EXPR ,expr))
                                       (FORMAT T ";~%")))) body))
     (FORMAT T "~&~v@{~C~:*~}}~%" *block-indentation* #\Space)))

(DEFMACRO return (value)
  `(PROGN
     (FORMAT T "return ")
     (RESOLVE-EXPR ,value)
     T))

(DEFMACRO dotimes (bind &BODY body)
  (LET ((bind-name (BUILD-C-SYMBOL (CAR bind))))
    `(PROGN
       (FORMAT T "for(int ~a = 0; ~a < " ,bind-name ,bind-name)
       (RESOLVE-EXPR ,@(CDR bind))
       (FORMAT T "; ~a++) " ,bind-name)
       (block NIL ,@body))))

(DEFUN header (path &OPTIONAL (type :system))
  (FORMAT T "#include ")
  (CASE type
    ((:local :absolute) (FORMAT T "\"~a\"" path))
    ((:system) (FORMAT T "<~a>" path)))
  (FORMAT T "~%"))

(DEFMACRO headers (&REST paths)
  `(PROGN
     ,@(MAPCAR #'(LAMBDA (path)
                   (IF (LISTP path)
                       `(header ,@path)
                       `(header ,path)))
               paths)
     (WRITE-LINE "")
     NIL))

(DEFMACRO aref (expr &REST indices)
  (WHEN (NULL indices) (WRITE-STRING "*"))
  `(PROGN
     (RESOLVE-EXPR ,expr)
     ,@(MAPCAR #'(LAMBDA (index)
                   `(PROGN
                      (WRITE-STRING "[")
                      (RESOLVE-EXPR ,index)
                      (WRITE-STRING "]")
                      )) indices)
     T))

(DEFMACRO div (&REST args)
  `(PROGN
     ,@(IF args
           `((WRITE-STRING "(")
             (RESOLVE-EXPR ,(CAR args))
             ,@(MAPCAR #'(LAMBDA (expr)
                           `(PROGN
                              (WRITE-STRING " / ")
                              (RESOLVE-EXPR ,expr))) (CDR args))
             (WRITE-STRING ")"))
           '((WRITE-STRING "1")))
     T))

(DEFMACRO mul (&REST args)
  `(PROGN
     ,@(IF args
           `((WRITE-STRING "(")
             (RESOLVE-EXPR ,(CAR args))
             ,@(MAPCAR #'(LAMBDA (expr)
                           `(PROGN
                              (WRITE-STRING " * ")
                              (RESOLVE-EXPR ,expr))) (CDR args))
             (WRITE-STRING ")"))
           '((WRITE-STRING "1")))
     T))

(DEFMACRO add (&REST args)
  `(PROGN
     ,@(IF args
           `((WRITE-STRING "(")
             (RESOLVE-EXPR ,(CAR args))
             ,@(MAPCAR #'(LAMBDA (expr)
                           `(PROGN
                              (WRITE-STRING " + ")
                              (RESOLVE-EXPR ,expr))) (CDR args))
             (WRITE-STRING ")"))
           '((WRITE-STRING "0")))
     T))

(DEFMACRO sub (first &REST args)
  `(PROGN
     ,@(IF args
           `((WRITE-STRING "(")
             (RESOLVE-EXPR ,first)
             ,@(MAPCAR #'(LAMBDA (expr)
                           `(PROGN
                              (WRITE-STRING " - ")
                              (RESOLVE-EXPR ,expr))) args)
             (WRITE-STRING ")"))
           `((WRITE-STRING "-")
             (RESOLVE-EXPR ,first)))
     T))

(DEFMACRO if (test if-stmt &OPTIONAL else-stmt)
  `(PROGN
     (WRITE-STRING "if(")
     (RESOLVE-EXPR ,test)
     (WRITE-STRING ")")
     (block NIL ,if-stmt)
     ,@(WHEN else-stmt
         `((WRITE-STRING "else")
           (block NIL ,else-stmt)))))

(DEFMACRO cond (first &REST stmts)
  `(PROGN
     (WRITE-STRING "if(")
     (RESOLVE-EXPR ,(CAR first))
     (WRITE-STRING ")")
     (block NIL ,@(CDR first))
     ,@(MAPCAR #'(LAMBDA (body)
                   `(PROGN
                      (WRITE-STRING "else if(")
                      (RESOLVE-EXPR ,(CAR body))
                      (WRITE-STRING ")")
                      (block NIL ,(CDR body))))
               stmts)))


(DEFUN comment (&REST comments)
  (MAPCAR #'(LAMBDA (comment) (FORMAT T "// ~a~%" comment)) comments)
  NIL)

(DEFMACRO defvar (name type &OPTIONAL value &KEY modifier)
  `(PROGN
     ,(WHEN modifier
        `(FORMAT T "~A " ,modifier))
     (WRITE-STRING (BUILD-VAR '(,name ,type)))
     ,@(WHEN value
         `((WRITE-STRING " = ")
           (RESOLVE-EXPR ,value)))
     T))

(DEFMACRO let (vars &BODY body)
  `(block NIL
     ,@(MAPCAR #'(LAMBDA (var) `(defvar ,@var)) vars)
     ,@body))

(DEFMACRO progn (&BODY body)
  `(block NIL ,@body))

(DEFMACRO mod (expr modulus)
  `(PROGN
     (WRITE-STRING "(")
     (RESOLVE-EXPR ,expr)
     (WRITE-STRING " % ")
     (RESOLVE-EXPR ,modulus)
     (WRITE-STRING ")")
     T))

(DEFMACRO do (test &BODY body)
  `(PROGN
     (WRITE-STRING "do")
     (block NIL ,@body)
     (INDENT ,*block-indentation*)
     (WRITE-STRING "while(")
     (RESOLVE-EXPR ,test)
     (WRITE-STRING ")")
     T))

(DEFMACRO while (test &BODY body)
  `(PROGN
     (WRITE-STRING "while(")
     (RESOLVE-EXPR ,test)
     (WRITE-STRING ")")
     (block NIL ,@body)
     T))

(DEFMACRO logical (op first second)
  `(PROGN
     (WRITE-STRING "(")
     (RESOLVE-EXPR ,first)
     (WRITE-STRING ,op)
     (RESOLVE-EXPR ,second)
     (WRITE-STRING ")")))

(DEFMACRO and (first second)
  `(logical "&&" ,first ,second))

(DEFMACRO or (first second)
  `(logical "||" ,first ,second))

(LET ((ALL-SYMBOLS '()))
  (DO-SYMBOLS (S (FIND-PACKAGE :C-ENV))
    (PUSH S ALL-SYMBOLS))
  (EXPORT ALL-SYMBOLS :C-ENV))
