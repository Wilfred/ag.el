(defmacro with-mock-func (func-name mock &rest body)
  "Evaluate BODY with the function FUNC-NAME (a symbol) bound to MOCK.
Returns the last value in BODY, same as `progn'."
  `(let ((old-func-value (symbol-function ',func-name)))
    (prog2
        (fset ',func-name ,mock)
        (progn ,@body)
      (fset ',func-name old-func-value))))

(defun add-one (x) (1+ x))

(with-mock-func add-one (lambda (x) (* x 2))
                (add-one 5))

(mocker-let
    ((compilation-start
      (command &optional mode name-function highlight-regexp)
      ((:record-cls 'mocker-stub-record :output nil))))

  (ag "foo" "/"))
