(require 'mocker)

(mocker-let
    ((compilation-start
      (command &optional mode name-function highlight-regexp)
      ((:record-cls 'mocker-stub-record :output nil))))

  (ag "foo" "/"))
