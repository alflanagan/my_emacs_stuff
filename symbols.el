(defun fred (arg) (+ 17 arg))
h(setq fred "wilma")

(prin1 (symbolp 'fred))

(prin1 (symbol-name 'fred))

(prin1 (symbol-plist 'fred))

(prin1 (symbol-function 'fred))

(prin1 (symbol-value 'fred))

