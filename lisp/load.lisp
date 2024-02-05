(defname 'function 'load
         (lambda (name)
           ((lambda (f)
              ((lambda (form)
                 (while (eq (eq form eof) nil)
                   (eval form)
                   (setq form (read-stream f))))
               (read-stream f))
              (fclose f))
            (fopen name "r"))
           ;; Disgusting hack: load loads print the first time it is called.
           (print (list "Loaded: " name))))
