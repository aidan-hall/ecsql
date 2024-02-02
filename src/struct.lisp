;;; Struct metadata vector layout:
;;; (vector size offsets)
;;; Offset layout:
;;; (cons name offset)

;;; → (list total-size offsets)
(defun struct-generate-offsets (members offset)
  (let ((offsets (make-vector (length members) nil))
        (i 0))
    
    (while members
      (let* ((elt (car members))
             (member-size (size-of (cadr elt)))
             (size 0))
        
        (aset offsets i (cons size elt))
        
        (incq offset member-size)

        (setq members (cdr members))
        (incq i 1)))
    
    (list offset offsets)))
