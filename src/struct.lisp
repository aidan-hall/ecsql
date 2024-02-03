;;; Struct metadata vector layout:
;;; vector(size struct-id members)
;;; Members:
;;; list(offset name-string type-symbol)

;;; → (list total-size offsets)
(defun struct-generate-offsets (members prefix offset)
  (let ((offsets nil))
    
    (while members
      (let* ((elt (car members))
             (type (cadr elt))
             (member-size (size-of type))
             (size 0)
             (name (concat prefix "-" (car elt))))
        
        (setq offsets (cons (list
                             offset
                             name
                             type)
                            offsets))
        (if (structp type)
            (setq offsets
                  (nconc (cadr (struct-generate-offsets
                                (struct-metadata-members (struct-metadata type))
                                name offset))
                         offsets)))
        
        (incq offset member-size)

        (setq members (cdr members))))
    
    (list offset offsets)))

(defun struct-metadata-size (metadata)
  (aref metadata 0))
(defun struct-metadata-id (metadata)
  (aref metadata 1))
(defun struct-metadata-members (metadata)
  (aref metadata 2))

;;; Generate getters and setters for a struct from a description of the offsets
(defun struct-accessors (struct-type offsets)
  (let* ((my-metadata (struct-metadata struct-type))
         (my-name (symbol-name struct-type))
         (my-id (struct-metadata-id my-metadata))
         (my-member-names (mapcar (function car) (struct-metadata-members my-metadata)))
         (my-member-symbols (mapcar (function intern) my-member-names))
         (my-size (struct-metadata-size my-metadata)))
    (if (>= (length my-name) 128)
        (wrong "struct name is way too long" struct-type)
      `(progn
         ;; A special method to copy a whole struct.
         (defun ,(intern (concat "copy-" my-name)) (dest src)
           (let ((dest-type (type-of dest)))
             (assert (eq dest-type ',struct-type))
             (assert (eq dest-type (type-of src))))
           (--struct-set-vec dest 0 src ,(struct-metadata-size my-metadata)))
         ;; Constructor
         (defun ,(intern (concat "make-" my-name))
             ;; Produce argument list
             ,my-member-symbols
           ;; Allocate the structure
           (let ((me (--struct-allocate ,my-size ,my-id)))
             ;; Assign the initial values
             (progn
               . ,(mapcar
                   (lambda (member-name)
                     `(,(intern (concat "set-" my-name "-" member-name))
                       me
                       ,(intern member-name)))
                   my-member-names))
             ;; Return the produced struct
             me))
         ;; Printer
         (defun ,(intern (concat "print-" my-name "-to")) (stream obj)
           (assert (eq (type-of obj) ',struct-type))
           (print-to stream
                     (list ',struct-type
                           . ,(mapcar
                               (lambda (member-name)
                                 `(,(intern (concat "get-" my-name "-" member-name)) obj))
                               my-member-names))))
         ;; Getters and setters
         . ,(mapcar
             (lambda (entry)
               ;; entry: (offset name-string type)
               (let ((offset (car entry))
                     (name (cadr entry))
                     (type (caddr entry)))
                 (let ((is-struct-type `(assert (eq (type-of structure) ',struct-type)))
                       (is-member-type `(assert (eq (type-of value) ',type)))
                       (getter (intern (concat "get-" name)))
                       (setter (intern (concat "set-" name))))
                   (if (structp type)
                       (let ((size (struct-metadata-size (struct-metadata type))))
                         `(progn
                            (defun ,getter (structure)
                              ,is-struct-type
                              (--struct-get-vec structure ,offset ,size))
                            (defun ,setter (structure value)
                              ,is-struct-type
                              ,is-member-type
                              (--struct-set-vec structure ,offset value ,size))))
                     `(progn
                        (defun ,getter (structure)
                          ,is-struct-type
                          (--struct-get-cell structure ,offset))
                        (defun ,setter (structure value)
                          ,is-struct-type
                          ,is-member-type
                          (--struct-set-cell structure ,offset value)))))))
             offsets)))))

(defun print-struct-to (stream obj)
  (assert (structp obj))
  (funcall (intern (concat "print-" (type-of obj) "-to")) stream obj))

(defun struct-install (name size offsets)
  (let ((id (--struct-register name)))
    (defname 'struct name (vector size id offsets))
    id))

(defmacro defstruct (name . members)
  (let* ((stringy-members
          (mapcar
           (lambda (member)
             (list (symbol-name (car member)) (cadr member)))
           members))
         (size-and-offsets (struct-generate-offsets stringy-members (symbol-name name) 0)))
    (struct-install name (car size-and-offsets) stringy-members)
    (struct-accessors name (cadr size-and-offsets))))

;;; Temporary stuff for testing
;; (struct-install 'v2f 2 '(("x" f32) ("y" f32)))
