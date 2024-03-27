(in-package :spiclum)

;;;; misc

(defmacro defpclass (class-name superclasses slot-specifiers &rest class-options)
  "Programmer macro for defining prevalence classes.

Handles the PREVALENCE-CLASS metaclass & the PREVALENCE-OBJECT superclass.
See PREVALENCE-CLASS for documentation on slot-specifier options."
  (when (find :metaclass class-options :key #'car)
    (error "DEFPCLASS uses implicit metaclass PREVALENCE-CLASS,~%~
            but an explicit metaclass was provided for ~S" class-name))
  ;; NOTE: It'd also be possible to inject PREVALENCE-OBJECT at the ensure-class-using-metaclass level
  `(defclass ,class-name (,@superclasses prevalence-object)
     ,slot-specifiers
     ,@class-options
     (:metaclass prevalence-class)))

(defun delete-object (obj)
  "Deletes OBJ by removing it from the object-store.

Note that a correct deletion requires the application
to ensure that no prevalence-object holds a reference
to the deleted object."
  (check-type obj prevalence-object)
  (with-recursive-locks (all-prevalence-slot-locks-for obj)
    (prevalence-remove-instance obj)))

;;;; worlds

(defun save-world (&key directory name)
  "Save a new world as per DIRECTORY and NAME.

If DIRECTORY or NAME (or both) are nil, then read the values
from *PREVALENCE-SYSTEM*'s STORAGE-PATH with time-of-call.

Writes the set of class definitions for prevalence-classes,
and the set of instances of prevalence-object."
  (when (or directory name)
    (with-accessors ((storage-path storage-path)) *prevalence-system*
      (setf storage-path
            (make-pathname :directory (or directory
                                          (pathname-directory storage-path))
                           :name (or name
                                     (pathname-name storage-path))))
      (ensure-directories-exist storage-path)))
  (update-prevalence-system-for-timestamp *prevalence-system*
                                          (ANSI-time))
  (initialize-prevalence-system-files *prevalence-system*)
  (let ((*saving-world-p* t))
    (maphash
     (lambda (name last-definition)
       (declare (ignore name))
       (destructuring-bind (name &rest args) last-definition
         (key-args (name args) serialize :ensure-class-using-metaclass)))
     (class-definition-store *prevalence-system*))
    (dolist (instance (find-all (find-class 'prevalence-object)))
      (serialize :make-instance :instance instance)))
  :ok)

(defun load-world (&key directory name)
  "Loads a world/transaction log (or creates empty ones).

Initializes the object-store/prevalence-system.
DIRECTORY and NAME are required.
NAME is the name for the prevalence-system, a string.
DIRECTORY is a pathname representing the directory (and whatever else necessary) for where to store the prevalence-system."
  ;; e.g. :directory (user-homedir-pathname) :name "spiclum-test"
  (assert (and directory name))
  (setf *prevalence-system*
        (make-instance 'prevalence-system
                       :storage-path (merge-pathnames (make-pathname :name name)
                                                      directory)))
  (let ((*package* (find-package :spiclum))
        (*persisting-p* nil))
    (load (world-file *prevalence-system*))
    (load (log-file *prevalence-system*))
    (force-all-thunks)
    (reset-uuid-seed-for-object-store)))


;;;; multi-setf/multi-psetf

(defun standard-writer-method-for-obj-p (name obj)
  "Return the most specific standard-writer-method, if any.

NAME names a function (potentially). E.g. '+ or '(setf some-slot).
OBJ is an arbitrary object (for which NAME might be a writer-method)."
  (when (and (fboundp name)
             (typep (fdefinition name) 'c2mop:generic-function))
    (find-if (rfix #'typep 'c2mop:standard-writer-method)
             (c2mop::compute-applicable-methods
              (fdefinition name)
              (list t obj)))))

(defun place->lock-lookup (place)
  (cond
    ((atom place) nil)
    ((= 2 (length place))
     (destructuring-bind (fn-name obj) place
       (let ((method-var (gensym "method")))
         `(lwhen (,method-var (standard-writer-method-for-obj-p
                               '(setf ,fn-name) ,obj))
            (prevalence-slot-locks
             (class-of ,obj)
             (list (c2mop:accessor-method-slot-definition
                    ,method-var)))))))
    ((and (= 3 (length place))
          (eq (car place) 'slot-value))
     (destructuring-bind (fn-name obj slot-name) place
       (declare (ignore fn-name))
       `(prevalence-slot-locks
         (class-of ,obj)
         (list (slot-by-name (class-of ,obj) ,slot-name)))))
    ((and (= 4 (length place))
          (eq (car place) 'c2mop:slot-value-using-class))
     (destructuring-bind (fn-name class obj slot) place
       (declare (ignore fn-name obj))
       `(prevalence-slot-locks
         ,class
         (list (slot-by-name ,class (c2mop:slot-definition-name ,slot))))))))

(defun multi-p/setf-locks (locks indirects pairs)
  (assert (evenp (length pairs)) (pairs) "p/setf (and multi-p/setf) operates on pairs, but PAIRS was odd length")
  (let ((inferred-locks (loop for (place values) on pairs by #'cddr
                              for lookup = (place->lock-lookup place)
                              when lookup collect lookup))
        (indirect-locks (mapcar #'place->lock-lookup indirects)))
    (assert (every #'identity indirect-locks) (indirects)
            "Couldn't make sense of all INDIRECTS to MULTI-SETF.~
             See PLACE->LOCK-LOOKUP. Received: ~S" indirects)
    `(remove nil (append ,@inferred-locks ,@indirect-locks ,locks))))

(defmacro map-digested-pairs (&body body)
  "Assumes DIGESTED-PAIRS is lexically bound.

As a list of lists (gensym :setf-expansion (vars vals store-vars writer-form reader-form)
                           :values-form values-form).
Makes (gensym vars vals store-vars writer-form reader-form values-form setf-expansion) available in BODY."
  `(mapcar (lambda (digested-pair)
             (destructuring-bind (gensym &key setf-expansion values-form)
                 digested-pair
               (declare (ignorable values-form))
               (destructuring-bind (vars vals store-vars writer-form reader-form)
                   setf-expansion
                 (declare (ignorable vars vals store-vars writer-form reader-form))
                 ,@body)))
           digested-pairs))

(defmacro multi-setf ((&key locks indirects) &body pairs)
  "Atomically setf the places in PAIRS (given LOCKS/INDIRECTS are sufficient).

PAIRS is as for SETF.

INDIRECTS is a list of places that access prevalence-objects that must be atomic
inside the MULTI-SETF. E.g. if a helper function accesses a set of slots that must
be accessed atomically, provide those as INDIRECTS. e.g. :indirects ((slot1 obj1) ... (slotn objn)).

LOCKS is a list of locks for non-prevalence-objects that are also necessary for atomic operation."
  (let* ((all-locks (multi-p/setf-locks locks indirects pairs))
         (return-var (gensym "return"))
         (digested-pairs
           (loop for (place values) on pairs by #'cddr
                 collect (list
                          (gensym)
                          :setf-expansion (multiple-value-list
                                           (get-setf-expansion place))
                          :values-form values))))
    `(let (,@(map-digested-pairs gensym) ,return-var)
       (with-recursive-locks ,all-locks
         (as-transaction
             (,@(map-digested-pairs
                  `(:do
                    (let* ,(mapcar #'list vars vals)
                      (setf ,gensym (multiple-value-list ,reader-form))
                      (setf ,return-var (multiple-value-list ,values-form))
                      (multiple-value-bind ,store-vars
                          (values-list ,return-var)
                        ,writer-form))
                    :undo
                    (let* ,(mapcar #'list vars vals)
                      (multiple-value-bind ,store-vars (values-list ,gensym)
                        ,writer-form)))))
           ;; We serialize through letting the individual setfs serialize.
           ;; This is transactive modulo interrupts.
           (values-list ,return-var))))))

(defmacro multi-psetf ((&key locks indirects) &body pairs)
  "As MULTI-SETF except psetf-like instead of setf-like."
  (let* ((all-locks (multi-p/setf-locks locks indirects pairs))
         (digested-pairs
           (loop for (place values) on pairs by #'cddr
                 collect (list
                          (gensym)
                          :setf-expansion (multiple-value-list
                                           (get-setf-expansion place))
                          :values-form values))))
    `(let (,@(map-digested-pairs gensym))
       (with-recursive-locks ,all-locks
         ,@(map-digested-pairs
             `(setf (getf ,gensym :old)
                    (let* ,(mapcar #'list vars vals)
                      (multiple-value-list ,reader-form))))
         ,@(map-digested-pairs
             `(setf (getf ,gensym :new)
                    (multiple-value-list ,values-form)))
         (as-transaction
             ((:do nil :undo (progn
                               ,@(map-digested-pairs
                                   `(let* ,(mapcar #'list vars vals)
                                      (multiple-value-bind ,store-vars
                                          (values-list (getf ,gensym :old))
                                        ,writer-form)))))
              (:undo nil :do (progn
                               ,@(map-digested-pairs
                                   `(let* ,(mapcar #'list vars vals)
                                      (multiple-value-bind ,store-vars
                                          (values-list (getf ,gensym :new))
                                        ,writer-form))))))
           nil)))))
