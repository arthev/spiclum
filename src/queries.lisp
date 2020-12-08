(in-package :spiclum)

;; Need to support... select and delete.
;; Let's keep it simple and only design for select for now.

;; (query :select :all
;;        :class left
;;        :strict t
;;        :where (:and (i-top 5)
;;                     (some-slot #'some-fn)
;;                     (:or (pu-left (partial #'< -5 :? 5))
;;                          (middle  (partial #'<= -5 :? 5)))))


(defun compound-form-p (form)
  (member (car form) '(:and :or :not)))

(defun and-form-p (form)
  (eq :and (car form)))

(defun or-form-p (form)
  (eq :or (car form)))

(defun not-form-p (form)
  (eq :not (car form)))

(defun form-compound (form)
  (car form))

(defun compound-form-children (form)
  (assert (compound-form-p form))
  (cdr form))

(defun direct-slot-filters (form)
  (labels ((internal (form)
             (cond ((not (compound-form-p form))
                    form)
                   ((and-form-p form)
                    (remove-if #'compound-form-p
                               (compound-form-children form))))))
    (mklist-of-lists (internal form))))

(defun initial-set-filters (select class forms)
  (loop for (slot-name comp-value) in forms
        for slotd = (slot-by-name class slot-name)
        when (and (eq (key slotd) (if (eq select :all)
                                      :index
                                      :precedence-unique))
                  (not (functionp comp-value)))
          collect (list slotd comp-value)))

(defun smallest-initial-set (class strict filters)
  (let ((best nil)
        (score nil))
    (loop for (slotd comp-value) in filters
          ;; This probably breaks if we ever try to do something
          ;; clever/efficient for class-unique keys
          for set = (mklist (prevalence-lookup-class-slot
                             (find-slot-defining-class class slotd)
                             slotd
                             comp-value))
          for filtered-set = (if strict
                                 (remove-if (complement (rfix #'eq class))
                                            set
                                            :key #'class-of)
                                 set)
          for length = (length filtered-set)
          when (or (null score)
                   (< length score))
            do (setf best filtered-set
                     score length))
    best))

(defun compute-initial-set (select class strict where)
  (let* ((direct-filters (direct-slot-filters where))
         (candidate-filters (initial-set-filters select class direct-filters)))
    (if candidate-filters
        (smallest-initial-set class strict candidate-filters)
        (find-all class :strict strict))))

(defun satisfies-simple-filter-p (instance filter class)
  (destructuring-bind (slot-name comp-value) filter
    (let ((slot-value (slot-value instance slot-name)))
      (if (functionp comp-value)
          (funcall comp-value slot-value)
          (funcall (equality (slot-by-name class slot-name))
                   slot-value
                   comp-value)))))

(defun satisfies-filter-p (instance where class)
  (labels ((%satisfies-filter-p (form)
             (cond ((null form)
                    t)
                   ((compound-form-p form)
                    (funcall (cond ((and-form-p form) #'every)
                                   ((or-form-p form) #'some)
                                   ((not-form-p form) #'notany))
                             #'%satisfies-filter-p (compound-form-children form)))
                   (t
                    (satisfies-simple-filter-p instance form class)))))
    (%satisfies-filter-p where)))

(defun call-query (&key select class strict where)
  (let ((initial-set (compute-initial-set select class strict where))
        (satisfies-p (rfix #'satisfies-filter-p where class)))
    (if (eq :all select)
        (remove-if (complement satisfies-p) initial-set)
        (find-if satisfies-p initial-set))))

(defun well-formed-query-where-p (form &key recursive-p)
  (cond ((and (not recursive-p)
              (null form))
         t)
        ((compound-form-p form)
         (and (if (not-form-p form)
                  (= 1 (length (compound-form-children form)))
                  t)
              (every (rfix #'well-formed-query-where-p :recursive-p t)
                     (compound-form-children form))))
        ((and (= 2 (length form))
              (symbolp (car form)))
         t)))

(defun canonicalize-query-where (form)
  (cond ((null form)
         nil)
        ((compound-form-p form)
         `(list ,(form-compound form)
                ,@(mapcar #'canonicalize-query-where
                          (compound-form-children form))))
        (t
         (destructuring-bind (slot-symbol comp-form) form
           `(list ',slot-symbol ,comp-form)))))

(defmacro query (&key (select :all) class (strict nil) where)
  (assert (and class (symbolp class)))
  (assert (well-formed-query-where-p where))
  `(call-query :select ,select
               :class (find-class ',class)
               :strict ,strict
               :where ,(canonicalize-query-where where)))

(defun find-all (class &key (strict nil))
  (let* ((relevant-classes (if strict (mklist class) (all-subclasses class)))
         (p-o-class (find-class 'prevalence-object))
         (r-c-l-slotd (slot-by-name p-o-class 'reverse-class-lookup)))
    (loop for class in relevant-classes
          append (prevalence-lookup-class-slot p-o-class r-c-l-slotd class))))

(defun find-by-uuid (uuid)
  (let ((class (find-class 'prevalence-object)))
    (prevalence-lookup-class-slot class (slot-by-name class 'uuid) uuid)))
