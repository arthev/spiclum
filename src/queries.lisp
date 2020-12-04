(in-package :spiclum)

;; Provide a decent set of ways to query objects - e.g. 'get all objects of class' and 'get all objects of class and subclasses'.
;; Maybe something like:
;; (query (:select :all
;;         :class left
;;         :strict t
;;         :where (:and (i-top (= _ 5))
;;                      (:or (pu-left (< -5 _ 5))
;;                           (middle  (<= -5 _ 5))))
;;         :sort (<= :key middle)))

;; Since slots have EQUALITY, maybe e.g.
;;         :where (:and (i-top 5) ...)
;;   could expand to
;;         :where (:and (i-top (lambda (x) (funcall (equality slotd) x 5))))
;;   ?

;; (query (:select :the
;;         :class top
;;         :where (pu-top (= _ 'yoodle))))

;; Must provide a way to remove objects from the object store
;; (at programmer layer). Complex things like cascading removals etc.
;; can be skipped: complex to know what makes for good semantics
;; there with the indexes and consistency etc.
;; So leave that up to programmer code.
;; Removals probably need serializing, though. (In general, any query action
;; with side effects will probably need serializing.)


(defun find-all (class &key (strict nil))
  (let* ((relevant-classes (if strict (mklist class) (all-subclasses class)))
         (p-o-class (find-class 'prevalence-object))
         (r-c-l-slotd (slot-by-name p-o-class 'reverse-class-lookup)))
    (loop for class in relevant-classes
          append (prevalence-lookup-class-slot p-o-class r-c-l-slotd class))))

(defun find-by-uuid (uuid)
  (let ((class (find-class 'prevalence-object)))
    (prevalence-lookup-class-slot class (slot-by-name class 'uuid) uuid)))
