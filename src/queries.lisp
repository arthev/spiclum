(in-package :spiclum)

(defun find-all (class &key (strict nil))
  ;; TODO: Fix this bogus O(n) implementation.
  ;; Might be we need a :index key on all prevalence-objects
  ;; mapping to their direct classes. Then we can lookup the
  ;; relevant classes directly.
  (let ((slot-table (prevalence-lookup-store 'prevalence-object 'uuid)))
    (loop for instance being the hash-values of slot-table
          for instance-class = (class-of instance)
          when (if strict
                   (eq instance-class class)
                   (c2mop:subclassp instance-class class))
            collect instance)))

(defun find-by-uuid (uuid)
  (let ((class (find-class 'prevalence-object)))
    (prevalence-lookup-class-slot class (slot-by-name class 'uuid) uuid)))
