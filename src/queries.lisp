(in-package :spiclum)

(defun find-all (class &key (strict nil))
  ;; TODO: Fix this bogus O(n) implementation.
  ;; Might be we need a :index key on all prevalence-objects
  ;; mapping to their direct classes. Then we can lookup the
  ;; relevant classes directly.
  (let ((value-hash
          (gethash 'uuid
                   (gethash 'prevalence-object
                            (hash-store *prevalence-system*)))))
    (loop for instance in (hash-values value-hash)
          for instance-class = (class-of instance)
          when (if strict
                   (eq instance-class class)
                   (c2mop:subclassp instance-class class))
            collect instance)))