(in-package :spiclum)

(5am:in-suite :prevalence.integration)

(5am:test :save/load-world
  (let* ((directory  "/home/arthur/spiclum-test")
         (name "spiclum-test")
         (path (make-pathname :directory directory :name name)))
    (flet ((list-files (type)
             (list-directory-for-type
              (cl-fad:pathname-directory-pathname path)
              type)))
      (ensure-directories-exist path)
      (mapc #'uiop:delete-file-if-exists
            (mapappend #'list-files (list *world-filename* *log-filename*)))
      (load-world directory name)
      (5am:is-true (typep *prevalence-system* 'prevalence-system)
                   "*prevalence-system* not bound to a prevalence-system~
                  after loading (empty/non-existent) world")
      (defpclass some-class ()
        ((a :initarg :a
            :accessor a
            :key :precedence-unique
            :equality #'equalp)
         (b :initarg :b
            :accessor b
            :key :index
            :equality #'equalp)))
      (let* ((instance1 (make-instance 'some-class
                                       :a 'yoodle
                                       :b (list 1 2 3 #(1 2 3))))
             (instance2 (make-instance 'some-class
                                       :a instance1
                                       :b (list 1 2 3 #(1 2 3)))))
        (mapc #'check-lookup-finds-object (list instance1 instance2))
        (5am:is-true (singlep (list-files *world-filename*)))
        (5am:is-true (singlep (list-files *log-filename*)))
        (sleep 1) ; Since file timestamps are only accurate to the second
        (save-world)
        (5am:is (= 2 (length (list-files *world-filename*))))
        (5am:is (= 2 (length (list-files *log-filename*))))
        (load-world directory name)
        (let ((new-instance1 (find-by-uuid 1))
              (new-instance2 (find-by-uuid 2)))
          (5am:is (not (eq instance1 new-instance1)))
          (5am:is (not (eq instance2 new-instance2)))
          (5am:is (eq (a instance1) (a new-instance1)))
          (5am:is (equalp (b instance1) (b new-instance1)))
          (5am:is (equalp (b instance2) (b new-instance2)))
          (5am:is (eq instance1 (a instance2)))
          (5am:is (eq new-instance1 (a new-instance2))))))))
