(in-package :spiclum)

(5am:in-suite :prevalence.integration)

(5am:test :save/load-world
  (let* ((directory (user-homedir-pathname))
         (name "spiclum-test")
         (path (merge-pathnames (make-pathname :name name) directory))
         (real-prevalence-system *prevalence-system*))
    (flet ((list-files (type)
             (list-directory-for-type
              (uiop/pathname:pathname-directory-pathname path)
              type)))
      (ensure-directories-exist path)
      (mapc #'uiop:delete-file-if-exists
            (mapappend #'list-files (list *world-filename* *log-filename*)))
      (load-world :directory directory :name name)
      (5am:is-true (typep *prevalence-system* 'prevalence-system)
                   "*prevalence-system* not bound to a prevalence-system~
                  after loading (empty/non-existent) world")
      (defpclass some-class ()
        ((a :initarg :a
            :accessor a
            :key :unique
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
        (load-world :directory directory :name name)
        (let ((new-instance1 (find-by-uuid 1))
              (new-instance2 (find-by-uuid 2)))
          (5am:is (not (eq instance1 new-instance1)))
          (5am:is (not (eq instance2 new-instance2)))
          (5am:is (eq (a instance1) (a new-instance1)))
          (5am:is (equalp (b instance1) (b new-instance1)))
          (5am:is (equalp (b instance2) (b new-instance2)))
          (5am:is (eq instance1 (a instance2)))
          (5am:is (eq new-instance1 (a new-instance2))))))
    (setf *prevalence-system* real-prevalence-system)))

(5am:test :prevalent-can-inherit-from-standard
  (let* ((directory (user-homedir-pathname))
         (name "spiclum-test")
         (path (merge-pathnames (make-pathname :name name) directory))
         (real-prevalence-system *prevalence-system*))
    (flet ((list-files (type)
             (list-directory-for-type
              (uiop/pathname:pathname-directory-pathname path)
              type)))
      (ensure-directories-exist path)
      (mapc #'uiop:delete-file-if-exists
            (mapappend #'list-files (list *world-filename* *log-filename*)))
      (load-world :directory directory :name name)
      (ignore-errors (setf (find-class 'some-standard-class) nil))
      (ignore-errors (setf (find-class 'some-prevalent-class) nil))
      (5am:is (zerop (hash-table-count (class-definition-store *prevalence-system*))))
      (eval '(defpclass some-prevalent-class (some-standard-class)
              ((a
                :initarg :a
                :accessor a
                :equality #'equal
                :key :unique))))
      (5am:is (= 1 (hash-table-count (class-definition-store *prevalence-system*))))
      (eval '(defclass some-standard-class ()
              ((some-slot
                :accessor some-slot
                :initarg :some-slot))))
      (5am:is (= 2 (hash-table-count (class-definition-store *prevalence-system*))))
      (let ((obj (make-instance 'some-prevalent-class
                                :a '(1 2 3)
                                :some-slot 'ok)))
        (5am:is (typep obj 'some-prevalent-class))
        (5am:is (typep obj 'some-standard-class))
        (5am:is (typep obj 'prevalence-object))
        (5am:is (typep (class-of obj) 'prevalence-class))
        (5am:is (= 1 (length (query :class some-prevalent-class))))
        (5am:is (eq obj (car (query :class some-prevalent-class))))
        (5am:is (null (query :class some-standard-class :strict t)))
        (5am:is (eq obj (car (query :class some-prevalent-class
                                    :where (a '(1 2 3))))))
        ;; This isn't a slot lookup but rather a big filter across
        ;; all instances of some-standard-class. Seems I implemented
        ;; a very general search mechanism...
        (5am:is (eq obj (car (query :class some-standard-class
                                    :where (some-slot 'ok))))))
      (ignore-errors (setf (find-class 'some-standard-class) nil))
      (ignore-errors (setf (find-class 'some-prevalent-class) nil))
      (setf *prevalence-system* nil)
      (load-world :directory directory :name name)
      (5am:is (= 2 (hash-table-count (class-definition-store *prevalence-system*))))
      (let ((objs (query :class some-standard-class)))
        (5am:is (= 1 (length objs)))
        (5am:is (equal '(1 2 3) (a (car objs))))
        (5am:is (equal 'ok (some-slot (car objs)))))
      (setf *prevalence-system* real-prevalence-system))))
