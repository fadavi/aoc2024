(defvar +input-file-path+ "../inputs/day10.txt")
(defvar +empty-hash-table+ (make-hash-table))

(defun hash-table-str (ht)
  "TODO: remove this function as soon as the implementation is done"
  (with-output-to-string (out)
    (write-char #\{ out)
    (let ((first t))
      (maphash (lambda (key value)
                 (unless first
                   (write-string ", " out))
                 (setf first nil)
                 (format out "~a: ~a" key value))
               ht))
    (write-char #\} out)))

(defstruct topo rows cols data reachable-peaks)

(defun read-lines (file-path)
  (let ((file (open file-path)))
    (loop for line = (read-line file nil)
	  for vect = (map 'list #'digit-char-p line)
	  while vect
	  collect vect)))

(defun point-hash (y x)
  (+ (* 1000 y) x))

(defun topo-parse (data)
  (let* ((rows (length data))
	 (cols (length (car data)))
	 (dims `(,rows ,cols)))
    (make-topo
     :rows rows
     :cols cols
     :data (make-array dims :initial-contents data)
     :reachable-peaks (make-array dims :initial-element nil))))

(defun hash-table-append (dest &rest hash-tables)
  (dolist (ht hash-tables)
    (maphash
     (lambda (k v) (setf (gethash k dest) v))
     ht)))

(defun topo-height-at (topo y x)
  (when (topo-contains-p topo y x)
    (aref (topo-data topo) y x)))

(defun topo-contains-p (topo y x)
  (and (< -1 y (topo-rows topo))
       (< -1 x (topo-cols topo))))

(defun topo-reachable-peaks-at (topo y x &optional (prev-height -1))
  (let* ((reachable-peaks (topo-reachable-peaks topo))
	 (height (topo-height-at topo y x)))
    (cond
      ((or (not (topo-contains-p topo y x))
	   (/= height (1+ prev-height)))
       +empty-hash-table+)
      ((aref reachable-peaks y x)
       (aref reachable-peaks y x))
      ((= height 9)
       (let ((peaks (make-hash-table)))
	 (setf (gethash `(,y ,x) peaks) t)
	 (setf (aref reachable-peaks y x) peaks)
	 peaks))
      (t
       (let ((peaks (make-hash-table)))
	 (hash-table-append
	  peaks
	  (topo-reachable-peaks-at topo (1- y) x height)
	  (topo-reachable-peaks-at topo (1+ y) x height)
	  (topo-reachable-peaks-at topo y (1- x) height)
	  (topo-reachable-peaks-at topo y (1+ x) height))
	 (setf (aref reachable-peaks y x) peaks)
	 peaks)))))

(defun topo-total-score (topo)
  (loop for y from 0 below (topo-rows topo)
	sum (loop for x from 0 below (topo-cols topo)
		  for height = (topo-height-at topo y x)
		  when (zerop height)
		    sum (hash-table-count (topo-reachable-peaks-at topo y x)))))

(defun main ()
  (let* ((lines (read-lines +input-file-path+))
	 (topo (topo-parse lines))
	 (total-score (topo-total-score topo)))
    (format t "Part 1 - Total Score: ~D~%" total-score)))

(mapcar #'compile '(read-lines
		    topo-parse
		    hash-table-append
		    topo-height-at
		    topo-contains-p
		    topo-reachable-peaks-at
		    topo-total-score
		    main))

(let* ((time0 (get-internal-real-time))
       (_ (main))
       (time1 (get-internal-real-time))
       (elapsed (- time1 time0))
       (elapsed-micros (* 1000000 (/ elapsed (float internal-time-units-per-second)))))
  (format t "Elapsed Time: ~,3f microseconds~%" elapsed-micros))
