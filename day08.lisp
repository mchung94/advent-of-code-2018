(defun read-input (&optional (filename "input/day08.txt"))
  "Read and return the list of numbers in the file."
  (with-open-file (in filename :direction :input)
    (loop for num = (read in nil nil)
          while num
          collect num)))

(defun read-node (numbers)
  "Read a node from the numbers and return it with the remaining unused numbers."
  (let* ((num-children (pop numbers))
         (num-metadata (pop numbers))
         (children ()))
    (dotimes (i num-children)
      (multiple-value-bind (child-node remaining-numbers) (read-node numbers)
        (push child-node children)
        (setf numbers remaining-numbers)))
    (values (cons (nreverse (coerce children 'vector))
                  (subseq numbers 0 num-metadata))
            (nthcdr num-metadata numbers))))

(defun sum-of-all-metadata (node)
  "Return the sum of all metadata in the node and its child nodes."
  (+ (reduce #'+ (cdr node))
     (reduce #'+ (car node) :key (lambda (child) (sum-of-all-metadata child)))))

(defun part1 ()
  "Return the answer for Day 8, Part 1."
  (sum-of-all-metadata (read-node (read-input))))

(defun node-value (node)
  "Return the value of the node."
  (let* ((children (car node))
         (metadata (cdr node))
         (num-children (length children)))
    (if (zerop num-children)
        (reduce #'+ metadata)
      (loop for i in (mapcar #'1- metadata)
            summing (if (<= 0 i (1- num-children))
                        (node-value (svref children i))
                      0)))))

(defun part2 ()
  "Return the answer for Day 8, Part 2."
  (node-value (read-node (read-input))))

(defparameter *test-input*
  '(2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))

(defun test-sum-of-all-metadata ()
  (assert (= 138 (sum-of-all-metadata (read-node *test-input*)))))

(defun test-node-value ()
  (assert (= 66 (node-value (read-node *test-input*)))))
