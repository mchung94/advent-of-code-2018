(defun read-input (&optional (filename "input/day01.txt"))
  "Return a list of the frequency changes in the file."
  (with-open-file (in filename :direction :input)
    (loop for change = (read in nil nil)
          while change
          collect change)))

(defun resulting-frequency (frequency-changes)
  "Return the frequency starting from zero and applying all the changes."
  (reduce #'+ frequency-changes))

(defun part1 ()
  "Return the answer for Day 1, Part 1."
  (resulting-frequency (read-input)))

(defun first-repeated-frequency (frequency-changes)
  "Return the first frequency reached twice, repeating change list until found."
  (let ((frequency 0)
        (seen (make-hash-table)))
    (loop
     (dolist (change frequency-changes)
       (when (gethash frequency seen)
         (return-from first-repeated-frequency frequency))
       (setf (gethash frequency seen) t)
       (incf frequency change)))))

(defun part2 ()
  "Return the answer for Day 1, Part 2."
  (first-repeated-frequency (read-input)))

(defun test-resulting-frequency ()
  (assert (= 3 (resulting-frequency '(+1 -2 +3 +1))))
  (assert (= 3 (resulting-frequency '(+1 +1 +1))))
  (assert (= 0 (resulting-frequency '(+1 +1 -2))))
  (assert (= -6 (resulting-frequency '(-1 -2 -3)))))

(defun test-first-repeated-frequency ()
  (assert (= 2 (first-repeated-frequency '(+1 -2 +3 +1))))
  (assert (= 0 (first-repeated-frequency '(+1 -1))))
  (assert (= 10 (first-repeated-frequency '(+3 +3 +4 -2 -4))))
  (assert (= 5 (first-repeated-frequency '(-6 +3 +8 +5 -6))))
  (assert (= 14 (first-repeated-frequency '(+7 +7 -2 -7 -4)))))
