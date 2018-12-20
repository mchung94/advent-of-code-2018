(defstruct claim
  "An elf's claim on an area of fabric."
  id left top width height coordinates)

(defun coordinates (left top width height)
  "Return a list of X, Y coordinates covering the given rectangle."
  (loop for y from top below (+ top height)
        nconc (loop for x from left below (+ left width)
                    collect (cons x y))))

(defun create-claim (string)
  "Return a claim given a claim string like '#1 @ 1,3: 4x4'."
  (let* ((number-pos (position #\# string))
         (at-pos (position #\@ string :start number-pos))
         (comma-pos (position #\, string :start at-pos))
         (colon-pos (position #\: string :start comma-pos))
         (x-pos (position #\x string :start colon-pos))
         (id (parse-integer string :start (1+ number-pos) :end at-pos))
         (left (parse-integer string :start (1+ at-pos) :end comma-pos))
         (top (parse-integer string :start (1+ comma-pos) :end colon-pos))
         (width (parse-integer string :start (1+ colon-pos) :end x-pos))
         (height (parse-integer string :start (1+ x-pos))))
    (make-claim :id id :left left :top top :width width :height height
                :coordinates (coordinates left top width height))))

(defun read-input (&optional (filename "input/day03.txt"))
  "Return a list of claims in the file."
  (with-open-file (in filename :direction :input)
    (loop for line = (read-line in nil nil)
          while line
          collect (create-claim line))))

(defun overlapped-coordinates (claims)
  "Return the X, Y coordinates that are within more than one claim."
  (let ((map (make-hash-table :test #'equal)))
    (dolist (claim claims)
      (dolist (coords (claim-coordinates claim))
        (if (gethash coords map)
            (incf (gethash coords map))
          (setf (gethash coords map) 1))))
    (maphash (lambda (k v) (unless (> v 1) (remhash k map))) map)
    map))

(defun part1 ()
  "Return the answer for Day 3, Part 1."
  (hash-table-count (overlapped-coordinates (read-input))))

(defun non-overlapped-claim (claims)
  "Return the claim that doesn't overlap with any other claim."
  (let ((overlapped (overlapped-coordinates claims)))
    (dolist (claim claims)
      (when (notany (lambda (coords) (gethash coords overlapped))
                    (claim-coordinates claim))
        (return claim)))))

(defun part2 ()
  "Return the answer for Day 3, Part 2."
  (claim-id (non-overlapped-claim (read-input))))

(defparameter *test-input*
  (mapcar #'create-claim '("#1 @ 1,3: 4x4"
                           "#2 @ 3,1: 4x4"
                           "#3 @ 5,5: 2x2")))

(defun test-overlapped-coordinates ()
  (assert (= 4 (hash-table-count (overlapped-coordinates *test-input*)))))

(defun test-non-overlapped-claim ()
  (assert (= 3 (claim-id (non-overlapped-claim *test-input*)))))
