(defun string->point (string)
  "Convert a string of X, Y coordinates into a cons containing X and Y."
  (let ((comma-pos (position #\, string)))
    (cons (parse-integer string :end comma-pos)
          (parse-integer string :start (1+ comma-pos)))))

(defun read-input (&optional (filename "input/day06.txt"))
  "Return a list of points from a file with X, Y coordinates on each line."
  (with-open-file (in filename :direction :input)
    (loop for line = (read-line in nil nil)
          while line
          collect (string->point line))))

(defun manhattan-distance (point1 point2)
  "Calculate the Manhattan distance between the two points."
  (+ (abs (- (car point1) (car point2)))
     (abs (- (cdr point1) (cdr point2)))))

(defun distances (point targets)
  "Return an a-list mapping each target to its distance from point."
  (loop for target in targets
        collect (cons target (manhattan-distance point target))))

(defun closest-target (point targets)
  "Return the target that's closest to point or NIL if there's a tie."
  (let ((sorted (sort (distances point targets) #'< :key #'cdr)))
    (when (or (null (rest sorted))
              (/= (cdr (first sorted)) (cdr (second sorted))))
      (car (first sorted)))))

(defun bounding-box (targets)
  "Return the left/right/top/bottom coords of the bounding box around targets."
  (let ((x (mapcar #'car targets))
        (y (mapcar #'cdr targets)))
    (list (reduce #'min x) (reduce #'max x) (reduce #'min y) (reduce #'max y))))

(defun all-points (left right top bottom)
  "Return a list of all points in the grid with the given boundaries."
  (loop for y from top to bottom
        nconc (loop for x from left to right
                    collect (cons x y))))

(defun areas (targets left right top bottom)
  "Return a mapping from targets to the bounding box points closest to it."
  (loop with target->points = (make-hash-table :test #'equal)
        for p in (all-points left right top bottom)
        for closest-target = (closest-target p targets)
        do (push p (gethash closest-target target->points))
        finally return target->points))

(defun largest-finite-area (targets)
  "Return the size of the largest finite area closest to a target."
  (destructuring-bind (left right top bottom) (bounding-box targets)
    (flet ((boundaryp (point)
             (let ((x (car point))
                   (y (cdr point)))
               (or (= x left) (= x right) (= y top) (= y bottom)))))
      (loop with target->points = (areas targets left right top bottom)
            for points being the hash-values in target->points
            unless (some #'boundaryp points)
            maximize (length points)))))

(defun part1 ()
  "Return the answer to Day 6, Part 1."
  (largest-finite-area (read-input)))

(defun size-of-area-under-limit (targets total-distance-limit)
  "Return the size of the region where the sum of distances to all targets is
under the limit."
  (loop with (left right top bottom) = (bounding-box targets)
        for p in (all-points left right top bottom)
        for total-distance = (reduce #'+ (distances p targets) :key #'cdr)
        when (< total-distance total-distance-limit)
        count p))

(defun part2 ()
  "Return the answer to Day 6, Part 2."
  (size-of-area-under-limit (read-input) 10000))

(defparameter *test-input*
  (mapcar #'string->point '("1, 1"
                            "1, 6"
                            "8, 3"
                            "3, 4"
                            "5, 5"
                            "8, 9")))

(defun test-largest-finite-area ()
  (assert (= 17 (largest-finite-area *test-input*))))

(defun test-size-of-area-under-limit ()
  (assert (= 16 (size-of-area-under-limit *test-input* 32))))