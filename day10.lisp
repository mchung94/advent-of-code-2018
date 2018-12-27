(defstruct point
  "A point moving through the sky with position and velocity."
  x y dx dy)

(defun create-point (string)
  "Return a point given a string description."
  (let* ((open1 (position #\< string))
         (comma1 (position #\, string :start open1))
         (close1 (position #\> string :start comma1))
         (open2 (position #\< string :start close1))
         (comma2 (position #\, string :start open2))
         (close2 (position #\> string :start comma2)))
    (make-point :x (parse-integer string :start (1+ open1) :end comma1)
                :y (parse-integer string :start (1+ comma1) :end close1)
                :dx (parse-integer string :start (1+ open2) :end comma2)
                :dy (parse-integer string :start (1+ comma2) :end close2))))

(defun read-input (&optional (filename "input/day10.txt"))
  "Return a list of points from the file."
  (with-open-file (in filename :direction :input)
    (loop for line = (read-line in nil nil)
          while line
          collect (create-point line))))

(defun move-points (points seconds)
  "Update the points to their new position after a number of seconds."
  (dolist (point points points)
    (incf (point-x point) (* seconds (point-dx point)))
    (incf (point-y point) (* seconds (point-dy point)))))

(defun bounding-box (points)
  "Return the left/right/top/bottom of the minimal bounding box around points."
  (let ((x (mapcar #'point-x points))
        (y (mapcar #'point-y points)))
    (list (reduce #'min x) (reduce #'max x) (reduce #'min y) (reduce #'max y))))

(defun area (points)
  "Return the size of the minimal bounding box around the points."
  (destructuring-bind (left right top bottom) (bounding-box points)
    (* (- right left) (- bottom top))))

(defun print-points (points)
  "Print the points on a grid in their current position."
  (destructuring-bind (left right top bottom) (bounding-box points)
    (let* ((width (- right left))
           (height (- bottom top))
           (dimensions (list (1+ height) (1+ width)))
           (grid (make-array dimensions :initial-element #\.)))
      (dolist (point points)
        (setf (aref grid (- (point-y point) top) (- (point-x point) left)) #\#))
      (loop for y from 0 to height
            do (loop for x from 0 to width
                     do (format t "~C" (aref grid y x))
                     finally (format t "~%"))))))

(defun print-message (points)
  "Print the message the points create, return the number of seconds to wait."
  (loop with points-copy = (mapcar #'copy-point points)
        for seconds from 0
        while (>= (area points-copy) (area (move-points points-copy 1)))
        finally (return (progn
			  (print-points (move-points points-copy -1))
			  seconds))))

(defun part1and2 ()
  "Print the answer for Day 10, Part 1, and return the answer for Part 2."
  (print-message (read-input)))

(defparameter *test-input*
  (mapcar #'create-point '("position=< 9,  1> velocity=< 0,  2>"
                           "position=< 7,  0> velocity=<-1,  0>"
                           "position=< 3, -2> velocity=<-1,  1>"
                           "position=< 6, 10> velocity=<-2, -1>"
                           "position=< 2, -4> velocity=< 2,  2>"
                           "position=<-6, 10> velocity=< 2, -2>"
                           "position=< 1,  8> velocity=< 1, -1>"
                           "position=< 1,  7> velocity=< 1,  0>"
                           "position=<-3, 11> velocity=< 1, -2>"
                           "position=< 7,  6> velocity=<-1, -1>"
                           "position=<-2,  3> velocity=< 1,  0>"
                           "position=<-4,  3> velocity=< 2,  0>"
                           "position=<10, -3> velocity=<-1,  1>"
                           "position=< 5, 11> velocity=< 1, -2>"
                           "position=< 4,  7> velocity=< 0, -1>"
                           "position=< 8, -2> velocity=< 0,  1>"
                           "position=<15,  0> velocity=<-2,  0>"
                           "position=< 1,  6> velocity=< 1,  0>"
                           "position=< 8,  9> velocity=< 0, -1>"
                           "position=< 3,  3> velocity=<-1,  1>"
                           "position=< 0,  5> velocity=< 0, -1>"
                           "position=<-2,  2> velocity=< 2,  0>"
                           "position=< 5, -2> velocity=< 1,  2>"
                           "position=< 1,  4> velocity=< 2,  1>"
                           "position=<-2,  7> velocity=< 2, -2>"
                           "position=< 3,  6> velocity=<-1, -1>"
                           "position=< 5,  0> velocity=< 1,  0>"
                           "position=<-6,  0> velocity=< 2,  0>"
                           "position=< 5,  9> velocity=< 1, -2>"
                           "position=<14,  7> velocity=<-2,  0>"
                           "position=<-3,  6> velocity=< 2, -1>")))

(defun test-print-message ()
  (assert (= 3 (print-message *test-input*))))
