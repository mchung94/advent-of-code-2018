(defun power-level (x y grid-serial-number)
  "Return the power level of the fuel cell at the given X, Y coordinates."
  (let* ((rack-id (+ x 10))
         (power (* rack-id y)))
    (incf power grid-serial-number)
    (setf power (* power rack-id))
    (setf power (rem (floor power 100) 10))
    (- power 5)))

(defun create-power-grid (grid-serial-number)
  "Create a 300x300 grid (ignore row/col 0) storing the fuel cell power levels."
  (loop with grid = (make-array '(301 301))
        for y from 1 to 300
        do (loop for x from 1 to 300
                 for power = (power-level x y grid-serial-number)
                 do (setf (aref grid x y) power))
        finally (return grid)))

(defun power-square (power-grid x y size &optional (accum 0 accum-supplied-p))
  "Return the power of the square of the given size with X, Y at the top left.
The optional accum parameter is (POWER-SQUARE POWER-GRID X Y (1- SIZE)), and
if it is passed in then we can just accumulate the last row/col to its value."
  (if accum-supplied-p
      (progn
        (loop with y-val = (+ y size -1)
              for x-val from x below (+ x size)
              do (incf accum (aref power-grid x-val y-val)))
        (loop with x-val = (+ x size -1)
              for y-val from y below (+ y size)
              do (incf accum (aref power-grid x-val y-val)))
        accum)
    (loop for y-val from y below (+ y size)
          sum (loop for x-val from x below (+ x size)
                    sum (aref power-grid x-val y-val)))))

(defun highest-power-3x3-square (power-grid)
  "Return the X, Y coordinates of the highest power 3x3 square."
  (loop with max-x and max-y and max-power
        for y from 1 to 298
        do (loop for x from 1 to 298
                 for power = (power-square power-grid x y 3)
                 when (or (not max-power) (< max-power power))
                 do (setf max-x x max-y y max-power power))
        finally (return (list max-x max-y))))

(defun part1 ()
  "Return the answer for Day 11, Part 1."
  (let ((answer (highest-power-3x3-square (create-power-grid 7989))))
    (format nil "~D,~D" (first answer) (second answer))))

(defun highest-power-square (power-grid)
  "Return the X, Y, and size of the highest power square of any size."
  (loop with max-x and max-y and max-power and max-size
        for y from 1 to 300
        do (loop for x from 1 to 300
                 do (loop for size from 1 to (min (- 300 (1- x)) (- 300 (1- y)))
                          for power = (aref power-grid x y) then (power-square power-grid x y size power)
                          when (or (not max-power) (< max-power power))
                          do (setf max-x x max-y y max-power power max-size size)))
        finally (return (list max-x max-y max-size))))

(defun part2 ()
  "Return the answer for Day 11, Part 2."
  (let ((answer (highest-power-square (create-power-grid 7989))))
    (format nil "~D,~D,~D" (first answer) (second answer) (third answer))))

(defun test-highest-power-3x3-square ()
  (assert (equal '(33 45) (highest-power-3x3-square (create-power-grid 18))))
  (assert (equal '(21 61) (highest-power-3x3-square (create-power-grid 42)))))

(defun test-highest-power-square ()
  (assert (equal '(90 269 16) (highest-power-square (create-power-grid 18))))
  (assert (equal '(232 251 12) (highest-power-square (create-power-grid 42)))))