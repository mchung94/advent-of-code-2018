(defun parse-game-description (string)
  "Return the number of players and the last marble from the description."
  (list (parse-integer string :end (position #\space string))
        (parse-integer string
                       :start (+ (search "worth " string) 6)
                       :end (search "points" string))))

(defun read-input (&optional (filename "input/day09.txt"))
  "Return the number of players and the last marble from the file."
  (with-open-file (in filename :direction :input)
    (parse-game-description (read-line in nil nil))))

(defstruct node
  "A node in a circular doubly-linked list."
  value prev next)

(defun make-circle ()
  "Create a new marble game circle with a single marble numbered zero in it."
  (let ((node (make-node :value 0)))
    (setf (node-prev node) node
          (node-next node) node)))

(defun clockwise (node steps)
  "Return the node that is the number of steps clockwise from the node."
  (let ((result-node node))
    (dotimes (i steps result-node)
      (setf result-node (node-next result-node)))))

(defun counter-clockwise (node steps)
  "Return the node that is the number of steps counter-clockwise from the node."
  (let ((result-node node))
    (dotimes (i steps result-node)
      (setf result-node (node-prev result-node)))))

(defun play-marble-game (num-players last-marble)
  "Play the marble game and return every player's score."
  (loop with scores = (make-array num-players :initial-element 0)
        with current-node = (make-circle)
        for marble from 1 to last-marble
        for player = (rem (1- marble) num-players)
        do (cond ((zerop (rem marble 23))
                  (setf current-node (counter-clockwise current-node 6))
                  (incf (svref scores player)
                        (+ marble (node-value (node-prev current-node))))
                  (setf (node-prev current-node)
                        (counter-clockwise current-node 2)
                        (node-next (node-prev current-node))
                        current-node))
                 (t
                  (let ((new-node (make-node :value marble
                                             :next (clockwise current-node 2)
                                             :prev (clockwise current-node 1))))
                    (setf (node-next (node-prev new-node)) new-node
                          (node-prev (node-next new-node)) new-node
                          current-node new-node))))
        finally return scores))

(defun highest-score (num-players last-marble)
  "Return the highest score after finishing the marble game."
  (reduce #'max (play-marble-game num-players last-marble)))

(defun part1 ()
  "Return the answer for Day 9, Part 1."
  (let* ((args (read-input))
         (num-players (first args))
         (last-marble (second args)))
    (highest-score num-players last-marble)))

(defun part2 ()
  "Return the answer for Day 9, Part 2."
  (let* ((args (read-input))
         (num-players (first args))
         (last-marble (* 100 (second args))))
    (highest-score num-players last-marble)))

(defun test-highest-score ()
  (assert (= 32 (highest-score 9 25)))
  (assert (= 8317 (highest-score 10 1618)))
  (assert (= 146373 (highest-score 13 7999)))
  (assert (= 2764 (highest-score 17 1104)))
  (assert (= 54718 (highest-score 21 6111)))
  (assert (= 37305 (highest-score 30 5807))))
