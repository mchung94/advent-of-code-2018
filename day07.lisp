(defun string->dependency (string)
  "Given a string describing a step and prereq, return a (prereq . step) cons."
  (cons (char string 5) (char string 36)))

(defun read-input (&optional (filename "input/day07.txt"))
  "Return a list of (prereq . step) conses from the input file."
  (with-open-file (in filename :direction :input)
    (loop for line = (read-line in nil nil)
          while line
          collect (string->dependency line))))

(defun create-graph (edges)
  "Create a graph mapping each step to its prerequisites."
  (loop with graph = (make-hash-table)
        for (prerequisite . step) in edges
        do (push prerequisite (gethash step graph))
        unless (gethash prerequisite graph)
        do (setf (gethash prerequisite graph) nil)
        finally (return graph)))

(defun available-steps (graph completed-steps)
  "Return a list of the next steps that can be performed."
  (loop for step being the hash-keys in graph using (hash-value prereqs)
        when (and (not (member step completed-steps))
                  (null (set-difference prereqs completed-steps)))
        collect step))

(defun step-order (dependencies)
  "Return the steps to be completed in order given the dependencies."
  (loop with graph = (create-graph dependencies)
        with completed-steps = ()
        for next = (sort (available-steps graph completed-steps) #'char<)
        until (null next)
        do (push (first next) completed-steps)
        finally (return (nreverse completed-steps))))

(defun part1 ()
  "Return the answer for Day 7, Part 1."
  (coerce (step-order (read-input)) 'string))

(defun step-time (step step-prep-seconds)
  "Return the length of time it takes to complete the step."
  (+ (1+ (position step "ABCDEFGHIJKLMNOPQRSTUVWXYZ")) step-prep-seconds))

(defun advance-time (work)
  "Advance time until there is a worker ready for work.
Update the work hashtable and return the number of seconds passed."
  (loop with min-time = (loop for v being the hash-values in work minimize v)
        for step being the hash-keys in work
        do (decf (gethash step work) min-time)
        finally (return min-time)))

(defun completed-work (work)
  "Return all the work items finished and remove them from the work hash table."
  (loop for step being the hash-keys in work using (hash-value time-left)
        when (zerop time-left)
        collect (prog1 step (remhash step work))))

(defun assign-work (work num-workers steps step-prep-seconds)
  "Give workers steps to work on given the steps that are ready to be started."
  (loop while (and steps (< (hash-table-count work) num-workers))
        do (let ((step (pop steps)))
             (setf (gethash step work) (step-time step step-prep-seconds)))))

(defun total-time (edges num-workers step-prep-seconds)
  "Return the total time it takes to complete the steps."
  (loop with elapsed-time = 0
        with graph = (create-graph edges)
        with work = (make-hash-table)
        with completed-steps = ()
        for steps = (set-difference (available-steps graph completed-steps)
                                    (loop for k being the hash-keys in work
                                          collect k))
        until (= (length completed-steps) (hash-table-count graph))
        do
        (assign-work work num-workers steps step-prep-seconds)
        (incf elapsed-time (advance-time work))
        (setf completed-steps (nconc (completed-work work) completed-steps))
        finally (return elapsed-time)))

(defun part2 ()
  "Return the answer for Day 7, Part 2."
  (total-time (read-input) 5 60))

(defparameter *test-input*
  (mapcar #'string->dependency
          '("Step C must be finished before step A can begin."
            "Step C must be finished before step F can begin."
            "Step A must be finished before step B can begin."
            "Step A must be finished before step D can begin."
            "Step B must be finished before step E can begin."
            "Step D must be finished before step E can begin."
            "Step F must be finished before step E can begin.")))
  
(defun test-step-order ()
  (assert (string= "CABDFE" (coerce (step-order *test-input*) 'string))))

(defun test-total-time ()
  (assert (= 15 (total-time *test-input* 2 0))))
