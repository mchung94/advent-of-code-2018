(defun parse-record (string)
  "Given a record string, return a cons containing timestamp and event message."
  (flet ((int (start &optional (length 2))
           (parse-integer string :start start :end (+ start length))))
    (cons (encode-universal-time 0 (int 15) (int 12) (int 9) (int 6) (int 1 4))
          (subseq string 19))))

(defun read-input (&optional (filename "input/day04.txt"))
  "Return a list of the records in the file."
  (with-open-file (in filename :direction :input)
    (loop for line = (read-line in nil nil)
          while line
          collect (parse-record line))))

(defun guard-id (event-description)
  "Return the numeric guard ID given a string indicating the shift beginning."
  (let* ((start (1+ (position #\# event-description)))
         (end (position #\space event-description :start start)))
    (parse-integer event-description :start start :end end)))

(defun minute (universal-time)
  "Return the minute from a universal time value."
  (nth-value 1 (decode-universal-time universal-time)))

(defun nap-chart (records)
  "Return an alist collecting the minutes each guard was asleep.
The alist keys are Guard IDs and the values are vectors indexed by minute,
showing how many times the guard was asleep at that minute."
  (loop with data = () and start = nil and guard = nil
        for (timestamp . event) in (sort records #'< :key #'car)
        do (cond ((string= event "falls asleep")
                  (setf start timestamp))
                 ((string= event "wakes up")
                  (loop with minutes = (cdr (assoc guard data))
                        for i from (minute start) below (minute timestamp)
                        do (incf (svref minutes i))))
                 (t
                  (setf guard (guard-id event))
                  (unless (assoc guard data)
                    (setf data (acons guard
                                      (make-array 60 :initial-element 0)
                                      data)))))
        finally return data))

(defun sleepiest-guard (naps)
  "Return the guard ID of the guard that slept the longest total time."
  (car (first (sort naps #'> :key (lambda (c) (reduce #'+ (cdr c)))))))

(defun sleepiest-minute (minutes)
  "Return the minute that the guard was asleep most."
  (position (reduce #'max minutes) minutes))

(defun strategy1 (records)
  "Multiply the ID of the sleepiest guard with their sleepiest minute."
  (let* ((naps (nap-chart records))
         (guard-id (sleepiest-guard naps))
         (minute (sleepiest-minute (cdr (assoc guard-id naps)))))
    (* guard-id minute)))

(defun part1 ()
  "Return the answer for Day 4, Part 1."
  (strategy1 (read-input)))

(defun guard-with-sleepiest-minute (naps)
  "Return the ID and minutenaps of the guard which is most frequently asleep on the
same minute."
  (first (sort naps #'> :key (lambda (c) (reduce #'max (cdr c))))))

(defun strategy2 (records)
  "Multiply the ID of the guard most frequently asleep on the same minute with
their sleepiest minute."
  (destructuring-bind (guard-id . minutes)
      (guard-with-sleepiest-minute (nap-chart records))
    (* guard-id (sleepiest-minute minutes))))

(defun part2 ()
  "Return the answer for Day 4, Part 2."
  (strategy2 (read-input)))

(defparameter *test-input*
  (mapcar #'parse-record '("[1518-11-01 00:00] Guard #10 begins shift"
                           "[1518-11-01 00:05] falls asleep"
                           "[1518-11-01 00:25] wakes up"
                           "[1518-11-01 00:30] falls asleep"
                           "[1518-11-01 00:55] wakes up"
                           "[1518-11-01 23:58] Guard #99 begins shift"
                           "[1518-11-02 00:40] falls asleep"
                           "[1518-11-02 00:50] wakes up"
                           "[1518-11-03 00:05] Guard #10 begins shift"
                           "[1518-11-03 00:24] falls asleep"
                           "[1518-11-03 00:29] wakes up"
                           "[1518-11-04 00:02] Guard #99 begins shift"
                           "[1518-11-04 00:36] falls asleep"
                           "[1518-11-04 00:46] wakes up"
                           "[1518-11-05 00:03] Guard #99 begins shift"
                           "[1518-11-05 00:45] falls asleep"
                           "[1518-11-05 00:55] wakes up")))

(defun test-strategy1 ()
  (assert (= 240 (strategy1 *test-input*))))

(defun test-strategy2 ()
  (assert (= 4455 (strategy2 *test-input*))))