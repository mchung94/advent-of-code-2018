(defun read-input (&optional (filename "input/day02.txt"))
  "Return a list of box IDs from the file."
  (with-open-file (in filename :direction :input)
    (loop for box-id = (read-line in nil nil)
          while box-id
          collect box-id)))

(defun letter->count (box-id)
  "Return a hash table mapping letters in box-id to how many times it appears."
  (loop with counts = (make-hash-table)
        for letter across box-id
        do (if (gethash letter counts)
               (incf (gethash letter counts))
             (setf (gethash letter counts) 1))
        finally return counts))

(defun checksum (box-ids)
  "Return the checksum of the given box IDs."
  (loop for box-id in box-ids
        for counts = (loop for v being the hash-values in (letter->count box-id)
                           collect v)
        counting (member 2 counts) into twos
        counting (member 3 counts) into threes
        finally return (* twos threes)))

(defun part1 ()
  "Return the answer for Day 2, Part 1."
  (checksum (read-input)))

(defun common-chars (string1 string2)
  "Return the letters that match at the same position in both strings."
  (coerce (loop for c1 across string1
                for c2 across string2
                when (char= c1 c2)
                collect c1)
          'string))

(defun common-chars-from-correct-box-ids (box-ids)
  "Return the common characters from the two correct box IDs."
  (loop for (box-id1 . others) on box-ids
        do (loop for box-id2 in others
                 for common = (common-chars box-id1 box-id2)
                 when (= (length box-id1) (1+ (length common)))
                 do (return-from common-chars-from-correct-box-ids common))))

(defun part2 ()
  "Return the answer for Day 2, Part 2."
  (common-chars-from-correct-box-ids (read-input)))

(defun test-checksum ()
  (assert (= 12 (checksum '("abcdef" "bababc" "abbcde" "abcccd"
                            "aabcdd" "abcdee" "ababab")))))

(defun test-common-chars-from-correct-box-ids ()
  (assert (string= "fgij" (common-chars-from-correct-box-ids
                           '("abcde" "fghij" "klmno" "pqrst"
                             "fguij" "axcye" "wvxyz")))))
