(defun read-input (&optional (filename "input/day05.txt"))
  "Read a polymer from a file."
  (with-open-file (in filename :direction :input)
    (read-line in)))

(defun reactablep (unit1 unit2)
  "Return T if the two units can react and destroy each other."
  (and (char= (char-upcase unit1) (char-upcase unit2))
       (char/= unit1 unit2)))

(defun react (polymer)
  "Trigger a reaction in the polymer and return the resulting units."
  (loop with stack = ()
        for unit across polymer
        do (if (and stack (reactablep unit (first stack)))
               (pop stack)
             (push unit stack))
        finally return (nreverse (coerce stack 'string))))

(defun part1 ()
  "Return the answer for Day 5, Part 1."
  (length (react (read-input))))

(defun shortest-filtered-polymer-length (polymer)
  "Return the length of the shortest polymer after filtering out a unit type."
  (loop for unit across (remove-duplicates (map 'string #'char-upcase polymer))
        for filtered = (remove unit polymer :test #'char-equal)
        minimizing (length (react filtered))))

(defun part2 ()
  "Return the answer for Day 5, Part 2."
  (shortest-filtered-polymer-length (read-input)))
        
(defun test-react ()
  (assert (string= "" (react "aA")))
  (assert (string= "" (react "abBA")))
  (assert (string= "abAB" (react "abAB")))
  (assert (string= "aabAAB" (react "aabAAB")))
  (assert (string= "dabCBAcaDA" (react "dabAcCaCBAcCcaDA"))))

(defun test-shortest-filtered-polymer-length ()
  (assert (= 4 (shortest-filtered-polymer-length "dabAcCaCBAcCcaDA"))))
