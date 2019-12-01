(defun calculate-fuel (number)
  "Given a NUMBER, divide by 3, round down, and subtract 2."
  (- (floor (/ number 3)) 2))

(defun main (&optional (list-of-numbers (map 'list 'parse-integer (uiop:read-file-lines "input.txt"))))
  (loop
    for parsed-number in list-of-numbers
    sum (let* ((intermediate-value (calculate-fuel parsed-number))
               (next-value (calculate-fuel intermediate-value)))
          (loop
            until (or (zerop next-value) (minusp next-value))
            do (progn
                  (setq intermediate-value (+ intermediate-value next-value))
                 (setq next-value (calculate-fuel next-value))))
          intermediate-value)))

(defun run-tests ()
  "Run a suite of test cases."
  (assert (= (main '(14)) 2))
  (assert (= (main '(1969)) 966))
  (assert (= (main '(100756)) 50346)))
