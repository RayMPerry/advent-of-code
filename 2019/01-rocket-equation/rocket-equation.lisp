(defun calculate-fuel (number)
  "Given a NUMBER, divide by 3, round down, and subtract 2."
  (- (floor (/ number 3)) 2))

(defun calculate-fuel-with-fuel (parsed-number)
  "Given a NUMBER, calculate the fuel requirement while considering the previous fuel requirement."
  (loop
    with next-value = (calculate-fuel (calculate-fuel parsed-number))
    until (or (zerop next-value) (minusp next-value))
    sum next-value into intermediate-value
    do (setq next-value (calculate-fuel next-value))
    finally (return (+ (calculate-fuel parsed-number) intermediate-value))))

(defun main (&optional (list-of-numbers (map 'list 'parse-integer (uiop:read-file-lines "input.txt"))))
  (loop
    for parsed-number in list-of-numbers
    sum (calculate-fuel-with-fuel parsed-number)))

(defun run-tests ()
  "Run a suite of test cases."
  (assert (= (main '(14)) 2))
  (assert (= (main '(1969)) 966))
  (assert (= (main '(100756)) 50346)))
