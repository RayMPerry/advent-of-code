(defun opcode-add (register opcode)
  (destructuring-bind (instruction &optional position1 position2 output)
    opcode
    (setf (nth output register) (+ (nth position1 register) (nth position2 register)))))

(defun opcode-multiply (register opcode)
    (destructuring-bind (instruction &optional position1 position2 output)
      opcode
      (setf (nth output register) (* (nth position1 register) (nth position2 register)))))

(defun parse-opcode (register opcode)
  "Given an OPCODE, perform certain operations."
  (case (car opcode)
    (1 (opcode-add register opcode))
    (2 (opcode-multiply register opcode))
    (otherwise (error "Invalid opcode"))))

(defun main (&optional (raw-opcodes (car (uiop:read-file-lines "input.txt"))))
  (let ((opcodes (map 'list 'parse-integer (uiop:split-string raw-opcodes :separator '(#\comma)))))
    (format nil "~{~A~^,~}"
    (block opcode-parsing
      (loop
        for program-counter from 0 to (length opcodes) by 4
        do (let ((opcode (subseq opcodes program-counter (min (+ program-counter 4) (length opcodes)))))
             (when (= (car opcode) 99) (return-from opcode-parsing opcodes))
             (parse-opcode opcodes opcode)))))))

(defun run-tests ()
  (assert (string= (main "1,0,0,0,99") "2,0,0,0,99"))
  (assert (string= (main "2,3,0,3,99") "2,3,0,6,99"))
  (assert (string= (main "2,4,4,5,99,0") "2,4,4,5,99,9801"))
  (assert (string= (main "1,1,1,4,99,5,6,0,99") "30,1,1,4,2,5,6,0,99")))
