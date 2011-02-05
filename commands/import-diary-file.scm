;;; import-diary-file.scm --- import diary-format data from FILE

;; Copyright (C) 2004-2009, 2011 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack)
  #:use-module ((ice-9 editing-buffer) #:select (editing-buffer)))

(define (validate/collect buf tp)
  (let* ((two-n "[0-9][0-9]")
         (date-regexp (fs "~A~A-~A-~A" two-n two-n two-n two-n))
         (amount-regexp (fs "-*~A*[.]~A" two-n two-n))
         (attr-regexp (let ((one (list->string
                                  (map (lambda (sym)
                                         (string-ref (symbol->string sym)
                                                     0))
                                       *attributes*))))
                        (fs "[~A][~A]*[ \t]" one one))))
    ;; do it!
    (editing-buffer buf
      (goto-char (point-min))
      (let ((skip-ws (lambda ()
                       (while (looking-at "[ \t]") (forward-char 1))
                       #t)))
        (let loop ((p (point)) (line 1))
          (define (bad area)
            (goto-char p)
            (insert (number->string line) ":\tbad " area "\n\t")
            (forward-line 1)
            (insert "\n"))
          (or (= p (point-max))
              (let* ((date (and (looking-at date-regexp)
                                (begin
                                  (forward-char 10)
                                  (buffer-substring p (+ 10 p)))))
                     (ws1 (and date (skip-ws)))
                     (amount (and ws1 (looking-at amount-regexp)
                                  (begin
                                    (goto-char (match-end 0))
                                    (string->number
                                     (buffer-substring
                                      (match-beginning 0)
                                      (match-end 0))))))
                     (ws2 (and amount (skip-ws)))
                     (attr (and ws2 (looking-at attr-regexp)
                                (begin
                                  (goto-char (match-end 0))
                                  (buffer-substring
                                   (match-beginning 0)
                                   (1- (match-end 0))))))
                     (ws3 (and attr (skip-ws)))
                     (details (false-if-exception
                               (with-input-from-string
                                   (fs "(~A)"
                                       (buffer-substring
                                        (point)
                                        (begin (forward-line 1)
                                               (point))))
                                 read)))
                     (chkd (lambda (pred)
                             (and-map pred details))))
                (cond ((not date) (bad "date"))
                      ((not amount) (bad "amount"))
                      ((not attr) (bad "attcode"))
                      ((not details) (bad "details"))
                      ((not (list? details)) (bad "details"))
                      ((not (chkd string?)) (bad "details (type)"))
                      ((not (chkd (lambda (d)
                                    (< 0 (string-length d)))))
                       (bad "details (empty)"))
                      ((not (chkd (lambda (d)
                                    (define (n-ws? p)
                                      (not (char-whitespace?
                                            (string-ref d p))))
                                    (and (n-ws? 0)
                                         (n-ws? (1- (string-length d)))))))
                       (bad "details (head/tail space)"))
                      (else (let ((new (list date amount attr details)))
                              (set-cdr! tp (list new))
                              (set! tp (cdr tp))
                              (delete-region p (point)))))
                (loop (point) (1+ line)))))))))

(define-command (import-diary-file file) ; init=#t
  (let* ((buf (editing-buffer (let ((name (if (symbol? file)
                                              (symbol->string file)
                                              file)))
                                (if (string=? "-" name)
                                    (begin (set! file "standard input")
                                           (current-input-port))
                                    (open-input-file name)))))
         (tbi (list #f))                ; to be inserted
         (tp tbi))
    ;; first pass: collect and flag errors
    (fso "etrack: Validating ~A ... please wait.~%" file)
    (validate/collect buf tp)
    (cond ((editing-buffer buf (= 1 (point-min) (point-max))))
          (else
           (fso "~A~%~A~%~%~A"
                "etrack: Sorry, no entries added to the database due to"
                "        errors encountered in the following lines:"
                (editing-buffer buf (buffer-string)))
           (fso "etrack: ~A~%\t~A~%\t~A~%\t~A~A:~%"
                "Please correct the errors and try again. Hints:"
                "- `date' format: YYYY-MM-DD"
                "- `amount' format: -ABCDE.FG (\"-\" is optional)"
                "- valid `attcodes' for " *name*)
           (fso "~:{\t\t~A\t~A~%~}" (map list *attcodes* *attributes*))
           (fso "\t- `details' are ~A~%\t  - ~A~%\t  - ~A~%"
                "one or more strings"
                "each string starts and ends with double-quote"
                "no leading/trailing spaces (internal spaces are ok)")
           (fso "\t  - example: ~S ~S ~S~%"
                "first" "second detail" "third and last detail")
           (all-done #f)))
    ;; second pass: do the inserts
    (set! tbi (cdr tbi))
    (fso "etrack: ~A new entries. Starting import ...~%" (length tbi))
    (for-each (lambda (new)
                (fso "insert: ~S => " new)
                (process-command `(insert ,new)))
              tbi)
    (fso "etrack: Import from ~A finished!~%" file)))

;;; import-diary-file.scm ends here
