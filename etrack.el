;;; etrack.el --- expense tracking user interface

;; Copyright (C) 2001-2009, 2011 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

;;; Commentary:

;;; Code:

(require 'cl)
(require 'comint)

;; constants

(defconst etrack-coding-map
  '((chinese-big5 . "BIG5")
    (chinese-iso-8bit . "EUC_CN")
    (cp1250 . "WIN1250")
    (cp1256 . "WIN1256")
    (cp1258 . "TCVN")
    (cp866 . "ALT")
    (cp874 . "WIN874")
    (cyrillic-iso-8bit . "ISO_8859_5")
    (cyrillic-koi8 . "KOI8")
    (emacs-mule . "MULE_INTERNAL")
    (euc-tw . "EUC_TW")
    (greek-iso-8bit . "ISO_8859_7")
    (hebrew-iso-8bit . "ISO_8859_8")
    (iso-8859-10 . "LATIN6")
    (iso-8859-13 . "LATIN7")
    (iso-8859-16 . "LATIN10")
    (iso-8859-6 . "ISO_8859_6")
    (iso-latin-1 . "LATIN1")
    (iso-latin-10 . "LATIN10")
    (iso-latin-2 . "LATIN2")
    (iso-latin-3 . "LATIN3")
    (iso-latin-4 . "LATIN4")
    (iso-latin-5 . "ISO_8859_5")
    (iso-latin-6 . "LATIN6")
    (iso-latin-7 . "LATIN7")
    (iso-latin-8 . "ISO_8859_8")
    (iso-latin-9 . "LATIN5")
    (iso-safe . "SQL_ASCII")
    (japanese-iso-8bit . "EUC_JP")
    (japanese-shift-jis . "SJIS")
    (korean-iso-8bit . "EUC_KR")
    (us-ascii . "SQL_ASCII")
    (utf-8 . "UNICODE")
    (vietnamese-tcvn . "TCVN")
    (windows-1250 . "WIN1250")
    (windows-1251 . "WIN")
    (windows-1256 . "WIN1256")
    (windows-1258 . "TCVN"))
  "Alist mapping an Emacs coding-system to a PostgreSQL client encoding.
This was snarfed from emacs-coding-system-postgresql-client-encoding.alist
with \"?\" and \"\" entries removed.")

;; variables

(defvar etrack-export-file nil
  "*Default output file for exporting data (\"x\" command).")

(defvar etrack-mode-map nil
  "Keymap for ETRACK-interaction mode.")

(defvar --etrack-state nil)

(defvar --etrack-hist:date nil)
(defvar --etrack-hist:details nil)

;; functions

(defun --etrack! (k v)
  (puthash k v --etrack-state))

(defun --etrack: (k)
  (gethash k --etrack-state))

(defun --etrack-w/c (n)                 ; "w/ cents"
  (let ((whole (format (if (> 0 n) "%04d" "%03d") n)))
    (concat (substring whole 0 -2) "." (substring whole -2))))

(defun --etrack-menu ()
  (widen)
  (goto-char (point-max))
  (insert "\n\n\f")
  (narrow-to-region (point) (point-max))
  (insert "Welcome to ETRACK for " (buffer-name) "!\n"
          "Please report bugs to ttn.\n"
          "\n"
          (format "=== Page %d ===\n\n"
                  (--etrack! :page (1+ (--etrack: :page))))
          "Choose a command by typing a number or letter.\n\n"
          " (1) query\n\n"
          " (2) add\n"
          " (3) delete\n"
          " (4) update" (if (--etrack: :sel) " marked" "") "\n"
          " (5) prune duplicates\n\n"
          " (t) add using template" (let ((pre (--etrack: :pre)))
                                      (if pre
                                          (format " %S" (car pre))
                                        ""))
          "\n"
          " (T) edit templates\n\n"
          " (c) calendar\n"
          " (m) mail session log\n"
          " (a) about this program\n"
          "\n"
          (if (--etrack: :sel)
              (format " (u) unmark all (%d entries)\n"
                      (length (--etrack: :sel)))
            "")
          " (x) export data" (if etrack-export-file
                                 (format " [%s]" etrack-export-file)
                               "") "\n"
          " (v) view mode\n\n"
          " (q) quit\n"
          "\n"
          "Your choice: ")
  (recenter -2))

(defun --etrack-sorry ()
  (interactive)
  (message "sorry, try one of [%s]"
           (concat '(?1 ?2 ?3 ?4 ?5 ?t ?T ?c ?m ?a)
                   (when (--etrack: :sel) (list ?u))
                   '(?x ?v ?q))))

(defun --etrack-bk (cmd &optional arg)
  (unless (keywordp cmd)
    (error "not a keyword: %S" cmd))
  (goto-char (point-max))
  (recenter -2)
  (comint-set-process-mark)
  (message "")
  (let ((s (format "(%s %S)\n" (substring (symbol-name cmd) 1) arg)))
    (if (not '(emacs-bug:               ; fixme (in emacs ;-)
               subproc-gets-eof-on-stdin-for-large-input))
        (comint-send-string (current-buffer) s)
      (let ((chunk 256) (left (length s)) (start 0))
        (while (< 0 left)
          (comint-send-string
           (current-buffer)
           (substring s start (min (length s) (+ start chunk))))
          (incf start chunk)
          (decf left chunk)
          (when (< 0 left)              ; is this ok?
            (sit-for 0.01)))))))

(defun --etrack-synch (rx)
  (let ((proc (get-buffer-process (current-buffer)))
        (beg (point))
        end)
    (save-excursion
      (while (not end)
        (goto-char beg)
        (if (re-search-forward rx (point-max) t)
            (setq end (point))
          (accept-process-output proc))))
    (goto-char end)))

(defun --etrack-process-filter:list-newline (proc string)
  (--etrack! :lnl-acc (concat (--etrack: :lnl-acc) string))
  (when (string-match "\n" string)
    (--etrack! :lnl (condition-case nil
                        (list (read (--etrack: :lnl-acc)))
                      (error
                       (message "Could not `read' (%s):\n%s\n(%s)"
                                "please report bug" (--etrack: :lnl-acc)
                                "Type C-g to continue.")
                       nil)))
    (--etrack! :lnl-acc nil)))

(defun --etrack-bk-read-list (cmd &optional arg)
  (let* ((proc (get-buffer-process (current-buffer)))
         (orig-filter (process-filter proc))
         (box nil))                     ; avoid elisp (eq nil ()) confusion
    (--etrack! :lnl nil)
    (sit-for 0)                         ; drain input
    (set-process-filter proc '--etrack-process-filter:list-newline)
    (--etrack-bk cmd arg)
    (message "")
    (unwind-protect
        (while (not (setq box (--etrack: :lnl)))
          (accept-process-output proc))
      (set-process-filter proc orig-filter))
    (car box)))

(defun --etrack-atts ()
  (or (--etrack: :atts)
      (--etrack! :atts (--etrack-bk-read-list :char-attributes-alist))))

(defun --etrack-read-amount (prompt &optional default)
  (replace-regexp-in-string "[,]" "." (read-string prompt default)))

(defun --etrack-retry-collection ()
  (interactive)
  (throw 'retry t))

(defun* --etrack-collect-new-entry (scr)
  (insert "Amount: ")
  (when (string= "" (setf (nth 2 scr) (--etrack-read-amount
                                       "Amount (NNNN.NN): "
                                       (nth 2 scr))))
    (return-from --etrack-collect-new-entry nil))
  (insert (nth 2 scr) "\n")
  (insert "Date: ")
  (let* ((today (or (nth 1 scr) (format-time-string "%Y-%m-%d")))
         (d (read-string "Date (YYYY-MM-DD): "
                         today '--etrack-hist:date today t)))
    (setf (nth 1 scr) (if (string= "" d)
                          today
                        d))
    (pushnew (nth 1 scr) --etrack-hist:date :test 'string=)
    (insert (nth 1 scr) "\n"))
  (insert "Attributes:")
  (let* ((p (point))
         (atts (--etrack-atts))
         (ok (mapcar 'car atts))
         (hint-string (apply 'string ok))
         hint-expanded-p so-far c)
    (dolist (c (reverse (string-to-list (or (nth 3 scr) ""))))
      (push c unread-command-events))
    (while (progn
             (setq c (read-char-exclusive
                      (concat "Attribute ([" hint-string
                              "] or ? or DEL or RET to finish): ")))
             (not (= c ?\C-m)))
      (cond ((= c ?\C-r)
             (--etrack-retry-collection))
            ((memq c so-far)
             (setq so-far (cons c (delq c so-far)))
             (delete-region p (point))
             (dolist (c (reverse so-far))
               (insert " " (cdr (assq c atts)))))
            ((memq c ok)
             (setq so-far (cons c so-far))
             (insert " " (cdr (assoc c atts))))
            ((= c ??)
             (if hint-expanded-p
                 (progn
                   (message "(see Reminder above)")
                   (sit-for 3))
               (search-backward "Attributes:")
               (insert "Attributes Reminder:\n"
                       (mapconcat (lambda (att)
                                    (format " %s" (cdr att)))
                                  atts "\n")
                       "\n")
               (setq hint-expanded-p t)
               (goto-char (point-max))))
            ((= c ?\C-?)
             (when so-far
               (setq so-far (cdr so-far))
               (zap-to-char -1 ?\s))))
      (setf (nth 3 scr) (apply 'string (nreverse so-far)))))
  (insert "\n")
  (insert "Details:\n")
  (let ((stuff (nth 4 scr))
        item acc)
    (while (progn
             (setq item (read-string (format "Item (RET %s to finish): "
                                             (if stuff
                                                 "on empty line"
                                               "alone"))
                                     (when stuff (pop stuff))
                                     '--etrack-hist:details nil t))
             (not (string= "" item)))
      (setq acc (cons item acc))
      (setf (nth 4 scr) (reverse acc))
      (insert "- " item "\n")))
  ;; return
  (if (y-or-n-p "Everything OK? ")
      (list (nth 1 scr)
            (string-to-number (nth 2 scr))
            (nth 3 scr)
            (nth 4 scr))
    t))

(defun --etrack-vw (&optional monthp minp stuff-query-p)
  (let ((viewing-p t) ks c)
    (if minp
        (goto-char (point-min))
      (recenter -2))
    (while viewing-p
      (clear-this-command-keys)
      (message "To view, use: SPC DEL [ ] P N%s%s q%s"
               (if monthp " m M" "")
               (if stuff-query-p " x" "")
               (if stuff-query-p ", or a number to begin another query" ""))
      ;; is this reliable?
      (setq ks (read-key-sequence nil)
            c (aref ks 0))
      (flet ((toggle-mark
              () (when (looking-at "[ ]+\\([0-9]+\\)[ ]+[0-9]\\{4\\}-.*")
                   (let ((id (string-to-number (match-string 1)))
                         (sel (--etrack: :sel))
                         n)
                     (funcall
                      (cond ((get-text-property (point) 'face)
                             (setq sel (delq id sel))
                             'remove-text-properties)
                            (t
                             (unless (memq id sel)
                               (setq sel (cons id sel)))
                             'add-text-properties))
                      (point) (match-end 0)
                      '(face underline font-lock-face underline))
                     (setq n (length (--etrack! :sel sel)))
                     (message "(%s marked entr%s)"
                              (if (= 0 n) "No" n)
                              (if (= 1 n) "y" "ies"))
                     (sit-for 1.5)))))
        (cond
         ((not (numberp c))
          (case c
            ((up) (ignore-errors (previous-line 1)))
            ((down) (ignore-errors (next-line 1)))
            ((backspace) (ignore-errors (scroll-down 4)))
            ((return) (toggle-mark))
            ((S-up S-down) (setq unread-command-events
                                 (append `(,(event-basic-type c) return)
                                         unread-command-events)))))
         ((= c ?\C-m) (toggle-mark))
         ((= c ?\C-n) (ignore-errors (next-line 1)))
         ((= c ?\C-p) (ignore-errors (previous-line 1)))
         ((= c ?\s) (ignore-errors (scroll-up 4)))
         ((= c ?\C-?) (ignore-errors (scroll-down 4)))
         ((= c ?[) (scroll-left 15))
         ((= c ?]) (scroll-right 15))
         ((= c ?P) (widen) (backward-page 1) (narrow-to-page))
         ((= c ?N) (widen) (forward-page 1) (narrow-to-page))
         ((and (= c ?m) monthp)
          (--etrack-bk :redisplay-last-result-by-month ""))
         ((and (= c ?x) stuff-query-p)
          (let ((which (if (and (--etrack: :sel)
                                (y-or-n-p "Export marked? (n for page) "))
                           '(?m)
                         `(?p ,@(string-to-list (number-to-string
                                                 (--etrack: :page)))
                              ?\n))))
            (setq unread-command-events (cons ?x which)
                  viewing-p nil)))
         ((and (memq c '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)) stuff-query-p)
          (push c unread-command-events)
          (push ?1 unread-command-events)
          (setq viewing-p nil))
         ((and (= c ?M) monthp)
          (--etrack-bk :redisplay-last-result-by-month
                       (read-string "Month ([YYYY-]MM,[YYYY-]MM): ")))
         ((= c ?q) (setq viewing-p nil)))))
    (widen)
    (remove-text-properties (point-min) (point-max) '(face underline))
    (goto-char (point-max))
    (narrow-to-page)
    (recenter -2))
  (--etrack-menu))

(defun --etrack-process-filter:query (proc string)
  (--etrack! :q-acc (concat (--etrack: :q-acc) string))
  (when (string-match "\n\\(EOQ\n\\|ABORT\\)" (--etrack: :q-acc))
    (let ((parent (current-buffer)))
      (with-current-buffer (get-buffer-create " *etrack query work*")
        (set (make-local-variable '--etrack-state)
             (with-current-buffer parent --etrack-state))
        (erase-buffer)
        (insert (--etrack: :q-acc))
        (--etrack! :q-acc nil)
        (while (re-search-backward "[\"{}]" (point-min) t)
          (delete-char 1))
        (while (search-forward "," (point-max) 0)
          (insert " "))
        (when (search-backward "\nEOQ\n" (point-min) t)
          (delete-char 5))
        (--etrack! :q (buffer-string))))))

(defun --etrack-bk-query (q-command arg)
  (let* ((proc (get-buffer-process (current-buffer)))
         (orig-filter (process-filter proc))
         (q-result-string nil))
    (--etrack! :q nil)
    (--etrack! :q-acc nil)
    (sit-for 0)                         ; drain input
    (set-process-filter proc '--etrack-process-filter:query)
    (--etrack-bk q-command arg)
    (message "")
    (unwind-protect
        (while (not (setq q-result-string (--etrack: :q)))
          (accept-process-output proc))
      (set-process-filter proc orig-filter))
    (insert q-result-string)
    (when (= ?- (char-before))          ; end w/ newline if necessary
      (insert "\n"))))

(defun --etrack-cmd-prep ()
  (let ((choice last-command-event))
    (goto-char (point-max))
    (insert choice "\n\n")
    (goto-char (point-min))
    (keep-lines (concat "\\(^=\\)\\|\\(^Y\\)\\|\\(("
                        (string choice)
                        ")\\)"))
    (forward-line 2)
    (transpose-lines 1)
    (end-of-line -1)
    (delete-char -2)
    (delete-char 1)
    (beginning-of-line)
    (delete-char -1)
    (insert (format-time-string " %Y-%m-%d %H:%M:%S ===\n"))
    (goto-char (point-max))
    (insert "\n")))

(defmacro --etrack-catch-quit (blurb finish &rest body)
  (declare (debug t) (indent 2))
  `(let ((inhibit-quit t))
     (unless (with-local-quit ,@body t)
       (let ((m (format "%s cancelled" ,blurb)))
         (goto-char (point-max))
         (insert "\n" m "\n")
         (message "%s" m))
       (sit-for 1)
       (setq quit-flag nil)
       ,@(when finish
           `((,finish))))))

(defun --etrack-cmd:query ()
  (interactive)
  (--etrack-cmd-prep)
  (insert (or (--etrack: :all-queries)
              (let ((proc (get-buffer-process (current-buffer)))
                    (i 0)
                    (p (point)))
                (sit-for 0)
                (--etrack-bk :list-queries)
                (while (not (and (< p (- (point) 5))
                                 (string= (buffer-substring
                                           (- (point) 5) (point))
                                          "\nEOL\n")))
                  (message "Thinking...%s" (make-string (/ (incf i) 5) ?.))
                  (accept-process-output proc))
                (forward-line -2)
                (let ((bye (point)))
                  (--etrack! :qmax (read (current-buffer)))
                  (delete-region bye (point-max)))
                (--etrack! :all-queries (buffer-substring p (point)))
                "")))
  (goto-char (point-min))
  (clear-this-command-keys)
  (--etrack-catch-quit "Query" --etrack-menu
    (let ((n (read-number (format "Query [1-%d]: " (--etrack: :qmax)))))
      (goto-char (point-max))
      (insert "\nQuery: ")
      (if (or (> 1 n) (< (--etrack: :qmax) n))
          (insert (message "No such query: %d" n) "\n\n")
        (insert (number-to-string n) " -- ")
        (beginning-of-line)
        (let ((p (point)))
          (re-search-backward "^01")
          (delete-region p (point)))
        (goto-char (point-max))
        (--etrack-bk-query :query (1- n))
        (when (--etrack: :sel)
          (let ((end (point)))
            (save-excursion
              (dolist (id (--etrack: :sel))
                (goto-char (point-min))
                (when (re-search-forward
                       (format "^[ ]+%d[ ]+[0-9][0-9][0-9][0-9]-.*" id)
                       end t)
                  (put-text-property (match-beginning 0) (match-end 0)
                                     'face 'underline))))))))
    (--etrack-vw t nil t))
  (goto-char (point-max))
  (recenter -2))

(defun --etrack-add-internal (activity pre)
  (--etrack-catch-quit activity nil
    (let ((minibuffer-local-map (copy-keymap minibuffer-local-map))
          (orig-pre (if pre (copy-list pre)
                      (make-list 5 nil)))
          (keep-going-p t)
          p data)
      (define-key minibuffer-local-map "\C-r" '--etrack-retry-collection)
      (while keep-going-p
        (setq p (point))
        (setq pre (copy-list orig-pre))
        (while (catch 'retry
                 (delete-region p (point-max))
                 (setq data (--etrack-collect-new-entry pre))
                 nil))
        (message "")
        (cond ((consp data)
               (--etrack-bk :insert data)
               (--etrack-synch "\n"))
              (t
               (insert "cancelled\n")
               (setq keep-going-p data)))
        (insert "\n"))))
  (--etrack-menu))

(defun --etrack-cmd:add ()
  (interactive)
  (--etrack-cmd-prep)
  (--etrack-add-internal "Add" nil))

(defun --etrack-cmd:delete ()
  (interactive)
  (--etrack-cmd-prep)
  (--etrack-catch-quit "Delete" nil
    (let (row entry)
      (while (progn
               (setq row (string-to-number
                          (read-string
                           "Delete entry: [N or RET to cancel] ")))
               (if (= 0 row)
                   (and (insert "cancelled\n\n") nil)
                 t))
        (setq entry (--etrack-bk-read-list :query-one-row-alist row))
        (if (not entry)
            (insert "(no such entry)\n")
          (pp entry (current-buffer))
          (recenter -2)
          (when (y-or-n-p "Really delete? ")
            (insert (format "Deleting entry %s ... " row))
            (--etrack-bk :delete-row row)))
        (insert "\n"))))
  (--etrack-menu))

(defun --etrack-select-and-update ()
  (let ((keep-going-p t))
    (while keep-going-p
      (goto-char (point-max))
      (insert "Entry number: ")
      (recenter -2)
      (let ((vattstr (mapconcat (lambda (x)
                                  (char-to-string
                                   (car x)))
                                (--etrack-atts) ""))
            (row (string-to-number
                  (read-string "Update entry: [N or RET to cancel] "))))
        (if (= 0 row)
            (progn
              (insert "cancelled\n\n")
              (setq keep-going-p nil))
          (insert (format "%s\n" row))
          (let ((data (--etrack-bk-read-list :query-one-row-alist row))
                (new-data nil))
            (if (not data)
                (insert "sorry, invalid number\n")
              (--etrack-bk-query :query-one-row row)
              (let ((new-date (read-string "Date: " (cdr (assq 'date data))
                                           '--etrack-hist:date))
                    (new-amount (--etrack-read-amount
                                 "Amount: "
                                 (--etrack-w/c (cdr (assq 'amount data)))))
                    (new-attcode (read-string
                                  "Attribute Code: "
                                  (cdr (assq 'attcode data))))
                    (new-details (read
                                  (read-string
                                   "Details: "
                                   (format "%S" (cdr (assq 'details data)))
                                   nil "nil" t))))
                (insert "New Date: " new-date "\n"
                        "New Amount: " new-amount "\n"
                        "New Attribute Code: " new-attcode "\n"
                        "New Details: " (format
                                         "%S" new-details)
                        "\n")
                (recenter -2)
                (cond ((let ((fc (mapcar 'car (--etrack-atts))))
                         (not (every (lambda (c)
                                       (member c fc))
                                     new-attcode)))
                       (insert "oops, invalid attribute code\n"
                               "[try one of: " vattstr "]\n"
                               "cancelled\n"))
                      ((or (not (listp new-details))
                           (not (every 'stringp new-details)))
                       (insert "oops, invalid details\n"
                               "[try something like: "
                               "(\"abc\" \"def\" \"ghi\")]\n"
                               "cancelled\n"))
                      ((y-or-n-p "Everything OK? ")
                       (--etrack-bk :update-row
                                    (list row
                                          new-date
                                          new-amount
                                          new-attcode
                                          new-details))
                       (insert "\n")
                       (recenter -2))
                      (t (insert "cancelled\n")))))))))))

(defun --etrack-cmd:prune-duplicates ()
  (interactive)
  (--etrack-cmd-prep)
  (let ((possible-duplicates (--etrack-bk-read-list :possible-duplicates))
        (keep-going-p t)
        (count 0))
    (insert (format "(%d pairs of possible duplicates)\n\n"
                    (/ (length possible-duplicates) 2)))
    (--etrack-catch-quit "Prune Duplicates" --etrack-menu
      (while (and (consp possible-duplicates) keep-going-p)
        (let ((a (car possible-duplicates))
              (b (cadr possible-duplicates)))
          (goto-char (point-max)) (recenter -2)
          (--etrack-bk-query :query-two-rows (cons a b))
          (goto-char (point-max)) (recenter -2)
          (insert "Prune: ")
          (message "To prune, use: 1 2 SPC q")
          (case (read-char-exclusive)
            ((?1)
             (insert (format "%d =>" a))
             (--etrack-bk :delete-row a)
             (incf count)
             (sit-for 1))
            ((?2)
             (insert (format "%d =>" b))
             (--etrack-bk :delete-row b)
             (incf count)
             (sit-for 1))
            ((?q) (insert "cancelled\n\n") (setq keep-going-p nil))
            (t (insert "skip\n")))
          (setq possible-duplicates (cddr possible-duplicates))))
      (when keep-going-p (insert "\nDone pruning!\n\n"))
      (insert (format "(%d deleted)\n\n" count))
      (recenter -2)
      (--etrack-vw))))

(defun --etrack-insert-template-extrep (prefix ent)
  (insert prefix (format "%S" (nth 0 ent)))
  (when (nth 2 ent) (insert ", " (nth 2 ent)))
  (when (nth 1 ent) (insert ", " (nth 1 ent)))
  (when (nth 3 ent) (insert ", ["
                            (mapconcat (lambda (c)
                                         (cdr (assq c (--etrack-atts))))
                                       (string-to-list (nth 3 ent))
                                       " ")
                            "]"))
  (when (nth 4 ent) (insert ", " (mapconcat 'identity (nth 4 ent) ", ")))
  (insert "\n")
  (recenter -2))

(defun --etrack-cmd:add-using-template ()
  (interactive)
  (--etrack-cmd-prep)
  (let ((all (--etrack-bk-read-list :templates-list))
        (activity "Add Using Template")
        sel p)
    (when (search-backward " \"" (point-min) t)
      (delete-region (point) (point-max))
      (insert "\n\n"))
    (setq p (point))
    (dolist (ent all)
      (--etrack-insert-template-extrep "" ent))
    (--etrack-catch-quit activity nil
      (if all
          (setq sel (assoc (let ((name (car (--etrack: :pre))))
                             (completing-read "Select template: "
                                              all nil t name nil ""))
                           all))
        (message "(No templates available)")
        (sit-for 2)))
    (delete-region p (point))
    (insert "Template: " (or (car sel) "(none)") "\n\n")
    (cond (sel (--etrack! :pre sel)
               (--etrack-add-internal
                (format "%s %S" activity (car sel))
                sel)
               (push ?\M-n unread-command-events)
               (push ?t unread-command-events))
          (t (--etrack-menu)))))

(defun --etrack-update-marked ()
  (let ((inhibit-read-only t)
        (top (point))
        (orig (mapcar (lambda (id)
                        (message "Thinking ... %d" id)
                        (let* ((data (--etrack-bk-read-list
                                      :query-one-row-alist id))
                               (a-cell (assq 'amount data))
                               (d-cell (assq 'details data)))
                          (setcdr a-cell (--etrack-w/c (cdr a-cell)))
                          (unless (cdr d-cell)
                            (setcdr d-cell (list "")))
                          (cons id data)))
                      (--etrack: :sel)))
        (m (make-sparse-keymap))
        (keep-going-p t)
        (everything-ok-p t)
        better)
    (define-key m "\C-i"
      (lambda () (interactive)
        (unless (get-text-property (point) 'read-only)
          (end-of-line))
        (goto-char (or (next-property-change (point))
                       (next-property-change (point-min))))
        (goto-char (next-property-change (point)))))
    (define-key m "\C-c\C-c"
      (lambda () (interactive)
        (throw 'exit nil)))
    (let ((standard-output (current-buffer)))
      (mapc (lambda (entry)
              (princ "\n")
              (pp entry))
            orig))
    (add-text-properties (point-min) (point-max) `(read-only t keymap ,m))
    (goto-char (point-min))
    (flet ((zonk-ro (re) (when (re-search-forward re (point-max) t)
                           (let ((b (match-beginning 1))
                                 (e (match-end 1)))
                             (put-text-property (1- b) b 'rear-nonsticky t)
                             (remove-text-properties b e '(read-only t))))))
      (while (zonk-ro "(date . \"\\(.*\\)\")")
        (zonk-ro "(amount . \"\\(.*\\)\")")
        (zonk-ro "(attcode . \"\\(.*\\)\")")
        (zonk-ro "(details \\(.*\\)).")))
    (while keep-going-p
      (goto-char top)
      (message "Type TAB to move around, C-c C-c to finish.")
      (let ((inhibit-read-only nil))
        (recursive-edit))               ; blech
      (save-excursion
        (goto-char top)
        (mapc (lambda (old)
                (let ((p (point))
                      (new (ignore-errors (read (current-buffer)))))
                  (while (not (bolp))
                    (forward-char 1))
                  (save-excursion
                    (unless new
                      (goto-char p)
                      (insert ";;; syntax error")
                      (setq everything-ok-p nil))
                    (if (and new (not (equal old new)))
                        (push new better)
                      (goto-char p)
                      (insert ";;; unchanged")))))
              orig))
      (cond ((not everything-ok-p)
             (setq keep-going-p
                   (y-or-n-p "Problems! Do you want to try to fix them? ")))
            ((not better)
             (message "(No changes)")
             (sit-for 3)
             (setq keep-going-p nil))
            (better
             (delete-region top (point-max))
             (goto-char top)
             (dolist (ent better)
               (let ((id (car ent))
                     (data (mapcar 'cdr (cdr ent))))
                 (insert "ID: " (format "%s" id) "\n"
                         "New Date: " (nth 0 data) "\n"
                         "New Amount: " (format "%s" (nth 1 data)) "\n"
                         "New Attribute Code: " (nth 2 data) "\n"
                         "New Details: " (format "%S" (nth 3 data))
                         "\n => ")
                 (recenter -2)
                 (--etrack-bk :update-row (cons id data))
                 (--etrack-synch "PG-RESULT.*\n")
                 (insert "\n")
                 (recenter -2)))
             (when (y-or-n-p "Unmark updated entries? ")
               (dolist (id (mapcar 'car better))
                 (--etrack! :sel (delq id (--etrack: :sel))))
               (message "(%d unmarked, %d remaining)"
                        (length better) (length (--etrack: :sel)))
               (sit-for 2))
             (setq keep-going-p nil))
            (t
             (setq keep-going-p nil))))
    (remove-text-properties (point-min) (point-max) '(read-only t keymap t))))

(defun --etrack-cmd:upd ()
  (interactive)
  (--etrack-cmd-prep)
  (--etrack-catch-quit "Update" --etrack-menu
    (if (--etrack: :sel)
        (--etrack-update-marked)
      (--etrack-select-and-update))
    (--etrack-vw)))

(defun --etrack-cmd:edit-templates ()
  (interactive)
  (--etrack-cmd-prep)
  (--etrack-catch-quit "Edit Templates" --etrack-menu
    (let ((keep-going-p t)
          (all (--etrack-bk-read-list :templates-list))
          (template-name-map (copy-keymap minibuffer-local-completion-map))
          name newp ent rev v)
      (define-key template-name-map " " 'self-insert-command)
      (insert "(You can add a template, or delete or modify an existing one.)\n")
      (while keep-going-p
        (goto-char (point-max))
        (recenter -2)
        (if (string= "" (setq name
                              (let ((minibuffer-local-completion-map
                                     template-name-map))
                                (completing-read
                                 "Template: [TAB for list, RET to finish editing] "
                                 all))))
            (progn
              (insert "\nfinished editing\n\n")
              (setq keep-going-p nil))
          (unless (setq newp nil ent (assoc name all))
            (insert (format "\nAdding: %S => " name))
            (--etrack-bk :template-munge `(add ,name))
            (--etrack-synch "PG-RESULT.*\n")
            (setq all (--etrack-bk-read-list :templates-list)
                  ent (assoc name all)
                  newp t))
          (cond ((not ent)
                 (insert "(Sorry, weird problem with that one.)\n"))
                ((and (not newp) (y-or-n-p (format "Delete %S? " name)))
                 (insert (format "\nDeleting: %S => " name))
                 (--etrack-bk :template-munge `(del ,name))
                 (--etrack-synch "PG-RESULT.*\n")
                 (setq all (--etrack-bk-read-list :templates-list)))
                (t
                 (--etrack-insert-template-extrep "\nModifying: " ent)
                 (setq rev (copy-sequence ent))
                 (flet ((rs (pr def) (let ((s (read-string
                                               (format "New %s: " pr)
                                               def)))
                                       (unless (string= "" s) s))))
                   (setf (nth 2 rev) (rs "Amount" (nth 2 ent))
                         (nth 1 rev) (rs "Date" (nth 1 ent))
                         (nth 3 rev) (rs "Attribute Code" (nth 3 ent))
                         (nth 4 rev) (let ((d (rs "Details"
                                                  (if (setq v (nth 4 ent))
                                                      (format "%S" v)
                                                    "()"))))
                                       (when (stringp d) (read d)))))
                 (let (change pretty-change)
                   (unless (equal (nth 2 ent) (nth 2 rev))
                     (push "Amount" pretty-change)
                     (let ((a (nth 2 rev)))
                       (push (and a (truncate (* 100 (string-to-number a))))
                             change))
                     (push 'amount change))
                   (unless (equal (nth 1 ent) (nth 1 rev))
                     (push "Date" pretty-change)
                     (push (nth 1 rev) change)
                     (push 'date change))
                   (unless (equal (nth 3 ent) (nth 3 rev))
                     (push "Attribute Code" pretty-change)
                     (push (nth 3 rev) change)
                     (push 'attcode change))
                   (unless (equal (nth 4 ent) (nth 4 rev))
                     (push "Details" pretty-change)
                     (push (nth 4 rev) change)
                     (push 'details change))
                   (if (not change)
                       (insert "(No changes.)\n")
                     (insert "Changing: " (mapconcat 'identity
                                                     (reverse pretty-change)
                                                     ", ")
                             ". => ")
                     (--etrack-bk :template-munge `(mod ,name ,@change))
                     (--etrack-synch "PG-RESULT.*\n")
                     (setq all (--etrack-bk-read-list :templates-list))
                     (--etrack-insert-template-extrep
                      "Entry Now: " (assoc name all)))))))))
    (--etrack-vw)))

(defun --etrack-cmd:unmark-all ()
  (interactive)
  (if (--etrack: :sel)
      (--etrack! :sel nil)
    (message "(No entries currently marked)"))
  (--etrack-menu))

(defun --etrack-cmd:show-calendar ()
  (interactive)
  (--etrack-cmd-prep)
  (--etrack-catch-quit "Show Calendar" --etrack-menu
    (insert (shell-command-to-string
             (concat "cal " (read-string
                             (concat "YYYY, or MM YYYY,"
                                     " or RET for current month: ")))))
    (--etrack-vw)))

(defun --etrack-cmd:mail-session-log (&optional bailing)
  (interactive)
  (unless bailing
    (--etrack-cmd-prep))
  (--etrack-catch-quit "Mail Session Log" nil
    (let ((recip (read-string "Mail to: "))
          (subj (read-string "Subject: "))
          (msg (read-string "Short Message: " nil nil nil t)))
      (let ((s (save-restriction
                 (widen)
                 (buffer-string))))
        (compose-mail recip subj)
        (goto-char (point-max))
        (insert msg "\n\n====================\n")
        (insert s))
      (mail-send-and-exit nil)
      (insert "OK, mail sent\n")
      (sit-for 2)))
  (--etrack-menu))

(defun --etrack-cmd:about ()
  (interactive)
  (--etrack-cmd-prep)
  (--etrack-bk :about)
  (--etrack-vw nil t))

(defun --etrack-cmd:export ()
  (interactive)
  (--etrack-cmd-prep)
  (--etrack-catch-quit "Export" --etrack-menu
    (let* ((ok (append (when (--etrack: :sel)
                         (list '(?m . "marked")))
                       '((?p . "page")
                         (?d . "date range")
                         (?a . "all"))))
           (from (let* ((prompt (format "Source: %s: "
                                        (mapconcat (lambda (pair)
                                                     (format "(%c) %s"
                                                             (car pair)
                                                             (cdr pair)))
                                                   ok ", ")))
                        (c (read-char prompt)))
                   (when (assq c ok)
                     (insert "Source: " (cdr (assq c ok)))
                     c)))
           blurb n i-list range filename)
      (case from
        (?m (setq i-list (--etrack: :sel))
            (insert (format " (%d entr%s)"
                            (length i-list)
                            (if (= 1 (length i-list))
                                "y" "ies"))))
        (?p (setq n (read-number "Page: "))
            (if (not (and (< 0 n) (< n (--etrack: :page))))
                (setq blurb "invalid")
              (save-excursion
                (save-restriction
                  (widen)
                  (when (search-backward (format "=== Page %d ===" n)
                                         (point-min) t)
                    (let ((p (point))
                          (q (progn (search-forward "\f") (point))))
                      (goto-char p)
                      (while (re-search-forward "^ +\\([0-9]+\\)" q t)
                        (push (string-to-number (match-string 1)) i-list))))))
              (setq blurb (case (length i-list)
                            (0 "no entries there")
                            (1 "one entry")
                            (t (format "%d entries" (length i-list))))))
            (insert (format " %d (%s)" n blurb)))
        (?d (setq range (replace-regexp-in-string
                         "[,;]" ":"
                         (read-string "Date range (default: all entries): ")))
            (insert " " range))
        (?a (setq range ":"))
        (t (insert "No source specified")))
      (insert "\n")
      (if (and (not i-list) (not range))
          (let ((m (buffer-substring (progn (forward-line -1) (point))
                                     (progn (forward-line 1) (1- (point))))))
            (message "(Export aborted: %s)" m)
            (--etrack-menu))
        (setq filename (read-file-name
                        "Export to file: "
                        (when etrack-export-file
                          (file-name-directory etrack-export-file))
                        etrack-export-file
                        nil
                        (when etrack-export-file
                          (file-name-nondirectory etrack-export-file))
                        (lambda (name)
                          (not (file-directory-p name)))))
        (when (file-directory-p filename)
          (error "Cannot export to directory: %s" filename))
        (let* ((all (--etrack-bk-read-list
                     :dump (cons (expand-file-name filename)
                                 (if range
                                     `(date ,(if (= 0 (length range))
                                                 ":"
                                               range))
                                   `(id ,i-list)))))
               (actual-range (car all))
               (size (cadr all))
               (bytes (caddr all)))
          (unless (equal etrack-export-file filename)
            (set (make-local-variable 'etrack-export-file) filename))
          (let ((m (format "Wrote %d bytes, %d entries (%s) to %s"
                           bytes
                           (car size)
                           (if (memq from '(?d ?a))
                               (if (eq '= (car actual-range))
                                   (cadr actual-range)
                                 (format "%s thru %s"
                                         (car actual-range)
                                         (cdr actual-range)))
                             (cdr (assq from ok)))
                           filename)))
            (insert m "\n")
            (message "%s" m)))
        (--etrack-menu)))))

(defun --etrack-process-sentinel (proc string)
  (when (string-match "\n+$" string)
    (setq string (substring string 0 (match-beginning 0))))
  (discard-input)
  (when (y-or-n-p (concat (format "Database connection error: %S" string)
                          "\nMail session log before exiting? "))
    (--etrack-cmd:mail-session-log t))
  (kill-buffer (process-buffer proc)))

(defun etrack-kill-window-system-crap ()
  (setq inhibit-splash-screen t)
  (when (boundp 'automatic-hscrolling)
    (setq automatic-hscrolling nil))
  (when (boundp 'menu-bar-mode)
    (menu-bar-mode -1))
  (when window-system
    (setq cursor-in-non-selected-windows nil)
    (scroll-bar-mode -1)
    (when (boundp 'tool-bar-mode)
      (tool-bar-mode -1))))

(defun --etrack-kill-buffer-hook-function ()
  (--etrack! :sel nil)
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc (set-process-sentinel proc nil)))
  (if (member "etrack" command-line-args)
      (kill-emacs)
    (message "Bye!")))

;; entry point (command)

(defun etrack ()
  (interactive)
  (unless (getenv "ETRACK_CONFIG")
    (error "Need to set env var ETRACK_CONFIG."))
  (let ((user-init "~/.etrack.el"))
    (when (file-exists-p user-init)
      (load (expand-file-name user-init) nil t t)))
  (let (name)
    (set-buffer (get-buffer-create " *etrack name*"))
    (let ((files (split-string (getenv "ETRACK_CONFIG") ":")))
      (mapc 'insert-file-contents files)
      (setq files (nreverse files))
      (goto-char (point-min))
      (while (re-search-forward "^(name" (point-max) t)
        (beginning-of-line)
        (push (cons (cdr (read (current-buffer))) (pop files)) name)))
    (kill-buffer nil)
    (setq name (if (= 1 (length name))
                   (caar name)
                 (switch-to-buffer (get-buffer-create "(choose a database)"))
                 (erase-buffer)
                 (let ((i 0)
                       (choices (mapcar (lambda (x) (cons (car x) x)) name))
                       choice)
                   (dolist (n name)
                     (insert (format "%2d -- %s\n" (incf i) (car n)))
                     (push (cons (number-to-string i) n) choices))
                   (setq choice (cdr (assoc (completing-read
                                             "Which database? "
                                             choices nil t)
                                            choices)))
                   (setenv "ETRACK_CONFIG" (cdr choice))
                   (prog1 (car choice)
                     (kill-buffer nil)))))
    (switch-to-buffer
     (make-comint name "guile" nil "-s"
                  (or
                   "/home/ttn/build/etrack/pre-inst.etrack.scm" ; zonkme
                   "/home/ttn/build/etrack/etrack.scm")
                  "--client-encoding"
                  (let* ((i/o default-process-coding-system)
                         (base-i (coding-system-base (car i/o)))
                         (base-o (coding-system-base (cdr i/o))))
                    (or (and (eq base-i base-o)
                             (cdr (assq base-i etrack-coding-map)))
                        "LATIN1"))
                  "repl")))
  (add-hook 'kill-buffer-hook '--etrack-kill-buffer-hook-function nil t)
  (let ((proc (get-buffer-process (current-buffer))))
    (set-process-sentinel proc '--etrack-process-sentinel)
    (when (fboundp 'set-process-query-on-exit-flag)
      (set-process-query-on-exit-flag proc nil)))
  (setq major-mode 'etrack-mode
        mode-name "Etrack")
  (use-local-map etrack-mode-map)
  (cond ((= 0 (buffer-size))
         (set (make-local-variable '--etrack-state)
              (make-hash-table :test 'eq :size 11))
         (--etrack! :page 0)
         (insert (format-time-string "=== Page 0 === %Y-%m-%d %H:%M:%S ===\n")
                 "Session Log\n\n\n"))
        (t
         (widen)
         (goto-char (point-max))
         (insert "\n" "(Session set aside temporarily)" "\n")
         (message "(Resuming already-active session)")))
  (recenter -2)
  (setq truncate-lines t)
  (set (make-local-variable 'scroll-step) 1)
  (--etrack-menu))

;; non-definition expressions (rest of init)

(unless etrack-mode-map
  (setq etrack-mode-map
        (let ((m (make-sparse-keymap))
              (init (list [(?1)] '--etrack-cmd:query
                          [(?2)] '--etrack-cmd:add
                          [(?3)] '--etrack-cmd:delete
                          [(?4)] '--etrack-cmd:upd
                          [(?5)] '--etrack-cmd:prune-duplicates
                          [(?t)] '--etrack-cmd:add-using-template
                          [(?T)] '--etrack-cmd:edit-templates
                          [(?u)] '--etrack-cmd:unmark-all
                          [(?c)] '--etrack-cmd:show-calendar
                          [(?m)] '--etrack-cmd:mail-session-log
                          [(?a)] '--etrack-cmd:about
                          [(?x)] '--etrack-cmd:export
                          [(?v)] (lambda ()
                                   (interactive)
                                   (--etrack-cmd-prep)
                                   (--etrack-vw))
                          [(?q)] '(lambda ()
                                    (interactive)
                                    (kill-buffer nil))
                          ;; these need to be here so that
                          ;; S-up and S-down work in view mode.
                          ;; hmmm.
                          [(shift up)] 'undefined
                          [(shift down)] 'undefined)))
          (define-key m [remap self-insert-command] '--etrack-sorry)
          (while init
            (define-key m (car init) (cadr init))
            (setq init (cddr init)))
          m)))

(provide 'etrack)

;;; etrack.el ends here
