;;; -*- MODE: emacs-lisp; tab-width: 4 -*-
;;; todotxt-mode is a major mode for editing todo.txt files
;;; (see http://www.todotxt.com for more information about format and apps)
;;;
;;; (c) 2012 Adolfo Villafiorita <adolfo.villafiorita@ict4g.net>
;;;
;;; This code is NOT part of GNU Emacs and is distributed under the conditions
;;; of the MIT License
;;;
;;; The MIT License
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

;;;
;;; Installation instructions.
;;;
;;; Put in your .emacs file:
;;;
;;; (add-to-list 'load-path "<DIR WHERE TODOTXT-MODE LIVES>")
;;; (require 'todotxt-mode)
;;;
;;; Usage instruction
;;;
;;; Open a todo.txt file
;;;
;;;   M-x todotxt-mode
;;;   M-x describe-mode
;;;
;;; For some more customization:
;;;
;;;   (setq todotxt-default-file (expand-file-name "<<WHERE YOUR TODO FILE LIVES>>"))
;;;   (define-key global-map "\C-ct" 'todotxt-add-todo)
;;;   (define-key global-map "\C-co" 'todotxt-open-file)
;;;

(defvar todotxt-mode-map nil "Keymap for todotxt-mode")
(when (not todotxt-mode-map)
  (setq todotxt-mode-map (make-sparse-keymap))
  (define-key todotxt-mode-map (kbd "C-i") 'dabbrev-expand)
  (define-key todotxt-mode-map (kbd "C-c d") 'todotxt-toggle-done)
  (define-key todotxt-mode-map (kbd "C-c a") 'todotxt-pri-a)
  (define-key todotxt-mode-map (kbd "C-c b") 'todotxt-pri-b)
  (define-key todotxt-mode-map (kbd "C-c p") 'todotxt-pri)
  (define-key todotxt-mode-map (kbd "C-c n") 'todotxt-nopri)
  (define-key todotxt-mode-map (kbd "C-c t") 'todotxt-add-todo)
)

;;;
;;; Font-lock keywords
;;;

(defvar todotxt-mode-keywords nil "Font lock keywords for todotxt-mode")
(setq todotxt-mode-keywords
  '(
	("^x .*$" 0 '(:foreground "gray80" :strike-through t))
	("^(A).*$" 0 '(:background "red"))
	("^(B).*$" 0 '(:background "orange"))
	("([A-Z]+)" . font-lock-builtin-face)
	("\\([a-zA-Z0-9_-]+\\):\\([a-zA-Z0-9._-]+\\)" . font-lock-variable-name-face)
	("+[a-zA-Z0-9_-]+" . font-lock-function-name-face)
	("@[a-zA-Z0-9_-]+" . font-lock-type-face)
	("#[a-zA-Z0-9_-]+" . font-lock-comment-face)
	)
)

;;;
;;; Todotxt Functions
;;;

(defvar todotxt-default-file (expand-file-name "~/todo.txt") "Default todotxt file")

(defun todotxt-open-file ()
  "Open the default todo.txt file.

The function uses the elisp variable todotxt-default-file, whose value you might want
to set in your .emacs file"
  (interactive)
  (find-file todotxt-default-file))


(defun todotxt-add-todo (todo)
  "Add a todo to the default todotxt.file.

Three reasons for using the command rather than writing directly to the todo.txt file:

a. the todo is inserted at the end of the todo.txt file (no matter where you are)
b. the function adds a timestamp to the todo
c. the function can be called from any buffer (remember to set the variable todotxt-file)"
  (interactive "sAdd a todo, e.g. (A) Call Mom @phone +FamilialPeace: ")
  (save-excursion
	(set-buffer (find-file-noselect todotxt-default-file))
	(goto-char (point-max))
	(if (not (equal 0 (current-column))) (insert "\n"))
	(insert (concat (format-time-string "%Y-%m-%d ") todo))
	(save-buffer)
	(message (concat "Todo inserted at the end of " todotxt-default-file))))
  

(defun todotxt-toggle-done()
  "Toggle done status on task at cursor."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "x \\([0-9]+-[0-9]+-[0-9]+ \\)*")
		(delete-region (match-beginning 0) (match-end 0))
	  (complete-and-instantiate))))

(defun complete-and-instantiate ()
  "Take todo at point. Mark it as done.  If it contains a REPEAT
directive, instantiate a new instance of the todo and add it at
the end of the current buffer.

Not meant to be used directly: call todotxt-toggle-done instead,
which ensures save-excursion and pointer at beginning-of-line."
  (let ( (todo-as-string (todotxt-get-current-todo)) )
	;; complete the current todo (this has to be done in any case
	(insert (concat "x " (format-time-string "%Y-%m-%d ")))
	;; instantiate a new one if necessary 
	(let ( (repetition (todotxt-get-repetition todo-as-string)) )
	  (if repetition
		  ;; notice that repetition is a list (and not a time)
		  (let ( (new-todo (todotxt-move-dates todo-as-string repetition)) )
			(goto-char (point-max))
			(if (not (bolp)) (insert "\n"))
			(insert new-todo)
			(message (concat "Inserted "
							 new-todo 
							 " at the end of the buffer")))))))

;;;
;;; lower level functions to manage todos
;;;

(defun todotxt-get-current-todo ()
  "Get the current todo (= the todo at the line where the cursor is) as a string.
 (the function copies the current line; in the context of a
todo.txt file this is equivalent to copying a todo)"
  (interactive)
  (buffer-substring (line-beginning-position)
					(line-end-position)))

;; (defun todotxt-replace-current-todo (new-todo-as-string)
;;   "Replace the todo at the current line with the todo passed as argument.
;;  (the function replaces the line at point with the string passed
;; as argument. If applied to a todo.txt file this is equivalent to
;; replacing the current todo with a new todo)"
;;   (save-excursion
;; 	(delete-region (line-beginning-position) (line-end-position))
;; 	(insert new-todo-as-string)))

(defun todotxt-get-time (type todo-as-string)
  "Get the date of a field in the current string. 

First argument type is either 'DUE' or 'START'. The function returns the encoded time
after 'DUE' or 'START' appearing in the todo."
  (let ((match (string-match 
				(concat type ":\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)") 
				todo-as-string)))
	(if match
		(let ((year  (string-to-number (match-string 1 todo-as-string)))
			  (month (string-to-number (match-string 2 todo-as-string)))
			  (day   (string-to-number (match-string 3 todo-as-string))))
		  (encode-time 0 0 0 day month year))
	  nil)))

(defun todotxt-set-time (type new-date todo-as-string)
  "Set the date of a field in the current string. 

  * First argument type is either 'DUE' or 'START'.  
  * Second argument 'new-date' is the time to insert; it can be nil, in
    which case nothing is done and todo-as-string is returned untouched. 
  * Third argument todo-as-string is the string where new-time is inserted."
  (if new-date
	  (let ((new-date-as-string (format-time-string "%Y-%m-%d" new-date)))
		(replace-regexp-in-string (concat type ":[0-9]+-[0-9]+-[0-9]+") 
								  (concat type ":" new-date-as-string)
								  todo-as-string))
	todo-as-string))


(defun todotxt-move-dates (todo-as-string interval)
  "Given a todo as a string (first argument), create a new todo in which 
START and DUE date are moved according to interval (second argument).

The second argument is typically the output of a todotxt-get-repetition call."
  ;; this function exploits the fact that todotxt-move-time and todotxt-set-time manage
  ;; nil values in input (and return what is expected: nil in the first case, the input string in 
  ;; the second case)
  (let ( (new-start-time (todotxt-add-interval interval (todotxt-get-time "START" todo-as-string))) 
		 (new-due-time (todotxt-add-interval interval (todotxt-get-time "DUE" todo-as-string))) )
	(todotxt-set-time "START" 
					  new-start-time 
					  (todotxt-set-time "DUE" new-due-time todo-as-string))))


(defun todotxt-add-interval (interval time)
  "Add interval to time. 
First argument interval is in the format required by encode-time.
Second argument time is a time (the output of a encode-time).

The function returns a time."
  (if (or (eq time nil) (eq interval nil))
	  nil
	(apply 'encode-time (todotxt-recursive-sum interval (decode-time time)))))

(defun todotxt-recursive-sum (a b)
  "Sum the elements of two lists, element by element.
Do so only for the N-th elements, where N is the length of the shortest list.
 (todotxt-recursive-sum '(1 2 3) '(3 4 5)) -> (4 6 8)"
  (if (and a b)
	  (cons (+ (car a) (car b)) (rec-sum (cdr a) (cdr b))) 
	nil))


(defvar todotxt-repetitions-assoc nil
  "Association list of repetitions and functions that take as input a string and return the time interval specified by the repetition, using the 'time' conventions.")
(setq todotxt-repetitions-assoc
  '(("daily"    . (lambda (x) '(0 0 0 1 0 0)))
	("weekly"   . (lambda (x) '(0 0 0 7 0 0)))
	("monthly"  . (lambda (x) '(0 0 0 0 1 0)))
	("yearly"   . (lambda (x) '(0 0 0 0 0 1)))
	("\\([0-9]+\\)\\.day"   . (lambda (x) (progn
											(string-match "\\([0-9]+\\)\\.day" x)
											(list 0 0 0 (string-to-int (match-string 1 x)) 0 0))))
	("\\([0-9]+\\)\\.week"  . (lambda (x) (progn
											(string-match "\\([0-9]+\\)\\.week" x)
											(list 0 0 0 (* 7 (string-to-int (match-string 1 x))) 0 0))))
	("\\([0-9]+\\)\\.month" . (lambda (x) (progn
											(string-match "\\([0-9]+\\)\\.month" x)
											(list 0 0 0 0 (string-to-int (match-string 1 x)) 0))))
	("\\([0-9]+\\)\\.year"  . (lambda (x) (progn
											(string-match "\\([0-9]+\\)\\.year" x)
											(list 0 0 0 0 0 (string-to-int (match-string 1 x))))))))

(defun todotxt-get-repetition (todo-as-string)
  "Extract a repetition string from todo-as-string (a string
representing a todo) and return a tuple (seconds minutes hours days months years)
encoding the repetition and good for encode-time.

The value of todotxt-repetitions-assoc encodes the specification
of the repetition strings."
  ;; the following code looks for the car's of repetitions-assoc (regular
  ;; expressions with repetitions) and applies the cdr of the matching pair if
  ;; the string matches. The output is a list of the type (nil nil nil
  ;; ... nil) or (nil nil (0 1 0) nil ...) -- with at most one element which
  ;; is not nil (and if not nil it is the repetition interval)
  (let ( (result (mapcar '(lambda (x)
							(if (string-match (concat "RECUR:" (car x)) todo-as-string)
								(funcall (cdr x) todo-as-string)
							  nil))
						 todotxt-repetitions-assoc)) )
	;; find the first non nil occurrence (if any), return nil otherwise
	(find-if '(lambda (x) (not (eq x nil))) result)))


(defun todotxt-pri (char)
  "Set (or change) priority of task at cursor.
The function works only if the task has not been marked as completed."
  (interactive "cSet new priority [A-Z] for task ")
  (if (not (or (and (>= char ?a) (<= char ?z))
	       (and (>= char ?A) (<= char ?Z))))
      (message "Priority has to be between 'A' and 'Z'.")
    (save-excursion
      (beginning-of-line)
      (if (looking-at "x ")
	  (message "Task is marked as done. Priority not changed.")
	;; be very permissive on the kind of priority we look at 
	;; (so that we can use the command to insert the correct priority)
	(if (looking-at "(.) ")
	    (delete-char 4)) ; delete also the space (and reinsert it)
	(insert (format "(%c) " (upcase char)))))))

(defun todotxt-pri-a ()
  "Set priority of task at cursor to 'A'.
See also todotxt-pri and todotxt-nopri."
  (interactive)
  (todotxt-pri ?A))

(defun todotxt-pri-b ()
  "Set priority of task at cursor to 'B'.
See also todotxt-pri and todotxt-nopri."
  (interactive)
  (todotxt-pri ?B))

(defun todotxt-nopri ()
  "Remove priority of task at cursor."
  (interactive)
  (if (looking-at "x ")
      (message "Task is marked as done. Priority not removed.")
    (if (looking-at "(.) ")
		(delete-char 4)
	  (message "Task at cursor does not seem to have a valid priority."))))

;;;
;;; Todotxt Major Mode
;;;

(defun todotxt-mode ()
  "Major mode for editing todo.txt files.

The mode (natively) implements a few commands to ease interaction
with a todo.txt file.  (The mode is not a replacement for the todo.txt 
app, which you can download from http://www.todotxt.com.)

Fontification eases presentation of information.

The mode does not depend upon the todo.txt app, thanks also to
the extremely simple and effective format defined for todo.txt
files.  For more information about todo.txt, http://www.todotxt.com.

The mode also implements a syntax extension to support task
repetition, due and start dates.  In particular the following
strings have a special meaning in a todo:

* DUE:YYYY-MM-DD (e.g. DUE:2012-12-15), to mark the due date
* START:YYYY-MM-DD (e.g. START:2012-11-15), to mark the first date when
  a todo can actually be started.
* RECUR:repetition, where repetition is any of:
  - 'daily', 'weekly', 'monthly', 'yearly' or 
  - N.period where period is 'year', 'month', 'week', 'day'
  (e.g. RECUR:yearly, RECUR:2.year), to mark that a taks repeats once 
  completed.

The following actions are taken by todotxt-mode:

* if a task containing a 'RECUR' directive is marked as complete
  using the todotxt-mark-done command, a new instance of the task
  is created with the correct DUE and START directives, if present.


\\{todotxt-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'todotxt-mode)
  (setq mode-name "todo.txt") ; for display purposes in mode line
  (use-local-map todotxt-mode-map)
  (setq font-lock-defaults '(todotxt-mode-keywords))
  (run-hooks 'todotxt-mode-hook))

(provide 'todotxt-mode)
