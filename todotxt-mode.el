;;; -*- MODE: emacs-lisp; tab-width: 4 -*-
;;; todotxt-mode is a major mode for editing todo.txt files
;;; (see http://www.todotxt.com for more information about format and apps)
;;;
;;; (c) 2012, 2013 Adolfo Villafiorita <adolfo.villafiorita@me.com>
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
  (define-key todotxt-mode-map (kbd "C-c C-p") 'todotxt-group-by-project)
  (define-key todotxt-mode-map (kbd "C-c C-t") 'todotxt-group-by-tag)
  (define-key todotxt-mode-map (kbd "C-c C-d") 'todotxt-group-by-date)
  (define-key todotxt-mode-map "x" 'todotxt-insert-x-maybe-complete)
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
	("^.*#waiting.*" 0 '(:foreground "DeepPink1")) ; special tag
	("^.*#important.*" 0 '(:foreground "IndianRed")) ; special tag
	("([A-Z]+)" . font-lock-builtin-face)
	("\\([a-zA-Z0-9_-]+\\):\\([a-zA-Z0-9._-]+\\)" . font-lock-variable-name-face)
	("+[a-zA-Z0-9_-]+" . font-lock-function-name-face)
	("@[a-zA-Z0-9_-]+" . font-lock-type-face)
	("#[a-zA-Z0-9_-]+" . font-lock-comment-face)
	("^[0-9]+-[0-9]+-[0-9]+" 0 '(:foreground "gray90"))
	)
)

;;;
;;; Todotxt Functions for managing the file
;;;

(defvar todotxt-default-file (expand-file-name "~/todo.txt")
  "Default todotxt file")
(defvar todotxt-default-archive-file (expand-file-name "~/done.txt")
  "Default todotxt archive file")

;;;
;;; 1. File Level Actions
;;;

(defun todotxt-open-file ()
  "Open the default todo.txt file.

The function uses the elisp variable todotxt-default-file, whose value you might want
to set in your .emacs file"
  (interactive)
  (find-file todotxt-default-file))

(defun todotxt-archive ()
  "Archive done tasks found in the current buffer."
  (interactive)
  (let ((archive-buffer (find-file-noselect todotxt-default-archive-file)))
	(save-excursion
	  (goto-char (point-min))
	  (while (not (eobp))
		; if we are looking at a done todo, we delete it from the
		; buffer (including the final \n and append it to the done
		; buffer *otherwise* we move one line (notice that deletion in
		; fact moves the pointer one line down or, better, move the
		; text one lines up... in any case we do not need to move forward
		(if (looking-at "^x.*[\r\n]")
			(let ((done-todo (buffer-substring (match-beginning 0) (match-end 0))))
			  (with-current-buffer archive-buffer
				(goto-char (point-max))
				(insert done-todo))
			  (delete-region (match-beginning 0) (match-end 0)))
		  (forward-line 1)))
	  (save-buffer archive-buffer))))


;;;
;;; 2. Todo Level Functions
;;; 2a. Add a todo
;;; 

(defvar todotxt-prepend-today-date t
  "Whether today's date is added to newly created todos.")

(defun todotxt-add-todo (todo)
  "Add a todo to the default todotxt.file.

Three reasons for using the command rather than writing directly to the todo.txt file:

a. the todo is inserted at the end of the todo.txt file
   (no matter where you are)

b. the function adds a timestamp to the todo
   (if todotxt-prepend-today-date is non nil)

c. the function can be called from any buffer
   (remember to set the variable todotxt-file)"
  (interactive "sAdd a todo, e.g. (A) Call Mom @phone +FamilialPeace: ")
  (with-current-buffer (find-file-noselect todotxt-default-file)
	(goto-char (point-max))
	(if (not (equal 0 (current-column))) (insert "\n"))
	(insert (todotxt-prepend-today-date todo))
	(save-buffer)
	(message (concat "Todo inserted at the end of " todotxt-default-file))))
  
(defalias 'todotxt-insert-todo 'todotxt-add-todo)

(defun todotxt-prepend-today-date (todo-as-string)
  "Prepend today's date to the argument (a string representing a todo), according to the value of todotxt-prepend-today-date"
  (if todotxt-prepend-today-date
	  (if (not (string-match "^([A-Za-z])" todo-as-string))
		  (concat (format-time-string "%Y-%m-%d ") todo-as-string)
		(concat (substring todo-as-string 0 3) (format-time-string " %Y-%m-%d ") (substring todo-as-string 3)))
	todo-as-string))


;;;
;;; 2. Todo Level Functions
;;; 2b. Todo priorities
;;;

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
;;; 2. Todo Level Functions
;;; 2c. Mark a todo as done
;;; 

(defun todotxt-toggle-done ()
  "Toggle done status on task at cursor."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "x \\([0-9]+-[0-9]+-[0-9]+ \\)*")
		(delete-region (match-beginning 0) (match-end 0))
	  (todotxt-complete-and-instantiate))))

(defun todotxt-complete-and-instantiate ()
  "Lower level function for todotxt-toggle-done.

Take todo at point. Mark it as done. If it contains a REPEAT
directive, instantiate a new instance of the todo and add it at
the end of the current buffer."
  (save-excursion
	(let ( (todo-text (todotxt-scrub-date-and-priority (todotxt-get-current-todo))) )
	  ;; complete the current todo (this has to be done in any case
	  (beginning-of-line)
	  ;; remove the priority from the completed todo
	  (if (looking-at "^\\(([A-Za-z]) \\)")
		  (delete-region (match-beginning 0) (match-end 0)))
	  (insert (concat "x " (format-time-string "%Y-%m-%d ")))
	  ;; instantiate a new todo if necessary 
	  (let ( (repetition (todotxt-get-repetition todo-text)) )
		(if repetition
			;; notice that repetition is a list (and not a time)
			(let ( (new-todo (todotxt-move-dates todo-text repetition)) )
			  (goto-char (point-max))
			  (if (not (bolp)) (insert "\n"))
			  (insert (todotxt-prepend-today-date new-todo))
			  (message (concat "Inserted "
							   new-todo 
							   " at the end of the buffer"))))))))

(defun todotxt-scrub-date-and-priority (todo-as-string)
  "Remove date and priority from the beginning of a todo, if present"
  (if (string-match "^\\(([A-Za-z]) \\)*[0-9]+-[0-9]+-[0-9]+[ ]*" todo-as-string)
	  (substring todo-as-string (match-end 0))
	todo-as-string))

(defun todotxt-insert-x-maybe-complete ()
  "Used to make sure that recurring tasks are instantiated even if a task is completed by writing an 'x' at the beginning of a line.

This function has to be bound to 'x' (in a mode local
fashion!). If 'x' is written at the beginning of the line, then
todotxt-complete-and-instantiate is executed; otherwise 'x' is
inserted."
  (interactive)
  (if (looking-at "^")
	  (todotxt-complete-and-instantiate)
	(insert "x")))
  

;;;
;;; 3. Lower level functions to manage todos
;;;

(defun todotxt-get-current-todo ()
  "Get the todo at the line where the cursor is as a string.

 (the function copies the current line; in the context of a
todo.txt file this is equivalent to copying a todo)"
  (interactive)
  (buffer-substring (line-beginning-position)
					(line-end-position)))

;;;
;;; Todos and projects, Todo and Tags
;;;

(defun todotxt-search-by-project () 
  "List all todos of a project, completing over project names."
  (interactive)
  (todotxt-show-machinery '(lambda () 
							(todotxt-collect-special-strings "\\+[a-zA-Z0-9_-]+"))))

(defun todotxt-search-by-tag () 
  "List all todos with a tag, completing over tag names."
  (interactive)
  (todotxt-show-machinery '(lambda () 
							(todotxt-collect-special-strings "#[a-zA-Z0-9_-]+"))))

(defun todotxt-search-by-date () 
  "List all todos with a special date tag (today, upcoming, ...), completing over date names."
  (interactive)
  (todotxt-highlight-dated-todos)
  (todotxt-show-machinery '(lambda () 
							(todotxt-collect-special-strings "-[a-zA-Z][a-zA-Z0-9_-]+"))))

(defun todotxt-search-by-person () 
  "List all todos with a person tag (e.g. @someone)"
  (interactive)
  (todotxt-highlight-dated-todos)
  (todotxt-show-machinery '(lambda () 
							(todotxt-collect-special-strings "@[a-zA-Z0-9_-]+"))))

;;
;; Lower level function to collect special tags
;;

(defun todotxt-show-machinery (function)
  "Low level machinery for displaying special marked todos (projects, tags, datetags)."
  (save-excursion
  (let ( (elements (funcall function)) )
	(let ( (found-set (completing-read "Enter what you want to display: " elements)))
	  (occur found-set)))))

(defun todotxt-collect-special-strings (regexp)
  "Return all occurrences of regexp in current-buffer as a list good for completing-read.
The function is the basic infrastructure for special marked strings in todotxt."
  (let ( (elements nil) )
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward regexp nil t)
		(add-to-list 'elements (buffer-substring-no-properties (match-beginning 0) (match-end 0)))))
	elements))

;;;
;;; present todos according to various criteria
;;;

(defun todotxt-group-by-date ()
  "Present todos grouped by date (today, upcoming and overdue)"
  (interactive)
  (progn
	(todotxt-highlight-dated-todos)
	(todotxt-get-and-print-todos-with-keys "Date" '("-today" "-ucpoming" "-overdue"))))

(defun todotxt-group-by-project ()
  "Present todos grouped by project"
  (interactive)
  (progn 
	(todotxt-highlight-dated-todos)
	(todotxt-get-and-print-todos-with-keys "Project" (todotxt-collect-special-strings "\\+[a-zA-Z0-9_-]+"))))

(defun todotxt-group-by-tag ()
  "Present todos grouped by tag"
  (interactive)
  (progn
	(todotxt-highlight-dated-todos)
	(todotxt-get-and-print-todos-with-keys "Tag" (todotxt-collect-special-strings "#[a-zA-Z0-9_-]+"))))

(defun todotxt-group-by-person ()
  "Present todos grouped by tag"
  (interactive)
  (progn
	(todotxt-highlight-dated-todos)
	(todotxt-get-and-print-todos-with-keys "Person" (todotxt-collect-special-strings "@[a-zA-Z0-9_-]+"))))

;;;
;;; Lower level machinery
;;; 

(defun todotxt-get-and-print-todos-with-keys (buffer-name key-list)
  "Organize todos according to key-list and output them to *Output* buffer"
  (let ( (output-list (todotxt-get-todos-with-keys key-list)) )
	(let ( (buffer (get-buffer-create (concat "*TODOTXT Grouping by " buffer-name "*"))) )
	  (set-buffer buffer)
	  (erase-buffer)
	  (todotxt-print-todos output-list)
	  (pop-to-buffer buffer))))

(defun todotxt-get-todos-with-keys (key-list)
  "Organize todos according to key-list"
  (if (not key-list)
		nil
	(cons (todotxt-get-todos-with-key (car key-list))
		  (todotxt-get-todos-with-keys (cdr key-list)))))

(defun todotxt-get-todos-with-key (key)
  "Get all the todos matching a key and return a list in the form (key . ((buffer point TODO1) ... (buffer point TODON)))"
  (let ( (output-list nil)
		 (buffer (current-buffer)) )
	(save-excursion
	  (goto-char (point-min))
	  (while (not (eobp))
		(let ( (todo (todotxt-get-current-todo)) )
		  (if (string-match key todo)
			  (setq output-list (append (list (list buffer (point) todo)) output-list)))
		  (forward-line 1)))
	  (cons key output-list))))

(defun todotxt-print-todos (list)
  "Print the todos organized by keys.

The structure is as follows:

  ( ('keyA' ( (TODO1) (TODO2) ...))
    ('keyB' ...))

where TODO is a list

  (BUFFER POINT TODO-TXT)"
  (while list
	; get the first element of the list (a group)
	(let ( (first (car list)) )
	  (insert "\n* TODOS MATCHING: " (car first) "\n")
	  (let ( (todos (cdr first)) )
		(while todos
		  (let ( (todo (car todos)) )
			(insert "\t")
			(insert-button (nth 2 todo)
						   'target-buffer (nth 0 todo)
						   'target-pos    (nth 1 todo)
; it does not work and I do not know why :-(
;						   'mouse-action '(lambda (x)
;											(pop-to-buffer (button-get x 'target-buffer))
;											(goto-char (button-get x 'target-pos)))
						   'action       '(lambda (x)
											(pop-to-buffer (button-get x 'target-buffer))
											(goto-char (button-get x 'target-pos))))
			(insert "\n")
			(setq todos (cdr todos)))))
	  (setq list (cdr list)))))

;;;
;;; Todos and done status
;;;
(defun todotxt-is-done (todo-as-string)
  (equal 0 (string-match "^x .*" todo-as-string)))

;;;
;;; Todos and dates
;;; (start, due, creation, completion)
;;;

;;
;; Tags configuration
;;

(defvar todotxt-due-tag "d"
  "The default tag used for due dates.
It defaults to 'd'; set it to 'due' for integration with TaskCoach")

(defvar todotxt-threshold-tag "t"
  "The default tag used for threshold dates (tasks are inactive before the threshold).
It defaults to 't'; set it to 'start' for integration with TaskCoach")

(defvar todotxt-repetition-tag "r"
  "The default tag used for repetition intervals.")

;;
;; Todos and dates
;; 

(defun todotxt-get-due (todo-as-string)
  "Get the due date of a todo."
  (todotxt-get-date todotxt-due-tag todo-as-string))

(defun todotxt-get-threshold (todo-as-string)
  "Get the threshold date of a todo."
  (todotxt-get-date todotxt-threshold-tag todo-as-string))


(defun todotxt-set-due (new-date todo-as-string)
  "Set the due date of a todo."
  (todotxt-set-date todotxt-due-tag new-date todo-as-string))

(defun todotxt-set-threshold (new-date todo-as-string)
  "Set the threshold date of a todo."
  (todotxt-set-date todotxt-threshold-tag new-date todo-as-string))

;;;
;;; Lower level functions for setting/getting time
;;;

(defun todotxt-get-date (type todo-as-string)
  "Get the date of a field in the current string. 

First argument TYPE is a string specifying a 'due' or a 'threshold' key (e.g. 'd', 't')
Second argument TODO-AS-STRING is a string representing a todo, possibly with no dates.

The function returns the calendrical date after TYPE appearing in the
todo.

Example

  (todotxt-get-date \"DUE\" \"DUE:2012-03-04\")
  => (0 0 0 4 3 2012)

The function is agostic to the key used (in fact, the key is passed as input to the
function). 

I.e. d:2012-03-13, DUE:2012-03-13, due:2012-03-13, START:2012-03-13 will all return
the same value, 2012-03-13"
  (let ((match (string-match 
				(concat type ":\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)") 
				todo-as-string)))
	(if match
		(let ((year  (string-to-number (match-string 1 todo-as-string)))
			  (month (string-to-number (match-string 2 todo-as-string)))
			  (day   (string-to-number (match-string 3 todo-as-string))))
		  (list 0 0 0 day month year))
	  nil)))

(defun todotxt-set-date (type new-date todo-as-string)
  "Set the date of a field in the current string. 

First argument TYPE is a string specifying a 'due' or a 'threshold' key (e.g. 'due', 'start')

Second argument NEW-DATE is the calendrical date to insert
instead of the current value; it can be nil, in which case
nothing is done.

Third argument TODO-AS-STRING is the todo in which the date has to be replaced."
  (if new-date
	  (let ((new-date-as-string 
			 (format-time-string "%Y-%m-%d" (apply 'encode-time new-date))))
		(replace-regexp-in-string (concat type ":[0-9]+-[0-9]+-[0-9]+") 
								  (concat type ":" new-date-as-string)
								  todo-as-string))
	todo-as-string))


(defun todotxt-move-dates (todo-as-string interval)
  "Move START and DUE date of a given amount

First argument TODO-AS-STRING is the todo in which START and DUE
dates have to be moved.

Second argument INTERVAL is the specification of how much the
dates have to be moved. It is in the form of a calendrical
date (e.g. (0 0 0 1 0 0)) and often it is derived from a recurrence
specification in the todo (see todotxt-get-repetition)."
  ;; this function exploits the fact that todotxt-add-interval and
  ;; todotxt-set-date manage nil values in input (and return what is expected:
  ;; nil in the first case, the input string in the second case)
  (let ( (new-threshold (todotxt-add-interval interval 
											  (todotxt-get-threshold todo-as-string)))
		 (new-due (todotxt-add-interval interval
										(todotxt-get-due todo-as-string))) )
	(todotxt-set-threshold new-threshold 
						   (todotxt-set-due new-due todo-as-string))))


(defun todotxt-add-interval (interval time)
  "Add INTERVAL to TIME. 

First argument INTERVAL is a calendrical representation of a time
interval (see todotxt-get-repetition).

Second argument TIME is a calendrical representation of a date.

The function moves TIME by the amount of time specified by INTERVAL.

Example

   (todotxt-add-interval '(0 0 0 1 0 0) '(0 0 0 10 12 2012))
   => (0 0 0 11 12 2012)"
  (if (or (eq time nil) (eq interval nil))
	  nil
	(todotxt-recursive-sum interval time)))

(defun todotxt-recursive-sum (a b)
  "Sum the elements of two lists, element by element.

Do so only for the first N-th elements, where N is the length of the shortest list.

 (todotxt-recursive-sum '(1 2 3) '(3 4 5)) -> (4 6 8)"
  (if (and a b)
	  (cons (+ (car a) (car b)) (todotxt-recursive-sum (cdr a) (cdr b))) 
	nil))

(defvar todotxt-repetitions-assoc nil
  "Association list of repetitions and functions that take as
  input a string and return the time interval specified by the
  repetition, using the 'time' conventions.")

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
  "Extract a repetition specification from a todo.

Argument TODO-AS-STRING is the todo from which we want to extract
the repetition specification.

The function returns a calendrical representation of the
repetition interval, that is a list in the form:

  (seconds minutes hours days months years)

encoding the repetition.

For instance 

  (todotxt-get-repetition \"r:2.weeks\")
  => (0 0 0 14 0 0)

The variable todotxt-repetitions-assoc encodes the specification
of the understood repetition strings."
  ;; the following code looks for the car's of repetitions-assoc (regular
  ;; expressions with repetitions) and applies the cdr of the matching pair if
  ;; the string matches. The output is a list of the type (nil nil nil
  ;; ... nil) or (nil nil (0 1 0) nil ...) -- with at most one element which
  ;; is not nil (and if not nil it is the repetition interval)
  (let ( (result (mapcar (lambda (x)
						   (if (string-match (concat todotxt-repetition-tag (car x)) todo-as-string)
							   (funcall (cdr x) todo-as-string)
							 nil))
						 todotxt-repetitions-assoc)) )
	;; find the first non nil occurrence (if any), return nil otherwise
	(todotxt-find-if (lambda (x) (not (eq x nil))) result)))

(defun todotxt-find-if (predicate list)
  "Reimplementation of find-if in cl package.

 (so that there is no need to load all cl package for just one function)."
  (if list
	  (if (funcall predicate (car list))
		  (car list)
		(todotxt-find-if predicate (cdr list)))
	nil))


;;;
;;; Chronic Expression
;;;

(defvar todotxt-repetitions-assoc nil
  "Association list of repetitions and functions that take as
  input a string and return the time interval specified by the
  repetition, using the 'time' conventions.")

(setq todotxt-repetitions-assoc
  '(("[0-9] day"    . (lambda (x) '(0 0 0 1 0 0)))


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

;;;
;;; Mark tasks according to dates
;;;

(defvar todotxt-upcoming-days 7
  "*How many days raise the upcoming flag.
Default value, 7, means that a task is marked as upcoming if its due date is in the next seven days.")

(defvar todotxt-today-face '(foreground-color . "red1")
  "*Face to use to highlight tasks due today.")

(defvar todotxt-overdue-face '(foreground-color . "dark red")
  "*Face to use to highlight tasks overdue tasks.")

(defvar todotxt-upcoming-face '(foreground-color . "dark blue")
  "*Face to use to highlight upcoming tasks.")

(defvar todotxt-inactive-face '(foreground-color . "gray90")
  "*Face to use to highlight inactive tasks.")


(defun todotxt-highlight-dated-todos ()
  (interactive)
  (save-excursion
	;; remove all the special strings (today, overdue, ...)
	(goto-char (point-min))
	(while (re-search-forward " -\\(overdue\\|today\\|upcoming\\|inactive\\)" nil t)
	  (replace-match ""))
	;; remove all overlays
	(remove-overlays (point-min) (point-max))
	;; now look for due and threshold dates and re-insert the string and the overlays
	(goto-char (point-min))
	  (while (not (eobp))
		(let ((current-todo (todotxt-get-current-todo)))
		  (let ( (due (todotxt-get-due current-todo))
				 (done (todotxt-is-done current-todo))
				 (threshold (todotxt-get-threshold current-todo)) )
			(if (not done)
				(progn
				  (if due
					  (progn
						(if (todotxt-overdue due) 
							(todotxt-highlight-dated-todo "-overdue" todotxt-overdue-face))
						(if (todotxt-today due) 
							(todotxt-highlight-dated-todo "-today" todotxt-today-face))
						(if (todotxt-next-n-days due todotxt-upcoming-days)
							(todotxt-highlight-dated-todo "-upcoming" todotxt-upcoming-face))))
				  (if threshold
					  (if (todotxt-inactive threshold)
						  (todotxt-highlight-dated-todo "-inactive" todotxt-inactive-face)))))
			(forward-line 1))))))

;;
;; predicates for special dates and special
;;

(defun todotxt-overdue (due)
  (let ( (due-in-days (time-to-days (apply 'encode-time due)))
		 (today-in-days (time-to-days (current-time))) )
	(< due-in-days today-in-days)))

(defun todotxt-today (due)
  (let ( (due-in-days (time-to-days (apply 'encode-time due)))
		 (today-in-days (time-to-days (current-time))) )
	(equal due-in-days today-in-days)))

(defun todotxt-inactive (threshold)
  (let ( (threshold-in-days (time-to-days (apply 'encode-time threshold)))
		 (today-in-days (time-to-days (current-time))) )
	(> threshold-in-days today-in-days)))

(defun todotxt-next-n-days (due n)
  (let ( (due-in-days (time-to-days (apply 'encode-time due)))
		 (today-in-days (time-to-days (current-time))) )
	(and (> (+ today-in-days n) due-in-days)
		 (not (todotxt-today due))
		 (not (todotxt-overdue due)))))
  

;;
;; insert special string and add overlay to current line
;; low level machinery for hightlight-dated-todos

(defun todotxt-highlight-dated-todo (string face)
  (save-excursion
	(end-of-line)
	(insert " " string)
	(let ( (overlay (make-overlay (line-beginning-position) (line-end-position))) )
	  (overlay-put overlay 'face face))))

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

* d:YYYY-MM-DD (e.g. d:2012-12-15), to mark the due date

* t:YYYY-MM-DD (e.g. t:2012-11-15), to mark the first
  date (threshold) when a todo can actually be started.

* r:repetition, where repetition is any of: - 'daily', 'weekly',
  'monthly', 'yearly' or - N.period where period is 'year',
  'month', 'week', 'day' (e.g. r:yearly, r:2.year), to mark that
  a taks repeats once completed.

The following actions are taken by todotxt-mode:

* if a task containing a 'r:' directive is marked as complete
  using the todotxt-mark-done command (or typing an 'x' in front
  of the todo), a new instance of the task is created with the
  correct due (d:) and threshold (t:) directives, if present.


\\{todotxt-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'todotxt-mode)
  (setq mode-name "todo.txt") ; for display purposes in mode line
  (use-local-map todotxt-mode-map)
  (setq font-lock-defaults '(todotxt-mode-keywords))
  (run-hooks 'todotxt-mode-hook))


(provide 'todotxt-mode)

