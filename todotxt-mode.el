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
	  (insert (concat "x " (format-time-string "%Y-%m-%d "))))))

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

\\{todotxt-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'todotxt-mode)
  (setq mode-name "todo.txt") ; for display purposes in mode line
  (use-local-map todotxt-mode-map)
  (setq font-lock-defaults '(todotxt-mode-keywords))
  (run-hooks 'todotxt-mode-hook))

(provide 'todotxt-mode)
