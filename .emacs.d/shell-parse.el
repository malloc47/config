;;; shell-parse.el

;; minor mode definitions
(define-minor-mode shell-parse-mode
 "Minor mode to parse arbitrary shell commands for terminal completion"
 nil
 " sp"
 '(([C-tab] . shell-parse-complete)
   ([C-M-tab] . shell-parse-complete-first)
   ([C-S-iso-lefttab] . shell-parse-complete-backwards)
   ("\C-c\C-q" . shell-parse-query))
 :group 'shell-parse)

(defcustom shell-parse-command
 "echo -en $(curl -H \"Accept: application/json\" \"http://suggestqueries.google.com/complete/search?client=firefox&q=%s\" 2>/dev/null)"
 "Command to be parsed.  Use a %s for the query to be substituted."
 :type 'string
 :group 'shell-parse)

(defcustom shell-parse-split
 ","
 "Delimiter for command output"
 :type 'string
 :group 'shell-parse)

;; persistent variables 
;; todo: make these local to buffer

(setq shell-parse-list '())
(setq shell-parse-word "")
(setq shell-parse-pos (point-min))

;; functions

(defun shell-parse-clear ()
 (message "cleared")
 (setq shell-parse-list '()))

(defun shell-parse-query ()
 (interactive)
 (if (thing-at-point 'word)
   (message
    (mapconcat 'identity 
	       (shell-parse-preprocess 
		(shell-parse-request 
		 (thing-at-point 'word)))
	       ","))
  (message "")))

(defun shell-parse-complete-first ()
 (interactive)
 (end-of-thing 'word)
 (let ((s (thing-at-point 'word)))
  (when s
   (let ((q (shell-parse-preprocess 
	      (shell-parse-request 
	       s))))
    (when q
     (let ((p (substring
	       (car q)
	       (length s))))
      (setq shell-parse-pos (point))
      (insert p)
      (setq shell-parse-list q)
      (setq shell-parse-word s)))))))

(defun shell-parse-complete ()
 (interactive)
 (shell-parse-complete-proto 'rotate-forwards))

(defun shell-parse-complete-backwards ()
 (interactive)
 (shell-parse-complete-proto 'rotate-backwards))

(defun shell-parse-complete-proto (f)
 ;; (interactive)
 (if (and 
      shell-parse-list
      (string= 
       (buffer-substring 
	(min shell-parse-pos (point-max)) 
	(point)) 
       (substring 
	(car shell-parse-list) 
	(length shell-parse-word))))
   (progn
    (end-of-thing 'word)
    (backward-delete-char 
     (- 
      (length (car shell-parse-list)) 
      (length shell-parse-word)))
    (setq shell-parse-list (funcall f shell-parse-list))
    (insert (substring
	     (car shell-parse-list)
	     (length shell-parse-word))))
  (let ((s (thing-at-point 'word)))
   (when s
    (let* ((q (shell-parse-preprocess 
	       (shell-parse-request 
		s))))
     (when q
      (let ((p (substring
		(car q)
		(length s))))
       (end-of-thing 'word)
       (setq shell-parse-pos (point))
       (insert p)
       (setq shell-parse-list q)
       (setq shell-parse-word s))))))))

(defun shell-parse-preprocess (query)
 (let ((l (split-string 
	   (apply 'string 
		  (removel 
		   '(?\" ?\[ ?\]) 	; todo: unhardwire this
		   (string-to-list query)))
	   shell-parse-split)))
  (if (> (length (car (cdr l))) 0)
    (remove
     (car l)
     (cdr l))
   nil)))

(defun shell-parse-request (query)
 (shell-command-to-string 
  (format
   shell-parse-command
   query)))

;; utility functions

(defun removel (el l)
 (cond (el (removel (cdr el) (remove (car el) l)))
       (t l)))

(defun rotate-forwards (l)
 (append (cdr l) (list (car l))))

(defun rotate-backwards (l)
 (append (last l) (butlast l)))