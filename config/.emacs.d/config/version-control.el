(provide 'version-control)

(ensure-packages-installed 'magit 'git-timemachine)

(require 'magit)
(setq magit-completing-read-function
    'magit-ido-completing-read)

(setq vc-follow-symlinks t)

(defun my-magit-hook ()
  ;; prevent magit from stealing M-h from winmove
  (define-key magit-mode-map (kbd "M-h") nil)
  (define-key magit-mode-map (kbd "M-H") nil)
  (define-key magit-mode-map (kbd "M-H") 'magit-show-only-files))

(add-hook 'magit-mode-hook 'my-magit-hook)

(defun shell->list (cmd)
  (split-string
   (shell-command-to-string cmd)))

(defun github-pr-compare (parent-branches)
  (interactive "sBranches to compare: ")
  (let* ((parents (split-string parent-branches))
         (user (shell-command-to-string "echo -n $(git config --get remote.origin.url | cut -d : -f 2 | cut -d / -f 1)"))
         (repo (shell-command-to-string "echo -n $(git config --get remote.origin.url | cut -d : -f 2 | cut -d / -f 2 | cut -d . -f 1)"))
         (branch-grouping
	  (mapcar
	   (lambda (ref)
	     (list ref
		   (shell->list
		    (concat "git branch --contains origin/"
			    ref
			    " | grep -v "
			    ref))))
	   (shell->list
	    (concat "curl https://'github.groupondev.com/api/v3/repos/"
		    user
		    "/"
		    repo
		    "/pulls?st&branch=master' 2>/dev/null | jq -r .[].head.ref")))))
    (with-output-to-temp-buffer "*merge-compare*"
      (princ (concat "branch," (mapconcat 'identity parents ",") "\n"
                     (mapconcat (lambda (grp)
                                  (concat (first grp) ","
                                          (mapconcat
                                           (lambda (p)
                                             (if (member p (second grp)) "Y" " "))
                                           parents
                                           ",")))
                                branch-grouping
                                "\n")))
      (with-current-buffer "*merge-compare*"
        (org-table-convert-region (point-min) (point-max) '(4))
        (goto-char (point-min))
        (forward-line)
        (open-line 1)
        (insert "|-")
        (org-mode)
        (org-table-recalculate)
        (help-mode)))))
