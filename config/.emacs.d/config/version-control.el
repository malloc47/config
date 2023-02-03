(provide 'version-control)

(use-package magit
  :ensure t
  :init
  (setq magit-define-global-key-bindings t)
  (setq vc-follow-symlinks t)
  :hook (git-commit-setup . git-commit-turn-on-flyspell))

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
