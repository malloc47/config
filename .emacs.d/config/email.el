(provide 'email)

(ensure-packages-installed 'w3m)

(require 'nnir)

(setq gnus-select-method '(nntp "news.gmane.org"))
(setq gnus-select-method '(nnimap "gmail"
				  (nnimap-address "imap.gmail.com")
				  (nnimap-server-port 993)
				  (nnimap-stream ssl)
				  (nnir-search-engine imap)
				  (nnimap-authinfo-file "~/.authinfo.gpg")
				  (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
				  (nnmail-expiry-wait 90)))

(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(setq smtpmail-auth-credentials "~/.authinfo.gpg")

(setq-default
  gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
  gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
  gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
  gnus-sum-thread-tree-false-root ""
  gnus-sum-thread-tree-indent ""
  gnus-sum-thread-tree-leaf-with-other "-> "
  gnus-sum-thread-tree-root ""
  gnus-sum-thread-tree-single-leaf "|_ "
  gnus-sum-thread-tree-vertical "|")

(setq gnus-large-newsgroup 50000)

(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)
        (not gnus-thread-sort-by-number)))

(setq gnus-read-active-file 'some)

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9.  ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups))

(add-hook 'gnus-group-mode-hook
	  (lambda () (local-set-key "o" 'my-gnus-group-list-subscribed-groups)))

(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-subject)

(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)

; Personal Information
(setq user-full-name "Jarrell Waggoner"
      user-mail-address "jwaggoner@groupon.com")

(setq gnus-posting-styles
      '((".*"
	 (name "Jarrell Waggoner"
	       (address "jwaggoner@gmail.com"
			(organization "Groupon")
			(signature-file "~/.signature"))))))

(setq mm-text-html-renderer 'w3m)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "jwaggoner@groupon.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "groupon.com")

(setq gnus-use-correct-string-widths nil)
