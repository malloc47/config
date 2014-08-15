(provide 'chat)

(ensure-packages-installed 'jabber)

(setq jabber-account-list
      '(("jwaggoner@groupon.com"
	 (:network-server . "talk.google.com")
	 (:connection-type . ssl))
	("jarrell.waggoner@gmail.com"
	 (:network-server . "talk.google.com")
	 (:connection-type . ssl))))

(setq jabber-chat-buffer-show-avatar nil)
(setq jabber-show-resources nil)
(setq jabber-roster-line-format "%c %-25n %u %-8s  %S")
(setq jabber-alert-presence-message-function (lambda (who oldstatus newstatus statustext) nil))
