(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(require 'mu4e)
(setq mu4e-sent-folder "/personal/Saved Items"
      mu4e-drafts-folder "/personal/Drafts"
      mu4e-trash-folder "/personal/Trash"
      mu4e-refile-folder "/archive")

(setq mu4e-get-mail-command "mbsync -a"
      mu4e-html2text-command "w3m -T text/html"
      mu4e-update-interval 300
      mu4e-headers-auto-update t
      mu4e-compose-signature-auto-include nil)

(setq mu4e-maildir-shortcuts
      '(("/personal/INBOX" . ?k)
	("/stony/INBOX" . ?s)))

(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-show-images t)

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(defvar my-mu4e-account-alist
  '(("personal"
     (mu4e-sent-folder "/personal/Saved Items")
     (mu4e-drafts-folder "/personal/Drafts")
     (mu4e-trash-folder "/personal/Trash")
     (user-mail-address "kaushik.harith@gmail.com")
     (user-full-name "Kaushik Harith")
     (smtpmail-smtp-user "kaushik.harith")
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-service 25))
    ("stony"
     (mu4e-sent-folder "/stony/Saved Items")
     (mu4e-drafts-folder "/stony/Drafts")
     (mu4e-trash-folder "/stony/Trash")
     (user-mail-address "KaushikSriniva.Harith@stonybrook.edu")
     (user-full-name "Kaushik Srinivasan Harith")
     (smtpmail-smtp-user "kaushiksriniva.harith@stonybrook.edu")
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-service 587))))

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
	  (if mu4e-compose-parent-message
	      (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
		(string-match "/\\(.*?\\)/" maildir)
		(match-string 1 maildir))
	    (completing-read (format "Compose with account: (%s) "
				     (mapconcat #'(lambda (var) (car var))
						my-mu4e-account-alist "/"))
			     (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
			     nil t nil nil (caar my-mu4e-account-alist))))
	 (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
	(mapc #'(lambda (var)
		  (set (car var) (cadr var)))
	      account-vars)
      (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

(setq smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-stream-type 'starttls)
