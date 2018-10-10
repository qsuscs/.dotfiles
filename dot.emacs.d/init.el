(package-initialize)
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/")))

(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

(let ((default-directory
	(concat user-emacs-directory
		(convert-standard-filename "elisp/"))))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'use-package)

(load "server")
(unless (server-running-p) (server-start))

(setq custom-file
      (concat user-emacs-directory
	      (convert-standard-filename "custom.el")))
(load custom-file :noerror)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config (color-theme-sanityinc-tomorrow-blue))

(use-package yaml-mode
  :mode "\\.yml\\'")

(if (< emacs-major-version 26)
    (use-package linum-relative
      :ensure t
      :config
      (linum-mode)
      (linum-relative-global-mode)
      (setq linum-relative-current-symbol ""))
  (setq-default display-line-numbers 'relative))

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (defun my-dont-show-line-numbers-hook ()
    (setq display-line-numbers nil))
  (add-hook 'pdf-view-mode-hook #'my-dont-show-line-numbers-hook))

(setq-default show-trailing-whitespace t)

(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-to-list 'auto-mode-alist '("/neomutt" . mail-mode))
(add-hook 'mail-mode-hook 'turn-on-auto-fill)
(add-hook 'mail-mode-hook 'mail-text)

(use-package tex
  :init
  (setq reftex-plug-into-AUCTeX t)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq fill-column 80))

(use-package auctex-latexmk
  :config
  (auctex-latexmk-setup))

(use-package muttrc-mode
  :mode "/muttrc")

(use-package rust-mode
  :mode "\\.rs\\'")

(load-library "git-commit")

(setq tramp-default-method "ssh")

(setq column-number-mode t)

(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-M-y") '(lambda ()
				 (interactive)
				 (insert (gui-get-primary-selection))))

(add-hook 'org-mode-hook 'turn-on-auto-fill)

(setq-default fill-column 80)

;;; Mail
(setq message-kill-buffer-on-exit t
      message-sendmail-envelope-from 'header
      message-send-mail-function 'message-send-mail-with-sendmail
      notmuch-mua-user-agent-function 'notmuch-mua-user-agent-full
      notmuch-always-prompt-for-sender t)
(use-package gnus-alias
  :ensure t
  :config
  (setq gnus-alias-identity-alist
	'(("Chaotikum"
	   nil
	   "Thomas Schneider <qsx@chaotikum.eu>"
	   nil
	   (("Fcc" . "Chaotikum/Sent"))
	   nil
	   nil)
	  ("RWTH"
	   "Chaotikum"
	   "Thomas Schneider <thomas.schneider@informatik.rwth-aachen.de>"
	   "RWTH Aachen"
	   nil
	   nil
	   nil)
	  ("FSMPI"
	   nil
	   "Thomas Schneider <thomas@fsmpi.rwth-aachen.de>"
	   "Fachschaft I/1 der RWTH Aachen"
	   (("Fcc" . "FSMPI/Sent"))
	   nil
	   "~/.signature-fsmpi")
	  ("AStA"
	   nil
	   "Thomas Schneider <tschneider@asta.rwth-aachen.de>"
	   "AStA der RWTH Aachen"
	   (("Fcc" . "AStA/Sent"))
	   nil
	   "~/.signature-asta")
	  ("Automata"
	   nil
	   "Thomas Schneider <schneider@automata.rwth-aachen.de>"
	   "Lehrstuhl für Informatik 7, RWTH Aachen"
	   (("Fcc" . "\"Automata/Sent Items\""))
	   nil
	   "~/.signature-automata")
	  ("CCCAC"
	   nil
	   "qsx <qsx@aachen.ccc.de>"
	   nil
	   (("Fcc" . "CCCAC/Sent"))
	   nil
	   nil))
	gnus-alias-default-identity "Chaotikum"
	gnus-alias-identity-rules
	'(("FSMPI" ("from" "fsmpi" both) "FSMPI")
	  ("AStA" ("from" "asta" both) "AStA")
	  ("Automata" ("from" "automata" both) "Automata")
	  ("RWTH" ("from" "rwth-aachen" both) "RWTH")
	  ("CCCAC" ("from" "aachen\.ccc\.de" both) "CCCAC")))
  (add-hook 'message-setup-hook 'gnus-alias-determine-identity))
(add-hook 'message-setup-hook
	  (lambda ()
	    (make-local-variable 'message-user-fqdn)
	    (setq message-user-fqdn
		  (car (reverse (split-string
				 (car (mail-header-parse-address
				       (message-field-value "From")))
				 "@"))))))
(setq mu4e-maildir "~/.maildir"
      mu4e-get-mail-command "mbsync -a"
      mu4e-change-filenames-when-moving t
      mu4e-view-show-images t
      mu4e-headers-include-related t
      mu4e-use-fancy-chars nil ; TODO
      mu4e-headers-fields '((:human-date . 12)
			    (:flags . 6)
			    (:mailing-list . 10)
			    (:from-or-to . 22)
			    (:size . 6)
			    (:maildir . 25)
			    (:thread-subject))
      mu4e-view-fields '(:from
			 :to
			 :cc
			 :bcc
			 :subject
			 :flags
			 :date
			 :maildir
			 :mailing-list
			 :message-id
			 :user-agent
			 :tags
			 :attachments
			 :signature
			 :decryption)
      mu4e-view-show-addresses t
      mu4e-attachment-dir "/tmp"
      mu4e-compose-context-policy 'ask
      mu4e-contexts `(,(make-mu4e-context
			:name "FSMPI"
			:match-func (lambda (msg)
				      (when msg
					(string-prefix-p "/FSMPI" (mu4e-message-field msg :maildir))))
			:vars '((user-mail-address . "thomas@fsmpi.rwth-aachen.de")
				(user-full-name . "Thomas Schneider")
				(message-user-fqdn . "fsmpi.rwth-aachen.de")
				(message-user-organization . "Fachschaft I/1 der RWTH Aachen")
				(mu4e-drafts-folder . "/FSMPI/Drafts")
				(mu4e-trash-folder . "/FSMPI/Trash")
				(mu4e-sent-folder . "/FSMPI/Sent")))
		      ,(make-mu4e-context
			:name "AStA"
			:match-func (lambda (msg)
				      (when msg
					(string-prefix-p "/AStA" (mu4e-message-field msg :maildir))))
			:vars '((user-mail-address . "tschneider@asta.rwth-aachen.de")
				(user-full-name . "Thomas Schneider")
				(message-user-fqdn . "asta.rwth-aachen.de")
				(message-user-organization . "AStA der RWTH Aachen")
				(mu4e-drafts-folder . "/AStA/Drafts")
				(mu4e-trash-folder . "/AStA/Trash")
				(mu4e-sent-folder . "/AStA/Sent")))
		      ,(make-mu4e-context
			:name "i7"
			:match-func (lambda (msg)
				      (when msg
					(string-prefix-p "/Automata" (mu4e-message-field msg :maildir))))
			:vars '((user-mail-address . "schneider@automata.rwth-aachen.de")
				(user-full-name . "Thomas Schneider")
				(message-user-fqdn . "automata.rwth-aachen.de")
				(message-user-organization . "Lehrstuhl für Informatik 7 der RWTH Aachen")
				(mu4e-drafts-folder . "/Automata/Drafts")
				(mu4e-trash-folder . "/Automata/Deleted Items")
				(mu4e-sent-folder . "/Automata/Sent Items")))
		      ,(make-mu4e-context
			:name "RWTH"
			:match-func (lambda (msg)
				      (when msg
					(string-prefix-p "/Chaotikum/INBOX/RWTH" (mu4e-message-field msg :maildir))))
			:vars '((user-mail-address . "thomas.schneider@informatik.rwth-aachen.de")
				(user-full-name . "Thomas Schneider")
				(message-user-fqdn . "informatik.rwth-aachen.de")
				(message-user-organization . "RWTH Aachen")
				(mu4e-drafts-folder . "/Chaotikum/Drafts")
				(mu4e-trash-folder . "/Chaotikum/Trash")
				(mu4e-sent-folder . "/Chaotikum/Sent")))
		      ,(make-mu4e-context
			:name "CCCAC"
			:match-func (lambda (msg)
				      (when msg
					(string-prefix-p "/CCCAC" (mu4e-message-field msg :maildir))))
			:vars '((user-mail-address . "qsx@aachen.ccc.de")
				(user-full-name . "qsx")
				(message-user-fqdn . "aachen.ccc.de")
				(mu4e-drafts-folder . "/CCCAC/Drafts")
				(mu4e-trash-folder . "/CCCAC/Trash")
				(mu4e-sent-folder . "/CCCAC/Sent")))
		      ,(make-mu4e-context
			:name "Xaotikum" ; OH COME ON
			:match-func (lambda (msg)
				      (when msg
					(string-prefix-p "/Chaotikum" (mu4e-message-field msg :maildir))))
			:vars '((user-mail-address . "qsx@chaotikum.eu")
				(message-user-fqdn . "chaotikum.eu")
				(user-full-name . "Thomas Schneider")
				(mu4e-drafts-folder . "/Chaotikum/Drafts")
				(mu4e-trash-folder . "/Chaotikum/Trash")
				(mu4e-sent-folder . "/Chaotikum/Sent"))))
      mu4e-user-mail-address-list (append
				   (delq nil
					 (mapcar (lambda (context)
						   (when (mu4e-context-vars context)
						     (cdr (assq 'user-mail-address (mu4e-context-vars context)))))
						 mu4e-contexts))
				   '("dl5qx@dl5qx.de"
				     "thomas.schneider4@rwth-aachen.de"
				     "thomas.schneider@cs.rwth-aachen.de"
				     "qsuscs@qsuscs.de"
				     "qsx@qsx.re")))
