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

(use-package linum-relative
  :ensure t
  :config
  (linum-mode)
  (linum-relative-global-mode)
  (setq linum-relative-current-symbol ""))

(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-to-list 'auto-mode-alist '("/neomutt" . mail-mode))
(add-hook 'mail-mode-hook 'turn-on-auto-fill)
(add-hook 'mail-mode-hook 'mail-text)

(use-package tex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq fill-column 80))

(use-package auctex-latexmk
  :config
  (auctex-latexmk-setup))

(setq reftex-plug-into-AUCTeX t)

(use-package muttrc-mode
  :mode "/muttrc")

(use-package rust-mode
  :mode "\\.rs\\'")

(setq tramp-default-method "ssh")

(setq column-number-mode t)

(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-M-y") '(lambda ()
				 (interactive)
				 (save-excursion (insert (gui-get-primary-selection)))))

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
