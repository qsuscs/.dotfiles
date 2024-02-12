(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
	("melpa-unstable" . "https://melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/"))

      package-archive-priorities '(("melpa-stable" . 1)
				   ("gnu" . 1)))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t
      use-package-verbose t)

(let ((--backup-directory (locate-user-emacs-file "backups")))
  (unless (file-exists-p --backup-directory)
    (make-directory --backup-directory t))
  (setq backup-directory-alist `(("." . ,--backup-directory))))
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
(defun qsx-backup-enable-predicate (name)
  (let ((directory
	 (file-name-directory (file-truename (expand-file-name name))))
	(auto-save-list-directory
	 (file-name-as-directory
	  (locate-user-emacs-file "auto-save-list"))))
    (if (string= directory auto-save-list-directory)
	nil
      (normal-backup-enable-predicate name))))
(setq backup-enable-predicate #'qsx-backup-enable-predicate)

(let ((default-directory (locate-user-emacs-file "elisp")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(load "server")
(unless (server-running-p) (server-start))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :noerror)

(load-theme 'deeper-blue)

(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time-mode)
(display-battery-mode)

(when (eq system-type 'darwin)
    (use-package exec-path-from-shell
      :ensure t
      :init (exec-path-from-shell-initialize))
    (setenv "LANG" "de_DE.UTF-8"))

(use-package yasnippet)

;; https://github.com/company-mode/company-mode/pull/1101
(add-to-list 'package-pinned-packages '("company" . "melpa-unstable"))
(use-package company
  :bind (:map company-mode-map
	      ([remap completion-at-point] . #'company-complete))
  :config
  (setq company-tooltip-align-annotations t)
  :hook (company-mode . yas-minor-mode))

(use-package company-math
  :config
  (add-hook 'TeX-mode-hook (defun qsx-TeX-mode-hook-company ()
			     (setq-local company-backends
					 (append '((company-math-symbols-latex company-latex-commands))
						 company-backends)))))
(use-package company-auctex
  :if (display-graphic-p)
  :ensure nil
  :config (company-auctex-init))
(use-package company-bibtex
  :if (display-graphic-p)
  :config (add-to-list 'company-backends 'company-bibtex))
(use-package company-reftex
  :if (display-graphic-p)
  :config (add-to-list 'company-backends 'company-reftex))
(use-package company-ansible
  :config (add-to-list 'company-backends 'company-ansible))
(use-package company-shell
  :config (add-to-list 'company-backends '(company-shell company-shell-env)))

(semantic-mode 1)
(use-package srefactor
  :bind (:map c-mode-map
	      ("M-RET" . #'srefactor-refactor-at-point)))

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package lsp-mode
  :commands lsp
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  :init
  (setq read-process-output-max (* 1024 1024) ;; 1 MiB
	gc-cons-threshold 6400000))
(use-package lsp-ui
  :commands lsp-ui-mode)
(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(use-package mercurial
  :ensure nil
  :bind-keymap ("C-c H" . hg-global-map))

(use-package magit
  :bind (("C-x g" . #'magit-status)
	 ("C-c m m" . #'magit-status)
	 ("C-c m p" . #'magit-dispatch-popup)
	 ("C-c m f" . #'magit-file-popup)
	 ("C-c m d" . #'magit-diff-buffer-file)))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config (setq aw-scope 'frame))

(use-package avy
  :bind (("C-." . avy-goto-char-timer)
	 ("C-:" . avy-goto-char-2)
	 ("C-," . avy-goto-line))
  :custom (avy-keys '(?c ?t ?i ?e ?n ?r ?s ?g)))

(use-package helm
  :defer nil
  :bind-keymap ("C-c h" . helm-command-map)
  :bind (("M-x" . #'helm-M-x)
	 ("s-x" . #'execute-extended-command)
	 ("M-y" . #'helm-show-kill-ring)
	 ("C-x b" . #'helm-mini)
	 ("C-x C-f" . #'helm-find-files)
	 ("C-h SPC" . #'helm-all-mark-rings)
	 :map helm-command-map
	 ("o" . #'helm-occur))
  :config
  (helm-mode 1)
  (helm-autoresize-mode t)
  (setq helm-split-window-inside-p t
	;; helm-split-window-default-side 'other
	helm-M-x-fuzzy-match t
	helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match t
	helm-ff-file-name-history-use-recentf t
	helm-echo-input-in-header-line t))
(use-package helm-rg
  :after helm)

(use-package form-feed)

(use-package vdirel
  :bind (:map message-mode-map
              ("C-c TAB" . vdirel-helm-select-email)))

(use-package apache-mode)

(use-package meson-mode)

(setq-default display-line-numbers 'relative)

(use-package stripe-buffer
  :config
  (defun qsx-stripe-listify-buffer-hook ()
    (stripe-listify-buffer))
  (dolist (h '(gnus-group-mode-hook
	       gnus-summary-mode-hook
	       package-menu-mode-hook))
    (add-hook h #'qsx-stripe-listify-buffer-hook)))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode-enable))

(show-paren-mode 1)

(use-package ansible)
(use-package poly-ansible)

(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook
	    (defun qsx-hl-indent-mode-hook ()
	      (highlight-indentation-mode))))

(use-package dockerfile-mode)

(defun qsx-dont-show-line-numbers-hook ()
  (setq display-line-numbers nil))

(dolist (h '(Man-mode-hook
	     eshell-mode-hook
	     ledger-report-mode-hook))
  (add-hook h #'qsx-dont-show-line-numbers-hook))

(use-package pdf-tools
  :if (and
       (display-graphic-p)
       (not (eq system-type 'darwin)))
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (add-hook 'pdf-view-mode-hook #'qsx-dont-show-line-numbers-hook))

(setq-default show-trailing-whitespace t)

(setq require-final-newline 'ask)

(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-to-list 'auto-mode-alist '("/neomutt" . mail-mode))
(add-hook 'mail-mode-hook #'turn-on-auto-fill)
(add-hook 'mail-mode-hook #'mail-text)

(defun qsx-enable-TeX-fold-mode ()
  (TeX-fold-mode 1))

(use-package tex
  :if (display-graphic-p)
  :ensure nil
  :config
  (setq TeX-auto-save nil
	TeX-parse-self t
	;; TeX-fold-mode
	font-latex-fontify-sectioning 'color
	font-latex-fontify-script 'multi-level
	fill-column 80)
  (dolist (f '(qsx-enable-TeX-fold-mode
	       turn-on-auto-fill
	       prettify-symbols-mode))
    (add-hook 'TeX-mode-hook f)))

(use-package reftex
  :if (display-graphic-p)
  :config
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'TeX-mode-hook #'reftex-mode))

(use-package auctex-latexmk
  :if (display-graphic-p)
  :ensure nil
  :config
  (auctex-latexmk-setup)
  (setq TeX-command-default "LatexMk"))

(use-package go-mode
  :hook (go-mode . lsp))

(use-package toml-mode)

(use-package rust-mode
  :hook (rust-mode . lsp)
  :mode "\\.rs\\'")
;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(setq c-default-style "linux")

(setq tramp-default-method "ssh"
      tramp-terminal-type "tramp")

(setq column-number-mode t)

(global-set-key (kbd "C-x C-k") #'kill-this-buffer)
(global-set-key (kbd "C-M-y") '(lambda ()
				 (interactive)
				 (insert (gui-get-primary-selection))))

(add-hook 'org-mode-hook #'turn-on-auto-fill)
(setq org-list-allow-alphabetical t)

(setq-default fill-column 80)

(add-hook 'text-mode-hook #'turn-on-auto-fill)

(use-package json-mode)

(use-package adoc-mode)

(use-package dpkg-dev-el
  ;; In Debian, it’s provided by a debian package, no need to install from MELPA
  :if (not (file-exists-p "/etc/debian_version")))

(use-package haskell-mode)

(use-package ledger-mode
  :config
  (setq ledger-default-date-format "%Y-%m-%d"
	ledger-use-iso-dates t))

;; Use keypad comma as decimal separator
(use-package calc
  :config
  (define-key calc-digit-map (kbd "<kp-separator>") "."))

;;; Mail
(use-package gnus
  :defer t
  :config
  (setq
   message-kill-buffer-on-exit t
   message-sendmail-envelope-from 'header
   message-send-mail-function 'message-send-mail-with-sendmail
   message-elide-ellipsis "[…]"
   message-forward-as-mime t
   ;; message-alternative-emails
   message-confirm-send t
   mail-user-agent 'gnus-user-agent
   read-mail-command 'gnus
   gnus-gcc-mark-as-read t
   gnus-user-date-format-alist `((,(gnus-seconds-today) . "          T%H:%M:%S")
				 (,(gnus-seconds-month) . "        %dT%H:%M:%S")
				 (,(gnus-seconds-year)  . "     %m-%dT%H:%M:%S")
				 (t                     .   "%Y-%m-%dT%H:%M:%S"))
   gnus-summary-line-format "%U%R %&user-date; %(%[%5k: %-23,23f%]%)%B%s\n"
   gnus-sum-thread-tree-single-indent "  "
   gnus-sorted-header-list '("^From:" "^Organization:" "^Sender:" "^To:" "^Newsgroups:" "^.?Cc:" "^Subject:" "^Date:" "^Resent-.*:" "^Reply-To:" "^Followup-To:" "^X-Clacks-Overhead:" "Openpgp:" "^Authentication-Results:" "^Message-ID:")
   gnus-visible-headers "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Organization:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:\\|^Message-ID:\\|^Authentication-Results:\\|^Sender:\\|^Resent-.*:\\|^X-Clacks-Overhead:\\|^Openpgp:\\|^User-Agent:\\|X-Mailer:\\|^List-Id:\\|^X-Spam-Score:"
   mm-verify-option 'known
   mml-secure-smime-encrypt-to-self t
   mml-secure-smime-sign-with-sender t
   gnus-buttonized-mime-types '("multipart/signed")
   smime-CA-directory "/etc/ssl/certs"
   gnus-select-method '(nntp "news.cis.dfn.de"
			     (nntp-open-connection-function nntp-open-ssl-stream)
			     (nntp-port-number 563)))
  (let ((maildir (cl-find-if #'file-directory-p
			     (mapcar (lambda (x) (concat (getenv "HOME") x))
				     '("/.nnmaildir"
				       "/.maildir"
				       "/Maildir"))))
	(notmuch (let ((notmuch-gnus (concat (getenv "HOME") "/.local/bin/notmuch-gnus")))
		   (if (file-executable-p notmuch-gnus)
		       notmuch-gnus
		     "notmuch")))
	(notmuch-database-path (car (process-lines "notmuch" "config" "get" "database.path"))))
    (setq
     gnus-secondary-select-methods
     `((nnmaildir
	"mail"
	(directory ,maildir)
	(gnus-search-engine gnus-search-notmuch
			    (program ,notmuch)
			    (remove-prefix ,(concat notmuch-database-path "/")))))))
  (add-hook 'message-setup-hook (defun qsx-message-add-my-headers ()
				  (message-add-header "Openpgp: id=E384009D3B54DCD321BF953295EE94A432583DB1; url=https://keys.openpgp.org/vks/v1/by-fingerprint/E384009D3B54DCD321BF953295EE94A432583DB1; preference=signencrypt"))))

(use-package gnus-alias
  :ensure t
  :bind (:map message-mode-map
              ("C-c i" . #'gnus-alias-select-identity))
  :config
  (setq gnus-alias-identity-alist
	'(("Chaotikum"
	   nil
	   "Thomas Schneider <qsx@chaotikum.eu>"
	   nil
	   (("Gcc" . "nnmaildir+mail:Chaotikum.Sent"))
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
	   (("Gcc" . "nnmaildir+mail:FSMPI.Sent"))
	   nil
	   "~/.signature-fsmpi")
	  ("AStA"
	   nil
	   "Thomas Schneider <tschneider@asta.rwth-aachen.de>"
	   "AStA der RWTH Aachen"
	   (("Gcc" . "nnmaildir+mail:AStA.Sent"))
	   nil
	   "~/.signature-asta")
	  ("Automata"
	   nil
	   "Thomas Schneider <schneider@automata.rwth-aachen.de>"
	   "Lehrstuhl für Informatik 7, RWTH Aachen"
	   (("Gcc" . "\"nnmaildir+mail:Automata.Sent Items\""))
	   nil
	   "~/.signature-automata")
	  ("CCCAC"
	   nil
	   "qsx <qsx@aachen.ccc.de>"
	   nil
	   (("Gcc" . "nnmaildir+mail:CCCAC.Sent"))
	   nil
	   nil))
	gnus-alias-default-identity "Chaotikum"
	gnus-alias-identity-rules
	'(("FSMPI" ("from" "fsmpi" both) "FSMPI")
	  ("AStA" ("from" "asta" both) "AStA")
	  ("Automata" ("from" "automata" both) "Automata")
	  ("RWTH" ("from" "rwth-aachen" both) "RWTH")
	  ("CCCAC" ("from" "aachen\.ccc\.de" both) "CCCAC")))
  (add-hook 'message-setup-hook #'gnus-alias-determine-identity))
