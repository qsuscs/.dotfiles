(package-initialize)
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/") t)

(let ((default-directory
	(concat user-emacs-directory
		(convert-standard-filename "elisp/"))))
  (normal-top-level-add-subdirs-to-load-path))

(require 'use-package)

(server-start)

(setq custom-file
      (concat user-emacs-directory
	      (convert-standard-filename "custom.el")))
(load custom-file :noerror)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config (color-theme-sanityinc-tomorrow-blue))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode))

(use-package linum-relative
  :ensure t)
(linum-mode)
(linum-relative-global-mode)

(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-hook 'mail-mode-hook '(lambda() (turn-on-auto-fill)))
