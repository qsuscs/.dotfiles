(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'auto-mode-alist '("\\.doc$" . doc-mode))
(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'auto-mode-alist '("\\.asciidoc$" . doc-mode))
(autoload 'doc-mode "doc-mode")
;(load-library "auctex_custom")
(server-start)
