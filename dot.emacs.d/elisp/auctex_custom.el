;;; Anpassungen für AucTeX
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(require 'tex-site)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-PDF-mode t) ;; .pdf statt .dvi per default
;;Zeilenumbruch
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
;;Syntax Higlight
(add-hook 'LaTeX-mode-hook 'turn-on-font-lock)
;; Mathe Modus
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;;; Anpassungen für RefTeX
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) 
(add-hook 'latex-mode-hook 'turn-on-reftex) 
(setq reftex-plug-into-AUCTeX t)
(setq reftex-external-file-finders
      '(("tex" . "kpsewhich -format=.tex %f")
       ("bib" . "kpsewhich -format=.bib %f")))
(require 'auctex-latexmk)
(auctex-latexmk-setup)
