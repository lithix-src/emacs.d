;;;; all
(defvar core-path 
  (concat (getenv "HOME") "/core"))
(defvar core-lib-path 
  (concat core-path "/lib"))

;;;; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
;; pair management

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

;; html-mode
(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-("))

;;;; java
(add-hook 'java-mode-hook
	  (load "site-java"))
(add-to-list 'auto-mode-alist
	     '("\\.java$" . java-mode))

;;;; scala
(require 'scala-mode)
(add-hook 'scala-mode-hook
	  (load "site-scala"))
(add-to-list 'auto-mode-alist
	     '("\\.scala$" . scala-mode))

;;;; python
; load bare essentials for some functionality
; I don't want depending on everything else in this file
(add-to-list 'auto-mode-alist
	 '("\\.py$" . python-mode))
(autoload 'python-mode "~/.emacs.d/site-emacs/site-python.el")
(autoload 'virtualenv-workon "~/.emacs.d/site-emacs/site-python.el")

;;;; elisp
(add-to-list 'auto-mode-alist
	 '("\\.el$" . emacs-lisp-mode))

;;;; org-mode
(autoload 'org-mode "~/.emacs.d/site-emacs/site-org.el")
(add-to-list 'auto-mode-alist
	 '("\\.org$" . org-mode))

;;;; ruby
(add-to-list 'auto-mode-alist
	     '("\\.rb$" . enh-ruby-mode))
(autoload 'enh-ruby-mode "~/.emacs.d/site-emacs/site-ruby.el")

;;;; gradle
(add-to-list 'auto-mode-alist
	     '("\\.gradle$" . groovy-mode))
