;; scala
(require 'scala-mode)
(add-hook 'scala-mode-hook
	  (load "site-scala"))
(add-to-list 'auto-mode-alist
	     '("\\.scala$" . scala-mode))

;; python
; load bare essentials for some functionality
; I don't want depending on everything else in this file
(add-to-list 'auto-mode-alist
	 '("\\.py$" . python-mode))
(autoload 'python-mode "~/.emacs.d/site-emacs/site-python.el")
(autoload 'virtualenv-workon "~/.emacs.d/site-emacs/site-python.el")

;; elisp
(add-to-list 'auto-mode-alist
	 '("\\.el$" . emacs-lisp-mode))

;; org-mode
(autoload 'org-mode "~/.emacs.d/site-emacs/site-org.el")
(add-to-list 'auto-mode-alist
	 '("\\.org$" . org-mode))

;; ruby
(autoload 'ruby-mode "~/.emacs.d/site-emacs/site-ruby.el")
(add-to-list 'auto-mode-alist
	     '("\\.rb$" . ruby-mode))
