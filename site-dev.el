;;;; all
(defvar core-path
  (concat (getenv "HOME") "/core"))
(defvar core-lib-path
  (concat core-path "/lib"))

;;;; yasnippets
(require 'yasnippet)
(yas/global-mode 1)

;;;; project management via projectile
(require 'grizzl)
(require 'projectile)
(setq projectile-enable-caching t)
(setq projectile-completeion-system 'grizzl)
(projectile-global-mode)
(setq projectile-switch-project-action 'projectile-dired)

(global-set-key (kbd "s-p") 'projectile-find-file)
(global-set-key (kbd "s-b") 'projectile-switch-to-buffer)
(global-set-key (kbd "s-g") 'projectile-grep)
(global-set-key (kbd "s-h") 'helm-projectile)
(global-set-key (kbd "s-P") 'projectile-switch-project)
(global-set-key (kbd "s-f") 'projectile-find-file-other-window)

;;;; multiple cursors are fun!
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-x s-e") 'mc/edit-lines)

;;;; textmade mode, cuz I'd too lazy do define these
;;;; shortcuts myself
(require 'textmate)
(textmate-mode)

;;;; gimme colors for all my color codes!
(require 'rainbow-mode)

;;;; ac-etags
(eval-after-load "etags"
  '(progn (ac-etags-setup)))

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
(load "site-python")

;;;; elisp
(add-to-list 'auto-mode-alist
	 '("\\.el$" . emacs-lisp-mode))

;;;; org-mode
(load "site-org")

;;;; ruby
(require 'enh-ruby-mode)
(add-to-list 'auto-mode-alist
	     '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist
	     '("Gemfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist
	     '("\\.erb$" . web-mode))
(autoload 'enh-ruby-mode "~/.emacs.d/site-ruby.el")

;;;; gradle
(add-to-list 'auto-mode-alist
	     '("\\.gradle$" . groovy-mode))
