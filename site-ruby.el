;;;; ~/.emacs.d/site-emacs/site-ruby.el
;;;;
;;;; Ruby support configurations
(autoload 'enh-ruby-mode "enh-ruby-mode")
(setq enh-ruby-mode-program "/usr/local/var/rbenv/shims/ruby")
(require 'ruby-mode)

(setenv "PAGER" "cat")

;; Setting rbenv path
(setenv "PATH"
	(concat (getenv "RBENV_ROOT") "/shims:"
		(getenv "RBENV_ROOT") "/bin:"
		(getenv "PATH")))
(setq exec-path
      (cons
       (concat (getenv "RBENV_ROOT") "/shims")
	    (cons
	     (concat (getenv "RBENV_ROOT") "/bin") exec-path)))

(robe-mode t)
(push 'company-robe company-backends)
(push 'ac-source-robe ac-sources)
(require 'rbenv)

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

;; rspec
(require 'rspec-mode)
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

;; smartparens
(require 'smartparens-config)
(require 'smartparens-ruby)
(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(rhtml-mode)
  (sp-local-pair "<" ">")
  (sp-local-pair "<%" "%>")
  (sp-local-pair "<%=" "%>")
  (sp-local-pair "<%-" "%>"))
