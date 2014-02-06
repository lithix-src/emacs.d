;;;; ~/.emacs.d/site-emacs/site-ruby.el
;;;;
;;;; Ruby support configurations
(autoload 'enh-ruby-mode "enh-ruby-mode")

(setenv "PAGER" "cat")

;; Setting rbenv path
(setenv "PATH"
	(concat (getenv "HOME") "/.rbenv/shims:"
		(getenv "HOME") "/.rbenv/bin:"
		(getenv "PATH")))
(setq exec-path
      (cons
       (concat (getenv "HOME") "/.rbenv/shims")
	    (cons
	     (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

(robe-mode t)
(push 'company-robe company-backends)
(push 'ac-source-robe ac-sources)
(require 'rbenv)

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

;; rspec
(require 'rspec-mode)
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))
