;;;; site-python.el --- Emacs configurations and utilities for python
;;;;
;;;; Some needed packages to install via pip before all this works
;;;; elpy, epc, jedi

;;;; Setup needed emacs packages.
;;;; Requires marmalade and melpa packages set
(setq to-install
      '(python-mode
    magit
    yasnippet
    jedi
    auto-complete
    autopair
    find-file-in-repository))

(mapc 'install-if-needed to-install)

;;;; setup emacs
(setenv "PYMACS_PYTHON" "python2.7")
(setq py-python-command-args '("--matplotlib" "--colors" "LightBG"))
(setq ansi-color-for-comint-mode t)
(require 'python-mode)
(require 'pymacs)
(require 'python-pep8)
(require 'python-pylint)
(require 'autopair)
(require 'flymake)

; fix broken reference
(setq py-mode-map python-mode-map)


;; initial settings and hooks
(setq py-electric-colon-active t)
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)

(add-hook 'python-mode-hook
	  (lambda ()
	    (setq jedi:jedi-server-command
		  '("python" "~/.emacs.d/elpa/jedi-0.1.2/jediepcserver.py/jedipcserver.py"))
	    (jedi:setup)
	    (jedi:ac-setup)
	    (setq jedi:complete-on-dot t)
	    (setq jedi:setup-keys t)
	    (local-set-key "\C-cd" 'jedi:show-doc)
	    (local-set-key (kbd "M-SPC") 'jedi:complete)
	    (local-set-key (kbd "M-.") 'jedi:goto-definition)))

(show-paren-mode t)

;; Flymake settings for Python
(defun flymake-python-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
	 'flymake-create-temp-inplace))
     (local-file (file-relative-name
	  temp-file
	  (file-name-directory buffer-file-name))))
    (list "epylint" (list local-file))))
(defun flymake-activate ()
  "Activates flymake when real buffer and you have write access"
  (if (and
       (buffer-file-name)
       (file-writable-p buffer-file-name))
      (progn
    (flymake-mode t)
    ;; this is necessary since there is no flymake-mode-hook...
    (local-set-key (kbd "C-c n") 'flymake-goto-next-error)
    (local-set-key (kbd "C-c p") 'flymake-goto-prev-error))))

(defun ca-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))

(add-hook 'post-command-hook 'ca-flymake-show-help)
(add-to-list 'flymake-allowed-file-name-masks
     '("\\.py\\'" flymake-python-init))
(add-hook 'python-mode-hook 'flymake-activate)
(add-hook 'python-mode-hook 'auto-complete-mode)

;; no tab inserts please please
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; virtualenv support
(setq virtual-env-use-ipython t)

(add-to-list 'load-path "~/.emacs.d/site-emacs/packages/pylookup")
(autoload 'pylookup-lookup "pylookup")
(autoload 'pylookup-update "pylookup")
(setq pylookup-program
      "~/.emacs.d/site-emacs/pylookup/packages/pylookup.py")
(setq pylookup-db-file
      "~/.emacs.d/site-emacs/pylookup/packages/pylookup.db")
(global-set-key "\C-ch" 'pylookup-lookup)
(setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))


;; keybindings

;; ipython support
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;;;; useful functions
(defadvice venv-mkvirtualenv (after install-common-tools)
  "Install commonly used packages in new venvs."
  (shell-command "pip install flake8 nose jedi ipython"))
