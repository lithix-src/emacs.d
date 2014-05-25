;;;; site-python.el --- Emacs configurations and utilities for python
;;;;
;;;; Some needed packages to install via pip before all this works
;;;; elpy, epc, jedi

;;;; NOTES
;; just removed the following from python-mode-hook
; (lambda nil (setq indent-tabs-mode py-indent-tabs-mode) (set (make-local-variable (quote beginning-of-defun-function))) (set (make-local-variable (quote end-of-defun-function)) (quote py-end-of-def-or-class)))
;
;;

;;;; setup emacs
(add-to-list 'auto-mode-alist
	 '("\\.py$" . python-mode))
(add-to-list 'auto-mode-alist
	 '("\\.wsgi$" . python-mode))

(setenv "PYMACS_PYTHON" "python2.7")

;; ensure we use UTF-8 for python files
(setenv "LC_CTYPE" "UTF-8")

(setq ansi-color-for-comint-mode t)
(require 'pymacs)
(require 'python-pep8)
(require 'python-pylint)
(require 'flymake)

;; pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

;; rope..cuz it keeps pissing me off without these configs
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

;; elpy
(elpy-enable t)
(when (executable-find "ipython")
  (elpy-use-ipython))
(when (el-get-package-installed-p 'flycheck)
    (setq elpy-default-minor-modes
          (remove 'flymake-mode
                  elpy-default-minor-modes)))

;; initial settings
(setq py-electric-colon-active t)

;; jedi
(setq jedi:complete-on-dot t
      jedi:setup-keys t)

;; no tab inserts please please
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; virtualenv support
(setq virtual-env-use-ipython t)

;; pylookup
(add-to-list 'load-path "~/.emacs.d/site-emacs/packages/pylookup")
(autoload 'pylookup-lookup "pylookup")
(autoload 'pylookup-update "pylookup")
(setq pylookup-program
      "~/.emacs.d/site-emacs/pylookup/packages/pylookup/pylookup.py")
(setq pylookup-db-file
      "~/.emacs.d/site-emacs/pylookup/packages/pylookup/pylookup.db")
(global-set-key "\C-ch" 'pylookup-lookup)
(setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))


;; keybindings


;;;; useful functions
(defadvice venv-mkvirtualenv (after install-common-tools)
  "Install commonly used packages in new venvs."
  (shell-command "pip install flake8 nose jedi ipython"))

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

(defun python-set-keys ()
  (define-key python-mode-map "\C-cd" 'jedi:show-doc)
  (define-key python-mode-map (kbd "M-SPC") 'jedi:complete)
  (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
  (define-key python-mode-map (kbd "M-?") 'jedi:show-doc)
  (define-key python-mode-map (kbd "\C-ci") 'yas/expand)
  ; fix broken reference
  (setq py-mode-map python-mode-map))

;; Flymake settings for Python
(defun flymake-python-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
	 'flymake-create-temp-inplace))
     (local-file (file-relative-name
	  temp-file
	  (file-name-directory buffer-file-name))))
    (list "epylint" (list local-file))))

;;;; hooks
(add-hook 'post-command-hook 'ca-flymake-show-help)
(add-to-list 'flymake-allowed-file-name-masks
     '("\\.py$'" flymake-python-init))
