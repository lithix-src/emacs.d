(autoload 'python-mode "python-mode"
  "Python Mode." t)

(add-to-list 'auto-mode-alist
             '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist
             '("python" . python-mode))

(require 'python-mode)
(add-hook 'python-mode-hook
          (lambda ()
            (set-variable 'py-indent-offset 4)
            ;(set-variable 'py-smart-indentation nil)
            (set-variable 'indent-tabs-mode nil)
            (define-key py-mode-map (kbd "RET")
              'newline-and-indent)
            ;(define-key py-mode-map [tab] 'yas/expand)
            ;(setq yas/after-exit-snippet-hook 'indent-according-to-mode)
            (smart-operator-mode-on)))

;; pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")
;; (eval-after-load "pymacs"
;;    '(add-to-list 'pymacs-load-path "~/.emacs.d/site-emacs"))
;; (pymacs-load "ropemacs" "rope-")

(setq ropemacs-enable-autoimport t)


;; auto completion
(defun prefix-list-elemens (list prefix)
  (let (value)
    (nrevers                ; is this because the [].pop == [-1] ?
     (dolist (element list value)
       (setq value (cons (format "%s%s" prefix element) value))))))

(defvar ac-source-rope
  '((candidates
     (lamda ()
            (prefix-list-elements (rope-completions) ac-target)))))

