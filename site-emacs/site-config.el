;;;; Post init configurations

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; global keybingdings
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; file handling
(global-unset-key (kbd "C-x f"))
(global-set-key (kbd "C-x f") 'find-file-other-window)
(global-unset-key (kbd "C-\\"))
(global-set-key (kbd "C-x C-d") 'dired-at-point)
(global-set-key (kbd "C-x F") 'find-file-other-frame)
(global-set-key [f7] 'find-file-in-repository)
(global-set-key (kbd "S-<f7>") 'find-file-in-project)
(global-set-key (kbd "C-x M-f") 'find-file-at-point)
(global-unset-key (kbd "M-,"))
(global-set-key (kbd "M-,") 'previous-buffer)
(global-set-key (kbd "M-.") 'next-buffer)

;;;; helm
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "C-x SPC") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;; eshell
;; greatness...
(require 'nyan-prompt)

(setenv "PATH"
	(concat
	 (concat
	  (getenv "HOME")
	  "/.rbenv/shims:")
	 (concat
	  (getenv "HOME")
	  "/.rbenv/bin:")
	 "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin" ":"
	 (getenv "PATH")))


;; nifty package that allows for easy switching of shell buffers
(require 'shell-switcher)
(add-hook 'eshell-load-hook 'nyan-prompt-enable)
(setq shell-switcher-mode t)
(global-set-key (kbd "M-'") 'shell-switcher-switch-buffer)
(global-set-key (kbd "C-M-'") 'shell-switcher-switch-buffer-other-window)
(global-set-key [f1] 'shell-switcher-new-shell)

;; packages
(global-set-key (kbd "M-P") 'package-list-packages)

;; scm
(global-set-key (kbd "C-?") 'magit-status)

;; editing
(global-set-key (kbd "C-\\") 'comment-region)

;; misc utility bindings
(global-set-key (kbd "<f1>") 'open-init)

;; full screen
; disable the stupidity that is os x full screen
(setq ns-use-native-fullscreen nil)

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
(set-frame-parameter
 nil 'fullscreen
 (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
(global-set-key (kbd "M-<return>") 'toggle-fullscreen)

;; keybinding functions
(defun open-init ()
  "Open emacs init file found in ~/.emacs.d and open it in another buffer"
  (interactive)
  (find-file-other-window "~/.emacs.d/init.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Simple package configurations
;;;;
;;;; If a packge configuration needs more than 10 or so lines,
;;;; I move it into it's own site-<package>.el
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; el-get
(require 'el-get)

;;;; ido
(require 'flx-ido)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
(setq flx-ido-threshhold 10000)

;;;; auto-complete.el
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'ac-modes 'web-mode)
(add-to-list 'ac-modes 'elsip-mode)
(add-to-list 'ac-modes 'python-mode)
(add-to-list 'ac-modes 'scala-mode)

(global-auto-complete-mode)
(setq
 ac-auto-start 2
 ac-override-local-map nil
 ac-use-menu-map t
 ac-candidate-limit 20)

;;;; whitespace-mode
(setq whitespace-style
      '(trailing
        lines
        space-before-tab
        indentation
        space-after-tab)
      whitespace-line-column 80)

;;;; ag
;; replacement for regular emacs grep
(require 'ag)
(setq ag-highlight-search t)


;;;; comint mode: cycle backwards and forwards through input history using up/down
(require 'comint)
(define-key comint-mode-map (kbd "M-<down>") 'comint-next-input)
(define-key comint-mode-map (kbd "M-<up>") 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
(define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)

;;;; window-numbering
;; gives me a number in the mode bar for which window i'm in
(window-numbering-mode t)

;;;; bookmarks
(global-set-key (kbd "<C-f6>") '(lambda () (interactive) (bookmark-set "SAVED")))
(global-set-key (kbd "<f6>") '(lambda () (interactive) (bookmark-jump "SAVED")))

;; flyspell
(setq-default ispell-program-name "/usr/local/bin/ispell")
(setq-default ispell-list-command "list")
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)
(mapcar (lambda (mode-hook) (add-hook mode-hook 'flyspell-prog-mode))
     '(c-mode-common-hook R-mode-hook emacs-lisp-mode-hook))


;;;; some default hooks
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'fundamental-mode-hook 'auto-fill-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'emacs-lisp-mode '(auto-complete-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Development
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "site-dev")
