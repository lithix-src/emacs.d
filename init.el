;;;; ~/.emacs.d/init.el --- Global emacs configuration.
;;;;
;;;; Bits and pieces thieved through out the web
;;;; Most of the org stuff comes from this site:
;;;;;; http://doc.norang.ca/org-mode.html#AgendaSetup
;;;; and more recently from Andrea's configurations for python
;;;;; https://github.com/AndreaCrotti/minimal-emacs-configuration
(require 'cl)

;;;; supporting envs
(setq emacs-conf-home
      (concat
       (getenv "HOME")
       "/.emacs.d/"))
(setq site-emacs
      (concat
       emacs-conf-home
       "site-emacs/"))

;;;; ui
(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(ido-mode t)
(setq fill-column 80)
(setq indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 4 120 4))

;;;; base configs
;; set the garbage collection threshold to
;; every 20mb allocations
(setq gc-cons-threshold 20000000)
(setq require-final-newline t)
(windmove-default-keybindings 'shift)
(show-paren-mode t)
;; Turn beep off
(setq visible-bell t)
(setq x-select-enabled-clipboard t)

;; first file found is loaded, order matters!
;; byte compiled files are preferred over .el files, recompile if you cange files!
(add-to-list 'load-path "~/.emacs.d/elpa/")
(add-to-list 'load-path emacs-conf-home)
(add-to-list 'load-path site-emacs)

;;;; temp files
(setq inhibit-splash-screen t)
(setq temporary-file-directory "~/.emacs.d/auto-saves")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
;; (when
;;     (load
;;      (expand-file-name "~/.emacs.d/elpa/package.el"))
;;   (package-initialize))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;;;; helper method to install deps easily
(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

(add-hook 'after-init-hook
	  (load "site-config"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; custom variables set by customize-*
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector (vector "#708183" "#c60007" "#728a05" "#a57705" "#2075c7" "#c61b6e" "#259185" "#042028"))
 '(background-color "#fcf4dc")
 '(background-mode light)
 '(cursor-color "#52676f")
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(desktop-clear-preserve-buffers (quote ("\\*Pymacs\\*" "\\*server\\*" "\\*tramp/.+\\*" "\\*Warnings\\*")))
 '(desktop-save nil)
 '(desktop-save-mode nil)
 '(ecb-layout-name "right1")
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote (("~/Projects" "Projects") ("~/bigdoor/repos" "BigDoor Repos") ("~/Dropbox/org" "Org") ("/" "/"))))
 '(fci-rule-color "#0a2832")
 '(foreground-color "#52676f")
 '(initial-scratch-message ";;;; scratch buffer

")
 '(ipython-command "ipython")
 '(py-python-command "python")
 '(pyflakes-cmd "/usr/local/bin/pyflakes")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#c60007") (40 . "#bd3612") (60 . "#a57705") (80 . "#728a05") (100 . "#259185") (120 . "#2075c7") (140 . "#c61b6e") (160 . "#5859b7") (180 . "#c60007") (200 . "#bd3612") (220 . "#a57705") (240 . "#728a05") (260 . "#259185") (280 . "#2075c7") (300 . "#c61b6e") (320 . "#5859b7") (340 . "#c60007") (360 . "#bd3612"))))
 '(vc-annotate-very-old-color nil)
 '(yas/root-directory (quote ("~/.emacs.d/site-emacs/snippets")) nil (yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
