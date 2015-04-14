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
(setq site-emacs-packages
      (concat
       site-emacs
       "packages/"))
(setenv "no_proxy" (concat "gist.github.com," (getenv "no_proxy")))

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

;; add a final new line to files before closing
(setq require-final-newline t)

;; hold shift to move through windows
(windmove-default-keybindings 'shift)

;; highlight matching parans
(show-paren-mode t)

;; Turn beep off
(setq visible-bell t)

;; enable native clipboard
(setq x-select-enabled-clipboard t)

;; first file found is loaded, order matters!
;; byte compiled files are preferred over .el files, recompile if you cange files!
(add-to-list 'load-path emacs-conf-home)
(add-to-list 'load-path site-emacs-packages)

;;;; temp files
(setq inhibit-splash-screen t)

;; keep all the auto saves and shadow files in one place
(setq temporary-file-directory "~/.emacs.d/auto-saves")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; http://cask.github.io/
;; cask, bundler for emacs
;; TODO: make this a submodule to my emacs repo
(require 'cask)
(cask-initialize)

;; https://github.com/rdallasgray/pallet
;; emacs packet manger
(require 'pallet)

(add-hook 'after-init-hook
	  (load "site-config"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; custom variables set by customize-*
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#708183" "#c60007" "#728a05" "#a57705" "#2075c7" "#c61b6e" "#259185" "#042028"))
 '(background-color "#fcf4dc")
 '(background-mode light)
 '(coffee-args-compile (quote ("-c" "--bare")))
 '(coffee-tab-width 2)
 '(cursor-color "#52676f")
 '(custom-enabled-themes (quote (misterioso)))
 '(custom-safe-themes
   (quote
    ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(desktop-clear-preserve-buffers
   (quote
    ("\\*Pymacs\\*" "\\*server\\*" "\\*tramp/.+\\*" "\\*Warnings\\*")))
 '(desktop-save nil)
 '(desktop-save-mode nil)
 '(ecb-layout-name "right1")
 '(ecb-options-version "2.40")
 '(ecb-source-path
   (quote
    (("~/Projects" "Projects")
     ("~/Dropbox/org" "Org")
     ("/" "/"))))
 '(eclim-eclipse-dirs (quote (eclipse-home)))
 '(elpy-rpc-backend "jedi")
 '(fci-rule-color "#0a2832")
 '(foreground-color "#52676f")
 '(initial-scratch-message ";;;; scratch buffer

")
 '(ipython-command "ipython")
 '(magit-auto-revert-mode nil)
 '(org-agenda-files (quote ("~/core/org/wiki.org" "~/core/org/refile.org")))
 '(url-proxy-services nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#c60007")
     (40 . "#bd3612")
     (60 . "#a57705")
     (80 . "#728a05")
     (100 . "#259185")
     (120 . "#2075c7")
     (140 . "#c61b6e")
     (160 . "#5859b7")
     (180 . "#c60007")
     (200 . "#bd3612")
     (220 . "#a57705")
     (240 . "#728a05")
     (260 . "#259185")
     (280 . "#2075c7")
     (300 . "#c61b6e")
     (320 . "#5859b7")
     (340 . "#c60007")
     (360 . "#bd3612"))))
 '(vc-annotate-very-old-color nil)
 '(yas-snippet-dirs
   (quote
    ("~/.emacs.d/snippets" "/Users/erik/core/site/site-config/emacs/.cask/24.3.1/elpa/yasnippet-20140427.1224/snippets" "/Users/erik/core/site/site-config/emacs/.cask/24.3.1/elpa/elpy-20140501.744/snippets/")) nil (yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
