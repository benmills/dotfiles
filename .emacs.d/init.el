;;; init.el --- My emacs config

;;; Commentary:
;;; Got all this from http://viget.com/extend/emacs-24-rails-development-environment-from-scratch-to-productive-in-5-minu

;;; Code:

(push "/usr/local/bin" exec-path)
(require 'cl)
(add-to-list 'load-path "~/.emacs.d/vendor")
(require 'go-alternate)
(require 'go-focused-test)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq inhibit-startup-message t)
(setq visible-bell t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode t)
(setq column-number-mode t)
(tooltip-mode -1)
(cd "~")

; Marks (from https://github.com/abedra/emacs.d/blob/master/init.org)
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

; Display settings (from abedra)
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

; Indentation
;(setq tab-width 2
;      indent-tabs-mode nil)

; Yes and No prompt
(defalias 'yes-or-no-p 'y-or-n-p)

; Packges
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

(defvar benmills/packages '(ac-slime
                            auto-complete
                            autopair
                            go-mode
                            magit
                            markdown-mode
                            puppet-mode
                            rvm
                            smex
                            flycheck
                            rspec-mode
                            yaml-mode
                            inf-ruby
                            color-theme-solarized
			    ruby-end
			    projectile
			    flx-ido
			    anzu
			    git-gutter
			    )
  "Default Packages")

(defun benmills/packages-installed-p ()
  (loop for pkg in benmills/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (benmills/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg benmills/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

; Ido mode
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

; rspec-mode
(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))
(ad-activate 'rspec-compile)

					; auto complete
(require 'auto-complete-config)
(ac-config-default)

; autopair
(require 'autopair)
;(autopair-global-mode)

; smex
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

; Ruby
(require 'rvm)
(rvm-use-default)
(setq ruby-deep-indent-paren nil)

; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda () (visual-line-mode t)))
(setq markdown-command "pandoc --smart -f markdown -t html")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("73b835431bdbc4e83a3b176a38ebb740fbac78aa2635e1d4827b3c8211e0bc99" "394504bd559027641b544952d6e9e1c6dcb306b4d1b2c4ad6b98d3e6b5459683" "f89e21c3aef10d2825f2f079962c2237cd9a45f4dc1958091be8a6f5b69bb70c" "8bb1e9a22e9e9d405ca9bdf20b91301eba12c0b9778413ba7600e48d2d3ad1fb" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; There's something similar (but fancier) in vc-git.el: vc-git-grep
;; -I means don't search through binary files
(defcustom git-grep-switches "--extended-regexp -I -n --ignore-case"
  "Switches to pass to `git grep'."
  :type 'string)

(defun git-grep (command-args)
  (interactive
   (list (read-shell-command "Run git-grep (like this): "
                             (format "git grep %s -e "
                                     git-grep-switches)
                             'git-grep-history)))
  (let ((grep-use-null-device nil))
    (grep command-args)))
(put 'upcase-region 'disabled nil)

; Look
(load-theme 'solarized-light t)
(set-face-attribute 'default nil :height 100 :family "Source Code Pro")
(fringe-mode 4)

; Projectile
(projectile-global-mode)

; Anzu
(global-anzu-mode +1)

; Git Gutter
(global-git-gutter-mode +1)


; golang
(setenv "GOPATH" "/Users/benmills/.go")
(add-hook
 'go-mode-hook
 '(lambda ()
    (auto-complete-mode)
    (define-key go-mode-map [?\C-c ?\C-t] 'run-go-tests)
    (define-key go-mode-map [?\C-c ?\C-g] 'find-test-name-at-point)
    (define-key go-mode-map [?\C-c ?\C-f] 'run-go-test-at-point)
    (define-key go-mode-map [?\C-c ?\C-b] 'run-go-test-for-buffer)
    (define-key go-mode-map [?\C-c ?\C-p] 'go-alternate-visit-at-point)
    (setq
     compilation-exit-message-function
     'compilation-autoclose-on-success)))
