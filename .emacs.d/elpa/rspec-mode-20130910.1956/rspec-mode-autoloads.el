;;; rspec-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (rspec-enable-appropriate-mode rspec-buffer-is-spec-p
;;;;;;  rspec-dired-mode rspec-verifiable-mode rspec-mode) "rspec-mode"
;;;;;;  "../../../../../.emacs.d/elpa/rspec-mode-20130910.1956/rspec-mode.el"
;;;;;;  "4628fbb4eecc549da4440362048b3ec6")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/rspec-mode-20130910.1956/rspec-mode.el

(autoload 'rspec-mode "rspec-mode" "\
Minor mode for RSpec files

\(fn &optional ARG)" t nil)

(autoload 'rspec-verifiable-mode "rspec-mode" "\
Minor mode for Ruby files that have specs

\(fn &optional ARG)" t nil)

(autoload 'rspec-dired-mode "rspec-mode" "\
Minor mode for Dired buffers with spec files

\(fn &optional ARG)" t nil)

(autoload 'rspec-buffer-is-spec-p "rspec-mode" "\
Returns true if the current buffer is a spec

\(fn)" nil nil)

(autoload 'rspec-enable-appropriate-mode "rspec-mode" "\


\(fn)" nil nil)

(dolist (hook '(ruby-mode-hook enh-ruby-mode-hook)) (add-hook hook 'rspec-enable-appropriate-mode))

(add-hook 'rails-minor-mode-hook 'rspec-verifiable-mode)

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/rspec-mode-20130910.1956/rspec-mode-pkg.el"
;;;;;;  "../../../../../.emacs.d/elpa/rspec-mode-20130910.1956/rspec-mode.el")
;;;;;;  (21112 15050 867671 0))

;;;***

(provide 'rspec-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rspec-mode-autoloads.el ends here
