;;; early-init.el --- My early init file.

;;; Commentary:

;; Kevin Depue (2025)
;; https://github.com/wusticality

;;; Code:

;; Prevent package.el from loading packages.
(setq package-enable-at-startup nil)

;; Don't start collecting garbage until we're done
;; initializing emacs. We reset these values after
;; emacs has started.
(setq gc-cons-threshold most-positive-fixnum)

;; Revert garbage collection settings after startup.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 16))))

(provide 'early-init)

;;; early-init.el ends here
