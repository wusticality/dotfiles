;;; wusticality-theme.el --- My Emacs color theme.

;;; Commentary:

;; Kevin Depue (2025)
;; My own color them for Emacs.

;;; Code:

(deftheme wusticality "My own personal theme.")

;; Declare the colors.
(defvar wusticality-background "#282c34")
(defvar wusticality-foreground "#bbbbbb")
(defvar wusticality-hline "#16191e")
(defvar wusticality-blue "#61afef")
(defvar wusticality-yellow "#e5c07b")
(defvar wusticality-gray "#5c6370")
(defvar wusticality-pink "#ef596f")
(defvar wusticality-magenta "#d55fde")
(defvar wusticality-green "#89ca78")
(defvar wusticality-orange "#ed8328")
(defvar wusticality-red "#be5046")
(defvar wusticality-slate "#20232a")
(defvar wusticality-dark-blue "#005faf")
(defvar wusticality-purple "#af87ff")
(defvar wusticality-cyan "#30b8a6")

;; Declare the theme.
(custom-theme-set-faces
 'wusticality

 ;;
 ;; basics
 ;;

 `(default ((t (:background ,wusticality-background :foreground ,wusticality-foreground))))
 `(cursor ((t (:background ,wusticality-red))))
 `(fringe ((t (:background ,wusticality-background))))
 `(vertical-border ((t (:background ,wusticality-hline :foreground ,wusticality-hline))))
 `(wusticality-selected-window ((t (:background ,wusticality-slate))))

 ;;
 ;; status
 ;;

 `(success ((t (:foreground ,wusticality-green))))
 `(warning ((t (:foreground ,wusticality-orange))))
 `(error ((t (:foreground ,wusticality-red))))
 `(link ((t (:foreground ,wusticality-blue :underline t))))
 `(link-visited ((t (:foreground ,wusticality-blue :underline t))))

 ;;
 ;; selection
 ;;

 `(region ((t (:background ,wusticality-hline))))
 `(highlight ((t (:background ,wusticality-hline))))
 `(hl-line ((t (:background ,wusticality-hline))))
 `(show-paren-match ((t (:background ,wusticality-dark-blue))))
 `(show-paren-mismatch ((t (:background ,wusticality-red))))
 `(lazy-highlight ((t (:background ,wusticality-purple :foreground ,wusticality-slate))))
 `(isearch ((t (:background ,wusticality-orange :foreground ,wusticality-hline :weight bold))))

 ;;
 ;; minibuffer
 ;;

 `(minibuffer-prompt ((t (:foreground ,wusticality-blue))))

 ;;
 ;; modeline
 ;;

 `(mode-line ((t (:background ,wusticality-hline :foreground ,wusticality-foreground :bold t :box nil))))
 ;; mode-line-buffer-id
 ;; mode-line-emphasis
 ;; mode-line-highlight
 `(mode-line-inactive ((t (:background ,wusticality-hline :foreground ,wusticality-foreground :bold t :box nil))))
 `(wusticality-modeline-modified ((t (:foreground ,wusticality-green :weight bold))))
 `(wusticality-modeline-buffer-name ((t (:foreground ,wusticality-magenta :weight bold))))
 `(wusticality-modeline-position ((t (:foreground ,wusticality-green :weight bold))))
 `(wusticality-modeline-line-column ((t (:foreground ,wusticality-orange :weight bold))))
 `(wusticality-modeline-major-mode ((t (:foreground ,wusticality-blue :weight bold))))
 `(wusticality-modeline-git-icon ((t (:foreground ,wusticality-green :weight bold))))
 `(wusticality-modeline-git-branch ((t (:foreground ,wusticality-green :weight bold))))
 `(wusticality-modeline-view-mode ((t (:foreground ,wusticality-yellow :weight bold))))

 ;;
 ;; header-line
 ;;

 `(header-line ((t (:background ,wusticality-hline :foreground ,wusticality-foreground :box nil))))
 `(wusticality-header-modified ((t (:foreground ,wusticality-green :weight bold))))
 `(wusticality-header-file ((t (:foreground ,wusticality-magenta :weight bold))))

 ;;
 ;; window-divider
 ;;

 `(window-divider ((t (:foreground ,wusticality-hline))))
 `(window-divider-first-pixel ((t (:foreground ,wusticality-hline))))
 `(window-divider-last-pixel ((t (:foreground ,wusticality-hline))))

 ;;
 ;; font lock
 ;;

 `(font-lock-bracket-face ((t (:foreground ,wusticality-yellow))))
 `(font-lock-builtin-face ((t (:foreground ,wusticality-blue))))
 `(font-lock-comment-delimiter-face ((t (:foreground ,wusticality-gray :italic t))))
 `(font-lock-comment-face ((t (:foreground ,wusticality-gray :italic t))))
 `(font-lock-constant-face ((t (:foreground ,wusticality-orange))))
 `(font-lock-delimiter-face ((t (:foreground ,wusticality-foreground))))
 `(font-lock-doc-face ((t (:foreground ,wusticality-gray :italic t))))
 `(font-lock-doc-markup-face ((t (:foreground ,wusticality-gray :italic t))))
 `(font-lock-escape-face ((t (:foreground ,wusticality-cyan))))
 `(font-lock-function-call-face ((t (:foreground ,wusticality-blue))))
 `(font-lock-function-name-face ((t (:foreground ,wusticality-blue))))
 `(font-lock-keyword-face ((t (:foreground ,wusticality-magenta :italic t))))
 `(font-lock-misc-punctuation-face ((t (:foreground ,wusticality-foreground))))
 `(font-lock-negation-char-face ((t (:inherit font-lock-operator-face))))
 `(font-lock-number-face ((t (:foreground ,wusticality-yellow))))
 `(font-lock-operator-face ((t (:foreground ,wusticality-magenta))))
 `(font-lock-preprocessor-face ((t (:foreground ,wusticality-yellow :italic t))))
 `(font-lock-property-name-face ((t (:foreground ,wusticality-foreground))))
 `(font-lock-property-use-face ((t (:foreground ,wusticality-foreground))))
 `(font-lock-punctuation-face ((t (:foreground ,wusticality-foreground))))
 `(font-lock-regexp-face ((t (:foreground ,wusticality-green))))
 `(font-lock-regexp-grouping-backslash ((t (:inherit font-lock-escape-face))))
 `(font-lock-regexp-grouping-construct ((t (:inherit font-lock-escape-face))))
 `(font-lock-string-face ((t (:foreground ,wusticality-green))))
 `(font-lock-type-face ((t (:foreground ,wusticality-cyan))))
 `(font-lock-variable-name-face ((t (:foreground ,wusticality-pink))))
 `(font-lock-variable-use-face ((t (:inherit font-lock-variable-name-face))))
 `(font-lock-warning-face ((t (:weight bold :foreground ,wusticality-orange))))

 ;;
 ;; ivy
 ;;

 ;; ivy-action
 ;; ivy-completions-annotations
 ;; ivy-confirm-face
 `(ivy-current-match ((t (:background ,wusticality-hline :extend t))))
 `(wusticality-ivy-selected-match ((t (:background ,wusticality-orange :foreground ,wusticality-hline :weight bold))))
 ;; ivy-cursor
 ;; ivy-grep-info
 ;; ivy-grep-line-number
 ;; ivy-highlight-face
 ;; ivy-match-required-face
 ;; ivy-minibuffer-match-face-1
 `(ivy-minibuffer-match-face-2 ((t (:background ,wusticality-purple :foreground ,wusticality-slate))))
 `(ivy-minibuffer-match-face-3 ((t (:background ,wusticality-purple :foreground ,wusticality-slate))))
 `(ivy-minibuffer-match-face-4 ((t (:background ,wusticality-purple :foreground ,wusticality-slate))))
 ;; ivy-minibuffer-match-highlight
 ;; ivy-modified-buffer
 ;; ivy-modified-outside-buffer
 ;; ivy-org
 ;; ivy-prompt-match
 ;; ivy-remote
 ;; ivy-separator
 ;; ivy-subdir
 ;; ivy-virtual
 ;; ivy-yanked-word

 ;;
 ;; swiper
 ;;

 `(swiper-match-face-1 ((t (:background ,wusticality-orange :foreground ,wusticality-hline :weight bold))))
 `(swiper-match-face-2 ((t (:background ,wusticality-orange :foreground ,wusticality-hline :weight bold))))
 ;; swiper-match-face-3
 ;; swiper-match-face-4
 `(swiper-background-match-face-1 ((t (:background ,wusticality-purple :foreground ,wusticality-slate))))
 `(swiper-background-match-face-2 ((t (:background ,wusticality-purple :foreground ,wusticality-slate))))
 ;; swiper-background-match-face-3
 ;; swiper-background-match-face-4
 `(swiper-line-face ((t (:background ,wusticality-hline :extend t))))

 ;;
 ;; counsel
 ;;

 ;; counsel--mark-ring-highlight
 ;; counsel-active-mode
 ;; counsel-application-name
 ;; counsel-evil-register-face
 ;; counsel-key-binding
 ;; counsel-outline-1
 ;; counsel-outline-2
 ;; counsel-outline-3
 ;; counsel-outline-4
 ;; counsel-outline-5
 ;; counsel-outline-6
 ;; counsel-outline-7
 ;; counsel-outline-8
 ;; counsel-outline-default
 ;; counsel-variable-documentation

 ;;
 ;; lsp
 ;;

 ;; lsp-details-face
 ;; lsp-dired-path-error-face
 ;; lsp-dired-path-face
 ;; lsp-dired-path-hint-face
 ;; lsp-dired-path-info-face
 ;; lsp-dired-path-warning-face
 ;; lsp-face-highlight-read
 ;; lsp-face-highlight-textual
 ;; lsp-face-highlight-write
 ;; lsp-face-rename
 `(lsp-face-semhl-class ((t (:inherit font-lock-type-face))))
 `(lsp-face-semhl-comment ((t (:inherit font-lock-comment-face))))
 `(lsp-face-semhl-constant ((t (:inherit font-lock-constant-face))))
 ;; lsp-face-semhl-decorator
 ;; lsp-face-semhl-default-library
 ;; lsp-face-semhl-definition
 ;; lsp-face-semhl-deprecated
 `(lsp-face-semhl-enum ((t (:inherit font-lock-type-face))))
 `(lsp-face-semhl-enum-member ((t (:foreground ,wusticality-yellow))))
 `(lsp-face-semhl-event ((t (:inherit font-lock-function-name-face))))
 `(lsp-face-semhl-function ((t (:inherit font-lock-function-name-face))))
 `(lsp-face-semhl-implementation ((t (:inherit font-lock-function-name-face))))
 `(lsp-face-semhl-interface ((t (:inherit font-lock-type-face))))
 `(lsp-face-semhl-keyword ((t (:inherit font-lock-keyword-face))))
 `(lsp-face-semhl-label ((t (:inherit font-lock-constant-face))))
 `(lsp-face-semhl-macro ((t (:inherit font-lock-builtin-face))))
 `(lsp-face-semhl-member ((t (:foreground ,wusticality-pink))))
 `(lsp-face-semhl-method ((t (:inherit font-lock-function-name-face))))
 `(lsp-face-semhl-modifier ((t (:inherit font-lock-keyword-face))))
 `(lsp-face-semhl-namespace ((t (:foreground ,wusticality-foreground))))
 `(lsp-face-semhl-number ((t (:inherit font-lock-number-face))))
 `(lsp-face-semhl-operator ((t (:inherit font-lock-operator-face))))
 `(lsp-face-semhl-parameter ((t (:foreground ,wusticality-orange))))
 `(lsp-face-semhl-property ((t (:foreground ,wusticality-pink))))
 `(lsp-face-semhl-regexp ((t (:inherit font-lock-regexp-face))))
 `(lsp-face-semhl-static ((t (:foreground ,wusticality-orange))))
 `(lsp-face-semhl-string ((t (:inherit font-lock-string-face))))
 `(lsp-face-semhl-struct ((t (:inherit font-lock-type-face))))
 `(lsp-face-semhl-type ((t (:inherit font-lock-type-face))))
 `(lsp-face-semhl-type-parameter ((t (:inherit font-lock-type-face))))
 `(lsp-face-semhl-variable ((t (:foreground ,wusticality-foreground))))
 ;; lsp-headerline-breadcrumb-deprecated-face
 ;; lsp-headerline-breadcrumb-path-error-face
 ;; lsp-headerline-breadcrumb-path-face
 ;; lsp-headerline-breadcrumb-path-hint-face
 ;; lsp-headerline-breadcrumb-path-info-face
 ;; lsp-headerline-breadcrumb-path-warning-face
 ;; lsp-headerline-breadcrumb-project-prefix-face
 ;; lsp-headerline-breadcrumb-separator-face
 ;; lsp-headerline-breadcrumb-symbols-error-face
 ;; lsp-headerline-breadcrumb-symbols-face
 ;; lsp-headerline-breadcrumb-symbols-hint-face
 ;; lsp-headerline-breadcrumb-symbols-info-face
 ;; lsp-headerline-breadcrumb-symbols-warning-face
 ;; lsp-headerline-breadcrumb-unknown-project-prefix-face
 ;; lsp-inlay-hint-face
 ;; lsp-inlay-hint-parameter-face
 ;; lsp-inlay-hint-type-face
 ;; lsp-inline-completion-overlay-face
 ;; lsp-installation-buffer-face
 ;; lsp-installation-finished-buffer-face
 ;; lsp-javascript-inlay-face
 ;; lsp-javascript-inlay-parameter-face
 ;; lsp-javascript-inlay-type-face
 ;; lsp-lens-face
 ;; lsp-lens-mouse-face
 ;; lsp-modeline-code-actions-face
 ;; lsp-modeline-code-actions-preferred-face
 ;; lsp-rename-placeholder-face
 ;; lsp-rust-analyzer-abstract-modifier-face
 ;; lsp-rust-analyzer-async-modifier-face
 ;; lsp-rust-analyzer-attribute-modifier-face
 ;; lsp-rust-analyzer-callable-modifier-face
 ;; lsp-rust-analyzer-constant-modifier-face
 ;; lsp-rust-analyzer-consuming-modifier-face
 ;; lsp-rust-analyzer-control-flow-modifier-face
 ;; lsp-rust-analyzer-crate-root-modifier-face
 `(lsp-rust-analyzer-declaration-modifier-face ((t)))
 ;; lsp-rust-analyzer-default-library-modifier-face
 ;; lsp-rust-analyzer-definition-modifier-face
 ;; lsp-rust-analyzer-deprecated-modifier-face
 ;; lsp-rust-analyzer-documentation-modifier-face
 ;; lsp-rust-analyzer-injected-modifier-face
 ;; lsp-rust-analyzer-inlay-face
 ;; lsp-rust-analyzer-inlay-param-face
 ;; lsp-rust-analyzer-inlay-type-face
 ;; lsp-rust-analyzer-intra-doc-link-modifier-face
 `(lsp-rust-analyzer-library-modifier-face ((t)))
 `(lsp-rust-analyzer-mutable-modifier-face ((t)))
 ;; lsp-rust-analyzer-public-modifier-face
 ;; lsp-rust-analyzer-readonly-modifier-face
 `(lsp-rust-analyzer-reference-modifier-face ((t)))
 ;; lsp-rust-analyzer-static-modifier-face
 ;; lsp-rust-analyzer-trait-modifier-face
 ;; lsp-rust-analyzer-unsafe-modifier-face
 ;; lsp-signature-face
 ;; lsp-signature-highlight-function-argument
 ;; lsp-signature-posframe

 ;;
 ;; magit
 ;;

 ;; git-commit-comment-action
 ;; git-commit-comment-branch-local
 ;; git-commit-comment-branch-remote
 ;; git-commit-comment-button
 ;; git-commit-comment-detached
 ;; git-commit-comment-file
 ;; git-commit-comment-heading
 ;; git-commit-keyword
 ;; git-commit-nonempty-second-line
 ;; git-commit-overlong-summary
 ;; git-commit-summary
 ;; git-commit-trailer-token
 ;; git-commit-trailer-value
 ;; git-rebase-action
 ;; git-rebase-comment-hash
 ;; git-rebase-comment-heading
 ;; git-rebase-description
 ;; git-rebase-hash
 ;; git-rebase-killed-action
 ;; git-rebase-label
 ;; magit-bisect-bad
 ;; magit-bisect-good
 ;; magit-bisect-skip
 ;; magit-blame-date
 ;; magit-blame-dimmed
 ;; magit-blame-hash
 ;; magit-blame-heading
 ;; magit-blame-highlight
 ;; magit-blame-margin
 ;; magit-blame-name
 ;; magit-blame-summary
 ;; magit-branch-current
 ;; magit-branch-local
 ;; magit-branch-remote
 ;; magit-branch-remote-head
 ;; magit-branch-upstream
 ;; magit-branch-warning
 ;; magit-cherry-equivalent
 ;; magit-cherry-unmatched
 ;; magit-diff-added
 ;; magit-diff-added-highlight
 ;; magit-diff-added-indicator
 ;; magit-diff-base
 ;; magit-diff-base-heading
 ;; magit-diff-base-highlight
 ;; magit-diff-base-indicator
 ;; magit-diff-conflict-heading
 ;; magit-diff-conflict-heading-highlight
 ;; magit-diff-context
 ;; magit-diff-context-highlight
 ;; magit-diff-file-heading
 ;; magit-diff-file-heading-highlight
 ;; magit-diff-file-heading-selection
 ;; magit-diff-hunk-heading
 ;; magit-diff-hunk-heading-highlight
 ;; magit-diff-hunk-heading-selection
 ;; magit-diff-hunk-region
 ;; magit-diff-lines-boundary
 ;; magit-diff-lines-heading
 ;; magit-diff-our
 ;; magit-diff-our-heading
 ;; magit-diff-our-highlight
 ;; magit-diff-our-indicator
 ;; magit-diff-removed
 ;; magit-diff-removed-highlight
 ;; magit-diff-removed-indicator
 ;; magit-diff-revision-summary
 ;; magit-diff-revision-summary-highlight
 ;; magit-diff-their
 ;; magit-diff-their-heading
 ;; magit-diff-their-highlight
 ;; magit-diff-their-indicator
 ;; magit-diff-whitespace-warning
 ;; magit-diffstat-added
 ;; magit-diffstat-removed
 ;; magit-dimmed
 ;; magit-filename
 ;; magit-hash
 ;; magit-head
 ;; magit-header-line
 ;; magit-header-line-key
 ;; magit-header-line-log-select
 ;; magit-keyword
 ;; magit-keyword-squash
 ;; magit-left-margin
 ;; magit-log-author
 ;; magit-log-date
 ;; magit-log-graph
 ;; magit-mode-line-process
 ;; magit-mode-line-process-error
 ;; magit-process-ng
 ;; magit-process-ok
 ;; magit-reflog-amend
 ;; magit-reflog-checkout
 ;; magit-reflog-cherry-pick
 ;; magit-reflog-commit
 ;; magit-reflog-merge
 ;; magit-reflog-other
 ;; magit-reflog-rebase
 ;; magit-reflog-remote
 ;; magit-reflog-reset
 ;; magit-refname
 ;; magit-refname-pullreq
 ;; magit-refname-stash
 ;; magit-refname-wip
 ;; magit-section-child-count
 ;; magit-section-heading
 ;; magit-section-heading-selection
 ;; magit-section-highlight
 ;; magit-section-secondary-heading
 ;; magit-sequence-done
 ;; magit-sequence-drop
 ;; magit-sequence-exec
 ;; magit-sequence-head
 ;; magit-sequence-onto
 ;; magit-sequence-part
 ;; magit-sequence-pick
 ;; magit-sequence-stop
 ;; magit-signature-bad
 ;; magit-signature-error
 ;; magit-signature-expired
 ;; magit-signature-expired-key
 ;; magit-signature-good
 ;; magit-signature-revoked
 ;; magit-signature-untrusted
 ;; magit-tag

 ;;
 ;; dirvish
 ;;

 ;; dirvish-file-device-number
 ;; dirvish-file-group-id
 ;; dirvish-file-inode-number
 ;; dirvish-file-link-number
 ;; dirvish-file-modes
 ;; dirvish-file-size
 ;; dirvish-file-time
 ;; dirvish-file-user-id
 ;; dirvish-free-space
 `(dirvish-hl-line ((t (:background ,wusticality-hline :extend t))))
 `(dirvish-hl-line-inactive ((t (:background ,wusticality-hline :extend t))))
 ;; dirvish-inactive
 ;; dirvish-media-info-heading
 ;; dirvish-media-info-property-key
 ;; dirvish-proc-failed
 ;; dirvish-proc-finished
 ;; dirvish-proc-running

 ;;
 ;; marginalia
 ;;

 ;; marginalia-archive
 ;; marginalia-char
 ;; marginalia-date
 ;; marginalia-documentation
 ;; marginalia-file-name
 ;; marginalia-file-owner
 ;; marginalia-file-priv-dir
 ;; marginalia-file-priv-exec
 ;; marginalia-file-priv-link
 ;; marginalia-file-priv-no
 ;; marginalia-file-priv-other
 ;; marginalia-file-priv-rare
 ;; marginalia-file-priv-read
 ;; marginalia-file-priv-write
 ;; marginalia-function
 ;; marginalia-installed
 ;; marginalia-key
 ;; marginalia-lighter
 ;; marginalia-list
 ;; marginalia-mode
 ;; marginalia-modified
 ;; marginalia-null
 ;; marginalia-number
 ;; marginalia-off
 ;; marginalia-on
 ;; marginalia-size
 ;; marginalia-string
 ;; marginalia-symbol
 ;; marginalia-true
 ;; marginalia-type
 ;; marginalia-value
 ;; marginalia-version

 ;;
 ;; flycheck
 ;;

 ;; flycheck-delimited-error
 `(flycheck-error ((t (:underline (:color ,wusticality-red :style line) :bold t))))
 ;; flycheck-error-delimiter
 ;; flycheck-error-list-checker-name
 ;; flycheck-error-list-column-number
 ;; flycheck-error-list-error
 ;; flycheck-error-list-error-message
 ;; flycheck-error-list-filename
 ;; flycheck-error-list-highlight
 ;; flycheck-error-list-id
 ;; flycheck-error-list-id-with-explainer
 ;; flycheck-error-list-info
 ;; flycheck-error-list-line-number
 ;; flycheck-error-list-warning
 ;; flycheck-fringe-error
 ;; flycheck-fringe-info
 ;; flycheck-fringe-warning
 `(flycheck-info ((t (:underline (:color ,wusticality-green :style line) :bold t))))
 ;; flycheck-verify-select-checker
 `(flycheck-warning ((t (:underline (:color ,wusticality-orange :style line) :bold t))))
 ;; flymake-error
 ;; flymake-note
 ;; flymake-warning

 ;;
 ;; hl-todo
 ;;

 `(hl-todo ((t (:weight bold :foreground ,wusticality-orange))))
 ;; hl-todo-flymake-type
 )

(provide-theme 'wusticality)

;;; wusticality-theme.el ends here
