;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Erich Grunewald"
      user-mail-address "erichgrunewald@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "Apercu" :size 16 :weight 'normal))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-wilmersdorf)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; org
(map! "C-c C-g" 'org-forward-heading-same-level) ; C-c C-b is already back
(map! "C-c C-x l" '+org/close-all-folds)
(map! "C-c C-x L" '+org/open-all-folds)
(setq company-global-modes '(not org-mode)) ; disable word completion
(setq org-journal-date-prefix "#+TITLE: "
      org-journal-time-prefix "* "
      org-journal-date-format "%a, %Y-%m-%d"
      org-journal-file-format "%Y-%m-%d.org")
(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline +org-capture-todo-file "Inbox")
         "* [ ] %?" :prepend t)
        ("s" "Someday todo" entry
         (file+headline "~/org/somedaymaybe.org" "Someday / Maybe")
         "* SOMEDAY %?")
        ("m" "Maybe todo" entry
         (file+headline "~/org/somedaymaybe.org" "Someday / Maybe")
         "* MAYBE %?")
        ("n" "Note" entry
         (file+headline +org-capture-notes-file "Inbox")
         "* %u %?\n%i" :prepend t)

        ("r" "Templates for reviews")
        ("rd" "Daily review" entry
         (file+headline "~/org/reviews.org" "Reviews")
         (file "~/org/templates/daily-review-template.org"))
        ("rw" "Weekly review" entry
         (file+headline "~/org/reviews.org" "Reviews")
         (file "~/org/templates/weekly-review-template.org"))
        ("rm" "Monthly review" entry
         (file+headline "~/org/reviews.org" "Reviews")
         (file "~/org/templates/monthly-review-template.org"))

        ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
        ;; {todo,notes,changelog}.org file is found in a parent directory.
        ;; Uses the basename from `+org-capture-todo-file',
        ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
        ("p" "Templates for projects")
        ("pt" "Project-local todo" entry  ; {project-root}/todo.org
         (file+headline +org-capture-project-todo-file "Inbox")
         "* TODO %?" :prepend t)
        ("pn" "Project-local notes" entry  ; {project-root}/notes.org
         (file+headline +org-capture-project-notes-file "Inbox")
         "* %U %?" :prepend t)
        ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
         (file+headline +org-capture-project-changelog-file "Unreleased")
         "* %U %?" :prepend t)

        ;; Will use {org-directory}/{+org-capture-projects-file} and store
        ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
        ;; support `:parents' to specify what headings to put them under, e.g.
        ;; :parents ("Projects")
        ("o" "Centralized templates for projects")
        ("ot" "Project todo" entry
         (function +org-capture-central-project-todo-file)
         "* TODO %?" :heading "Tasks" :prepend nil)
        ("on" "Project notes" entry
         (function +org-capture-central-project-notes-file)
         "* %U %?" :heading "Notes" :prepend t)
        ("oc" "Project changelog" entry
         (function +org-capture-central-project-changelog-file)
         "* %U %?" :heading "Changelog" :prepend t)
        ))

(defun erich/org-to-clipboard-as-markdown ()
  "Export marked org text to Markdown and put it in clipboard.

I found this somewhere but cannot locate the source now."
  ;; TODO: don't convert unicode chars to ascii
  (interactive)
  ;; TODO: fix unused variable warning
  (save-window-excursion (let ((org-export-with-toc nil))
                           (with-current-buffer (org-md-export-as-markdown)
                             (with-no-warnings (mark-whole-buffer))
                             (clipboard-kill-region (point-min) (point-max))
                             (kill-buffer-and-window)))))

;; org-roam
(setq org-roam-directory (file-truename "~/org-roam"))

;; projectile
(setq projectile-project-search-path "~/Documents/code/")

;; mit/gnu scheme
(setq geiser-active-implementations '(mit))
(setq geiser-mit-binary "/usr/local/bin/mit-scheme")
(setq geiser-repl-use-other-window nil)

;; julia
(setenv "JULIA_NUM_THREADS" "8") ; tell julia to use all 8 virtual cores
(setq lsp-julia-package-dir nil)
(setq lsp-enable-folding t)
(setq lsp-julia-default-environment "~/.julia/environments/v1.6")
;; (after! julia-repl (set-popup-rule! "^\\*julia.*\\*$" :ignore t))

;; python
(map! :desc "Show the Poetry menu." "C-c y" 'poetry)

;; general utility
(map! "C-c b" 'pop-global-mark) ; return to last mark
(map! "C-S-k" 'kill-visual-line) ; note sure about this one, delete?
(map! "C-c C-w" 'fixup-whitespace) ; removes extra whitespace around cursor
(map! "C-M-q" 'indent-pp-sexp) ; correctly indents sexp starting after cursor
(map! "C-x C-a" 'ace-jump-mode) ; jump to char, line or word
(map! "C-x C-M-a" 'ace-jump-mode-pop-mark) ; return to previous ace position

;; layout
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; start maximized

(defun erich/split-windows ()
  "Splits windows my way.
=> https://www.simplify.ba/articles/2016/01/25/display-buffer-alist/"
  (interactive)
  ;; Create new window right of the current one. Current window is 80 characters
  ;; (columns) wide.
  (split-window-right 80)
  ;; Go to next window.
  (other-window 1)
  ;; Create new window below current one.
  (split-window-below)
  ;; In current window, either open correct REPL or start shell.
  (if (and (projectile-project-root)
           (--any? (s-contains? ".jl" it)
                   (projectile-project-files (projectile-project-root))))
      (+eval/open-repl-same-window)
    (shell))
  ;; Never open any buffer in window with shell.
  (set-window-dedicated-p (nth 1 (window-list)) t)
  ;; Go to original window.
  (other-window -2))

;; half-page scrolling
(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")
(map! "M-p" 'View-scroll-half-page-backward)
(map! "M-n" 'View-scroll-half-page-forward)

;; editing

(defun erich/snake-case ()
  "Snake-case current region."
  (interactive)
  (if (use-region-p)
      (progn (kill-region (region-beginning) (region-end))
             (insert-before-markers (s-snake-case (pop kill-ring)))
             (when kill-ring-yank-pointer
               (setq kill-ring-yank-pointer kill-ring)))
    (message "No region marked")))

(defun erich/kebab-case ()
  "Kebab-case current region."
  (interactive)
  (if (use-region-p)
      (progn (kill-region (region-beginning) (region-end))
             (insert-before-markers (s-dashed-words (pop kill-ring)))
             (when kill-ring-yank-pointer
               (setq kill-ring-yank-pointer kill-ring)))
    (message "No region marked")))

(defun erich/upper-camel-case ()
  "Upper-camel-case current region."
  (interactive)
  (if (use-region-p)
      (progn (kill-region (region-beginning) (region-end))
             (insert-before-markers (s-upper-camel-case (pop kill-ring)))
             (when kill-ring-yank-pointer
               (setq kill-ring-yank-pointer kill-ring)))
    (message "No region marked")))

(defun erich/lower-camel-case ()
  "Lower-camel-case current region."
  (interactive)
  (if (use-region-p)
      (progn (kill-region (region-beginning) (region-end))
             (insert-before-markers (s-lower-camel-case (pop kill-ring)))
             (when kill-ring-yank-pointer
               (setq kill-ring-yank-pointer kill-ring)))
    (message "No region marked")))

;; blog commands

(defun erich/blog-post-create ()
  "Creates a new empty blog post."
  (interactive)
  (let ((root-dir (projectile-project-root)))
    (if (and root-dir (s-contains? "/blog/" root-dir))
        (let ((title (read-string "Post title: "))
              (date (read-string "Date (yyyy-mm-dd): ")))
          (let ((file-path
                 (concat root-dir "posts/"
                         (s-dashed-words (s-replace "'" "" title)) ".md")))
            (progn (write-region (concat
                                  "---\n"
                                  "layout: layouts/post.njk\n"
                                  "title: " title "\n"
                                  "date: " date "\n"
                                  "tags: post\n"
                                  "---\n")
                                 nil
                                 file-path)
                   (find-file file-path)
                   (goto-char (point-min))
                   (message "Done!"))))
      (message "Not in /blog directory."))))

(defun erich/blog-post-fix-urls ()
  "Fixes all URLs, both to the blog itself & to images (points them to the /img
dir) in a blog post."
  (interactive)
  (progn
    (goto-char (point-min))
    (while (re-search-forward "\\(\\[.*\\]+\\)\(https:\/\/www\.erichgrunewald\.com\\([^[:space:]]+\\)\)" nil t)
      (replace-match "\\1({{ '\\2' | url }})"))
    (goto-char (point-min))
    (while (re-search-forward "\\(!\\[.*\\]+\\)\(\\([^{].+\\)\)" nil t)
      (replace-match "\\1({{ '/img/\\2' | url }})"))
    (message "Done!")))
