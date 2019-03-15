;;; doom-ayu-mirage-theme.el --- based on Ayu
(require 'doom-themes)

;; the default
(setq hl-todo-keyword-faces
      `(("TODO"  . ,(face-foreground 'warning))
        ("FIXME" . ,(face-foreground 'error))
        ("NOTE"  . ,(face-foreground 'warning))))

(defgroup doom-ayu-mirage-theme nil
    "Options for doom-themes"
    :group 'doom-themes)

(defcustom doom-ayu-mirage-brighter-modeline nil
    "If non-nil, more vivid colors will be used to style the mode-line."
    :group 'doom-ayu-mirage-theme
    :type 'boolean)

(defcustom doom-ayu-mirage-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-ayu-mirage-theme
  :type 'boolean)

(defcustom doom-ayu-mirage-comment-bg doom-ayu-mirage-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-ayu-mirage-theme
  :type 'boolean)

(defcustom doom-ayu-mirage-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-ayu-mirage-theme
  :type '(or integer boolean))

(def-doom-theme doom-ayu-mirage
  "A theme based on Ayu for SublimeText 2 and 3"

  ;; name        default     256       16
  (
   ;(bg-alt       '("#212733" nil       nil))
   ;(bg       '("#181e24" nil       nil))
   (bg '("#1f2430" nil nil))
   (bg-alt       '("#232834" nil       nil))
   ;(fg           '("#9099ab" "#2d2d2d" "white"))
   (fg           '("#d9d7ce" "#d9d7ce" "white"))
   (fg-alt       '("#3f4e5a" "#bfbfbf" "brightwhite"))

   (base0        '("#151a1e" "black"   "black"))
   (base1        '("#1c2328" "#1c2328" "brightblack"))
   (base2        '("#232b32" "#232b32" "brightblack"))
   (base3        '("#2a343c" "#2a343c" "brightblack"))
   (base4        '("#313d46" "#313d46" "brightblack"))
   (base5        '("#384550" "#384550" "brightblack"))
   (base6        '("#3f4e5a" "#3f4e5a" "brightblack"))
   (base7        '("#465764" "#465764" "brightblack"))
   (base8        '("#9099ab" "#9099ab" "white"))

   (grey         base4)
   (red          '("#ff3333" "#ff3333" "red"))
   (orange       '("#ff7733" "#ff7733" "brightred"))
   (green        '("#b8cc52" "#b7cc52" "green"))
   (teal         '("#5ccfe6" "#5ccfe6" "brightgreen"))
   ;(yellow       '("#e7c547" "#e7c547" "yellow"))
   (yellow       '("#fccb65" "#e7c547" "yellow"))
   (blue         '("#36a3d9" "#36a3d9" "brightblue"))
   (dark-blue    '("#465764" "#465764" "blue"))
   (magenta      '("#f07178" "#f07178" "magenta"))
   (violet       '("#a37acc" "#a37acc" "brightmagenta"))
   (cyan         '("#95e6cb" "#95e6cb" "brightcyan"))
   (dark-cyan    '("#41858c" "#41858c" "cyan"))

   (ayu-accent   '("#ffcc66" "#ffcc66" nil))
   (ayu-ui       '("#707a8c" "#707a8c" nil))
   (ayu-tag      '("#5ccfe6" "#5ccfe6" nil))
   (ayu-func     '("#ffd57f" "#ffd580" nil))
   (ayu-entity   '("#73D0FF" "#73d0ff" nil))
   (ayu-string   '("#bbe67e" "#bae67e" nil))
   (ayu-regexp   '("#95E6CB" "#95e6cb" nil))
   (ayu-markup   '("#f07178" "#f28779" nil))
   (ayu-keyword  '("#ffae57" "#ffa759" nil))
   (ayu-special  '("#ffc44c" "#ffe6b3" nil))
   (ayu-comment  '("#5C6773" "#5c6773" nil))
   (ayu-constant '("#D4BFFF" "#d4bfff" nil))
   (ayu-operator '("#80d4ff" "#f29e74" nil))
   (ayu-error    '("#FF3333" "#ff3333" nil))
   (ayu-guide    '("#3D4751" "#3d4751" nil))
   (ayu-line     '("#242B38" "#242B38" nil))
   ;(ayu-select   '("#343F4C" "#343F4C" nil))
   (ayu-select   '("#ffa64e" nil nil))
   (ayu-panel    '("#272D38" "#272D38" nil))

   (vcs-added    '("#a6cc70" nil      nil))
   (vcs-modified '("#77a8d9" nil      nil))
   (vcs-removed  '("#f27983" nil      nil))

   (ayu-ui-line          (doom-darken bg 0.15))
   (ayu-ui-panel-bg      (doom-lighten bg 0.1))
   (ayu-ui-panel-shadow  (doom-darken bg 0.3))
   (ayu-ui-panel-border  (doom-darken bg 0.4))
   (ayu-ui-gutter-norm   (doom-blend bg ayu-ui 0.4))
   (ayu-ui-gutter-active (doom-blend bg ayu-ui 0.8))
   (ayu-ui-select-bg     (doom-blend vcs-modified bg 0.87))
   (ayu-ui-select-idle   (doom-blend vcs-modified bg 0.92))
   (ayu-ui-select-border (doom-blend vcs-modified bg 0.8))

   ;; face categories -- required for all themes
   (highlight      yellow)
   (vertical-bar   ayu-guide)
   (selection      ayu-select)
   (builtin        blue)
   (comments       (if doom-ayu-mirage-brighter-comments (doom-lighten ayu-comment 0.25) ayu-comment))
   (doc-comments   yellow)
   ;(doc-comments   ayu-comment)
   (constants      yellow)
   (functions      ayu-func)
   (keywords       ayu-keyword)
   (methods        ayu-func)
   (operators      ayu-operator)
   (type           blue)
   (strings        ayu-string)
   (variables      fg)
   (numbers        violet)
   (region         base3)
   (error          ayu-error)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-ayu-mirage-brighter-modeline)
   (-modeline-pad
    (when doom-ayu-mirage-padded-modeline
      (if (integerp doom-ayu-mirage-padded-modeline) doom-ayu-mirage-padded-modeline 4)))

   ;;(region-fg (when doom-nord-region-highlight bg-alt))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  (
   (lazy-highlight :background yellow :foreground base0 :distant-foreground orange :weight 'bold)
   (hl-todo :foreground red :weight 'bold)
  ;;(((region &override) :foreground region-fg)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((paren-face-match &override) :foreground magenta :background base3 :weight 'ultra-bold)
   ((paren-face-mismatch &override) :foreground base3 :background red :weight 'ultra-bold)
   ;;((vimish-fold-overlay &override) :inherit 'font-lock-comment-face :background base3 :weight 'light)
   ;;((vimish-fold-fringe &override)  :foreground accent)
   (rust-unsafe-face :foreground keywords)

   (font-lock-preprocessor-face :foreground yellow)

   ;(font-lock-comment-face
    ;:foreground comments
    ;:background (if doom-ayu-mirage-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   ;;(doom-modeline-project-root-dir :foreground base8)

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; ediff
   (ediff-fine-diff-A    :background (doom-darken violet 0.4) :weight 'bold)
   (ediff-current-diff-A :background (doom-darken base0 0.25))

   ;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-url-face    :foreground teal :weight 'normal)
   (markdown-reference-face :foreground base6)
   ((markdown-bold-face &override)   :foreground fg)
   ((markdown-italic-face &override) :foreground fg-alt)
   ;;(markdown-code-face :background (doom-lighten base3 0.05))

   ;; outline (affects org-mode)
   ((outline-1 &override) :foreground blue)
   ((outline-2 &override) :foreground green)
   ((outline-3 &override) :foreground teal)
   ((outline-4 &override) :foreground (doom-darken blue 0.2))
   ((outline-5 &override) :foreground (doom-darken green 0.2))
   ((outline-6 &override) :foreground (doom-darken teal 0.2))
   ((outline-7 &override) :foreground (doom-darken blue 0.4))
   ((outline-8 &override) :foreground (doom-darken green 0.4))

   ;; org-mode
   (org-hide :foreground hidden)
   (org-block :background base2)
   (org-block-begin-line :background base2 :foreground comments)
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ;; ()


  )

;;; doom-ayu-mirage-theme.el ends here
