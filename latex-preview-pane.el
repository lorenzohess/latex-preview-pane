;;; latex-preview-pane.el --- Makes LaTeX editing less painful by providing a updatable preview pane

;; Copyright (C) 2015 John L. Singleton <jsinglet@gmail.com>

;; Author: John L. Singleton <jsinglet@gmail.com>
;; Keywords: latex, preview
;; Version: 20151021
;; URL: http://www.emacswiki.org/emacs/LaTeXPreviewPane
;; Package-Requires: ((emacs "24.1"))

;;; Commentary:

;; latex-preview-pane is a minor mode for Emacs that enables you to preview your LaTeX files directly in Emacs.
;; It supports PDF previews, your choice of pdflatex or xelatex, and it highlights errors in your LaTeX buffer.
;;
;; To enable, place the following in your .emacs file:
;;
;; (latex-preview-pane-enable)
;;
;; As an alternative, you may enable it on the fly with:
;;
;; M-x latex-preview-pane-mode
;;
;; The latest version of latex-preview-pane can always be found at
;; https://github.com/jsinglet/latex-preview-pane
;;
;; You can find the documentation for latex-preview-pane either on GitHub (above) or
;; on EmacsWiki at: http://www.emacswiki.org/emacs/LaTeXPreviewPane

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:


(require 'doc-view)
(require 'cl-lib)

(defvar latex-preview-pane-current-version "20151021")
;;
;; Get rid of free variables warnings
;;

(defvar message-latex-preview-pane-welcome)
(defvar message-no-preview-yet)

;;;###autoload
(defun latex-preview-pane-enable ()
  "Enable `latex-preview-pane-mode' in `latex-mode'."
  (add-hook 'latex-mode-hook (lambda () (latex-preview-pane-mode 1))))

(defun lpp/window-containing-preview ()
  "Return the `latex-preview-pane' window.

Iterate through all windows of all frames and test for the lpp window with the
`is-latex-preview-pane' window parameter.

Returns nil if no lpp window is found.

NOTE: calls (setq docViewWindow currentWindow), which affects global state."
  (let (windows
        i
        docViewWindow)
    ;; Get list of all windows.
    (setq windows (cl-reduce #'append (mapcar `window-list (frame-list))))
    (setq i 0)
    ;; Loop over all windows and test for lpp window.
    (progn
      (while (and (not docViewWindow) (<= i (length windows)))
        (let ((currentWindow (pop windows)))
          (if (window-parameter currentWindow 'is-latex-preview-pane)
              (setq docViewWindow currentWindow)))
        (setq i (1+ i)))
      docViewWindow)))

;;
;; Init procedure:
;; 1) Find a window with doc-view-mode turned on in this frame.
;; 2) If no such window can be found, split this window vertically.
;; 2a) Display startup message, shortcuts, etc. Pause for 3 seconds.
;; 3) TeX the current file. (that is, start the refresh loop)
;;

;;;###autoload
(defun init-latex-preview-pane ()
  "Init procedure for lpp.

To create a new frame, ."
  (progn
    ;; lpp:make sure the current window isn't the preview pane
    (set-window-parameter nil 'is-latex-preview-pane nil)
    (if (eq (lpp/window-containing-preview) nil)
        ;; lpp: tag the newly created window
        (set-window-parameter
         ;; TODO: unclear what this does. It should return a window for set-window-parameter to work.
         ;; C-x C-e makes it split the window. Seems to return the split window.
         (if latex-preview-pane-use-frame
             (car (window-list (make-frame))) ;; does this car have an effect?
           (split-window nil nil preview-orientation)) ;; seems to default to currently selected window
         'is-latex-preview-pane t))
    ;; Display lpp welcome message in new buffer in preview window
    (lpp/display-startup (lpp/window-containing-preview))
    ;; lpp: add the save hook
    ;; Really this adds the lpp-update hook as a buffer-local hook
    (add-hook 'after-save-hook 'latex-preview-pane-update nil 'make-it-local)
    ;; lpp: refresh that pane
    ;; Really this runs lpp-update on the current buffer (?)
    (run-at-time "0 min 3 sec" nil 'latex-preview-pane-update)))


(defun lpp/get-message (f)
  "Return contents of file F as a string."
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-string)))


(defun lpp/display-startup (where)
  "Display the LPP welcome message in a new buffer in window WHERE."
  ;; Save current buffer
  (let ((old-buff (current-buffer)))
    (progn
      ;; Open WHERE in lpp welcome buffer
      (set-window-buffer
       where (get-buffer-create "*Latex Preview Pane Welcome*"))
      ;; Set current buffer to lpp welcome buffer
      (set-buffer (get-buffer "*Latex Preview Pane Welcome*"))
      ;; Delete contents lpp welcome buffer
      (erase-buffer)
      ;; Add welcome message
      (insert message-latex-preview-pane-welcome)
      ;; Go back to buffer
      (set-buffer old-buff))))


;;
;; System specific configuration.
;;

(defvar lpp/view-buffer-command
  (pcase system-type
    (`windows-nt "start")
    (`darwin "open")
    (`gnu/linux "xdg-open")
    (`gnu/kfreebsd "xdg-open"))
  "Command used to view a file with the system's native tools.")


;;
;; Updates an external preview program of the current latex file
;;
;;;###autoload
(defun latex-preview-update ()
  "Open the PDF corresponding with the `lpp/buffer-file-name' TeX file."
  (interactive)
  ;; Get the name of the PDF.
  (let ((pdf-file
         (replace-regexp-in-string "\.tex$" ".pdf" (lpp/buffer-file-name))))
    ;; Make sure PDF file exists
    (if (not (file-exists-p pdf-file))
        (message
         (concat
          "File "
          pdf-file
          " does not exist. Save your current buffer to generate it."))
      (if (eq system-type 'windows-nt) ;; Check if user is on Windows
          (w32-shell-execute "open" pdf-file nil nil)
        ;; Use `lpp/view-buffer-command' if on non windows-nt system.
        (start-process
         "Preview" ;; process name
         (get-buffer-create "*pdflatex-buffer*")
         ;; TODO: unclear how this opens the preview in Emacs.
         lpp/view-buffer-command ;; xdg-open on GNU/Linux
         ;; TODO: I don't know why this below can't just be pdf-file
         (replace-regexp-in-string "\.tex$" ".pdf" (lpp/buffer-file-name)))))))


;;
;; If a preview pane is open, updates the preview pane on save.
;;
;;;###autoload
(defun latex-preview-pane-update ()
  "Call `latex-preview-pane-update-p' after a bunch of checks.

Check that `latex-preview-pane-mode' is bound and buffer-local is t.
Check that preview window exists.
Erase *pdflatex-buffer* if it is non-nil."
  (interactive)
  ;; If `latex-preview-pane-mode' is bound and buffer-local `latex-preview-pane-mode' is t, proceed.
  (when (and (boundp 'latex-preview-pane-mode) latex-preview-pane-mode)
    ;; Init preview window if it doesn't exist.
    (if (eq (lpp/window-containing-preview) nil)
        (init-latex-preview-pane)
      (progn
        ;; If *pdflatex-buffer* is non-nil, erase it.
        (if (not (eq (get-buffer "*pdflatex-buffer*") nil))
            ;; Save current-buffer and go back to it after erasing.
            (let ((old-buff (current-buffer)))
              (progn
                (set-buffer "*pdflatex-buffer*")
                (erase-buffer)
                (set-buffer old-buff)))) ;; no ELSE
        (message "Updating LaTeX Preview Pane")
        ;; Update preview pane.
        (latex-preview-pane-update-p)))))


(defun lpp/last-backtrace ()
  "Retrieve error message from `*pdflatex-buffer*'."
  ;; Save current buffer.
  (let ((old-buff (current-buffer)))
    ;; Visit pdflatex buffer
    (set-buffer (get-buffer "*pdflatex-buffer*"))
    ;; Get error message in pdflatex buffer
    (let ((error-msg (buffer-substring (point-min) (point-max))))
      ;; Visit previous buffer
      (set-buffer old-buff)
      ;; Reverse error-msg.
      ;; NOTE: probably need mapconcat, identity, and the newlines because they
      ;; get somehow get consumed.
      (mapconcat 'identity (reverse (split-string error-msg "\n")) "\n"))))


(defun latex-pp-filter (condp lst)
  "Remove elts from LST which are nil or which fail CONDP.

If (CONDP ELT) is nil, remove that element from LST. If (CONDP ELT) is non-nil,
and ELT is non-nil, ELT remains in LST."
  (delq
   nil
   (mapcar
    (lambda (x) (and (funcall condp x) x)) ;; if (condp x) is nil, return nil. return x, unless x is nil
    lst)))

(defface bad-face '((t (:foreground "White" :background "Red")))
  "Face for errors."
  :group 'latex-preview-pane)

(defun lpp/chomp (str)
  "Chomp leading and tailing whitespace from STR.

Returns modified STR."
  ;; NOTE: string-match returns index of start of first match. How is this used?
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    ;; NOTE: this changes global state of STR
    (setq str (replace-match "" t t str)))
  str)

(defun lpp/line-errors ()
  "Return list of line numbers from error message.

The lines are returned if they begin with the regexp l\\.[0-9]*.

NOTE: I don't know if these are the lines /with/ the errors or if it's just all
lines. Will need to look at the actual error message.

TODO: just based on this function's logic I feel like there has to be a
better way to achieve the desired result."
  ;; Save current buffer
  (let ((old-buff (current-buffer)))
    (set-buffer (get-buffer "*pdflatex-buffer*"))
    ;; Get error msg
    (let ((error-msg (buffer-substring (point-min) (point-max))))
      ;; Go back to previous buffer
      (set-buffer old-buff)
      ;; lpp: get all the line numbers.
      (mapcar
       (lambda (what) (lpp/chomp (substring what 2))) ;; trim whitespace around first two characters of each line
       ;; List of lines in error-msg which start with regexp
       (latex-pp-filter
        (lambda (what)
          (eq (string-match "l\\.[0-9]*" what) 0)) ;; eq if string-match index is 0
        (split-string error-msg "\n"))))))


(defun lpp/line-errors-to-layovers (errors)
  "Apply an overlay of `bad-face' to each line in ERRORS."
  (mapcar
   (lambda
     (what) ;; `what' is the string of each line number of each error (elt of list ERRORS)
     (let
         ((line (string-to-number what))) ;; convert line number string to number
       (let (layoverStart
             layoverEnd)
         (goto-char (point-min)) ;; start at beginning
         (forward-line (1- line)) ;; go backward by magnitude of (1-line)
         (setq layoverStart (point))
         (setq layoverEnd (+ 1 (line-end-position)))
         ;; lpp: (message (format "Adding Layover On Line: %d, Start: %d, End: %d" line layoverStart layoverEnd))
         ;; lpp: create the layover
         (overlay-put (make-overlay layoverStart layoverEnd) 'face 'bad-face))))
   errors))

(defun lpp/display-backtrace ()
  "Insert error message and last backtrace into lpp error buffer."
  (let ((old-buff (current-buffer)))
    (progn
      ;; Display lpp error buffer in preview window
      (set-window-buffer
       (lpp/window-containing-preview)
       (get-buffer-create "*Latex Preview Pane Errors*"))
      ;; Switch to lpp error buffer for editing
      (set-buffer (get-buffer "*Latex Preview Pane Errors*"))
      ;; Erase lpp error buffer and insert `message-no-preview-yet'
      (erase-buffer)
      (insert message-no-preview-yet)
      (set-buffer (get-buffer "*Latex Preview Pane Errors*"))
      ;; Insert `lpp/last-backtrace'
      (insert (lpp/last-backtrace))
      ;; Switch to previous buffer
      (set-buffer old-buff))))

;; Why is this here?
(boundp 'TeX-master)

(defun lpp/is-tex (string)
  "Check if STRING matches a given regexp.

Returns t if STRING matches `(: , \".tex\" eos)'."
  (and (string-match (rx-to-string `(: , ".tex" eos) t) string) t))

(defun lpp/auctex-buffer-file-name ()
  "Check TeX-master variable and return its name."
  (cond
   ;; If 'TeX-master is not bound, message:
   ((eq (boundp 'TeX-master) nil)
    (message
     "The TeX master variable is not defined. To use this mode you must be using AUCTeX on this buffer."))
   ;; If TeX-master is nil, message:
   ((eq TeX-master nil)
    (message "AUCTeX is enabled but TeX-master is not yet set. Please set it."))
   ;; If TeX-master is t, return buffer-file-name.
   ((eq TeX-master t)
    buffer-file-name)
   (t
    (if (lpp/is-tex TeX-master)
        TeX-master ;; return TeX-master if lpp/is-tex
      (concat TeX-master ".tex"))))) ;; else append tex extension

(defun lpp/get-file ()
  "Prompt user for file path with completion and input history support."
  (read-file-name "Location of Master TeX File:"))


(defun lpp/prompt-and-save-buffer-file-name ()
  "Set `lpp-TeX-master' and ask user for it if necessary.

NOTE: still not sure how this integrates and matters to lpp."
  (progn
    (cond
     ;; If buffer-local master tex file is unbound, ask user for it.
     ((eq (boundp 'lpp-TeX-master) nil)
      (set (make-local-variable 'lpp-TeX-master) (lpp/get-file)))
     ;; If `lpp-TeX-master' is nil, ask user for it.
     ((eq lpp-TeX-master nil)
      (set (make-local-variable 'lpp-TeX-master) (lpp/get-file))))
    ;; If `lpp-TeX-master' is a tex file, return it, else give it the .tex ext and return it.
    (if (lpp/is-tex lpp-TeX-master)
        lpp-TeX-master
      (concat lpp-TeX-master ".tex"))))

(defun lpp/buffer-file-name ()
  "Return `buffer-file-name', TeX-master file name, or ask if neither are set."
  (if (eq latex-preview-pane-multifile-mode 'off)
      buffer-file-name
    (if (eq latex-preview-pane-multifile-mode 'auctex)
        (lpp/auctex-buffer-file-name)
      (lpp/prompt-and-save-buffer-file-name))))

;;
;; Take a string like "../main" and extract: the path leading UP
;;


(defun lpp/invoke-pdf-latex-command ()
  "Compile the tex buffer and put its output in `*pdflatex-buffer'."
  (let ((buff (expand-file-name (lpp/buffer-file-name)))
        ;; Bind `default-directory'.
        (default-directory (file-name-directory (expand-file-name) (lpp/buffer-file-name))))
    ;; Handle shell-escape-mode
    (if shell-escape-mode
        (call-process pdf-latex-command   ;; program
                      nil                 ;; infile
                      "*pdflatex-buffer*" ;; destination
                      nil                 ;; display
                      shell-escape-mode   ;; args BUG: this evals to t and should be a string, "-shell-escape"
                      buff)               ;; args
      (call-process pdf-latex-command nil "*pdflatex-buffer*" nil buff))))


;;;###autoload
(defun latex-preview-pane-update-p ()
  "."
  ;; If invoke compilation command evals to 1,
  (if (eq (lpp/invoke-pdf-latex-command) 1)
      ;; Then, show backtrace and add error overlays
      (progn
        (lpp/display-backtrace)
        (remove-overlays)
        (lpp/line-errors-to-layovers (lpp/line-errors)))
    ;; Else: bind pdf buffer name
    (let ((pdf-filename
           (replace-regexp-in-string "\.tex$" ".pdf" (lpp/buffer-file-name)))
          (tex-buff (current-buffer))
          (pdf-buff-name
           (replace-regexp-in-string
            "\.tex" ".pdf"
            (buffer-name (get-file-buffer (lpp/buffer-file-name))))))
      ;; Remove overlays from what current buffer?
      (remove-overlays)
      ;; lpp: if the file doesn't exist, say that the file isn't available due to error messages
      ;; If pdf file exists
      (if (file-exists-p pdf-filename)
          ;; If pdf buffer doesn't exist yet
          (if (eq (get-buffer pdf-buff-name) nil)
              ;; Then read pdf file into pdf-buff and shown pdf-buff.
              (let ((pdf-buff (find-file-noselect pdf-filename 'nowarn)))
                (buffer-disable-undo pdf-buff)
                (set-window-buffer (lpp/window-containing-preview) pdf-buff))
            ;; Else show pdf buffer and refresh it with disk file contents
            (progn
              (set-window-buffer (lpp/window-containing-preview) pdf-buff-name)
              (with-current-buffer pdf-buff-name
                (doc-view-revert-buffer nil t))))))))

;;
;; Mode definition
;;

(defvar latex-preview-pane-mode-map (make-keymap)
  "Latex preview pane keymap.")

(defun lpp/set-multifile-mode (mode)
  "Set `latex-preview-pane-multifile-mode' to MODE and make `lpp-TeX-master' nil."
  (progn
    ;; Set default of latex-preview-pane-multifile-mode to `mode'.
    (customize-set-variable 'latex-preview-pane-multifile-mode mode)
    ;; Set buffer-local `lpp-TeX-master' nil.
    (set (make-local-variable 'lpp-TeX-master) nil)))


;; Define pop-up menu, callable by `words-menu' with lpp keymap.
(easy-menu-define words-menu latex-preview-pane-mode-map
  "Menu for working with Latex Preview Pane."
  ;; Menu:
  '("LaTeX Preview Pane" ;; Menu bar item name
    ;; Menu items: [NAME CALLBACK ENABLE]
    ["LaTeX Preview Pane Actions" nil :active nil]

    ["Refresh Preview" latex-preview-pane-update]
    ["Open Preview in External Viewer" latex-preview-update]
    ["Disable LaTeX Preview Pane in this Buffer"
     (latex-preview-pane-mode 'toggle)]
    ["Customize LaTeX Preview Pane" (customize-group 'latex-preview-pane)]
    ["--" nil]
    ["Multi-File Mode" nil :active nil]
    ["Off"
     (lpp/set-multifile-mode 'off)
     :style radio
     :selected (eq latex-preview-pane-multifile-mode 'off)]
    ["Use AUCTeX/TeX-master"
     (lpp/set-multifile-mode 'auctex)
     :style radio
     :selected (eq latex-preview-pane-multifile-mode 'auctex)]

    ["Prompt"
     (lpp/set-multifile-mode 'prompt)
     :style radio
     :selected (eq latex-preview-pane-multifile-mode 'prompt)]))

;; Set keybindings for lpp.
(define-key latex-preview-pane-mode-map (kbd "M-p") 'latex-preview-pane-update)
(define-key latex-preview-pane-mode-map (kbd "s-p") 'latex-preview-pane-update)
(define-key latex-preview-pane-mode-map (kbd "M-P") 'latex-preview-update)
(define-key latex-preview-pane-mode-map (kbd "s-P") 'latex-preview-update)

;;;###autoload
(define-minor-mode latex-preview-pane-mode
  "Toggle Latex Preview Pane Mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When Latex Preview Pane mode is enabled, saving a latex file will cause
a PDF preview pane of your document to appear."
  ;; lpp: The initial value.
  :init-value nil
  ;; lpp: The indicator for the mode line.
  :lighter " Latex Preview Pane"
  ;; lpp: The minor mode bindings.
  :keymap latex-preview-pane-mode-map
  :group 'latex-preview-pane
  ;; lpp: if we are turning on the mode, init the view
  (if (and (boundp 'latex-preview-pane-mode) latex-preview-pane-mode)
      (init-latex-preview-pane)
    ;; lpp: otherwise, kill the window
    ;; TODO: figure out what else might need to be reset. Is the lack thereof
    ;; why lpp is buggy?
    (delete-window (lpp/window-containing-preview))))

;; lpp: set some messages for later
;; Set the default values of `message-latex-preview-pane-welcome' and `message-no-preview-yet'.
(let ((installation-dir
       (if load-file-name
           (file-name-as-directory (file-name-directory load-file-name))
         nil)))
  (defvar message-latex-preview-pane-welcome
    (lpp/get-message
     (expand-file-name "message-latex-preview-pane-welcome.txt"
                       installation-dir))
    "The `message-latex-preview-pane-welcome.txt' file as a string.")
  (defvar message-no-preview-yet
    (lpp/get-message
     (expand-file-name "message-no-preview-yet.txt" installation-dir))
    "The `message-no-preview-yet.txt' file as a string."))

(defgroup latex-preview-pane nil
  "Settings that are used in the Latex Preview Pane."
  :group 'latex-preview-pane)

(defcustom pdf-latex-command "pdflatex"
  "The command to produce a PDF file from a latex document."
  :type 'string
  :group 'latex-preview-pane)

(defcustom shell-escape-mode nil
  "Should the pdflatex command use shell escaping?"
  ;; NOTE: this doesn't seem to work.
  :type
  '(choice
    (const :tag "Use shell escaping (-shell-escape)" "-shell-escape")
    (const :tag "Do not use shell escaping" nil))
  :group 'latex-preview-pane)


(defcustom preview-orientation 'right
  "The side of the tex buffer with respect to which pane should be displayed.

Valid values are above, below, left, or right. Defaults to right."
  :type
  '(choice
    (const :tag "Display preview on right" right)
    (const :tag "Display preview on left" left)
    (const :tag "Display preview above" above)
    (const :tag "Display preview below" below))
  :group 'latex-preview-pane)


(defcustom latex-preview-pane-multifile-mode 'off
  "Options for how lpp to handle multifile projects.

LaTeX Preview Pane can support multifile TeX projects.
This variable tells LPP how to behave in different situations."
  :type
  '(choice
    (const :tag "Off" off)
    (const :tag "Use AUCTeX (via the TeX-master variable)" auctex)
    (const :tag "Prompt" prompt))
  :group 'latex-preview-pane)

(defcustom latex-preview-pane-use-frame nil
  "Whether preview should open in new frame."
  :type 'boolean
  :group 'latex-preview-pane)

;;
;; Some utility functions
;;

(defun lpp/packing-list ()
  "List of lpp files for packaging (?)."
  '("README"
    "README.md"
    "latex-preview-pane-pkg.el"
    "latex-preview-pane.el"
    "message-latex-preview-pane-welcome.txt"
    "message-no-preview-yet.txt"
    "ss-error.PNG"
    "ss.PNG"))

;; lpp: for making distributions
(defun lpp/make-dist ()
  "Make tarball of `lpp/packing-list' files."
  (let ((dist-dir
         (concat "latex-preview-pane-" latex-preview-pane-current-version)))
    (let ((dist-file (concat dist-dir ".tar")))

      ;; lpp: (call-process "rm" nil "*dist-buffer*" nil ("-fr" dist-dir))
      (call-process "mkdir" nil "*dist-buffer*" nil dist-dir)

      ;; lpp: copy it over
      (mapc
       (lambda (f)
         (progn
           (message (concat "Copying " f "..."))
           (call-process "cp" nil "*dist-buffer*" nil f dist-dir)))
       (lpp/packing-list))


      (call-process "tar"
                    nil
                    "*dist-buffer*"
                    nil
                    "-cvf"
                    dist-file
                    (concat dist-dir "/"))
      (message (concat "Package " dist-file " created.")))))

;; lpp: (lpp/make-dist)


(provide 'latex-preview-pane)


;;; latex-preview-pane.el ends here
