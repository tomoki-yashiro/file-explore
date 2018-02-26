;;; -*- emacs-lisp -*-

(require 'folder-mode)

(eval-when-compile
  (defvar folder-get-children-function nil)

  (defvar hl-line-overlay nil))

(defvar file-explorer-debug nil
  "* if non-nil show debug message.")

(defvar find-file-run-file-explorer nil)

(defface file-explorer-mark-file-face
  '((((class color) (background light))
     (:background "violet" :bold t))
    (t
     (:bold t)))
  ""
  :group 'file-explorer-faces)

(defvar file-explorer-font-lock-keywords
  '(("^\\*.*\n" . 'file-explorer-mark-file-face)))

(defvar file-explorer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'file-explorer-goto)
    (define-key map "O" 'file-explorer-other-window)

    (define-key map "!" 'file-explorer-shell-command)
    (define-key map "|" 'file-explorer-grep-find)

    (set-keymap-parent map folder-mode-map)
    map))

(defun file-explorer-info-short (path)
  "")

(defun file-explorer-info-long (path)
  (let ((attr (file-attributes path))
        (space " "))
    (concat (nth 8 attr)
            space
            (folder-truncate-string (int-to-string (nth 7 attr)) 8 t)
            space
            (format-time-string "%D %T" (nth 5 attr))
            space)))

(defun file-explorer-open (path)
  (or (folder-exist-p path)
      (let ((dir (file-explorer-dirname path)))
        (or (string= dir path)
            (file-explorer-open dir))))
  (folder-open-path path))

(defun file-explorer-goto (file)
  (interactive "f")
  (or (file-exists-p file)
      (error "file not exist."))
  (setq file (directory-file-name file))
  (let ((dir (file-name-directory file)))
    (if (string-match (concat "^" (regexp-quote default-directory)) dir)
        (progn
          (or (folder-exist-p (directory-file-name dir))
              (file-explorer-goto dir))
          (folder-open-path (directory-file-name dir))
          (folder-goto-path file)))))

(defun file-explorer-basename (path)
  (if (string-match "[^/]+$" path)
      (match-string 0 path)
    "/"))

(defun file-explorer-dirname (path)
  (if (string-match "/$" path)
      (setq path (substring path 0 (1- (length path)))))
  (if (string-match "\\(.+\\)/[^/]+$" path)
      (match-string 1 path)
    "/"))

(defun file-explorer-folder-child (children)
  (let (dirs files)
    (while children
      (if (file-directory-p (car children))
          (setq dirs (cons (car children) dirs))
        (setq files (cons (car children) files)))
      (setq children (cdr children)))
    (append
     (mapcar (lambda (dir)
               (folder-make-info dir t (file-explorer-basename dir)))
             (sort dirs 'string<))
     (mapcar (lambda (file)
               (folder-make-info file nil (file-explorer-basename file)))
             (sort files 'string<)))))

(defun file-explorer-make-info (dir files)
  (setq dir (file-name-as-directory dir))
  (let (full)
    (mapcar (lambda (f)
              (setq full (concat dir f))
              (folder-make-info full (file-directory-p full) f))
            files)))

(defun file-explorer-get-child (path)
  (setq path (file-name-as-directory path))
  (let (dirs files)
    (mapc (lambda (f)
            (if (file-directory-p (concat path f))
                (setq dirs (cons f dirs))
              (setq files (cons f files))))
          (directory-files path nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)" t))
    (file-explorer-make-info path (append (sort dirs 'string<)
                                          (sort files 'string<)))))

(defun file-explorer-process (path)
  (find-file path))

(defun file-explorer-get-files ()
  (mapcar 'file-relative-name
          (let ((files (folder-get-path)))
            (if (stringp files)
                (list files) files))))

(defun file-explorer-select (file)
  (if (<= (+ (window-hscroll) (window-width))
          (save-excursion
            (end-of-line 1)
            (current-column)))
      (message "%s" (buffer-substring-no-properties
                     (1+ (point)) (line-end-position 1)))))

(defun file-explorer-directory (file)
  (if (file-directory-p file)
      file
    (directory-file-name (file-name-directory file))))

(defun file-explorer-shell-command (command files)
  (interactive (list (read-string "Command: " nil nil t)
                     (folder-current-path)))
  (or (stringp command)
      (error "Command is not string"))
  (or files
      (error "File not selected."))
  (if (stringp files)
      (setq files (list files)))
  (shell-command (concat command " "
                         (mapconcat 'shell-quote-argument files " "))))

(defun file-explorer-grep-find (dir)
  (interactive (list (file-explorer-directory (folder-current-path))))
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (call-interactively 'grep-find)))

(defun file-explorer-noselect (top &optional name get-child-funcs info-functions)
  (when find-file-run-file-explorer
    (setq top (directory-file-name (expand-file-name top)))

    (or name
        (setq name "Explorer"))
    (or get-child-funcs
        (setq get-child-funcs '(file-explorer-get-child)))
    (or info-functions
        (setq info-functions
              '(file-explorer-info-short file-explorer-info-long)))

    (let* ((buffer-name (format "*%s %s*" name top))
           (buffer (get-buffer buffer-name)))

      (unless buffer
        (setq buffer (set-buffer (generate-new-buffer buffer-name)))
        (kill-all-local-variables)

        (setq major-mode 'file-explorer-mode
              mode-name name
              default-directory (file-name-as-directory top)
              buffer-read-only t
              truncate-lines t)

        (folder-setup (folder-make-info top t top)
                      get-child-funcs
                      info-functions
                      '(file-explorer-process)
                      )

        (use-local-map file-explorer-mode-map)

        (set (make-local-variable 'hl-line-overlay) (make-overlay 1 1))
        (overlay-put hl-line-overlay 'face 'underline)

        (hl-line-mode 1)
        (buffer-disable-undo)
        (set-buffer-modified-p nil)
        (setq buffer-read-only t))

      buffer)))

(defun file-explorer (top &optional name get-child-funcs info-functions)
  (interactive "DDir: ")
  (let* ((find-file-run-file-explorer t)
         (buffer (file-explorer-noselect top name
                                         get-child-funcs info-functions)))
    (switch-to-buffer buffer)))

(defun file-explorer-other-window (dir)
  (interactive (list
                (let ((default-directory
                        (file-explorer-directory (folder-current-path))))
                  (read-directory-name "Dir: "))))
  (file-explorer dir mode-name))

(add-hook 'folder-select-hook 'file-explorer-select)

(setq find-directory-functions
      (cons 'file-explorer-noselect find-directory-functions))

(provide 'file-explorer)
