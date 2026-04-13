;;; my-locate-library.el --- Fast library path resolution via pre-built index -*- lexical-binding: t; -*-

;; Author: Fujisawa Electric Management Office
;; URL: https://github.com/zawatton21/my-locate-library
;; Version: 0.1.0
;; Keywords: lisp, performance
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;;
;; This package accelerates `locate-library', `require', and `load' by
;; pre-building a symbol-property index of all files on `load-path'.
;;
;; Usage:
;;
;; 1. After `load-path' is finalized, call `my-locate-library-build'.
;; 2. Call `my-locate-library-enable' to install advice on
;;    `locate-library', `require', and `load'.
;; 3. Call `my-locate-library-disable' to revert to the original functions.
;; 4. If `load-path' changes, call `my-locate-library-rebuild'.

;;; Code:

(require 'seq)

(defvar my-locate-library-list nil
  "List of interned symbols whose property `my-locate-library-path' is set.
Used for cleanup when rebuilding.")

(defvar my-locate-library-load-path nil
  "Snapshot of `load-path' at build time.
Used to detect changes cheaply.")

(defconst my-locate-library-file-extensions
  (get-load-suffixes)
  "List of file extensions considered loadable.")

(defconst my-locate-library-file-regexp
  (concat "\\`\\(.*\\)\\("
          (mapconcat #'regexp-quote my-locate-library-file-extensions "\\|")
          "\\)\\'")
  "Regexp matching loadable files, capturing base name and extension.")

;;;###autoload
(defun my-locate-library-build ()
  "Build the path index for `my-locate-library'.

Scan all directories in `load-path' and store each file's full path
as a symbol property.  Call this after `load-path' is finalized."
  (setq my-locate-library-load-path load-path)
  (dolist (dir load-path)
    (when (file-directory-p dir)
      (let (files)
        (dolist (file (directory-files dir))
          (when (string-match my-locate-library-file-regexp file)
            (let ((curr-base (match-string 1 file))
                  (curr-ext (match-string 2 file)))
              (if (equal (car (car files)) curr-base)
                  (when (< (my-locate-library-ext-priority curr-ext)
                           (my-locate-library-ext-priority (cdr (car files))))
                    (setcdr (car files) curr-ext))
                (push (cons curr-base curr-ext) files)))))
        (dolist (base-ext files)
          (my-locate-library-set-path dir (car base-ext) (cdr base-ext)))))))

(defun my-locate-library-ext-priority (extension)
  "Return priority of EXTENSION as an integer.
Lower value means higher priority (e.g. .elc > .el)."
  (seq-position my-locate-library-file-extensions extension))

(defun my-locate-library-set-path (dir base ext)
  "Intern BASE and store DIR/BASE+EXT as its `my-locate-library-path' property.
First entry wins (respects `load-path' ordering)."
  (let ((sym (intern base)))
    (unless (get sym 'my-locate-library-path)
      (let ((path (file-name-concat dir (concat base ext))))
        (put sym 'my-locate-library-path path)
        (push sym my-locate-library-list)))))

;;;###autoload
(defun my-locate-library-clean ()
  "Remove all cached path properties and reset the index."
  (dolist (sym my-locate-library-list)
    (put sym 'my-locate-library-path nil))
  (setq my-locate-library-list nil))

;;;###autoload
(defun my-locate-library-rebuild ()
  "Clean and rebuild the path index."
  (my-locate-library-clean)
  (my-locate-library-build))

;;;###autoload
(defun my-locate-library (file)
  "Return the full path for FILE, or nil if not found.

FILE can be a symbol (faster) or a string.  Unlike `locate-library',
this returns instantly from the pre-built index.

If `load-path' has changed since the last build, automatically rebuild."
  (unless (eq load-path my-locate-library-load-path)
    (warn "load-path change detected on (my-locate-library %s)" file)
    (my-locate-library-rebuild))
  (get (if (stringp file) (intern file) file) 'my-locate-library-path))

;; ------------------------------------------------------------
;; Advice functions
;; ------------------------------------------------------------

(defun my-locate-library-advice (orig-fun
                                 library &optional
                                 nosuffix path interactive-call)
  "Around advice for `locate-library'.
Falls back to ORIG-FUN for non-standard calls."
  (if (or (not (stringp library))
          (file-name-extension library)
          (file-name-directory library)
          nosuffix path interactive-call)
      (funcall orig-fun library nosuffix path interactive-call)
    (my-locate-library (intern library))))

(defun my-locate-library-require-advice (orig-fun
                                         feature &optional filename noerror)
  "Around advice for `require'.
Supplies the cached path when FILENAME is not given."
  (unless filename
    (setq filename (my-locate-library feature)))
  (funcall orig-fun feature filename noerror))

(defconst my-locate-library-load-suffixes-with-nil
  (cons nil (get-load-suffixes))
  "Load suffixes including nil for files without extension.")

(defun my-locate-library-load-advice (orig-fun
                                      file &optional
                                      noerror nomessage nosuffix must-suffix)
  "Around advice for `load'.
Resolves FILE from the index when possible."
  (funcall orig-fun
           (or (and (stringp file)
                    (not nosuffix)
                    (not must-suffix)
                    (not (file-name-directory file))
                    (member (file-name-extension file)
                            my-locate-library-load-suffixes-with-nil)
                    (my-locate-library (file-name-base file)))
               file)
           noerror nomessage nosuffix must-suffix))

;; ------------------------------------------------------------
;; Enable / Disable
;; ------------------------------------------------------------

;;;###autoload
(defun my-locate-library-enable ()
  "Install advice to accelerate `locate-library', `require', and `load'.

Call `my-locate-library-build' first to populate the index.
Use `my-locate-library-disable' to revert."
  (advice-add #'locate-library :around #'my-locate-library-advice)
  (advice-add #'require :around #'my-locate-library-require-advice)
  (advice-add #'load :around #'my-locate-library-load-advice))

;;;###autoload
(defun my-locate-library-disable ()
  "Remove all advice installed by `my-locate-library-enable'."
  (advice-remove #'locate-library #'my-locate-library-advice)
  (advice-remove #'require #'my-locate-library-require-advice)
  (advice-remove #'load #'my-locate-library-load-advice))

(provide 'my-locate-library)
;;; my-locate-library.el ends here
