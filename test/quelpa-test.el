;;; quelpa-test.el --- Tests for quelpa  -*- lexical-binding: t; -*-

(require 'quelpa)
(require 'ert)

;;; Test infrastructure

(defmacro quelpa-test-sandbox (&rest body)
  "Run BODY with all quelpa/package paths redirected to a temp directory.
Creates a fresh temporary `user-emacs-directory' so that tests never
install packages into the real Emacs configuration.  The sandbox
directory is deleted when BODY finishes (or signals)."
  (declare (indent 0) (debug t))
  `(let* ((sandbox (make-temp-file "quelpa-test-" t))
          (user-emacs-directory (file-name-as-directory sandbox))
          (quelpa-dir (expand-file-name "quelpa" user-emacs-directory))
          (quelpa-melpa-dir (expand-file-name "melpa" quelpa-dir))
          (quelpa-build-dir (expand-file-name "build" quelpa-dir))
          (quelpa-packages-dir (expand-file-name "packages" quelpa-dir))
          (quelpa-persistent-cache-file (expand-file-name "cache" quelpa-dir))
          (quelpa-melpa-recipe-stores (list (expand-file-name
                                            "recipes" quelpa-melpa-dir)))
           (package-user-dir (expand-file-name "elpa" user-emacs-directory))
           ;; Reset state so `quelpa-setup-p' re-initializes.
          (quelpa-initialized-p nil)
          (quelpa-cache nil)
          (package-alist nil))
     ;; Ensure the recipes directory exists so `directory-files'
     ;; inside `quelpa-get-melpa-recipe' doesn't signal.
     (make-directory (car quelpa-melpa-recipe-stores) t)
     (unwind-protect
         (progn ,@body)
       (delete-directory sandbox t))))

(defmacro quelpa-deftest (name arglist &rest body)
  "Define Quelpa ERT test.
Defines ERT test with `quelpa-' prepended to NAME and
`quelpa-setup-p' as a precondition to BODY.  ARGLIST is passed to
`ert-deftest', which see.

Each test runs inside `quelpa-test-sandbox', so packages are
installed into a disposable temporary directory."
  (declare (indent 2))
  (let ((name (intern (concat "quelpa-" (symbol-name name))))
        (name-async (intern (concat "quelpa-" (symbol-name name) "-async")))
        (ert-plist (cl-loop while (keywordp (car body))
                            collect (pop body)
                            collect (pop body))))
    `(progn
       (ert-deftest ,name ()
         ,@ert-plist
         (quelpa-test-sandbox
           (should (quelpa-setup-p))
           (cl-macrolet ((should-install (quelpa-args)
                           (let* ((name (pcase quelpa-args
                                          ((pred atom) quelpa-args)
                                          (`((,name . ,_) . ,_) name)
                                          (`(,(and name (pred atom)) . ,_) name))))
                             `(progn
                                (quelpa ',quelpa-args)
                                (should (package-installed-p ',name))))))
             ,@body)))

       (ert-deftest ,name-async ()
         ,@ert-plist
         (quelpa-test-sandbox
           (should (quelpa-setup-p))
           (cl-macrolet ((should-install (quelpa-args)
                           (let* ((name (pcase quelpa-args
                                          ((pred atom) quelpa-args)
                                          (`((,name . ,_) . ,_) name)
                                          (`(,(and name (pred atom)) . ,_) name))))
                             `(progn
                                (quelpa ',quelpa-args)
                                (should (package-installed-p ',name))))))
             (let ((quelpa-async-p t)
                   (quelpa--in-async-context t))
               ,@body)))))))

;;; Unit tests (mocked, no network)

(ert-deftest quelpa-build--parse-count-test ()
  "Parse numeric counts with safe fallback."
  (should (= (quelpa-build--parse-count "17") 17))
  (should (= (quelpa-build--parse-count "rev 0" 1) 1))
  (should (= (quelpa-build--parse-count "count: 41 patches") 41))
  (should (= (quelpa-build--parse-count "not a number") 0))
  (should (= (quelpa-build--parse-count nil) 0)))

(ert-deftest quelpa-build--melpa-version-test ()
  "Build MELPA generated versions as date plus count."
  (should (equal (quelpa-build--melpa-version "20260102.122345" 17)
                 "20260102.17"))
  (should (equal (quelpa-build--melpa-version "20260102.1" 0)
                 "20260102.0"))
  (should (equal (quelpa-build--melpa-version "20260102.122345" nil)
                 "20260102.0"))
  (should (equal (quelpa-build--melpa-version "20260102.122345" "bad")
                 "20260102.0")))

(ert-deftest quelpa-build--run-process-count-test ()
  "Process count helper returns parsed counts or zero."
  (cl-letf (((symbol-function 'quelpa-build--run-process)
             (lambda (_dir _command &rest _args)
               (insert "23\n")
               t)))
    (should (= (quelpa-build--run-process-count "/tmp/repo" "vcs" "count") 23)))
  (cl-letf (((symbol-function 'quelpa-build--run-process)
             (lambda (&rest _args)
               (error "boom"))))
    (should (= (quelpa-build--run-process-count "/tmp/repo" "vcs" "count") 0))))

(ert-deftest quelpa-file-version-directory-count-test ()
  "Directory versions use date/count generated versions."
  (should (equal (quelpa-file-version "/tmp/pkg" 'directory nil "20260102.122345")
                 "20260102.0")))

(ert-deftest quelpa-file-version-single-file-count-test ()
  "Single-file generated suffixes use date/count generated versions."
  (cl-letf (((symbol-function 'quelpa-get-package-desc)
             (lambda (_file-path) nil)))
    (should (equal (quelpa-file-version "/tmp/pkg.el" 'file nil "20260102.122345")
                   "0pre0.20260102.0"))))

(ert-deftest quelpa-build--grab-wiki-file-count-test ()
  "Wiki last-modified timestamps become date/count generated versions."
  (cl-letf (((symbol-function 'quelpa-build--url-copy-file)
             (lambda (_url _newname &optional _ok-if-already-exists)
               "Last-Modified: Fri, 02 Jan 2026 12:23:45 GMT\n"))
            ((symbol-function 'file-attributes)
             (lambda (_filename) '(nil nil nil nil nil nil nil 1))))
    (should (equal (quelpa-build--grab-wiki-file "pkg.el")
                   "20260102.0"))))

(ert-deftest quelpa-build--vcs-version-count-test ()
  "Git checkout versions combine existing dates with VCS counts."
  (let ((dir (make-temp-file "quelpa-git-count-" t))
        (config '(:fetcher git :url "https://example.invalid/pkg.git")))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" dir))
          (cl-letf (((symbol-function 'quelpa-build--git-repo)
                     (lambda (_dir _remote) "https://example.invalid/pkg.git"))
                    ((symbol-function 'quelpa-build--git-head-branch)
                     (lambda (_dir) "master"))
                    ((symbol-function 'quelpa-build--update-git-to-ref)
                     (lambda (&rest _args) t))
                    ((symbol-function 'quelpa-build--expand-source-file-list)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'quelpa-build--run-process-match)
                     (lambda (_regexp _dir command &rest args)
                       (when (and (equal command "git")
                                  (equal args '("rev-parse" "--is-shallow-repository")))
                         "false")))
                    ((symbol-function 'quelpa-build--run-process-count)
                     (lambda (_dir _command &rest _args) 17))
                    ((symbol-function 'quelpa-build--run-process)
                     (lambda (_dir command &rest args)
                       (when (and (equal command "git")
                                  (member "log" args))
                         (insert "2026-01-02 12:23:45 +0000"))
                       t)))
            (let ((quelpa-build-stable nil)
                  (quelpa--git-version '(2 42)))
              (should (equal (quelpa-build--checkout-git 'pkg config dir)
                             "20260102.17")))))
      (delete-directory dir t))))

(ert-deftest quelpa-build--checkout-git-shallow-count-test ()
  "Git checkout versions use count zero for shallow repositories."
  (let ((dir (make-temp-file "quelpa-git-shallow-count-" t))
        (config '(:fetcher git :url "https://example.invalid/pkg.git"))
        count-called)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" dir))
          (cl-letf (((symbol-function 'quelpa-build--git-repo)
                     (lambda (_dir _remote) "https://example.invalid/pkg.git"))
                    ((symbol-function 'quelpa-build--git-head-branch)
                     (lambda (_dir) "master"))
                    ((symbol-function 'quelpa-build--update-git-to-ref)
                     (lambda (&rest _args) t))
                    ((symbol-function 'quelpa-build--expand-source-file-list)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'quelpa-build--run-process-match)
                     (lambda (_regexp _dir command &rest args)
                       (when (and (equal command "git")
                                  (equal args '("rev-parse" "--is-shallow-repository")))
                         "true")))
                    ((symbol-function 'quelpa-build--run-process-count)
                     (lambda (&rest _args)
                       (setq count-called t)
                       23))
                    ((symbol-function 'quelpa-build--run-process)
                     (lambda (_dir command &rest args)
                       (when (and (equal command "git")
                                  (member "log" args))
                         (insert "2026-01-02 12:23:45 +0000"))
                       t)))
            (let ((quelpa-build-stable nil)
                  (quelpa--git-version '(2 42)))
              (should (equal (quelpa-build--checkout-git 'pkg config dir)
                             "20260102.0"))
              (should-not count-called))))
      (delete-directory dir t))))

(ert-deftest quelpa-build--checkout-hg-ancestor-count-test ()
  "Mercurial checkout asks Mercurial to count ancestors."
  (let ((dir (make-temp-file "quelpa-hg-count-" t))
        (config '(:fetcher hg :url "https://example.invalid/pkg"))
        captured-args)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".hg" dir))
          (cl-letf (((symbol-function 'quelpa-build--hg-repo)
                     (lambda (_dir) "https://example.invalid/pkg"))
                    ((symbol-function 'quelpa-build--expand-source-file-list)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'quelpa-build--run-process-count)
                     (lambda (_dir command &rest args)
                       (setq captured-args (cons command args))
                       3))
                    ((symbol-function 'quelpa-build--run-process)
                     (lambda (_dir command &rest args)
                       (when (and (equal command "hg")
                                  (member "log" args))
                         (insert "2026-01-02 12:23 +0000"))
                       t)))
            (let ((quelpa-build-stable nil))
              (should (equal (quelpa-build--checkout-hg 'pkg config dir)
                             "20260102.3"))
              (should (equal (car captured-args) "hg"))
              (should (equal (cdr captured-args)
                             '("log" "-r" "."
                               "--template" "{revset('ancestors(.)')|count}\n"))))))
      (delete-directory dir t))))

(ert-deftest quelpa-build--elpa-release-prefix-test ()
  "Release prefix uses package metadata or 0.0.0 fallback."
  (cl-letf (((symbol-function 'quelpa-build--pkg-info)
             (lambda (_name _files _build-dir)
               ["pkg" nil nil (1 2)])))
    (should (equal (quelpa-build--elpa-release-prefix 'pkg nil "/tmp/build")
                   '(1 2 0))))
  (cl-letf (((symbol-function 'quelpa-build--pkg-info)
             (lambda (_name _files _build-dir) nil)))
    (should (equal (quelpa-build--elpa-release-prefix 'pkg nil "/tmp/build")
                   '(0 0 0)))))

(ert-deftest quelpa-build--elpa-release-prefix-invalid-metadata-test ()
  "Malformed release metadata falls back to 0.0.0."
  (dolist (release (list nil "1.2" '(1 "2") '(1 . 2)))
    (cl-letf (((symbol-function 'quelpa-build--pkg-info)
               (lambda (_name _files _build-dir)
                 (vector "pkg" nil nil release))))
      (should (equal (quelpa-build--elpa-release-prefix 'pkg nil "/tmp/build")
                     '(0 0 0))))))

(ert-deftest quelpa-build-elpa-version-count-test ()
  "ELPA versions append the new MELPA date/count suffix."
  (let ((captured-version nil))
    (cl-letf (((symbol-function 'quelpa-checkout)
               (lambda (_rcp _dir) "20260102.17"))
              ((symbol-function 'quelpa-build--config-file-list)
               (lambda (_config) nil))
              ((symbol-function 'quelpa-build--pkg-info)
               (lambda (_name _files _build-dir)
                 ["pkg" nil nil (1 2)]))
              ((symbol-function 'quelpa-build-package)
               (lambda (_name version _files _build-dir _packages-dir)
                 (setq captured-version version)
                 '(pkg [(0) nil nil tar])))
              ((symbol-function 'quelpa-archive-file-name)
               (lambda (_archive-entry) "pkg-1.2.0.20260102.17.tar")))
      (let ((quelpa-version-type 'elpa)
            (quelpa-stable-p nil))
        (should (equal (quelpa-build '(pkg :fetcher git))
                       "pkg-1.2.0.20260102.17.tar"))
        (should (equal captured-version "1.2.0.20260102.17"))))))

(ert-deftest quelpa-build-elpa-version-count-fallback-test ()
  "ELPA versions use 0.0.0 when release metadata is unavailable."
  (let ((captured-version nil))
    (cl-letf (((symbol-function 'quelpa-checkout)
               (lambda (_rcp _dir) "20260102.17"))
              ((symbol-function 'quelpa-build--config-file-list)
               (lambda (_config) nil))
              ((symbol-function 'quelpa-build--pkg-info)
               (lambda (_name _files _build-dir) nil))
              ((symbol-function 'quelpa-build-package)
               (lambda (_name version _files _build-dir _packages-dir)
                 (setq captured-version version)
                 '(pkg [(0) nil nil tar])))
              ((symbol-function 'quelpa-archive-file-name)
               (lambda (_archive-entry) "pkg-0.0.0.20260102.17.tar")))
      (let ((quelpa-version-type 'elpa)
            (quelpa-stable-p nil))
        (should (equal (quelpa-build '(pkg :fetcher git))
                       "pkg-0.0.0.20260102.17.tar"))
        (should (equal captured-version "0.0.0.20260102.17"))))))

(ert-deftest quelpa-build--checkout-cvs-count-current-output-test ()
  "CVS checkout counts only revisions from the current command output."
  (let* ((dir (make-temp-file "quelpa-cvs-count-" t))
         (cvs-dir (expand-file-name "CVS" dir))
         (config '(:fetcher cvs :url ":pserver:example.invalid:/cvsroot"
                            :module "pkg")))
    (unwind-protect
        (progn
          (make-directory cvs-dir)
          (with-temp-file (expand-file-name "Root" cvs-dir)
            (insert ":pserver:example.invalid:/cvsroot"))
          (with-temp-file (expand-file-name "Repository" cvs-dir)
            (insert "pkg"))
          (with-temp-file (expand-file-name "Entries" cvs-dir)
            (insert "/pkg.el/1.2/Fri Jan  2 12:23:45 2026//\n"))
          (with-current-buffer (get-buffer-create "*quelpa-build-checkout*")
            (erase-buffer)
            (insert "revision 9.9\nrevision 9.8\n"))
          (cl-letf (((symbol-function 'quelpa-build--expand-source-file-list)
                     (lambda (&rest _args) '("pkg.el")))
                    ((symbol-function 'quelpa-build--run-process)
                     (lambda (_dir command &rest args)
                       (when (and (equal command "cvs")
                                  (member "log" args))
                         (insert "RCS file: /cvsroot/pkg/pkg.el,v\n"
                                 "revision 1.2\n"))
                       t)))
            (let ((quelpa-build-stable nil))
              (should (equal (quelpa-build--checkout-cvs 'pkg config dir)
                             "20260102.1")))))
      (when (get-buffer "*quelpa-build-checkout*")
        (kill-buffer "*quelpa-build-checkout*"))
      (delete-directory dir t))))

(quelpa-deftest expand-recipe ()
  "Should be expanding correctly as return value and into buffer."
  (let ((package-build-rcp '(package-build :repo "melpa/package-build" :fetcher github)))
    ;; Write a mock recipe file so `quelpa-get-melpa-recipe' can find it.
    (with-temp-file (expand-file-name "package-build"
                                      (car quelpa-melpa-recipe-stores))
      (prin1 package-build-rcp (current-buffer)))
    (should
     (equal
      (quelpa-expand-recipe 'package-build)
      package-build-rcp))
    (should
     (equal
      (with-temp-buffer
        (cl-letf (((symbol-function 'quelpa-interactive-candidate)
                   (lambda ()
                     (interactive)
                     'package-build)))
          (call-interactively 'quelpa-expand-recipe))
        (buffer-string))
      (prin1-to-string package-build-rcp)))))

(quelpa-deftest arg-rcp ()
  "Ensure `quelpa-arg-rcp' always returns the correct RCP format."
  (let ((quelpa-rcp '(quelpa :repo "quelpa/quelpa" :fetcher github))
        (package-build-rcp '(package-build :repo "melpa/package-build" :fetcher github)))
    ;; Write a mock recipe file so bare-name lookups work.
    (with-temp-file (expand-file-name "package-build"
                                      (car quelpa-melpa-recipe-stores))
      (prin1 package-build-rcp (current-buffer)))
    (should (equal (quelpa-arg-rcp quelpa-rcp)
                   quelpa-rcp))
    (should (equal (quelpa-arg-rcp 'package-build)
                   package-build-rcp))
    (should (equal (quelpa-arg-rcp '(package-build))
                   package-build-rcp))
    (should (equal (quelpa-arg-rcp "package-build")
                   package-build-rcp))))

(quelpa-deftest version>-p ()
  "Passed version should correctly be tested against the mocked
`package-alist' and built-in packages."
  (let ((package-alist (cond ((functionp 'package-desc-vers)
                              ;; old package-alist format
                              '((quelpa . [(1 0 0 20140406 42)
                                           ((package-build (0)))
                                           "Emacs Lisp packages built directly from source"])))
                             ((version< emacs-version "26.1")
                              ;; New package-alist format, but before Emacs 26.
                              '((quelpa
                                 [cl-struct-package-desc
                                  quelpa
                                  (1 0 0 20140406 42)
                                  "Emacs Lisp packages built directly from source"
                                  ((package-build (0))) nil nil "test" nil nil])))
                             ((functionp 'record)
                              ;; Emacs 26+ records.
                              `((quelpa
                                 ,(record 'package-desc
                                          'quelpa
                                          '(1 0 0 20140406 42)
                                          "Emacs Lisp packages built directly from source"
                                          '((package-build (0))) nil nil "test" nil nil)))))))
    (should (quelpa-version>-p 'quelpa nil))
    (should (quelpa-version>-p 'quelpa "0"))
    (should-not (quelpa-version>-p 'quelpa "1.0.0.20140406.42"))
    (should (quelpa-version>-p 'quelpa "1.0.0.20140406.41"))
    (should (quelpa-version=-p '(quelpa :version-type elpa) "20140406.42"))
    (should (quelpa-version=-p '(quelpa :version-type elpa) "20140406.42"))
    ;; For not installed package, it should always older than any package version
    (should-not (quelpa-version>-p 'foo "0snapshot0.0.1"))
    (let ((package--builtin-versions '((foobar . (20140406 42)))))
      (should (quelpa-version>-p 'foobar "0"))
      (should-not (quelpa-version>-p 'foobar "20140406.42"))
      (should (quelpa-version>-p 'foobar "20140406.41")))))

(quelpa-deftest check-hash ()
  "Make sure that old file hash is correctly compared with the new one
  and only when it has changed the new time-stamp is returned."
  (cl-letf* ((stamp-info '("20140413.90742" . "7e4c099e65d254f62e64b581c42ddeb3c487064b"))
             (hash "4935a306e358cbd0d9bd200e13ceb1e44942b323")
             ((symbol-function 'quelpa-build--read-from-file) (lambda (file) stamp-info))
             ((symbol-function 'quelpa-build--dump) (lambda (content file)))
             ((symbol-function 'quelpa-build--expand-source-file-list) 'ignore)
             ((symbol-function 'secure-hash) (lambda (&rest args) hash))
             ((symbol-function 'delete-directory) 'ignore)
             ((symbol-function 'make-directory) 'ignore)
             ((symbol-function 'copy-directory) 'ignore)
             ((symbol-function 'copy-file) 'ignore))
    (should-not (equal (quelpa-check-hash 'foobar nil "/" "baz")
                       (car stamp-info)))
    (setq hash (cdr stamp-info))
    (should (equal (quelpa-check-hash 'foobar nil "/" "baz")
                   "20140413.0"))))

(quelpa-deftest cache ()
  "Ensure that installing a package with a different recipe will
update an existing cache item."
  (cl-letf ((quelpa-cache nil)
            ((symbol-function 'quelpa-package-install) (lambda (&rest _) '(1 0)))
            ((symbol-function 'quelpa--delete-obsoleted-package) 'ignore))
    (quelpa '(makey :fetcher github :repo "mickeynp/makey"))
    ;; Re-install by bare name.  `quelpa-arg-rcp' resolves the name
    ;; from `quelpa-melpa-recipe-stores'; include the cache so the
    ;; test doesn't depend on a real MELPA checkout.
    (let ((quelpa-melpa-recipe-stores
           (append quelpa-melpa-recipe-stores (list quelpa-cache))))
      (quelpa 'makey))
    (should (equal quelpa-cache '((makey :fetcher github :repo "mickeynp/makey"))))
    (quelpa '(makey :fetcher github :repo "foo/makey"))
    (should (equal quelpa-cache '((makey :fetcher github :repo "foo/makey"))))
    ))

(quelpa-deftest cache-regressions ()
  (cl-letf ((quelpa-cache nil)
            ((symbol-function 'quelpa-package-install) (lambda (&rest _) '(1 0)))
            ((symbol-function 'quelpa--delete-obsoleted-package) 'ignore))
    (quelpa '(multiple-cursors :fetcher github :repo "magnars/multiple-cursors.el" :stable t))
    (should (equal quelpa-cache '((multiple-cursors :fetcher github
                                                    :repo "magnars/multiple-cursors.el"
                                                    :stable t))))
    (let ((quelpa-stable-p t))
      (quelpa '(multiple-cursors :fetcher github
                                 :repo "magnars/multiple-cursors.el")
              :stable nil))
    (should (equal quelpa-cache '((multiple-cursors :fetcher github
                                                    :repo "magnars/multiple-cursors.el"))))))

(quelpa-deftest stable ()
  (cl-letf ((quelpa-cache nil)
            ((symbol-function 'quelpa-package-install) (lambda (&rest _) "1.0.0"))
            ((symbol-function 'package-delete) #'ignore))
    (quelpa '(2048-game :fetcher hg :url "https://hg.sr.ht/~zck/game-2048" :stable t))
    (quelpa '(elx :fetcher github :repo "emacscollective/elx") :stable t)
    (let ((quelpa-stable-p t))
      (quelpa '(imgur :fetcher github :repo "myuhe/imgur.el")))
    (should (equal (mapcar (lambda (item) (plist-get (cdr item) :stable))
                           quelpa-cache)
                   '(t t t)))))

(quelpa-deftest stable-upgrade ()
  ;; FIXME: Fails due to: (void-function quelpa-build--checkout-nil)
  :expected-result :failed
  (should-install (anzu :stable t))
  (should-install ((company :repo "company-mode/company-mode" :fetcher github)
                   :stable t))
  (should-install ((scss-mode :repo "antonj/scss-mode" :fetcher github)
                   :stable t))
  ;; Upgrade to non-stable.
  ;; TODO: Probably need to compare versions before and after.
  (should-install (anzu :upgrade t))
  (should-install ((company :repo "company-mode/company-mode" :fetcher github)
                   :upgrade t))
  (should-install ((scss-mode :repo "antonj/scss-mode" :fetcher github)
                   :upgrade t)))

;;;; Installation tests

;; These tests test installing packages from various kinds of repos.
;; Copied from the old ci/.emacs file.

(quelpa-deftest add-recipe ()
  (should (add-to-list 'quelpa-melpa-recipe-stores
                       '((let-alist :fetcher url
                                    :url "http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/let-alist.el"
                                    :version original)))))

(quelpa-deftest github ()
  "Test installing from GitHub."
  (should-install (anaconda-mode
                   :fetcher github :repo "proofit404/anaconda-mode"
                   :files ("*.el" "*.py" "vendor/jedi/jedi"
                           ("jsonrpc" "vendor/jsonrpc/jsonrpc/*.py"))))
  (should-install ag))

(quelpa-deftest gitlab ()
  (should-install gcmh))

(quelpa-deftest codeberg ()
  (should-install spell-fu))

(quelpa-deftest sourcehut ()
  (should-install myrddin-mode))

(quelpa-deftest hg ()
  (should-install 2048-game)
  ;; [2020-01-26 Sun 00:07] Nose repo is uncloneable due to TLS error,
  ;; but probably a local problem.
  ;; There is no package nose
  ;; (should-install nose)
  (should-install minesweeper))

(quelpa-deftest svn ()
  (should-install (confluence
                   :fetcher svn :url "https://svn.code.sf.net/p/confluence-el/code/trunk/"
                   :files ("confluence*.el" "*.dtd" "*.xsl"))))

(quelpa-deftest bzr ()
  (should-install (weblogger :fetcher bzr :url "lp:weblogger-el")))

(quelpa-deftest wiki ()
  (should-install (key-chord :fetcher wiki))
  (should-install (buffer-move :fetcher wiki))
  (should-install move-text))

(quelpa-deftest url ()
  ;; FIXME: For some reason this test seems to fail in batch mode but works interactively.
  :expected-result :failed
  (should-install (ox-rss
                   :url "https://code.orgmode.org/bzg/org-mode/raw/master/contrib/lisp/ox-rss.el"
                   :fetcher url))
  (should-install (rainbow-mode
                   :url "http://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/packages/rainbow-mode/rainbow-mode.el"
                   :fetcher url))
  (should-install (thingatpt+
                   :url "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/thingatpt+.el"
                   :fetcher url)))

(quelpa-deftest file ()
  ;; FIXME: Helm isn't checked out to this path, of course.
  :expected-result :failed
  (should-install (helm
                   :fetcher file
                   :files ("*.el" "emacs-helm.sh"
                           (:exclude "helm.el" "helm-lib.el" "helm-source.el" "helm-match-plugin.el" "helm-core-pkg.el"))
                   :path "~/emacs-packages/helm")))

;;;; Footer

(provide 'quelpa-tests)
